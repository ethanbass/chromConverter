#!/usr/bin/env python
"""Submit one run to OpenLab CDS 2.x and wait for it to finish.

    python olsubmit.py METHOD.amx --vial D1F-A1 --instrument HPLC-1 --project Demo

METHOD is an acquisition method (.amx), given as an absolute path or as a path
relative to the project's Methods folder. The script waits for the run to
finish and prints the path of the new .dx file on stdout, writing progress to
stderr. With --no-wait it prints the run ID and returns once the run is queued.

Exit status: 0 if the run completed, 1 if it was aborted, timed out or left no
.dx file, and 2 if it could not be submitted or the script failed. Stopping
the script does not stop the run, which stays in the OpenLab run queue.

The .dx search assumes the results folder is on a disk this PC can read, as on
a workstation with local storage.

Runs on the OpenLab PC with pythonnet (pip install pythonnet). In OpenLab 2.7
and 2.8 the acquisition automation assemblies are 32-bit, so they need a 32-bit
Python.

The calls to the OpenLab automation interface follow hplccon.py by Jesse
Hopkins, from the BioCAT beamline control software
(https://github.com/biocatiit/beamline-control-user, GPL-3).
"""

import argparse
import glob
import os
import struct
import sys
import threading
import time
import traceback
from collections import deque

DEFAULT_CONNECTION = "net.pipe://localhost/Agilent/OpenLAB/"
DEFAULT_AGILENT_DIR = r"C:\Program Files (x86)\Agilent Technologies"
GAC_DIR = r"C:\Windows\Microsoft.NET\assembly"
TERMINAL_STATUSES = {"completed", "aborted", "unexisting"}

ASSEMBLIES = [
    ("OpenLAB Services/Common", "Agilent.OpenLab.SharedServices", True),
    ("OpenLAB Services/Common", "Agilent.OpenLab.SharedServices.Common", True),
    ("OpenLAB Services/Server", "Agilent.OpenLab.Storage", True),
    (None, "Agilent.OpenLAB.Acquisition.AutomationInstrument", True),
    (None, "Agilent.OpenLAB.Acquisition.AutomationCore", True),
    ("OpenLAB Acquisition", "Agilent.OpenLab.Framework.DataAccess.CoreTypes",
     False),
    ("OpenLAB Acquisition", "Agilent.OpenLab.RawData", False),
]


class SubmitError(Exception):
    pass


def log(msg):
    print(time.strftime("%H:%M:%S"), msg, file=sys.stderr, flush=True)


def python_bits():
    return struct.calcsize("P") * 8


def find_assembly(name, folder, agilent_dir, gac_dir=GAC_DIR, bits=None):
    """Return the path to an assembly and the GAC folder it was found in."""
    if folder is not None:
        path = os.path.join(agilent_dir, folder, name + ".dll")
        return (path, None) if os.path.exists(path) else (None, None)
    bits = bits or python_bits()
    gacs = (["GAC_64", "GAC_MSIL", "GAC_32"] if bits == 64
            else ["GAC_32", "GAC_MSIL", "GAC_64"])
    for gac in gacs:
        hits = sorted(glob.glob(os.path.join(gac_dir, gac, name, "*",
                                             name + ".dll")))
        if hits:
            return hits[-1], gac
    return None, None


def load_assemblies(agilent_dir):
    try:
        import clr
    except ImportError:
        raise SubmitError("pythonnet is not installed: pip install pythonnet")

    bits = python_bits()
    for folder, name, required in ASSEMBLIES:
        path, gac = find_assembly(name, folder, agilent_dir, bits=bits)
        if path is None:
            if required:
                raise SubmitError(
                    "Could not find {}.dll. Is OpenLab CDS installed on this "
                    "PC? If it is installed elsewhere, use --agilent-dir."
                    .format(name))
            log("{}.dll not found; continuing without it".format(name))
            continue
        if (gac, bits) in (("GAC_32", 64), ("GAC_64", 32)):
            raise SubmitError(
                "{} is {}-bit, but this Python is {}-bit. Run the script with "
                "a {}-bit Python.".format(name, gac[-2:], bits, gac[-2:]))
        clr.AddReference(path)


def local_path(uri):
    try:
        return uri.LocalPath
    except AttributeError:
        from urllib.parse import unquote
        return unquote(uri.AbsolutePath)


def resolve_method_path(method, project_path):
    if not os.path.isabs(method):
        method = os.path.join(project_path, "Methods", method)
    if not method.lower().endswith(".amx"):
        method += ".amx"
    method = os.path.abspath(method)
    if not os.path.exists(method):
        raise SubmitError("Method not found: {}".format(method))
    return method


def pick_injector(sources, wanted=None):
    sources = list(sources)
    names = [s.DisplayName for s in sources]
    if wanted is not None:
        if wanted not in names:
            raise SubmitError("No injection source named {!r}. Available: {}"
                              .format(wanted, ", ".join(names)))
        return sources[names.index(wanted)]
    if "HipAls" in names:
        return sources[names.index("HipAls")]
    if len(sources) == 1:
        return sources[0]
    raise SubmitError("Several injection sources ({}); choose one with "
                      "--injector.".format(", ".join(names)))


def find_result(result_dir, since):
    files = [f for f in glob.glob(os.path.join(result_dir, "**", "*.dx"),
                                  recursive=True)
             if os.path.getmtime(f) >= since]
    return max(files, key=os.path.getmtime) if files else None


class Session:
    """A connection to one OpenLab instrument and project."""

    def __init__(self, connection, instrument, project, timeout_s=120):
        from Agilent.OpenLab.SharedServices import Connection
        from Agilent.OpenLAB.Acquisition.AutomationInstrument import (
            InstrumentController)

        conn = Connection(connection)
        ticket = conn.GetTicketContainer().ToString()

        instruments = {}
        for info in conn.Instruments.GetAllInstrumentsInfo():
            instruments[conn.Instruments.GetInstrument(info.Id).Name] = info.Id
        projects = {p.Name: p.Id for p in conn.Projects.GetAllProjectsInfo()}
        if instrument not in instruments:
            raise SubmitError("No instrument named {!r}. Available: {}".format(
                instrument, ", ".join(instruments)))
        if project not in projects:
            raise SubmitError("No project named {!r}. Available: {}".format(
                project, ", ".join(projects)))
        self.project_path = local_path(
            conn.Projects.GetProject(projects[project]).RootPath)

        self.events = deque()
        self._ready = threading.Event()
        self.controller = InstrumentController()
        self.controller.AppInitialized += self._on_initialized
        self.controller.RunRecordChanged += self._on_record
        self.controller.RunRecordCollectionChanged += self._on_collection

        log("connecting to {} ({})".format(instrument, project))
        self.controller.EstablishConnection(
            connection, ticket, "{}".format(instruments[instrument]),
            "{}".format(projects[project]))
        if not self._ready.wait(timeout_s):
            raise SubmitError("Timed out connecting to the instrument.")
        while not self.controller.IsConnected:
            time.sleep(0.1)
        time.sleep(1)

    def _on_initialized(self, source, args):
        self._ready.set()

    def _on_record(self, source, args):
        self.events.append([args.UpdatedRecord])

    def _on_collection(self, source, args):
        self.events.append(list(args.RunRecords))

    def drain_statuses(self, statuses):
        while self.events:
            for rec in self.events.popleft():
                statuses[rec.Id.ToString()] = rec.Status.ToString()

    def submit(self, method_path, vial, result_dir, injector=None,
               processing_method=None, injection_volume=None,
               sample_name=None, result_name=None):
        from Agilent.OpenLAB.Acquisition.AutomationCore import SingleRunParams

        params = SingleRunParams()
        params.AcquisitionMethod = method_path
        params.SampleLocation = vial
        params.SelectedInjection = pick_injector(
            self.controller.InjectionSources, injector)
        params.ResultPath = result_dir
        if processing_method is not None:
            params.ProcessingMethod = processing_method
        if injection_volume is not None:
            params.InjectionVolume = float(injection_volume)
        if sample_name is not None:
            params.SampleName = sample_name
        if result_name is not None:
            params.ResultFilename = result_name
        return self.controller.SubmitSingleRun(params).RunId.ToString()

    def wait(self, run_id, timeout_s=None, poll_s=0.5):
        statuses = {}
        last = None
        deadline = None if timeout_s is None else time.time() + timeout_s
        while True:
            self.drain_statuses(statuses)
            status = statuses.get(run_id)
            if status != last:
                log("run {}: {}".format(run_id, status))
                last = status
            if status is not None and status.lower() in TERMINAL_STATUSES:
                return status
            if deadline is not None and time.time() > deadline:
                return None
            time.sleep(poll_s)

    def close(self):
        self.controller.AppInitialized -= self._on_initialized
        self.controller.RunRecordChanged -= self._on_record
        self.controller.RunRecordCollectionChanged -= self._on_collection
        self.controller.Disconnect()


def parse_args(argv):
    p = argparse.ArgumentParser(
        description="Submit one run to OpenLab CDS and wait for it.")
    p.add_argument("method", help="acquisition method (.amx)")
    p.add_argument("--vial", required=True,
                   help="sample location, e.g. D1F-A1")
    p.add_argument("--instrument", default=os.environ.get("OLSUBMIT_INSTRUMENT"),
                   help="instrument name in the OpenLab Control Panel "
                        "(default: $OLSUBMIT_INSTRUMENT)")
    p.add_argument("--project", default=os.environ.get("OLSUBMIT_PROJECT"),
                   help="project name (default: $OLSUBMIT_PROJECT)")
    p.add_argument("--injection-volume", type=float,
                   help="injection volume in uL; defaults to the method's")
    p.add_argument("--sample-name")
    p.add_argument("--processing-method",
                   help="processing method (.pmx), absolute or relative to "
                        "the project's Methods folder")
    p.add_argument("--result-path",
                   help="results folder, absolute or relative to the "
                        "project's Results folder")
    p.add_argument("--result-name", help="result file name")
    p.add_argument("--injector",
                   help="injection source; defaults to HipAls or the only one")
    p.add_argument("--timeout", type=float, default=120,
                   help="minutes to wait for the run (default 120; 0 for no "
                        "limit)")
    p.add_argument("--no-wait", action="store_true",
                   help="print the run ID and return once the run is queued")
    p.add_argument("--connection", default=DEFAULT_CONNECTION)
    p.add_argument("--agilent-dir", default=DEFAULT_AGILENT_DIR)
    args = p.parse_args(argv)
    if args.instrument is None or args.project is None:
        p.error("--instrument and --project are required unless "
                "OLSUBMIT_INSTRUMENT and OLSUBMIT_PROJECT are set")
    return args


def main(argv=None):
    args = parse_args(argv)
    session = None
    try:
        load_assemblies(args.agilent_dir)
        session = Session(args.connection, args.instrument, args.project)
        method = resolve_method_path(args.method, session.project_path)
        processing = None
        if args.processing_method is not None:
            processing = os.path.abspath(os.path.join(
                session.project_path, "Methods", args.processing_method))
        result_dir = os.path.join(session.project_path, "Results",
                                  args.result_path or "")
        submitted_at = time.time()
        run_id = session.submit(method, args.vial, result_dir,
                                injector=args.injector,
                                processing_method=processing,
                                injection_volume=args.injection_volume,
                                sample_name=args.sample_name,
                                result_name=args.result_name)
        log("submitted {} as run {}".format(os.path.basename(method), run_id))
        if args.no_wait:
            print(run_id)
            return 0

        status = session.wait(run_id, args.timeout * 60 or None)
        if status is None:
            log("timed out after {} min; the run is still in the queue"
                .format(args.timeout))
            return 1
        if status.lower() != "completed":
            log("run ended with status {}".format(status))
            return 1
        result = find_result(result_dir, submitted_at - 5)
        if result is None:
            log("run completed, but no new .dx file was found under "
                + result_dir)
            return 1
        print(result)
        return 0
    except SubmitError as e:
        log("error: {}".format(e))
        return 2
    except Exception:
        log("error:\n" + traceback.format_exc())
        return 2
    finally:
        if session is not None:
            session.close()


if __name__ == "__main__":
    sys.exit(main())
