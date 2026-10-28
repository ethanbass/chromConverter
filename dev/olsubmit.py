#!/usr/bin/env python
"""Submit one run or a sequence to OpenLab CDS 2.x and wait for it to finish.

    python olsubmit.py METHOD.amx --vial D1F-A1 --instrument HPLC-1 --project Demo
    python olsubmit.py --sequence runs.csv --instrument HPLC-1 --project Demo
    python olsubmit.py TEMPLATE.amx --save-as new.amx --gradient 0:5,2:50,4:95 \
        --stop-time 5 --vial D1F-A1 --instrument HPLC-1 --project Demo
    python olsubmit.py new.amx --vial D1F-A1 --append RUN_ID --wait-row 30 \
        --result-path loop --result-name run2 --instrument HPLC-1 --project Demo

METHOD is an acquisition method (.amx), given as an absolute path or as a path
relative to the project's Methods folder. The script waits for the run to
finish and prints the path of the new .dx file on stdout, writing progress to
stderr. With --sequence it prints one .dx path per row of the CSV, in row
order, and an empty line for a row that left no .dx file. With --no-wait it
prints the run ID and returns once the run is queued.

With --save-as, the script first copies METHOD to a new method, loads the copy
in OpenLab, changes the settings given (--flow, --stop-time, --post-time,
--temperature, --gradient) and saves it. OpenLab writes the file, so its
checksums and reports stay valid. --edit-only prints the new method's path
without submitting it.

--wait-row ends the run or sequence with a wait row, which keeps the sequence
open: the script returns when the sequence reaches it, and prints the run ID on
the first line of stdout, before the .dx paths. --append RUN_ID adds METHOD or
the rows of --sequence to that sequence while it waits, ends the wait, and
returns once the new rows have run. Give the same --result-path each time, and
a new --wait-row to keep the sequence open for the next call.

Exit status: 0 if the run completed, 1 if it was aborted, timed out or left no
.dx file, and 2 if it could not be submitted or the script failed. Stopping
the script does not stop the run, which stays in the OpenLab run queue.

With --shutdown-method, a run that is aborted or times out is followed by a
shutdown run with that method. --shutdown-now submits the shutdown run alone. An abort can leave the pump at settings that
are in no method: after a sample tray error, Norbert II's pump ran at about
1 mL/min until the next run started.

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
import csv
import glob
import os
import shutil
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
WAITING = "waiting"
CHANNELS = (2, 3, 4)
SEQUENCE_COLUMNS = ("method", "vial", "injection_volume", "sample_name",
                    "processing_method", "result_name")

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


def dotnet_details(e):
    """Return a .NET exception's full text and the fault details of its inner
    exceptions, or "" for a Python exception."""
    if not hasattr(e, "ToString"):
        return ""
    lines = [e.ToString()]
    try:
        inner = list(getattr(e, "InnerExceptions", None) or [])
        if getattr(e, "InnerException", None) is not None and not inner:
            inner = [e.InnerException]
        for x in inner:
            detail = getattr(x, "Detail", None)
            if detail is None:
                continue
            for prop in detail.GetType().GetProperties():
                value = prop.GetValue(detail)
                if value is not None and not isinstance(value, str) and \
                        hasattr(value, "__iter__"):
                    value = list(value)
                lines.append("fault {}: {}".format(prop.Name, value))
    except Exception as err:
        lines.append("could not read the fault details: {}".format(err))
    return "\n".join(lines)


def local_path(uri):
    try:
        return uri.LocalPath
    except AttributeError:
        from urllib.parse import unquote
        return unquote(uri.AbsolutePath)


def resolve_method_path(method, project_path, ext=".amx", new=False):
    if not os.path.isabs(method):
        method = os.path.join(project_path, "Methods", method)
    if not method.lower().endswith(ext):
        method += ext
    method = os.path.abspath(method)
    if new and os.path.exists(method):
        raise SubmitError("Method already exists: {}".format(method))
    if not new and not os.path.exists(method):
        raise SubmitError("Method not found: {}".format(method))
    return method


def parse_gradient(text):
    """Parse TIME:B[:C[:D]],... into [(time, [B, C, D]), ...]."""
    steps = []
    for item in text.split(","):
        try:
            values = [float(x) for x in item.split(":")]
        except ValueError:
            values = []
        if not 2 <= len(values) <= 4:
            raise argparse.ArgumentTypeError(
                "{!r} is not TIME:B[:C[:D]]".format(item))
        pct = values[1:] + [0.0] * (4 - len(values))
        if any(p < 0 for p in pct) or sum(pct) > 100:
            raise argparse.ArgumentTypeError(
                "at {} min, the percentages of B, C and D must not be "
                "negative or add up to more than 100".format(values[0]))
        steps.append((values[0], pct))
    times = [t for t, _ in steps]
    if times[0] != 0 or any(a >= b for a, b in zip(times, times[1:])):
        raise argparse.ArgumentTypeError(
            "times must start at 0, for the starting composition, and "
            "increase")
    return steps


def op_errors(result):
    return "; ".join(result.AllErrorMessages or []) or "none"


def id_values(pairs):
    from System import Array
    from Agilent.OpenLAB.Acquisition.AutomationCore import ResourceIdAndValue
    items = []
    for pid, value in pairs:
        item = ResourceIdAndValue()
        item.PropertyId = pid
        item.PropertyValue = value
        items.append(item)
    return Array[ResourceIdAndValue](items)


def as_id_values(value):
    """Turn a nested ResourceValueAndResult[] into the ResourceIdAndValue[]
    that SetMethodResourceProperties can serialize."""
    if value is None or isinstance(value, (str, int, float, bool)):
        return value
    return id_values([(v.PropertyId, as_id_values(v.PropertyValue))
                      for v in value])


def pick_injector(sources, wanted=None):
    if wanted is not None and wanted.lower() == "none":
        return None
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


def read_sequence(path, project_path, defaults):
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        reader.fieldnames = [h.strip().lower()
                             for h in reader.fieldnames or ()]
        unknown = set(reader.fieldnames or ()) - set(SEQUENCE_COLUMNS)
        if unknown:
            raise SubmitError("Unknown columns in {}: {}. Allowed: {}".format(
                path, ", ".join(sorted(unknown)), ", ".join(SEQUENCE_COLUMNS)))
        rows = [{k: v.strip() for k, v in row.items()
                 if k in SEQUENCE_COLUMNS and v and v.strip()}
                for row in reader]
    rows = [row for row in rows if row]
    if not rows:
        raise SubmitError("{} has no rows".format(path))
    for i, row in enumerate(rows, 1):
        for key, value in defaults.items():
            row.setdefault(key, value)
        if "method" not in row or "vial" not in row:
            raise SubmitError("Row {} of {} needs a method and a vial"
                              .format(i, path))
        row["method"] = resolve_method_path(row["method"], project_path)
        if "processing_method" in row:
            row["processing_method"] = resolve_method_path(
                row["processing_method"], project_path, ".pmx")
        if "injection_volume" in row:
            try:
                row["injection_volume"] = float(row["injection_volume"])
            except ValueError:
                raise SubmitError("Row {} of {}: injection_volume {!r} is not "
                                  "a number".format(i, path,
                                                    row["injection_volume"]))
        row.setdefault("result_name", "{:03d}".format(i))
        if has_token(row["result_name"]):
            raise SubmitError("Row {} of {}: result_name can't contain < or >"
                              .format(i, path))
    names = [row["result_name"].lower() for row in rows]
    if len(set(names)) < len(names):
        raise SubmitError("result_name must differ between rows, ignoring "
                          "case")
    return rows


def has_token(name):
    return "<" in name or ">" in name


def find_result(result_dir, since, name=None):
    pattern = "*.dx" if name is None else glob.escape(name) + ".dx"
    files = [f for f in glob.glob(os.path.join(glob.escape(result_dir), "**",
                                               pattern), recursive=True)
             if os.path.getmtime(f) >= since]
    return max(files, key=os.path.getmtime) if files else None


class Session:
    """A connection to one OpenLab instrument and project."""

    def __init__(self, connection, instrument, project, timeout_s=120,
                 init_wait_s=30, retry_s=5):
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
        log("connecting to {} ({})".format(instrument, project))
        deadline = time.time() + timeout_s
        for attempt in (1, 2):
            self._ready = threading.Event()
            self.controller = InstrumentController()
            self.controller.AppInitialized += self._on_initialized
            self.controller.RunRecordChanged += self._on_record
            self.controller.RunRecordCollectionChanged += self._on_collection
            try:
                established = self.controller.EstablishConnection(
                    connection, ticket, "{}".format(instruments[instrument]),
                    "{}".format(projects[project]))
                log("EstablishConnection returned {}".format(established))
                while not self.controller.IsConnected:
                    if time.time() > deadline:
                        raise SubmitError(
                            "Timed out connecting to the instrument "
                            "(CanTakeInstrumentControl: {}).".format(
                                self._can_take_control()))
                    time.sleep(0.1)
                if self._ready.wait(max(0, min(init_wait_s,
                                               deadline - time.time()))):
                    break
            except BaseException:
                self.close()
                raise
            if attempt == 1:
                log("connected without an AppInitialized event; "
                    "reconnecting")
                self.close()
                time.sleep(retry_s)
            else:
                log("connected without an AppInitialized event")
        time.sleep(min(1, retry_s))

    def _can_take_control(self):
        try:
            return self.controller.CanTakeInstrumentControl()
        except Exception as e:
            return "unknown, {}".format(e)

    def _on_initialized(self, source, args):
        self._ready.set()

    def _on_record(self, source, args):
        self.events.append([args.UpdatedRecord])

    def _on_collection(self, source, args):
        self.events.append(list(args.RunRecords))

    def updates(self):
        while self.events:
            for rec in self.events.popleft():
                yield rec.Id.ToString(), rec.Status.ToString()

    def drain_statuses(self, statuses):
        for run_id, status in self.updates():
            statuses[run_id] = status

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
        params.UseMethodInjectionVolume = injection_volume is None
        if injection_volume is not None:
            params.InjectionVolume = float(injection_volume)
        if sample_name is not None:
            params.SampleName = sample_name
        if result_name is not None:
            params.ResultFilename = result_name
        return self.controller.SubmitSingleRun(params).RunId.ToString()

    def records(self, rows, wait_minutes=None, injector=None):
        from System import TimeSpan
        from Agilent.OpenLAB.Acquisition.AutomationCore import (
            SequenceRecord, SequenceWaitEntryAction)

        injection = pick_injector(self.controller.InjectionSources, injector)
        records = []
        for row in rows:
            rec = SequenceRecord()
            rec.AcquisitionMethodPath = row["method"]
            rec.SampleLocation = row["vial"]
            rec.SelectedInjection = injection
            rec.DataFilename = row["result_name"]
            if "processing_method" in row:
                rec.ProcessingMethodPath = row["processing_method"]
            rec.UseMethodInjectionVolume = "injection_volume" not in row
            if "injection_volume" in row:
                rec.InjectionVolume = row["injection_volume"]
            if "sample_name" in row:
                rec.SampleName = row["sample_name"]
            records.append(rec)
        if wait_minutes:
            action = SequenceWaitEntryAction()
            action.WaitTime = TimeSpan.FromMinutes(wait_minutes)
            rec = SequenceRecord()
            rec.Action = action
            records.append(rec)
        return records

    def submit_sequence(self, rows, result_name, injector=None,
                        wait_minutes=None):
        from System.Collections.ObjectModel import ObservableCollection
        from Agilent.OpenLAB.Acquisition.AutomationCore import (
            SequenceRecord, SequenceSet, SequenceSetType)

        records = ObservableCollection[SequenceRecord]()
        for rec in self.records(rows, wait_minutes, injector):
            records.Add(rec)
        sequence = SequenceSet()
        sequence.SequenceType = getattr(SequenceSetType, "None")
        sequence.SequenceRecordSet = records
        return self.controller.SubmitSequenceRun(sequence,
                                                 result_name).RunId.ToString()

    def append(self, run_id, rows, wait_minutes=None, injector=None,
               resume_timeout_s=180, resume_poll_s=5):
        """Add rows to a sequence that is at a wait row, then end the wait."""
        from System import Guid
        from System.Collections.ObjectModel import ObservableCollection
        from Agilent.OpenLAB.Acquisition.AutomationCore import SequenceRecord

        c = self.controller
        paused = c.PauseAndEditSequence(Guid.Parse(run_id))
        if str(paused.OperationResult) != "Success":
            raise SubmitError(
                "OpenLab would not edit sequence {}, which must be at a wait "
                "row: {}; {}".format(run_id, paused.OperationResult,
                                     op_errors(paused)))
        try:
            live = paused.SequenceSet
            taken = {(rec.DataFilename or "").lower()
                     for rec in live.SequenceRecordSet}
            clash = [row["result_name"] for row in rows
                     if row["result_name"].lower() in taken]
            if clash:
                raise SubmitError("The sequence already has rows named {}"
                                  .format(", ".join(clash)))
            records = ObservableCollection[SequenceRecord]()
            for rec in list(live.SequenceRecordSet) + self.records(
                    rows, wait_minutes, injector):
                records.Add(rec)
            live.SequenceRecordSet = records
            updated = c.UpdateAndResumeSequence(live, "olsubmit", None)[0]
            if str(updated.OperationResult) != "Success":
                raise SubmitError("OpenLab would not update the sequence: "
                                  "{}; {}".format(updated.OperationResult,
                                                  op_errors(updated)))
        except BaseException:
            try:
                cancelled = c.CancelEditSequence()
                if str(cancelled.Result) != "Success":
                    log("CancelEditSequence: {}; the sequence may still be "
                        "in Editing".format(cancelled.Result))
            except Exception as e:
                log("CancelEditSequence failed: {}".format(e))
            raise
        deadline = time.time() + resume_timeout_s
        while True:
            resumed = c.ResumeWaitingSequence()
            if resumed.RunId.ToString().strip("0-"):
                return
            if time.time() > deadline:
                log("could not end the wait row ({}); the new rows run when "
                    "it runs out".format(op_errors(resumed)))
                return
            time.sleep(resume_poll_s)

    def submit_shutdown(self, method_path, result_dir):
        from Agilent.OpenLAB.Acquisition.AutomationCore import (
            ShutdownRunParams)

        params = ShutdownRunParams()
        params.AcquisitionMethod = method_path
        params.ResultPath = result_dir
        result = self.controller.SubmitShutdownRun(params)
        return result.RunId.ToString(), op_errors(result)

    def module_key(self, device_type):
        for module in self.controller.Modules:
            if module.DeviceType == device_type:
                return module.Hashkey
        raise SubmitError("The instrument has no {} module".format(
            device_type))

    def get_resources(self, key, ids):
        from System import Array, String
        values = {v.PropertyId: v for v in self.controller.
                  GetMethodResourceProperties(key, Array[String](ids),
                                              None)[-1] or []}
        bad = [i for i in ids if i not in values or
               str(values[i].Result) != "OK"]
        if bad:
            raise SubmitError("Could not read {} from the {} settings".format(
                ", ".join(bad), key))
        return {i: values[i].PropertyValue for i in ids}

    def set_resources(self, key, pairs):
        result, items = self.controller.SetMethodResourceProperties(
            key, id_values(pairs), None)
        items = list(items or [])
        failed = ["{} ({} {})".format(pid, x.Result, x.ResultString)
                  for (pid, _), x in zip(pairs, items)
                  if str(x.Result) != "OK"]
        if len(items) != len(pairs):
            failed.append("{} results for {} settings".format(
                len(items), len(pairs)))
        if str(result) != "OK" or failed:
            raise SubmitError("OpenLab would not change the {} settings: {} "
                              "{}".format(key, result, ", ".join(failed)))

    def edit_method(self, template, path, flow=None, stop_time=None,
                    post_time=None, temperature=None, gradient=None):
        """Save a copy of template with new settings through OpenLab's method
        editor. gradient is a list of (time, [B, C, D]) from time 0."""
        c = self.controller
        shutil.copyfile(template, path)
        try:
            loaded = c.LoadAcquisitionMethod(path, False)
            if str(loaded) == "NotResolvable":
                loaded = c.LoadAcquisitionMethod(path, True)
                log("LoadAcquisitionMethod: NotResolvable, then {} when "
                    "allowed to resolve".format(loaded))
            if str(loaded) not in ("OK", "Resolved"):
                raise SubmitError("OpenLab could not load {}: {}".format(
                    path, loaded))
            pump = []
            if flow is not None:
                pump.append(("Flow", flow))
            if stop_time is not None:
                pump += [("StopTime_Mode", "Set"), ("StopTime_Time", stop_time)]
            if post_time == 0:
                pump.append(("PostTime_Mode", "Off"))
            elif post_time is not None:
                pump += [("PostTime_Mode", "Set"), ("PostTime_Time", post_time)]
            if gradient is not None or flow is not None:
                pump += self.timetable(self.module_key("Pump"), gradient,
                                       flow is not None)
            modes = [x for x in pump if x[0].endswith("_Mode")]
            for pairs in (modes, [x for x in pump if x not in modes]):
                if pairs:
                    self.set_resources(self.module_key("Pump"), pairs)
            if temperature is not None:
                self.set_resources(self.module_key("ColumnCompartment"), [
                    ("TemperatureControl_Mode", "Set"),
                    ("TemperatureControl_Temperature", temperature),
                    ("TemperatureControl2_Mode", "Set"),
                    ("TemperatureControl2_Temperature", temperature)])
            saved = c.SaveAcquisitionMethod(path)
            if str(saved) not in ("OK", "NotDirty"):
                raise SubmitError("OpenLab could not save {}: {}".format(
                    path, saved))
            status = c.CheckAcquisitionMethodStatus(path)
            if not status.CanLoadMethod:
                raise SubmitError("OpenLab saved {} but cannot load it: {}"
                                  .format(path, status.CannotLoadMethodReason))
        except BaseException:
            os.remove(path)
            raise
        return path

    def timetable(self, key, gradient, drop_flow):
        """Return the pump settings for a new gradient: the starting
        composition and a timetable that keeps the template's other rows."""
        flags = ["PumpChannel{}_IsUsed".format(n) for n in CHANNELS]
        current = self.get_resources(key, ["TimeTable"] + flags)
        drop = ({"SolventComposition"} if gradient is not None else set()) | \
            ({"Flow"} if drop_flow else set())
        rows = [(r.PropertyId, as_id_values(r.PropertyValue))
                for r in current["TimeTable"] or [] if r.PropertyId not in drop]
        pairs = []
        if gradient is not None:
            used = [bool(current[f]) for f in flags]
            for t, pct in gradient:
                for n, p, u in zip(CHANNELS, pct, used):
                    if p and not u:
                        raise SubmitError("The method does not use pump "
                                          "channel {}".format("ABCD"[n - 1]))
            pairs += [("PumpChannel{}_CompositionPercentage".format(n), p)
                      for n, p, u in zip(CHANNELS, gradient[0][1], used) if u]
            rows += [("SolventComposition", id_values(
                [("Time", t)] + [("PumpChannel{}_Percentage".format(n), p)
                                 for n, p in zip(CHANNELS, pct)]))
                for t, pct in gradient[1:]]
        return pairs + [("TimeTable", id_values(rows))]

    def wait(self, run_id, timeout_s=None, poll_s=0.5, pause=False):
        """Wait for the run to end. With pause, also return when a sequence
        reaches a wait row after acquiring an injection."""
        last = None
        moved = False
        deadline = None if timeout_s is None else time.time() + timeout_s
        while True:
            for run, status in self.updates():
                if run != run_id:
                    continue
                if status != last:
                    log("run {}: {}".format(run_id, status))
                    last = status
                lower = status.lower()
                if lower in TERMINAL_STATUSES or (
                        pause and moved and lower == WAITING):
                    return status
                moved = moved or lower == "acquiring"
            if deadline is not None and time.time() > deadline:
                return None
            time.sleep(poll_s)

    def close(self):
        try:
            self.controller.AppInitialized -= self._on_initialized
            self.controller.RunRecordChanged -= self._on_record
            self.controller.RunRecordCollectionChanged -= self._on_collection
        except Exception as e:
            log("error removing event handlers: {}".format(e))
        try:
            self.controller.Disconnect()
        except Exception as e:
            log("error disconnecting: {}".format(e))


def parse_args(argv):
    p = argparse.ArgumentParser(
        description="Submit one run or a sequence to OpenLab CDS and wait "
                    "for it.")
    p.add_argument("method", nargs="?", help="acquisition method (.amx)")
    p.add_argument("--vial", help="sample location, e.g. D1F-A1")
    p.add_argument("--sequence",
                   help="CSV file with one row per injection instead of "
                        "METHOD, with columns method and vial and optionally "
                        "injection_volume, sample_name, processing_method and "
                        "result_name (the .dx file name; default 001, 002, "
                        "...). --injection-volume, --sample-name and "
                        "--processing-method fill empty cells")
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
                        "project's Results folder. The newest .dx file in it "
                        "is taken as this run's, so no other run should write "
                        "there unless each run has its own --result-name "
                        "(default: a new folder named after the submission "
                        "time)")
    p.add_argument("--result-name",
                   help="result file name, used to find the run's .dx file; "
                        "with --sequence, the result set name")
    p.add_argument("--save-as",
                   help="save METHOD with the settings below as this new "
                        "method, absolute or relative to the project's "
                        "Methods folder, and submit that")
    p.add_argument("--edit-only", action="store_true",
                   help="with --save-as, print the new method's path instead "
                        "of submitting it")
    p.add_argument("--flow", type=float,
                   help="constant flow in mL/min; removes flow steps")
    p.add_argument("--stop-time", type=float, help="stop time in minutes")
    p.add_argument("--post-time", type=float,
                   help="post time in minutes; 0 turns it off")
    p.add_argument("--temperature", type=float,
                   help="column temperature in degrees C, both sides")
    p.add_argument("--gradient", type=parse_gradient,
                   help="solvent composition steps TIME:B[:C[:D]], separated "
                        "by commas, e.g. 0:5,2:50,4:95, in minutes and percent "
                        "of channels B, C and D (A is the rest). The first "
                        "step, at time 0, sets the starting composition, and "
                        "the others replace the composition steps")
    p.add_argument("--wait-row", type=float, metavar="MINUTES",
                   help="end the run or sequence with a wait row this long, "
                        "and return when the sequence reaches it")
    p.add_argument("--append", metavar="RUN_ID",
                   help="add METHOD or the --sequence rows to this sequence, "
                        "which must be at a wait row, and end the wait; needs "
                        "the sequence's --result-path")
    p.add_argument("--injector",
                   help="injection source; defaults to HipAls or the only one. "
                        "\"none\" runs the method without an injection, like "
                        "No Injection in OpenLab (untested)")
    p.add_argument("--timeout", type=float, default=120,
                   help="minutes to wait for the run, or for the whole "
                        "sequence (default 120; 0 for no limit)")
    p.add_argument("--no-wait", action="store_true",
                   help="print the run ID and return once the run is queued")
    p.add_argument("--shutdown-method",
                   help="method for a shutdown run if the run is aborted or "
                        "times out, absolute or relative to the project's "
                        "Methods folder")
    p.add_argument("--shutdown-now", action="store_true",
                   help="submit only a shutdown run with --shutdown-method, "
                        "print its run ID and return")
    p.add_argument("--connection", default=DEFAULT_CONNECTION)
    p.add_argument("--agilent-dir", default=DEFAULT_AGILENT_DIR)
    args = p.parse_args(argv)
    if args.shutdown_now:
        if args.shutdown_method is None:
            p.error("--shutdown-now needs --shutdown-method")
        if args.method is not None or args.sequence is not None:
            p.error("--shutdown-now submits nothing but the shutdown run")
    elif (args.method is None) == (args.sequence is None):
        p.error("give either METHOD or --sequence")
    edits = [o for o in ("flow", "stop_time", "post_time", "temperature",
                         "gradient") if getattr(args, o) is not None]
    if edits and args.save_as is None:
        p.error("--{} needs --save-as".format(edits[0].replace("_", "-")))
    if args.save_as is not None and args.method is None:
        p.error("--save-as needs METHOD")
    if args.save_as is not None and not edits:
        p.error("--save-as needs a setting to change, such as --flow")
    if args.edit_only and args.save_as is None:
        p.error("--edit-only needs --save-as")
    if args.edit_only and (args.wait_row or args.append or args.no_wait):
        p.error("--edit-only submits nothing, so it can't take --wait-row, "
                "--append or --no-wait")
    if args.method is not None and args.vial is None and not args.edit_only:
        p.error("--vial is required with METHOD")
    if args.wait_row is not None and args.wait_row <= 0:
        p.error("--wait-row must be positive")
    if args.append is not None and args.result_path is None:
        p.error("--append needs the sequence's --result-path")
    if args.append is not None and args.sequence is not None and \
            args.result_name is not None:
        p.error("--result-name can't rename the result set with --append")
    if args.method is not None and args.result_name and \
            has_token(args.result_name):
        p.error("--result-name can't contain < or >, since the script finds "
                "the .dx file by that name")
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
        result_dir = os.path.abspath(os.path.join(
            session.project_path, "Results",
            args.result_path or time.strftime("olsubmit_%Y%m%d_%H%M%S")))
        shutdown = None
        if args.shutdown_method is not None:
            shutdown = resolve_method_path(args.shutdown_method,
                                           session.project_path)
        if args.shutdown_now:
            shutdown_id, problems = session.submit_shutdown(shutdown,
                                                            result_dir)
            log("submitted shutdown run {}; errors: {}".format(shutdown_id,
                                                               problems))
            print(shutdown_id)
            return 0 if problems == "none" else 2
        if args.method is not None:
            method = resolve_method_path(args.method, session.project_path)
            if args.save_as is not None:
                method = session.edit_method(
                    method, resolve_method_path(
                        args.save_as, session.project_path, new=True),
                    flow=args.flow, stop_time=args.stop_time,
                    post_time=args.post_time, temperature=args.temperature,
                    gradient=args.gradient)
                log("saved " + method)
                if args.edit_only:
                    print(method)
                    return 0
        defaults = {k: getattr(args, k) for k in
                    ("injection_volume", "sample_name", "processing_method")
                    if getattr(args, k) is not None}
        if "processing_method" in defaults:
            defaults["processing_method"] = resolve_method_path(
                defaults["processing_method"], session.project_path, ".pmx")
        submitted_at = time.time()
        if args.sequence is None and args.append is None and \
                args.wait_row is None:
            run_id = session.submit(method, args.vial, result_dir,
                                    injector=args.injector,
                                    result_name=args.result_name, **defaults)
            names = [args.result_name]
            what = os.path.basename(method)
        else:
            if args.sequence is not None:
                rows = read_sequence(args.sequence, session.project_path,
                                     defaults)
                set_name = args.result_name or "<D>"
            else:
                rows = [dict(defaults, method=method, vial=args.vial,
                             result_name=args.result_name or "001")]
                set_name = "<D>"
            if args.append is not None:
                run_id = args.append
                session.append(run_id, rows, args.wait_row, args.injector)
                what = "{} row(s) to sequence".format(len(rows))
            else:
                run_id = session.submit_sequence(
                    rows, os.path.join(result_dir, set_name),
                    injector=args.injector, wait_minutes=args.wait_row)
                what = "a {}-row sequence".format(len(rows))
            names = [row["result_name"] for row in rows]
        log("submitted {} as run {}".format(what, run_id))
        if args.no_wait or args.wait_row is not None:
            print(run_id, flush=True)
        if args.no_wait:
            return 0

        status = session.wait(run_id, args.timeout * 60 or None,
                              pause=args.wait_row is not None)
        if status is None or status.lower() not in ("completed", WAITING):
            if status is None:
                log("timed out after {} min; the run is still in the queue"
                    .format(args.timeout))
            else:
                log("run ended with status {}".format(status))
            if shutdown is not None:
                try:
                    shutdown_id, problems = session.submit_shutdown(
                        shutdown, result_dir)
                    log("submitted shutdown run {} with {}; errors: {}"
                        .format(shutdown_id, os.path.basename(shutdown),
                                problems))
                except Exception as e:
                    log("could not submit the shutdown run: {}\n{}".format(
                        e, dotnet_details(e)))
            return 1
        results = [find_result(result_dir, submitted_at - 5, name)
                   for name in names]
        print("\n".join(r or "" for r in results))
        if None in results:
            missing = [name or "*" for name, r in zip(names, results)
                       if r is None]
            log("run completed, but no new .dx file was found under {} for: {}"
                .format(result_dir, ", ".join(missing)))
            found = glob.glob(os.path.join(glob.escape(result_dir), "**",
                                           "*.dx"), recursive=True)
            log(".dx files there: " + (", ".join(found) or "none"))
            return 1
        return 0
    except SubmitError as e:
        log("error: {}".format(e))
        return 2
    except Exception as e:
        log("error:\n" + traceback.format_exc())
        details = dotnet_details(e)
        if details:
            log(".NET exception:\n" + details)
        return 2
    finally:
        if session is not None:
            session.close()


if __name__ == "__main__":
    sys.exit(main())
