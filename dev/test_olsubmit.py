"""Checks for the parts of olsubmit.py that do not need OpenLab.

    python test_olsubmit.py
"""

import contextlib
import io
import os
import sys
import tempfile
import time
from collections import deque, namedtuple
from types import SimpleNamespace

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import olsubmit as ol


def touch(path, mtime=None):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    open(path, "w").close()
    if mtime is not None:
        os.utime(path, (mtime, mtime))


def test_find_assembly(tmp_path):
    tmp = str(tmp_path)
    gac = os.path.join(tmp, "gac")
    for folder in ("GAC_32", "GAC_MSIL"):
        touch(os.path.join(gac, folder, "A.B", "v4.0_2.7.0.0__x", "A.B.dll"))
    assert ol.find_assembly("A.B", None, tmp, gac, bits=32)[1] == "GAC_32"
    assert ol.find_assembly("A.B", None, tmp, gac, bits=64)[1] == "GAC_MSIL"
    assert ol.find_assembly("C.D", None, tmp, gac, bits=32) == (None, None)
    touch(os.path.join(tmp, "Services", "E.F.dll"))
    assert ol.find_assembly("E.F", "Services", tmp, gac)[0] == \
        os.path.join(tmp, "Services", "E.F.dll")


def test_pick_injector():
    Source = namedtuple("Source", "DisplayName")
    hip, other = Source("HipAls"), Source("Manual")
    assert ol.pick_injector([other, hip]) is hip
    assert ol.pick_injector([other]) is other
    assert ol.pick_injector([other, hip], "Manual") is other
    for sources, wanted in (([other, Source("X")], None), ([hip], "Y")):
        try:
            ol.pick_injector(sources, wanted)
        except ol.SubmitError:
            pass
        else:
            raise AssertionError("expected SubmitError")


def test_resolve_method_path(tmp_path):
    tmp = str(tmp_path)
    method = os.path.join(tmp, "Methods", "sub", "m1.amx")
    touch(method)
    assert ol.resolve_method_path("sub/m1", tmp) == os.path.abspath(method)
    assert ol.resolve_method_path(method, "/elsewhere") == method
    touch(os.path.join(tmp, "Methods", "p1.pmx"))
    assert ol.resolve_method_path("p1", tmp, ".pmx") == \
        os.path.abspath(os.path.join(tmp, "Methods", "p1.pmx"))
    try:
        ol.resolve_method_path("missing.amx", tmp)
    except ol.SubmitError:
        pass
    else:
        raise AssertionError("expected SubmitError")


def test_find_result(tmp_path):
    tmp = os.path.join(str(tmp_path), "Demo [2026]")
    now = time.time()
    touch(os.path.join(tmp, "set.rslt", "old.dx"), now - 100)
    touch(os.path.join(tmp, "set.rslt", "new.dx"), now)
    touch(os.path.join(tmp, "set.rslt", "newer.rx"), now + 1)
    assert ol.find_result(tmp, now - 10).endswith("new.dx")
    assert ol.find_result(tmp, now + 10) is None


def test_wait():
    class Text:
        def __init__(self, value):
            self.value = value

        def ToString(self):
            return self.value

    def record(run_id, status):
        return SimpleNamespace(Id=Text(run_id), Status=Text(status))

    session = object.__new__(ol.Session)
    session.events = deque([[record("r1", "Pending")],
                            [record("r2", "Completed"),
                             record("r1", "Acquiring")],
                            [record("r1", "Completed")]])
    assert session.wait("r1", timeout_s=5, poll_s=0) == "Completed"

    session.events = deque([[record("r1", "Unexisting")]])
    assert session.wait("r1", timeout_s=5, poll_s=0) == "Unexisting"

    session.events = deque([[record("r1", "Acquiring")]])
    assert session.wait("r1", timeout_s=0.05, poll_s=0.01) is None


def test_close_logs_errors():
    class Event:
        def __isub__(self, handler):
            raise RuntimeError("handler not found")

    def disconnect():
        raise RuntimeError("pipe closed")

    session = object.__new__(ol.Session)
    session.controller = SimpleNamespace(
        AppInitialized=Event(), RunRecordChanged=Event(),
        RunRecordCollectionChanged=Event(), Disconnect=disconnect)
    session.close()


class FakeSession:
    project_path = None
    submitted = {}
    skip = ()

    def __init__(self, connection, instrument, project):
        pass

    def submit(self, method, vial, result_dir, **kwargs):
        self.submitted.update(kwargs, result_dir=result_dir)
        now = time.time()
        name = kwargs.get("result_name") or "ours"
        touch(os.path.join(result_dir, "set.rslt", name + ".dx"), now)
        touch(os.path.join(self.project_path, "Results", "other.rslt",
                           "other.dx"), now + 1)
        return "r1"

    def submit_sequence(self, rows, result_name, injector=None):
        self.submitted.update(rows=rows, result_name=result_name)
        now = time.time()
        result_set = os.path.dirname(result_name)
        for i, row in enumerate(rows):
            if row["result_name"] in self.skip:
                continue
            touch(os.path.join(result_set, "set.rslt",
                               row["result_name"] + ".dx"), now - i)
        touch(os.path.join(self.project_path, "Results", "other.rslt",
                           "001.dx"), now + 1)
        return "s1"

    def wait(self, run_id, timeout_s):
        return "Completed"

    def close(self):
        pass


def run_main(project, argv, skip=()):
    FakeSession.project_path = project
    FakeSession.submitted = {}
    FakeSession.skip = skip
    saved = ol.Session, ol.load_assemblies
    ol.Session, ol.load_assemblies = FakeSession, lambda agilent_dir: None
    out = io.StringIO()
    try:
        with contextlib.redirect_stdout(out):
            code = ol.main(argv + ["--instrument", "I", "--project", "P"])
    finally:
        ol.Session, ol.load_assemblies = saved
    return code, out.getvalue().splitlines()


def test_main(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    touch(os.path.join(project, "Methods", "p1.pmx"))
    code, out = run_main(project, ["m1", "--vial", "A1",
                                   "--processing-method", "p1"])
    assert code == 0
    assert len(out) == 1 and out[0].endswith("ours.dx")
    assert FakeSession.submitted["processing_method"] == \
        os.path.abspath(os.path.join(project, "Methods", "p1.pmx"))


def test_main_shared_folder(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    touch(os.path.join(project, "Results", "batch", "set.rslt", "run0.dx"),
          time.time() + 10)
    code, out = run_main(project, ["m1", "--vial", "A1", "--result-path",
                                   "batch", "--result-name", "run1"])
    assert code == 0
    assert out == [os.path.join(project, "Results", "batch", "set.rslt",
                                "run1.dx")]


def test_read_sequence(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    touch(os.path.join(project, "Methods", "p1.pmx"))
    csv_path = os.path.join(project, "runs.csv")
    with open(csv_path, "w") as f:
        f.write("method,vial,injection_volume,result_name\n"
                "m1,A1,5,\n"
                "m1, A2 ,,blank\n")
    rows = ol.read_sequence(csv_path, project,
                            {"injection_volume": 10.0, "processing_method": "p1"})
    assert [r["vial"] for r in rows] == ["A1", "A2"]
    assert [r["injection_volume"] for r in rows] == [5.0, 10.0]
    assert [r["result_name"] for r in rows] == ["001", "blank"]
    assert rows[1]["processing_method"].endswith("p1.pmx")
    with open(csv_path, "w", encoding="utf-8-sig") as f:
        f.write("Method, Vial\nm1,A1\n,\n")
    assert len(ol.read_sequence(csv_path, project, {})) == 1
    for text, message in (
            ("method,vial,volume\nm1,A1,5\n", "Unknown columns"),
            ("method\nm1\n", "needs a method and a vial"),
            ("method,vial,result_name\nm1,A1,x\nm1,A2,X\n", "must differ"),
            ("method,vial,injection_volume\nm1,A1,5uL\n", "not a number"),
            ("method,vial,result_name\nm1,A1,<D>-x\n", "< or >"),
            ("method,vial\n", "no rows")):
        with open(csv_path, "w") as f:
            f.write(text)
        try:
            ol.read_sequence(csv_path, project, {})
        except ol.SubmitError as e:
            assert message in str(e), (message, str(e))
        else:
            raise AssertionError("expected SubmitError for " + repr(text))


def test_main_sequence(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    csv_path = os.path.join(project, "runs.csv")
    with open(csv_path, "w") as f:
        f.write("method,vial\nm1,A1\nm1,A2\n")
    code, out = run_main(project, ["--sequence", csv_path,
                                   "--result-path", "batch"])
    assert code == 0
    assert FakeSession.submitted["result_name"] == \
        os.path.join(project, "Results", "batch", "<D>")
    assert [os.path.relpath(f, project) for f in out] == [
        os.path.join("Results", "batch", "set.rslt", "001.dx"),
        os.path.join("Results", "batch", "set.rslt", "002.dx")]


def test_main_sequence_missing_row(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    csv_path = os.path.join(project, "runs.csv")
    with open(csv_path, "w") as f:
        f.write("method,vial\nm1,A1\nm1,A2\nm1,A3\n")
    code, out = run_main(project, ["--sequence", csv_path], skip={"002"})
    assert code == 1
    assert [os.path.basename(f) for f in out] == ["001.dx", "", "003.dx"]


def test_parse_args():
    base = ["--instrument", "I", "--project", "P"]
    for argv in ([], ["m1", "--sequence", "s.csv"], ["m1"],
                 ["m1", "--vial", "A1", "--result-name", "<D>-x"]):
        try:
            with contextlib.redirect_stderr(io.StringIO()):
                ol.parse_args(argv + base)
        except SystemExit:
            pass
        else:
            raise AssertionError("expected an error for {}".format(argv))
    args = ol.parse_args(["--sequence", "s.csv", "--result-name", "<D>-x"]
                         + base)
    assert args.result_name == "<D>-x"


if __name__ == "__main__":
    for name, fn in sorted(globals().items()):
        if name.startswith("test_"):
            if fn.__code__.co_argcount:
                with tempfile.TemporaryDirectory() as tmp:
                    fn(tmp)
            else:
                fn()
            print("ok", name)
