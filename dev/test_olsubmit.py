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


def test_main(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    touch(os.path.join(project, "Methods", "p1.pmx"))
    submitted = {}

    class FakeSession:
        def __init__(self, connection, instrument, project_path):
            self.project_path = project

        def submit(self, method, vial, result_dir, **kwargs):
            submitted.update(kwargs, result_dir=result_dir)
            now = time.time()
            touch(os.path.join(result_dir, "set.rslt", "ours.dx"), now)
            touch(os.path.join(project, "Results", "other.rslt", "other.dx"),
                  now + 1)
            return "r1"

        def wait(self, run_id, timeout_s):
            return "Completed"

        def close(self):
            pass

    saved = ol.Session, ol.load_assemblies
    ol.Session, ol.load_assemblies = FakeSession, lambda agilent_dir: None
    out = io.StringIO()
    try:
        with contextlib.redirect_stdout(out):
            code = ol.main(["m1", "--vial", "A1", "--instrument", "I",
                            "--project", "P", "--processing-method", "p1"])
    finally:
        ol.Session, ol.load_assemblies = saved
    assert code == 0
    assert out.getvalue().strip().endswith("ours.dx")
    assert submitted["processing_method"] == \
        os.path.abspath(os.path.join(project, "Methods", "p1.pmx"))


if __name__ == "__main__":
    for name, fn in sorted(globals().items()):
        if name.startswith("test_"):
            if fn.__code__.co_argcount:
                with tempfile.TemporaryDirectory() as tmp:
                    fn(tmp)
            else:
                fn()
            print("ok", name)
