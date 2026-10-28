"""Checks for the parts of olsubmit.py that do not need OpenLab.

    python test_olsubmit.py
"""

import contextlib
import io
import os
import sys
import tempfile
import time
import types
from collections import deque, namedtuple
from types import SimpleNamespace as NS

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import olsubmit as ol


def stub_dotnet():
    """Stand-ins for the .NET types olsubmit.py imports."""
    class Typed:
        def __getitem__(self, item):
            return Collection

    class Collection(list):
        def Add(self, x):
            self.append(x)

    class Obj:
        Action = DataFilename = None

    def module(name, **attrs):
        m = types.ModuleType(name)
        m.__dict__.update(attrs)
        sys.modules.setdefault(name, m)

    module("System", Array=Typed(), String=str, Guid=NS(Parse=str),
           TimeSpan=NS(FromMinutes=lambda m: ("min", m)))
    module("System.Collections.ObjectModel", ObservableCollection=Typed())
    for name in ("Agilent", "Agilent.OpenLAB", "Agilent.OpenLAB.Acquisition"):
        module(name)
    module("Agilent.OpenLAB.Acquisition.AutomationCore",
           **{n: type(n, (Obj,), {}) for n in (
               "ResourceIdAndValue", "SequenceRecord",
               "SequenceWaitEntryAction", "SequenceSet")},
           SequenceSetType=NS(**{"None": 0}),
           SingleRunParams=type("SingleRunParams", (Obj,), {}))
    module("Agilent.OpenLab")
    module("Agilent.OpenLab.SharedServices", Connection=FakeConnection)
    module("Agilent.OpenLAB.Acquisition.AutomationInstrument",
           InstrumentController=FakeController)


class Event(list):
    def __iadd__(self, handler):
        self.append(handler)
        return self

    def __isub__(self, handler):
        self.remove(handler)
        return self


class FakeController:
    """Fires AppInitialized only from the second instance on."""
    made = 0

    def __init__(self):
        FakeController.made += 1
        self.n = FakeController.made
        self.AppInitialized = Event()
        self.RunRecordChanged = Event()
        self.RunRecordCollectionChanged = Event()
        self.IsConnected = False
        self.InjectionSources = [NS(DisplayName="HipAls")]
        self.submitted = None

    def EstablishConnection(self, *args):
        self.IsConnected = True
        if self.n > 1:
            for handler in self.AppInitialized:
                handler(self, None)
        return True

    def Disconnect(self):
        self.IsConnected = False

    def SubmitSingleRun(self, params):
        self.submitted = params
        return NS(RunId=NS(ToString=lambda: "r1"))


class FakeConnection:
    def __init__(self, address):
        info = NS(Id="i1")
        self.Instruments = NS(GetAllInstrumentsInfo=lambda: [info],
                              GetInstrument=lambda i: NS(Name="LC"))
        self.Projects = NS(
            GetAllProjectsInfo=lambda: [NS(Name="P", Id="p1")],
            GetProject=lambda i: NS(RootPath=NS(LocalPath="/proj")))

    def GetTicketContainer(self):
        return NS(ToString=lambda: "ticket")


stub_dotnet()


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
    assert ol.pick_injector([hip], "None") is None
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
        return NS(Id=Text(run_id), Status=Text(status))

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
    session.controller = NS(
        AppInitialized=Event(), RunRecordChanged=Event(),
        RunRecordCollectionChanged=Event(), Disconnect=disconnect)
    session.close()


class FakeSession:
    project_path = None
    submitted = {}
    skip = ()
    status = "Completed"

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

    def edit_method(self, template, path, **edits):
        self.submitted.update(edits=edits)
        touch(path)
        return path

    def append(self, run_id, rows, wait_minutes=None, injector=None):
        self.submitted.update(rows=rows, run_id=run_id,
                              wait_minutes=wait_minutes)
        for row in rows:
            touch(os.path.join(self.project_path, "Results", "loop",
                               "set.rslt", row["result_name"] + ".dx"))

    def submit_sequence(self, rows, result_name, injector=None,
                        wait_minutes=None):
        self.submitted.update(rows=rows, result_name=result_name,
                              wait_minutes=wait_minutes)
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

    def wait(self, run_id, timeout_s, pause=False):
        return "Waiting" if pause and self.status == "Completed" else \
            self.status

    def submit_shutdown(self, method, result_dir):
        self.submitted.update(shutdown=method)
        return "x1", "none"

    def close(self):
        pass


def run_main(project, argv, skip=(), status="Completed"):
    FakeSession.project_path = project
    FakeSession.submitted = {}
    FakeSession.skip = skip
    FakeSession.status = status
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


def test_main_shutdown(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    touch(os.path.join(project, "Methods", "off.amx"))
    argv = ["m1", "--vial", "A1", "--shutdown-method", "off"]
    with contextlib.redirect_stderr(io.StringIO()):
        code, out = run_main(project, argv, status="Aborted")
    assert code == 1 and out == []
    assert FakeSession.submitted["shutdown"] == \
        os.path.join(project, "Methods", "off.amx")
    code, out = run_main(project, argv)
    assert code == 0 and "shutdown" not in FakeSession.submitted
    with contextlib.redirect_stderr(io.StringIO()):
        code, _ = run_main(project, argv[:-1] + ["missing"])
    assert code == 2 and "result_dir" not in FakeSession.submitted
    with contextlib.redirect_stderr(io.StringIO()):
        code, out = run_main(project, ["--shutdown-method", "off",
                                       "--shutdown-now"])
    assert code == 0 and out == ["x1"]
    assert FakeSession.submitted == {
        "shutdown": os.path.join(project, "Methods", "off.amx")}


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
                 ["m1", "--vial", "A1", "--result-name", "<D>-x"],
                 ["m1", "--vial", "A1", "--flow", "1"],
                 ["--sequence", "s.csv", "--save-as", "n"],
                 ["m1", "--edit-only"],
                 ["m1", "--save-as", "n", "--edit-only"],
                 ["m1", "--save-as", "n", "--flow", "1", "--edit-only",
                  "--wait-row", "5"],
                 ["m1", "--save-as", "n", "--gradient", "1:5"],
                 ["m1", "--vial", "A1", "--wait-row", "0"],
                 ["m1", "--vial", "A1", "--append", "s1"],
                 ["--sequence", "s.csv", "--append", "s1", "--result-path",
                  "loop", "--result-name", "x"]):
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
    args = ol.parse_args(["m1", "--save-as", "n", "--edit-only",
                          "--gradient", "0:5,2:95"] + base)
    assert args.vial is None
    assert args.gradient == [(0, [5, 0, 0]), (2, [95, 0, 0])]


def test_parse_gradient():
    assert ol.parse_gradient("0:5,2:50:10,4:95") == [
        (0, [5, 0, 0]), (2, [50, 10, 0]), (4, [95, 0, 0])]
    for text in ("1:5", "0:5,0:6", "0:5,2:4,1:3", "0:5,x", "0:60:50",
                 "0:-1", "0", "0:1:1:1:1"):
        try:
            ol.parse_gradient(text)
        except ol.argparse.ArgumentTypeError:
            pass
        else:
            raise AssertionError("expected an error for " + text)


def plain(value):
    if isinstance(value, list):
        return [(x.PropertyId, plain(x.PropertyValue)) for x in value]
    return value


def ok(n):
    return [NS(Result="OK", ResultString="") for _ in range(n)]


class FakeEditor:
    Modules = [NS(DeviceType="Pump", Hashkey="pump#1"),
               NS(DeviceType="ColumnCompartment", Hashkey="cc#1")]

    def __init__(self):
        row = lambda kind, **v: NS(PropertyId=kind, PropertyValue=[
            NS(PropertyId=k, PropertyValue=x) for k, x in v.items()])
        self.values = {
            "TimeTable": [row("SolventComposition", Time=1.0,
                              PumpChannel2_Percentage=5.0),
                          row("Flow", Time=2.0, Flow=0.5)],
            "PumpChannel2_IsUsed": True, "PumpChannel3_IsUsed": False,
            "PumpChannel4_IsUsed": False}
        self.set = {}
        self.calls = 0
        self.drop = 0
        self.saved = "OK"
        self.load = "OK"

    def LoadAcquisitionMethod(self, path, resolve):
        return "Resolved" if resolve else self.load

    def GetMethodResourceProperties(self, key, ids, _):
        return "OK", [NS(PropertyId=i, Result="OK",
                         PropertyValue=self.values[i]) for i in ids]

    def SetMethodResourceProperties(self, key, items, _):
        self.set.setdefault(key, []).extend(plain(items))
        self.calls += 1
        return "OK", ok(len(items) - self.drop)

    def SaveAcquisitionMethod(self, path):
        return self.saved

    def CheckAcquisitionMethodStatus(self, path):
        return NS(CanLoadMethod=True, CannotLoadMethodReason="")


def fake_session(controller):
    session = object.__new__(ol.Session)
    session.controller = controller
    session.events = deque()
    return session


def test_edit_method(tmp_path):
    template = os.path.join(str(tmp_path), "t.amx")
    touch(template)
    path = os.path.join(str(tmp_path), "new.amx")
    sc = lambda t, b: ("SolventComposition", [
        ("Time", t), ("PumpChannel2_Percentage", b),
        ("PumpChannel3_Percentage", 0), ("PumpChannel4_Percentage", 0)])

    editor = FakeEditor()
    fake_session(editor).edit_method(
        template, path, stop_time=5,
        gradient=[(0, [10, 0, 0]), (2, [50, 0, 0]), (4, [95, 0, 0])])
    assert os.path.exists(path)
    assert editor.set == {"pump#1": [
        ("StopTime_Mode", "Set"), ("StopTime_Time", 5),
        ("PumpChannel2_CompositionPercentage", 10),
        ("TimeTable", [("Flow", [("Time", 2.0), ("Flow", 0.5)]),
                       sc(2, 50), sc(4, 95)])]}

    assert editor.calls == 2

    editor = FakeEditor()
    editor.saved = "NotDirty"
    editor.load = "NotResolvable"
    os.remove(path)
    fake_session(editor).edit_method(template, path, flow=0.4, post_time=0,
                                     temperature=30)
    assert editor.set["pump#1"] == [
        ("PostTime_Mode", "Off"), ("Flow", 0.4),
        ("TimeTable", [("SolventComposition", [
            ("Time", 1.0), ("PumpChannel2_Percentage", 5.0)])])]
    assert editor.set["cc#1"][1] == ("TemperatureControl_Temperature", 30)

    os.remove(path)
    editor = FakeEditor()
    editor.drop = 1
    for session, edits, message in (
            (fake_session(FakeEditor()), {"gradient": [(0, [10, 5, 0])]},
             "channel C"),
            (fake_session(editor), {"flow": 0.4}, "1 results for 2")):
        try:
            session.edit_method(template, path, **edits)
        except ol.SubmitError as e:
            assert message in str(e), str(e)
        else:
            raise AssertionError("expected SubmitError")
        assert not os.path.exists(path)


class Id:
    def __init__(self, value):
        self.value = value

    def ToString(self):
        return self.value


class FakeQueue:
    InjectionSources = [NS(DisplayName="HipAls")]

    def __init__(self):
        self.rows = (NS(DataFilename="row1", Action=None),
                     NS(DataFilename=None, Action="wait"))
        self.resumes = 0
        self.cancelled = False
        self.updated = None

    def PauseAndEditSequence(self, run_id):
        return NS(OperationResult="Success", AllErrorMessages=[],
                  SequenceSet=NS(SequenceRecordSet=self.rows))

    def UpdateAndResumeSequence(self, live, reason, ref):
        self.updated = list(live.SequenceRecordSet)
        return NS(OperationResult="Success", AllErrorMessages=[]), live

    def ResumeWaitingSequence(self):
        self.resumes += 1
        run = "s1" if self.resumes > 1 else \
            "00000000-0000-0000-0000-000000000000"
        return NS(RunId=Id(run), AllErrorMessages=["Failed to resume"])

    def CancelEditSequence(self):
        self.cancelled = True
        return NS(Result="Success")


def test_append():
    queue = FakeQueue()
    row = {"method": "m.amx", "vial": "A1", "result_name": "run2"}
    fake_session(queue).append("s1", [row], wait_minutes=30, resume_poll_s=0)
    assert queue.updated[:2] == list(queue.rows)
    assert [r.DataFilename for r in queue.updated[2:]] == ["run2", None]
    assert queue.updated[3].Action.WaitTime == ("min", 30)
    assert queue.resumes == 2 and not queue.cancelled

    queue = FakeQueue()
    try:
        fake_session(queue).append("s1", [dict(row, result_name="ROW1")])
    except ol.SubmitError as e:
        assert "row1" in str(e).lower()
    else:
        raise AssertionError("expected SubmitError")
    assert queue.cancelled and queue.updated is None


def test_reconnect_and_injection_volume():
    FakeController.made = 0
    with contextlib.redirect_stderr(io.StringIO()) as err:
        session = ol.Session("pipe", "LC", "P", init_wait_s=0.05, retry_s=0)
    assert FakeController.made == 2 and session._ready.is_set()
    assert "reconnecting" in err.getvalue()
    session.submit("m.amx", "A1", "/r")
    assert session.controller.submitted.UseMethodInjectionVolume is True
    session.submit("m.amx", "A1", "/r", injection_volume=5)
    params = session.controller.submitted
    assert params.UseMethodInjectionVolume is False
    assert params.InjectionVolume == 5
    rows = [{"method": "m", "vial": "A1", "result_name": "a"},
            {"method": "m", "vial": "A1", "result_name": "b",
             "injection_volume": 2.0}]
    recs = session.records(rows)
    assert [r.UseMethodInjectionVolume for r in recs] == [True, False]


def test_wait_pause():
    def record(status):
        return NS(Id=Id("s1"), Status=Id(status))

    session = fake_session(None)
    session.events = deque([[record("Waiting")],
                            [record("Editing"), record("Validating"),
                             record("Waiting")],
                            [record("Acquiring")], [record("Waiting")],
                            [record("Acquiring")]])
    assert session.wait("s1", timeout_s=5, poll_s=0, pause=True) == "Waiting"
    assert len(session.events) == 1

    session.events = deque([[record("Waiting")]])
    assert session.wait("s1", timeout_s=0.05, poll_s=0.01, pause=True) is None
    session.events = deque([[record("Acquiring")], [record("Waiting")]])
    assert session.wait("s1", timeout_s=0.05, poll_s=0.01) is None


def test_main_edit(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    argv = ["m1", "--save-as", "new", "--flow", "0.5", "--edit-only"]
    code, out = run_main(project, argv)
    assert code == 0
    assert out == [os.path.join(project, "Methods", "new.amx")]
    assert FakeSession.submitted["edits"]["flow"] == 0.5
    with contextlib.redirect_stderr(io.StringIO()):
        assert run_main(project, argv)[0] == 2


def test_main_wait_row_and_append(tmp_path):
    project = str(tmp_path)
    touch(os.path.join(project, "Methods", "m1.amx"))
    loop = os.path.join(project, "Results", "loop")
    code, out = run_main(project, ["m1", "--vial", "A1", "--wait-row", "30",
                                   "--result-path", "loop",
                                   "--result-name", "run1"])
    assert code == 0
    assert out == ["s1", os.path.join(loop, "set.rslt", "run1.dx")]
    assert FakeSession.submitted["wait_minutes"] == 30
    assert FakeSession.submitted["result_name"] == os.path.join(loop, "<D>")

    code, out = run_main(project, ["m1", "--vial", "A1", "--append", "s1",
                                   "--result-path", "loop",
                                   "--result-name", "run2"])
    assert code == 0
    assert out == [os.path.join(loop, "set.rslt", "run2.dx")]
    assert FakeSession.submitted["run_id"] == "s1"
    assert FakeSession.submitted["rows"][0]["result_name"] == "run2"


if __name__ == "__main__":
    for name, fn in sorted(globals().items()):
        if name.startswith("test_"):
            if fn.__code__.co_argcount:
                with tempfile.TemporaryDirectory() as tmp:
                    fn(tmp)
            else:
                fn()
            print("ok", name)
