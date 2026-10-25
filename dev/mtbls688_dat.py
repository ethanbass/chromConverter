import lzma, pickle, sys, io
import numpy as np
OK = {("numpy.core.multiarray", "_reconstruct"), ("numpy", "ndarray"), ("numpy", "dtype")}
class U(pickle.Unpickler):
    def find_class(self, mod, name):
        if (mod, name) in OK:
            return getattr(np, name) if mod == "numpy" else np.core.multiarray._reconstruct
        raise pickle.UnpicklingError(f"blocked {mod}.{name}")
def load(path):
    raw = open(path, "rb").read()
    dec = lzma.LZMADecompressor(format=lzma.FORMAT_RAW, filters=[{"id": lzma.FILTER_LZMA1, "dict_size": 1 << 26, "lc": 3, "lp": 0, "pb": 2}])
    return U(io.BytesIO(dec.decompress(raw[5:])), encoding="latin1").load()
def walk(node, scans):
    if isinstance(node, dict):
        if node.get("tag") == "scan":
            scans.append(node)
        for c in node.get("children", []) or []:
            walk(c, scans)
if __name__ == "__main__":
    d = load(sys.argv[1]); scans = []; walk(d, scans)
    print("scans", len(scans))
    npk = [int(s["attrib"].get("peaksCount", 0)) for s in scans]
    nmz = [len(s["data"]["mz"]) if isinstance(s.get("data"), dict) else -1 for s in scans]
    print("peaksCount sum", sum(npk), "array len sum", sum(nmz), "nonempty arrays", sum(n > 0 for n in nmz))
    for s in scans[:3] + [x for x in scans if isinstance(x.get("data"), dict) and len(x["data"]["mz"])][:2]:
        print(s["attrib"], {k: (v[:5] if hasattr(v, "__len__") else v) for k, v in s["data"].items()} if isinstance(s.get("data"), dict) else s.get("data"))
