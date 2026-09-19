"""Read spectra from an mzML file, for comparing a parser against msconvert.

Deliberately dependency-free and strict: it resolves array identity, precision
and compression from the cvParams rather than assuming an order, because the
comparisons this supports are at the sub-ppm level.
"""
import base64, re, struct, zlib, io

MZ_ARRAY, INT_ARRAY = "MS:1000514", "MS:1000515"
F64, F32, ZLIB, NOCOMP = "MS:1000523", "MS:1000521", "MS:1000574", "MS:1000576"


def _decode(block):
    acc = set(re.findall(r'accession="(MS:\d+)"', block))
    b64 = re.search(r"<binary>([^<]*)</binary>", block)
    if b64 is None or not b64.group(1).strip():
        return None, acc
    raw = base64.b64decode(b64.group(1).strip())
    if ZLIB in acc:
        raw = zlib.decompress(raw)
    if F64 in acc:
        fmt, size = "<%dd", 8
    elif F32 in acc:
        fmt, size = "<%df", 4
    else:
        raise ValueError("no precision cvParam on binaryDataArray")
    return struct.unpack(fmt % (len(raw) // size), raw), acc


def spectra(path, limit=None, want_ms_level=None):
    """Yield dicts with index, scan id, ms_level, rt, mz, intensity."""
    buf = ""
    n = 0
    with io.open(path, encoding="utf-8", errors="replace") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), ""):
            buf += chunk
            while True:
                i = buf.find("<spectrum ")
                if i < 0:
                    break
                j = buf.find("</spectrum>", i)
                if j < 0:
                    break
                block = buf[i:j]
                buf = buf[j + 11:]
                attrs = dict(re.findall(r'(\w+)="([^"]*)"', block[:400]))
                lvl = re.search(r'accession="MS:1000511"[^/]*value="(\d+)"', block)
                lvl = int(lvl.group(1)) if lvl else None
                if want_ms_level is not None and lvl != want_ms_level:
                    continue
                rt = re.search(r'accession="MS:1000016"[^/]*value="([-0-9.eE]+)"', block)
                arrays = {}
                for bda in re.findall(r"<binaryDataArray[ >].*?</binaryDataArray>",
                                      block, re.S):
                    vals, acc = _decode(bda)
                    if MZ_ARRAY in acc:
                        arrays["mz"] = vals
                    elif INT_ARRAY in acc:
                        arrays["intensity"] = vals
                yield {
                    "index": int(attrs.get("index", -1)),
                    "id": attrs.get("id", ""),
                    "ms_level": lvl,
                    "rt": float(rt.group(1)) if rt else None,
                    "mz": arrays.get("mz"),
                    "intensity": arrays.get("intensity"),
                }
                n += 1
                if limit and n >= limit:
                    return
            if buf.count("</spectrumList>"):
                pass
