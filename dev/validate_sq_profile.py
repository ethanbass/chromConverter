"""Check the 'Shimadzu' single quadrupole profile scans against msconvert.

Usage: python3 dev/validate_sq_profile.py <run.lcd> <run.mzML>

The mzML is an msconvert (ProteoWizard 3.0.26198) export of the same run,
without peak picking. For each profile scan (bit 4 of the event flags at
header byte 44), the stored uint16 points are decoded, a value with bit 15 set
as (v & 0x7FFF) * 32, and compared with the mzML spectrum and `TIC Data`:

- whether `TIC Data` is the sum of the decoded points times 2/3, exactly and
  after rounding;
- whether the exported intensities are a 9-point quadratic Savitzky-Golay
  filter of stored points 12 to n - 15, with integer floor division;
- whether the exported m/z axis is 150.0115238 + i * 0.0666686528.

It prints a count of each outcome. The axis constants were fitted to the one
file it has been run on, Zenodo 18633076 `Medin(20um)_Day_0_Figure1.lcd`
(LCMS-2020, m/z 150-2000). There, in all 1,140 profile scans, `TIC Data` is
round(sum * 2/3) and the axis matches; the smoothing matches in 293 scans.
"""
import olefile, sys, struct, numpy as np, base64, zlib, collections
import xml.etree.ElementTree as ET
lcd, mzml = sys.argv[1], sys.argv[2]
o = olefile.OleFileIO(lcd)
g = lambda n: o.openstream("Mass Raw Data/" + n).read()
ms = g("MS Raw Data"); off = np.append(np.frombuffer(g("Spectrum Index"), "<u4"), len(ms))
tic = np.frombuffer(g("TIC Data"), "<u8")
ns = "{http://psi.hupo.org/ms/mzml}"
SG = np.array([-21, 14, 39, 54, 59, 54, 39, 14, -21], dtype=np.int64)
AXIS = 150.0115238 + 0.0666686528 * np.arange(27750)
k = 0; shown = 0; c = collections.Counter(); steps = []; ratios = []
for ev, el in ET.iterparse(mzml):
    if el.tag != ns + "spectrum": continue
    u = struct.unpack("<32H", ms[off[k]:off[k] + 64])
    if u[22] & 0x10:
        npts = u[30] | (u[31] << 16)
        v = np.frombuffer(ms[off[k] + 64:off[k + 1]], "<u2").astype(np.int64)
        dec = np.where(v & 0x8000, (v & 0x7FFF) * 32, v)
        names = {p.get("name"): p.get("value") for p in el.iter(ns + "cvParam")}
        arrs = {}
        for b in el.iter(ns + "binaryDataArray"):
            pn = {p.get("name") for p in b.iter(ns + "cvParam")}
            d = base64.b64decode(b.find(ns + "binary").text or "")
            if "zlib compression" in pn: d = zlib.decompress(d)
            arrs["mz" if "m/z array" in pn else "int"] = np.frombuffer(d, "<f8" if "64-bit float" in pn else "<f4")
        mz, it = arrs["mz"], arrs["int"]
        c[f"n raw={npts} mzml={len(mz)}"] += 1
        c["tic mzml==TIC Data " + str(float(names["total ion current"]) == tic[k])] += 1
        c["TIC Data == raw sum * 2/3 " + str(dec.sum() * 2 == tic[k] * 3)] += 1
        c["TIC Data == round(raw sum * 2/3) " + str(round(dec.sum() * 2 / 3) == tic[k])] += 1
        sg = np.convolve(dec, SG[::-1], mode="valid")[8:8 + len(it)] // 231
        c["export == SG9 of raw[12:-14] " + str(np.array_equal(sg, it.astype(np.int64)))] += 1
        c["axis == 150.0115238 + i * 0.0666686528 " + str(np.abs(mz - AXIS).max() < 6e-5 if len(mz) == len(AXIS) else False)] += 1
    k += 1; el.clear()
print(dict(c))
