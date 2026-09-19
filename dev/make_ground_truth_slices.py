"""Build compact ground-truth slices from msconvert output.

The mzML files themselves are 40-60 MB each, which is far too much to ship as
test data, but only a handful of spectra are needed to pin down m/z, MS level
and polarity. This writes one gzipped CSV per fixture.

Profile spectra hold 10-20k points each, so those files get only a couple of
spectra; MRM and SIM spectra hold 1-5 points, so those can afford hundreds.

NOTE for profile data (shimadzu_tlm_scan, and the product-ion spectra of
shimadzu_tlm_sim): the *intensities* in these slices are not the raw stored
values. 'ProteoWizard' returns the vendor's ringing-suppressed profile, which
runs ~7% high on total ion current and ~20% low at the peak apex. Validate
profile intensities against the file's own `TIC Data` stream instead. The m/z
grid, MS level and polarity in these slices are correct.

    python3 dev/make_ground_truth_slices.py
"""
import csv, gzip, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from mzml_ground_truth import spectra

DEST = "/Users/ethanbass/R_packages/chromConverterExtraTests/inst"
CONV = "/Users/ethanbass/windows"

# (fixture, mzML, how many spectra to scan, filter)
#   filter: None = take all scanned; "sparse" = keep spectra with <100 points
#           plus the first 2 dense ones (keeps profile files small)
JOBS = [
    ("shimadzu_qtof",         f"{CONV}/convert/shimadzu_qtof.mzML",              200, None),
    ("shimadzu_qtof_neg",     f"{CONV}/convert/20220412_NEG_NTJ12.mzML",          20, None),
    ("shimadzu_tlm_mrm",      f"{CONV}/convert/Std-DOX-LCMS-ESIpos.mzML",        200, None),
    ("shimadzu_tlm_mrm_multi",
     f"{CONV}/99582_08.04.2025_2102_Oxilipine_Sample_PEX_12__412_13.mzML",       400, None),
    ("shimadzu_tlm_sim",      f"{CONV}/95616_bcin.mzML",                          60, "sparse"),
    ("shimadzu_tlm_scan",     f"{CONV}/87242_blank.mzML",                          8, "sparse"),
]

def build(name, mzml, limit, mode):
    if not os.path.exists(mzml):
        print(f"  {name}: mzML missing, skipped"); return
    rows, dense = [], 0
    for s in spectra(mzml, limit=limit):
        n = 0 if s["mz"] is None else len(s["mz"])
        if mode == "sparse" and n >= 100:
            if dense >= 2:
                continue
            dense += 1
        if n == 0:
            rows.append((s["index"], s["ms_level"], "", ""))
            continue
        for m, i in zip(s["mz"], s["intensity"]):
            rows.append((s["index"], s["ms_level"], f"{m:.7f}", f"{i:.0f}"))
    out = os.path.join(DEST, f"{name}_gt.csv.gz")
    with gzip.open(out, "wt", compresslevel=9, newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["scan", "ms_level", "mz", "intensity"])
        w.writerows(rows)
    print(f"  {name:<24} {len(rows):>8} rows  {os.path.getsize(out)/1024:>8.1f} KB")

if __name__ == "__main__":
    print("writing ground-truth slices:")
    for job in JOBS:
        build(*job)
