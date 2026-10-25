# Public Shimadzu raw data: `.lcd`, `.qgd` and `.gcd`

A listing of the public datasets that hold 'Shimadzu' LabSolutions (`.lcd`,
`.gcd`) and GCMSsolution (`.qgd`) files, and of which ones chromConverter has
been checked against. The full table, one row per dataset and extension, is
`dev/shimadzu_datasets.csv`.

## How the list was made

`dev/find_shimadzu_datasets.R` queries the GNPS2 file index
(`datasetcache.gnps2.org`), which covers every file in MassIVE, MetaboLights
and Metabolomics Workbench. The datasets were listed again on 2026-10-21. Their
titles and instruments come from the MassIVE PROXI API, the MetaboLights
`i_Investigation.txt` and assay files, and the Workbench `analysis` records.

- The index records file sizes only for MassIVE, so MetaboLights and Workbench
  rows have no size.
- Many MassIVE datasets give their instrument only as the placeholder
  "instrument model". The CSV leaves these blank.
- An instrument list names every instrument in the study, so it can name an LC
  or MS from another vendor beside the 'Shimadzu' one. A MetaboLights `.lcd`
  study whose MS is not 'Shimadzu' (MTBLS14820, an Agilent 6546) probably holds
  LC or PDA data only.
- The GNPS index does not cover PRIDE. PXD034978 and PXD019638 (`.qgd`) and
  PXD025121 and PXD020792 (IT-TOF `.lcd`) were found separately.
- MassIVE datasets are CC0 by default. MetaboLights studies state their own
  licence, either CC0 1.0 or the EMBL-EBI Terms of Use; the CSV gives it.

| Extension | Datasets | Files | MassIVE | MetaboLights | Workbench |
|---|---|---|---|---|---|
| `.lcd` | 38 | 7,324 | 19 | 15 | 4 |
| `.qgd` | 60 | 2,776 | 21 | 21 | 18 |
| `.gcd` | 1 | 33 | 1 | 0 | 0 |

## Datasets the readers were checked against

| Dataset | Instrument | Used for |
|---|---|---|
| MSV000084197 | Q-TOF (model not stated) | `shimadzu_qtof.lcd`; Q-TOF intensity scale and flight-time calibration, checked against the paired 'ProteoWizard' mzML (2,983 peaks in 400 scans) |
| MSV000091160 | Q-TOF (model not stated) | `shimadzu_qtof_neg.lcd`; a second intensity divisor (1.94), and the negative-mode calibration |
| MSV000100201 | Q-TOF (model not stated) | all 9 files checked against their LabSolutions 5.120 mzXML for the flight-time encoding |
| MSV000087242 | LCMS-8040 | `shimadzu_tlm_scan.lcd`; scan types 10 (MS1 scan) and 14 (product-ion scan) |
| MSV000095616 | LCMS-8040 | `shimadzu_tlm_sim.lcd`; scan type 11 (SIM) |
| MSV000100356 | not stated | `shimadzu_tlm_mrm.lcd`; MRM |
| MSV000099582 | LCMS-8040 | `shimadzu_tlm_mrm_multi.lcd`; scan type 15 (MRM), 179 events in both polarities, byte-exact on 3,000 spectra |
| MSV000096132 | GCMS-QP2010 SE | `.qgd` GC method streams |
| MSV000102857 | GCMS-QP2010 Ultra | `.qgd` GC method streams |
| MSV000101547 | GCMS-QP2020 NX on a GC-2030 | `.qgd` GC method, checked against GCMSsolution method printouts |
| MSV000102055 | GCMS-QP2020 NX on a GC-2030 | `.qgd` GC method, checked against printouts |
| MSV000097538 | not stated | `n_Alkane_01072024_method_only.qgd.gz`; GC method, checked against printouts |
| PXD034978 (PRIDE) | GC-MS/MS, model not stated | `QC__8122021_7.qgd`; 8-byte spectrum index and MRM scans; summed intensities match `TIC Data` in all 84,420 scans |
| MTBLS11411 | GCMS-TQ8050 NX | `240403_24_Disease A3.qgd`; full scans (50–500) interleaved with MRM through the run; summed intensities match `TIC Data` in all 96,433 scans; no format code 1 (4-byte) scans |
| MTBLS688 | LCMS-IT-TOF | `1_1k10_20ul_1.lcd`, `7_plus__1k5_10ul_1`, `K1-1_1`, `10-1_1`, `7-1_1` (2014–2016); IT-TOF decoder and calibration, exact against the vendor's `TTFLDataExport` (`.dat`) in all 3,002 scans of each (`dev/validate_ittof_mtbls688.R`) |
| MTBLS432 | LCMS-IT-TOF | `6-wk_HZ_CC_male_1_63__30min_pos-neg_26.lcd`; 4 events (60–200 and 140–1250, both polarities), m/z against the vendor's ANDI-MS centroids (`_Seg1Ev1–4.CDF`) |
| MSV000085762 | LCMS-IT-TOF | `QC__30min_pos-neg_42.lcd` (GNPS copy of MTBLS432); decodes, TIC matches in all 6,844 scans |
| MSV000085792 | LCMS-IT-TOF (MTBLS727) | `008.lcd`; decodes, TIC matches in all 11,102 scans; negative calibrant table starts at m/z 113; no vendor export to check m/z |
| PXD020792 (PRIDE) | LCMS-IT-TOF | all 5 files; `shimadzu_ittof.lcd` is `UY02-01-01 (p400).LCD`; MS1 plus 3 MS2 events, precursors and fragments against the Mascot Distiller `.mgf` |
| PXD025121 (PRIDE) | LCMS-IT-TOF | `1`, `7`, `24-1`, `29.lcd`; TIC exact once the per-scan intensity multiplier is applied; `7.lcd` MS2 against its Mascot `.dat` (`F015801`) |

The `.lcd` ground-truth slices in chromConverterExtraTests (`*_gt.csv.gz`) come
from 'ProteoWizard' conversions of these files. The remaining 'Shimadzu'
fixtures there come from outside these repositories: `Anthocyanin.lcd`
(issue #11), `multichannel_chrom.lcd` (Andrew Tolonen, MIT), `FS19_214.gcd`
(Andrew Legan, CC0), `B4NF.7_C23.qgd` (Dryad doi:10.5061/dryad.8gtht76s4, CC0)
and `shimadzu_tlm_dda.lcd` (Paul Ivanov, CC0).

## Candidates not yet checked

`.lcd` mass spectrometry:

- LCMS-9030 Q-TOF: MassIVE MSV000093343, MSV000093475 and MSV000093477
  (51–96 MB per file).
- LCMS-8040 / 8060 triple quadrupole: MassIVE MSV000094482 and MSV000088672;
  MetaboLights MTBLS2376 (LCMS-8040, 430 files); Workbench ST002729, ST002984
  and ST003912 (LCMS-8060).
- Instrument not stated: MassIVE MSV000100348 (56 files, all under 1 MB).
  Resolved on 2026-10-25 from one file each: MSV000085811 (MTBLS728, 360
  files), MSV000080304 and MSV000079642 are triple quadrupoles (`TLM Raw
  Data`; MSV000085811 is tagged LCMS-IT-TOF in MassIVE, wrongly);
  MSV000085792 and MSV000085762 are IT-TOF (see above).
- LCMS-IT-TOF not yet checked: the other 27 PXD025121 files (each with a
  Mascot `.dat`); the other 2,144 MTBLS688 runs (each
  with its `.dat` export, so `dev/validate_ittof_mtbls688.R` applies) and 92
  MTBLS432 runs (with CDFs); the other 113 MSV000085792 files. The 19 other
  MassIVE datasets tagged LCMS-IT-TOF, and MetaboLights MTBLS4437, have no
  `.lcd` files in the GNPS2 index (checked 2026-10-25).
- Workbench ST001852 lists a GCMS-TQ8040 with a CBM-20A, but holds 2,088 `.lcd`
  files; what they contain is unknown.

`.lcd` LC data, mostly Nexera or Prominence systems: MetaboLights MTBLS688
(2,149 files), MTBLS13204, MTBLS12691, MTBLS5861, MTBLS700, MTBLS727,
MTBLS15755, MTBLS432, MTBLS11411, MTBLS14423, MTBLS14566, MTBLS1960 and
MTBLS7425.

`.qgd` single quadrupole GC-MS:

- MassIVE MSV000090939, MSV000095239 and MSV000090495 (QP2010 SE), and 10
  datasets whose instrument is not stated (MSV000095999, MSV000094511,
  MSV000097242, MSV000094920, MSV000095829, MSV000093614, MSV000091001,
  MSV000095442, MSV000097443 and MSV000098901).
- MassIVE datasets whose stated instrument is not a GC-MS: MSV000099226 (a
  Waters ACQUITY UPLC), MSV000099403 (only the ontology term `MS:1003572`) and
  MSV000088672 (an LCMS-8060).
- MetaboLights MTBLS11326 (QP2020 NX), MTBLS2634, MTBLS2984 and MTBLS457
  (QP2010 Ultra), MTBLS871 and MTBLS872 (QP2010), MTBLS8006 (QP2010 Plus),
  MTBLS808, MTBLS12737 and MTBLS13996.
- PRIDE PXD019638.
- Workbench ST001609, ST001612, ST002080, ST003309, ST003861, ST004909 and
  ST005173 (QP2010 Ultra or Plus), ST002102 and ST004089 (QP2020 NX), and
  ST002933.

`.qgd` triple quadrupole GC-MS/MS (TQ8040, TQ8050 NX). PXD034978 (MRM only) and
MTBLS11411 (full scans mixed with MRM) have been checked; see above:

- MetaboLights MTBLS1858, MTBLS1898, MTBLS2007, MTBLS2015, MTBLS2235, MTBLS2322,
  MTBLS2323, MTBLS2324, MTBLS5383, MTBLS11411 and MTBLS14089.
- Workbench ST003959, ST003960, ST003961 and ST003962 (TQ8050 NX), and ST004249,
  ST004530 and ST004739 (TQ8040).

`.gcd`: MassIVE MSV000088672 (33 files, 1–2 MB) is the only dataset with GC
files, inside a study that lists an LCMS-8060.

Workbench ST001170 lists only a Sciex TripleTOF with a Shimadzu LC-30A, yet holds
26 `.qgd` files.
