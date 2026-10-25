# Public Agilent MS data: OpenLab CDS and MassHunter

A survey made on 2026-10-20 to find test files for the native `MSScan.bin` /
`MSPeak.bin` reader (`read_agilent_msdata` in `R/read_agilent_dx.R`).

## What was searched

- MassIVE / GNPS: the GNPS2 file-name index (`datasetcache.gnps2.org`) and the
  metadata of all 20,426 datasets.
- MetaboLights: the file lists of all 3,551 public studies.
- Metabolomics Workbench: the mwTab files of all 4,343 MS studies.
- PRIDE and EBI BioStudies: keyword and file searches.
- 4TU.ResearchData: the file lists of all 12,527 public datasets.
- Zenodo, Figshare, Dryad, OSF, Harvard and UNC Dataverse, GitHub and GitLab.

## In brief

- OpenLab CDS MS data exists publicly only from one group, the Leiden Ancient
  Adhesives group on 4TU. All of it comes from one 5977 GC/MSD and one method.
- No repository holds raw LC/MSD single quadrupole data (61xx, iQ, XT), either
  from OpenLab or from MassHunter. The only runs that switch polarity are
  triple quadrupole MRM (MTBLS15503, MTBLS6337), which the native reader cannot
  decode.
- MassHunter `.d` data is plentiful: 315 MassIVE datasets contain
  `AcqData/MSScan.bin`, and 136 of them pair a `.d` with an mzML or mzXML of
  the same name.

## Fixtures in chromConverterExtraTests

| Fixture | Source | Licence | Checked against |
|---|---|---|---|
| `openlab_gcms.sirslt` | Morín Cave, 4TU, doi:10.4121/fc28fa90-cba2-485a-9221-033be73a0e04.v2 (injection `20220718 MOR47_1`) | CC BY 4.0 | each scan's TIC and base peak, exact |
| `masshunter_gcms_sim.D` | MetaboLights MTBLS15607 (`AKI_1.D`, 5977 SIM, two time segments) | CC0 1.0 (stated in its `i_Investigation.txt`) | its `data.ms`, within that file's precision |
| `masshunter_qqq_sim.d` + `.mzML` | ProteoWizard Agilent test archive (`reserpine-MS2sim-010`, G6410A SIM) | Apache 2.0 (the repository's licence; the test data carry no terms of their own) | ProteoWizard's TIC and SIM chromatograms, exact |

## OpenLab CDS result sets

| Source | Licence | Contents | Notes |
|---|---|---|---|
| Morín Cave, 4TU, doi:10.4121/fc28fa90-cba2-485a-9221-033be73a0e04.v2 | CC BY 4.0 | 30 `.sirslt`, 5977 with FID, EI full scan m/z 35–950 | all 30 TIC-exact |
| Zambia adhesives, 4TU, doi:10.4121/b1c2692e-38fa-4526-88ab-15b2ff782a0a | CC BY 4.0 | 27 zipped `.sirslt`, 2.5–4 MB each | same group and workflow |
| Steenbokfontein Cave, 4TU, doi:10.4121/ddc23e52-c230-45a8-a921-56469c0129e7 | CC BY 4.0 | 8 zipped `.sirslt` | same group and workflow |
| Aceramic birch tar, 4TU, doi:10.4121/87cdb877-3757-4e05-8618-906140752a83 | CC BY 4.0 | 29 zipped `.sirslt` | same group and workflow |
| Palaeolithic birch tar, 4TU, doi:10.4121/21908796.v1 | CC BY 4.0 | 8 zips, no `.sirslt` folder | same group and workflow |
| ombayley/ChromTroller (GitHub), `docs/example_datasets/hplc/simple/*.dx` | GPL-3.0 | 5 `.dx` from OpenLab 2.8, LC/MSD XT (G6135C) | `MSData/*.xml` only, no `.bin` files |

Non-MS OpenLab data: Zenodo 14316687 (54 GC-FID `.dx`/`.rx` with CSV and PDF
reports, CC BY) and cloudbywu/chemapp on GitHub (9 LC `.sirslt`, MIT).

## MassHunter `.d` files decoded in this survey

`Format:width` is `SpectrumFormatID` and bytes per point of the `MSPeak.bin`
spectra (see `mspeak_types`). Format 2 is a profile in `MSProfile.bin`, given
here with the size of its header: 16 bytes (two doubles, then float32
intensities) or 8 bytes (two float32s, then int32 intensities). Either way the
m/z step is the scan's `SamplingPeriod`; the stored step can be slightly
smaller (0.09999847 against 0.1 in the 8-byte files).

`1:8` is ambiguous. The OpenLab 5977 scans store float32 m/z and float32
intensities, while the MRM scans of `Thyrxox 5 TS Diff Scan B.d` (MS level 2,
scan type 256) store float32 m/z and int32 intensities. `read_mspeak_spectra`
reads `1:8` as integers at MS level 2, which matches ProteoWizard on `Thyrxox`.

MS2 (scan types 512, product ion, and 256, MRM) is read with `MzOfInterest` as
the precursor. On the ProteoWizard files below, every product ion point and SRM
chromatogram point matches its mzML exactly. Each MRM scan record has one
`MzOfInterest`, and a run with several precursors gives each its own scan
(`MRM Neg C5.d`). Neutral loss (2048) and precursor ion (1024) scans are skipped.

| Source | Instrument and mode | Format:width | Reference | Result |
|---|---|---|---|---|
| Figshare doi:10.6084/m9.figshare.28050092 (`Yucca_rostrata.D`, Craig Schenck), CC BY 4.0 | 8890 / 5977C, full scan | 1:16 | `data.ms` | match, 4,847 scans |
| MetaboLights MTBLS15607 (`AKI_1.D`), CC0 | 5977, SIM, 2 time segments | 1:16 | `data.ms` | match |
| 4TU doi:10.4121/89ba9ee3-f072-4dda-97ba-683cce975a3b (13C yeast extract), CC BY-NC 4.0 | 5975, SIM, 11+ time segments (40 `.D`) | 1:16 | `data.ms` | 40 of 40 match exactly |
| MetaboLights MTBLS13943 (`A-03.D`) | 5977B, SIM/Scan, 14 time segments | 1:16 | `MaxY`; mzML exists but was not compared | spectrum maxima match `MaxY` |
| ProteoWizard `reserpine-MS2sim-010.d` | G6410A, SIM | 3:8 (f32 m/z, i32 intensity) | ProteoWizard mzML | exact |
| ProteoWizard `MRM Neg C5.d` | G6410A, MRM | 3:8 | ProteoWizard SRM chromatograms | MS2, exact |
| MetaboLights MTBLS243, MTBLS15503; PRIDE PXD071097 | QQQ, dMRM | 3:12 | — | MS2 only, so TIC/BPC only |
| ProteoWizard `GFb_4Scan_TimeSegs_1530_100ng.d`, `Thyrxox 5 TS Diff Scan B.d` | G6410A (`Thyrxox` names no model, only a tandem quadrupole), MS1 profile scans and MRM | 2, 8-byte header; MRM in 3:8 (`GFb`) and 1:8 (`Thyrxox`, int32 intensities) | ProteoWizard mzML | 63 and 82 MS1 scans match, every intensity exact and m/z within one float32 step; product ion and MRM scans read as MS2, exact |
| ProteoWizard `RS080806_APCI_PIscan_CE35.d`, `RS080806_NL_448.2_001.d`, `RS080906_MMI_PIscan_CE35_250ms.d` | G6410A, product ion scans of m/z 609.3 (`PIscan`) and a neutral loss scan (`NL`), all profile | 2, 8-byte header | ProteoWizard mzML (`PIscan`) | product ion scans read as MS2, exact; the `NL` scans are skipped, and its mzML holds no spectra |
| chromConverterExtraTests `masshunter.d` | G6410A, product ion profile scans (precursor m/z 611.3) | 2, 16-byte header | each scan's TIC and base peak | read as MS2, all 49 scans match |
| MetaboLights MTBLS13090 (`D01B_NEG.d`) | 6224 CE-TOF | 2:12 | — | unsupported; TIC/BPC only |
| MetaboLights MTBLS1334, MTBLS599; MassIVE MSV000081960 | Q-TOF | — | — | scan records are not fixed width; refused |
| ProteoWizard `ImsSynth*.d` | ion mobility (synthetic) | — | — | different scan schema; refused |

## MassHunter `.d` candidates not yet decoded

Licences were not checked dataset by dataset. MassIVE datasets are CC0 by
default, and MetaboLights studies state their own licence, either CC0 1.0 or
the EMBL-EBI Terms of Use. Confirm before shipping a fixture.

GC/MSD and GC triple quadrupole:

- MassIVE MSV000086159: 5977B SIM, 52 `.d`, `tic_front.csv` but no mzML.
- MassIVE MSV000093092 (5973N), MSV000101792, MSV000102547 and MSV000093030
  (5977B): full scan, paired with mzML.
- MassIVE MSV000094784: GC triple quadrupole (G7000), 27 time segments.
- MetaboLights MTBLS1636 (5975, mzXML) and MTBLS13781 (5977B, mzML).
- MetaboLights MTBLS13904: 7010C with an FID channel and ANDI CDF exports.
- 4TU doi:10.4121/5e6b4b53-6188-4254-a729-47991bbb8832 (Nasonia, CC BY 4.0):
  7010B full scan in format 1, a 4.97 GB tar with no open exports.

LC triple quadrupole:

- MassIVE MSV000090568: 6490 MRM, 804 `.d` with mzML holding chromatograms only.
- MassIVE MSV000080197 (6460, with DAD), MSV000089914 (6495C, dMRM),
  MSV000088072 and MSV000082739.
- MetaboLights MTBLS15503 and MTBLS6337: positive/negative switching between
  transitions, with mzML or mzXML.
- MetaboLights MTBLS4722 (6470), MTBLS253 (dMRM), MTBLS5152 (profile only, a
  candidate for the profile reader).

Q-TOF, TOF and ion mobility (the native reader refuses these):

- MassIVE MSV000093589 and MSV000093591 (6545A, DDA, mzXML), MSV000102533
  (6530, APCI), MSV000086911 (6520A, mzML), MSV000085979 (6560 IM-QTOF),
  MSV000102903 (6545B, profile only), MSV000094184 (6230B TOF).
- MetaboLights MTBLS1334 (6545B, profile and MS/MS, mzML), MTBLS599 (6540B,
  mzXML), MTBLS2328, MTBLS449 and MTBLS5893.
- 4TU doi:10.4121/51da0e25-3c0f-48b8-93b0-1b0b0c3f0cdb.v4 (mulberry, CC BY
  4.0): 36 G6540B `.d` with `mzdata.xml` exports, in a 1.37 GB RAR.
- Figshare doi:10.6084/m9.figshare.31972497 (6546A, CC BY) and
  doi:10.6084/m9.figshare.25777749 (6230B CZE-MS, CC BY).
- GitHub: michaelmarty/UniDecImporter `TestData/test_agilent.d` (G6560B, with
  mzML, BSD-3) and PNNL-CompBio/ion-mob-ms (46 IM-QTOF `.d` with mzML, BSD-2).
- GC/Q-TOF (7200): MassIVE MSV000082511, MSV000084401, MSV000084856 and
  MSV000082276, all paired with mzML but 1–3 GB per run.

ChemStation-format LC/MSD (`MSD1.MS`, not MassHunter): MetaboLights MTBLS10724.
