# Read 'Shimadzu' IT-TOF stream

Read the `TTFL Raw Data` mass spectrometry stream written by the
'Shimadzu' LCMS-IT-TOF to `.lcd` files.

## Usage

``` r
read_sz_ittof(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = "long",
  levels = c("MS1", "MS2"),
  read_metadata = TRUE,
  metadata_format = "shimadzu_lcd",
  scale = TRUE
)
```

## Arguments

- path:

  Path to `.lcd` file.

- format_out:

  Class of output. Either `data.table` or `data.frame`. Spectra are long
  and mixed-type, so `matrix` resolves to `data.table`.

- data_format:

  Ignored: spectra are always returned in `long` format.

- levels:

  Which MS levels to return, spelled `MS1` and `MS2`. Both are decoded
  either way.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata.

- scale:

  Logical. Whether to scale the stored values to the units of the total
  ion current. `FALSE` returns them as stored.

## Value

A named list holding whichever of the requested `levels` the file has,
each a long table with columns `scan`, `rt` (minutes), `mz` and
`intensity`, and a `precursor_mz` column before `mz` for MS2. A per-scan
summary (retention time, event, MS level, polarity, precursor m/z, point
count) is attached to each as a `scan_info` attribute.

## Details

`Data Index` holds one 16-byte record per scan: the scan's byte offset
into `MS Raw Data` (`uint64`), the cycle number and a scan counter. Each
scan in `MS Raw Data` is a 256-byte header followed by a
run-length-encoded profile. The header fields read here are:

|  |  |  |
|----|----|----|
| **Offset** | **Type** | **Field** |
| 0–3 | `uint32` | Scan index (0-based) |
| 4–7 | `uint32` | Cycle number |
| 8–11 | `uint32` | Start of the cycle (milliseconds), as in `Retention Time` |
| 12–15 | `uint32` | Acquisition time of the scan (milliseconds) |
| 16–19 | `uint32` | Acquisition event (0-based) |
| 20–23 | `uint32` | MS level in the upper 16 bits, polarity in the lower 16 (0 = positive, 1 = negative) |
| 144–147 | `uint32` | Last flight-time bin of the scan range |
| 148–151 | `uint32` | Flight time of bin 0 |
| 154–155 | `uint16` | Intensity multiplier |
| 164–167 | `uint32` | Length of the profile in 16-bit words |
| 168–171 | `uint32` | Total ion current |
| 176–179 | `uint32` | Precursor m/z x 10000 (MS2 and above) |

**Profile.** The profile is a sequence of 16-bit words. A word with bit
15 clear is a gap: the number of empty flight-time bins to skip. A word
with bit 15 set opens a run, and its lower 15 bits give the number of
intensities that follow, one per consecutive bin; the intensities
themselves use all 16 bits. A run of length 0 ends the scan.

**Intensities.** The header's total ion current, and the `TIC Data`
streams, equal the sum of the stored values times the scan's intensity
multiplier, so `scale = TRUE` multiplies them by it. The multiplier is
usually 128, but reaches 4096 in some intense scans of PXD025121.
'LabSolutions' displays and exports the profile after a 5-point moving
average, which is not applied here.

**Mass axis.** Bin `b` of a scan has flight time `t = b + t0`, where
`t0` is read from the header. `TTFL Tuning/Tuning Result 00` stores, for
each polarity, the reference masses of the calibrant (sodium
trifluoroacetate clusters) with their measured flight times. Between two
reference ions, `sqrt(mz)` is the quadratic in `t` through that pair and
the next ion up; below the first, it is the line through the first two.
The last interval and anything above it reuse the quadratic through the
last three ions.

Checked against the vendor's own `TTFLDataExport` profile export of an
MTBLS688 file, this reproduces every intensity of all 3002 scans
exactly, once the 5-point moving average is applied, and every m/z to
the four decimal places the export keeps. The centroids of an MTBLS432
file (vendor ANDI-MS exports) and of a PXD020792 file (Mascot Distiller
peak lists) agree to within a fraction of a flight-time bin. Above the
last reference ion, the median error against the Mascot Distiller
centroids of PXD025121 files stays within 2 ppm up to m/z 1500.

## Author

Ethan Bass
