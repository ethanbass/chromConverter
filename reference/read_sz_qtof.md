# Read 'Shimadzu' QTOF stream

Read QTOF stream from 'Shimadzu LabSolutions' `.lcd` files.

## Usage

``` r
read_sz_qtof(
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

  Format of output. Spectra have no wide representation, so this is
  always `long`.

- levels:

  Which MS levels to return, spelled `MS1` and `MS2`. Both are decoded
  either way, since the level of a scan is recorded in the scan.

- read_metadata:

  Logical. Whether to read metadata from the file.

- metadata_format:

  Format to output metadata in.

- scale:

  Logical. Whether to scale raw detector counts to the intensities
  reported by 'LabSolutions'. Defaults to `TRUE`, which errors if the
  accumulation count is missing from the file or differs between its
  acquisition events, since there is no safe divisor to guess; `FALSE`
  returns the raw counts.

## Value

A named list holding whichever of the requested `levels` the file
contains, each a `data.table` or `data.frame` in long format with
columns `scan`, `rt`, `mz` and `intensity`, and a `precursor_mz` column
before `mz` for product-ion spectra. Each carries a `scan_info`
attribute giving one row per spectrum of that level, including the ones
that hold no peaks, with the acquisition event, MS level, DDA cycle, ion
polarity, selected precursor and peak count.

## Details

Data for each scan is stored in three contiguous blocks: a 64-byte
header (72 bytes in some files), a block of flight times, followed by a
block of intensities.

**Scan Header** (64 bytes, little-endian):

|  |  |  |
|----|----|----|
| **Offset** | **Type** | **Field** |
| 0–3 | `uint32` | Data-dependent acquisition (DDA) cycle: sequential over survey scans, repeated by the product-ion scans acquired from each |
| 4–7 | `uint32` | Retention time (milliseconds) |
| 8–15 | `uint64` | Base peak flight time (0 if empty scan) |
| 16–19 | `uint32` | Base peak raw intensity (0 if empty scan) |
| 20–23 | `uint32` | Scan index (0-based; increments for every scan including empty ones) |
| 24–27 | `uint32` | Data block size in bytes (`n_peaks * (8 + int_width)`; 0 if empty scan) |
| 28–31 | `uint32` | MS level in the upper 16 bits, acquisition event in the lower 16 bits (e.g. `0x00010001` = MS1 event 1, `0x00020003` = MS2 event 3) |
| 32–35 | `uint32` | Padding |
| 36–39 | `uint32` | Intensity width in bytes (`int_width`) in the low byte; the high byte is sometimes `0x01`. 1, 2 and 4 all occur on scans with data |
| 40–43 | `uint32` | Instrument constant (scan window / detector setting) |
| 44–47 | `uint32` | Instrument constant |
| 48–51 | `uint32` | Instrument constant |
| 52–55 | `uint32` | Instrument constant |
| 56–63 | — | Padding |

**m/z block** (n x 8 bytes) Each peak's flight time is an unsigned
64-bit little-endian integer. R has no 64-bit integer type, so it is
read as two 32-bit halves and recombined as `low + high * 2^32`. The low
half is unsigned, but R has no unsigned 32-bit type and reads it as
signed, so it is corrected before recombining. Left uncorrected, it
would shift half of all flight times by `2^32` (2 to 9 ppm of flight
time in the files examined, so twice that in m/z) and read `0x80000000`
as `NA`. Conversion is \$\$mz = ((t-B)/A)^2\$\$

`A` and `B` are fitted per file from the `TOF Calibration Table` stream
(see `read_sz_qtof_calibration`), which stores calibration points for
both polarities; the polarity of the acquisition selects between them.
This matters: the coefficients differ by ~1.5% between polarities on one
of the two instruments examined (0.006% on the other), which is ~3% in
m/z, so fixed coefficients are not usable. The tuning calibration alone
is still a few ppm out, because it is recorded before the run; folding
in the mass correction the file caches for the run closes that gap (see
`read_sz_qtof_mass_correction`). Measured against 'ProteoWizard'
conversions of three files from two instruments — which read the file
through Shimadzu's own library — the m/z then agree to 0.03-0.1 ppm
(median). That figure is the resolution of the comparison rather than of
this parser: the vendor reports m/z to four decimal places, which is 0.5
ppm at the bottom of the mass range and 0.05 ppm at the top.

**Intensity block** (n x m bytes) Each peak's intensity is stored as a
little-endian unsigned integer of `m` bytes, where `m` is the
`int_width` field from the scan header (2 for most scans, 4 for scans
containing large values, and occasionally 1). The stored values are raw
detector counts summed over every TOF accumulation in the scan — the
individual time-of-flight spectra the instrument adds together to make
one. LabSolutions (and the 'Shimadzu' library used by 'ProteoWizard')
normalizes them to a nominal 100 accumulations: \$\$intensity =
round(raw \times 100 / n)\$\$ where `n` is the number of accumulations
per scan (rounding is half-up). `n` is read from the `Status` stream,
which stores it once per acquisition event; it is 376 in the reference
file, giving a divisor of 3.76.
