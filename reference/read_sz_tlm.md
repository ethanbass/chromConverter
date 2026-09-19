# Read 'Shimadzu' TLM stream

Read the `TLM Raw Data` mass spectrometry stream from 'Shimadzu
LabSolutions' `.lcd` files.

## Usage

``` r
read_sz_tlm(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = "long",
  levels = c("MS1", "MS2"),
  sparse = TRUE,
  read_metadata = TRUE,
  metadata_format = "shimadzu_lcd"
)
```

## Arguments

- path:

  Path to `.lcd` file.

- format_out:

  Class of output. Either `data.table` or `data.frame`. Spectra are long
  and mixed-type, so `matrix` resolves to `data.table`.

- data_format:

  Ignored: spectra have no wide representation and are always returned
  in `long` format.

- levels:

  Which MS levels to return, spelled `MS1` and `MS2`. Both are decoded
  either way, since the level of a scan is recorded inside its own
  compressed record.

- sparse:

  Logical. Whether to return spectra in sparse format (excluding zeros),
  as
  [`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md)
  does. Defaults to `TRUE`, since a profile scan is mostly empty and
  keeping its zeros costs roughly an order of magnitude more memory.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata.

## Value

A named list holding whichever of `MS1` and `MS2` the file has, each a
long table with columns `scan`, `rt` (minutes), `mz` and `intensity`,
preceded by `precursor_mz` where the scans carry one. A per-scan summary
(retention time, event, MS level, polarity, precursor m/z, point count)
is attached to each as a `scan_info` attribute.

## Details

Spectra live in the `MS Raw Data` stream and are located through
`Spectrum Index`, which holds one 24-byte entry per spectrum:

|            |          |                           |
|------------|----------|---------------------------|
| **Offset** | **Type** | **Field**                 |
| 0–3        | `uint32` | Record size in bytes      |
| 4–7        | `uint32` | Event number              |
| 8–11       | `uint32` | Offset into `MS Raw Data` |
| 12–15      | `uint32` | Padding                   |
| 16–19      | `uint32` | Cycle number              |
| 20–23      | `uint32` | Scan number               |

Each record begins with a 12-byte wrapper (`0xFFFFFFFF`, uncompressed
size, compressed size) followed by a zlib stream. The decompressed block
opens with a 44-byte header shared by every scan type:

|  |  |  |
|----|----|----|
| **Offset** | **Type** | **Field** |
| 0–3 | `uint32` | Retention time (milliseconds) |
| 4–7 | `uint32` | Retention time of the first scan in the cycle |
| 8–11 | `uint32` | Event number |
| 12–15 | `uint32` | Scan counter within the event |
| 16–19 | `uint32` | Scan index (0-based) |
| 20–21 | `uint16` | Scan type: 10 = MS1 profile, 14 = MS2 profile, 15 = MRM/SIM |
| 22–23 | `uint16` | MS level + 1 (0 in a truncated final scan) |
| 24–27 | `uint32` | Constant (`0x00010000`) |
| 28–31 | `uint32` | Last precursor m/z x 100 (stale outside MS2 scans) |
| 32–35 | `uint32` | Instrument state bit field; bit 0 follows polarity |
| 36–39 | `uint32` | Polarity (0 = positive, 1 = negative) |
| 40–43 | `uint32` | Number of data points (profile) or transitions (MRM) |

**Profile scans** (type 10 and 14) follow the header with two m/z pairs
stored as m/z x 100 — the isolation window (equal low and high values
for MS2; the scan range for MS1), then the scan range — and then `n`
unsigned 32-bit intensities. Bit 31 of an intensity marks detector
saturation and is not part of the value. Intensities are already in the
units LabSolutions reports, so no scaling is applied.

Points lie on an evenly spaced m/z grid running from the low end of the
scan range, with a bin width of `(high - low) / (100 * n)` (0.1 in the
files seen so far). The grid overhangs the acquired range by 10 bins at
the bottom and 9 at the top; those are dropped, which is what makes the
summed intensity match the `TIC Data` stream exactly.

**MRM and SIM scans** (type 15) instead follow the header with `n`
12-byte transitions: Q1 m/z x 100, Q3 m/z x 100, and intensity.

## Author

Ethan Bass
