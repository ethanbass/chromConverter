# Read 'Shimadzu' single quadrupole stream

Read the `Mass Raw Data` mass spectrometry stream written by 'Shimadzu'
single quadrupole instruments, such as the LCMS-2020, to `.lcd` files.

## Usage

``` r
read_sz_sq(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = "long",
  levels = c("MS1", "MS2"),
  centroided = TRUE,
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

  Which MS levels to return, spelled `MS1` and `MS2`. A single
  quadrupole records only `MS1`.

- centroided:

  Logical. Whether to return the centroided scans (`TRUE`) or the
  profile scans (`FALSE`).

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata.

## Value

A named list holding `MS1` if it was requested, a long table with
columns `scan`, `rt` (minutes), `mz` and `intensity`. A per-scan summary
(retention time, event, MS level, polarity, point count) is attached as
a `scan_info` attribute.

## Details

Spectra are centroided and returned as stored, except that each spectrum
is sorted by m/z, as ProteoWizard's `msconvert` also does, because the
stored m/z values do not always increase within a scan. The intensities
of a scan sum to its value in `TIC Data`, so no scaling is applied.

Some methods record profile scans as a separate event. With
`centroided = FALSE` these are returned instead of the centroids, so
events recorded only as centroids, such as the negative scans of the one
file checked, are absent. The stored values are multiplied by 2/3, which
puts their sum in the units of `TIC Data` in the one file checked. The
vendor's export applies a 9-point Savitzky-Golay filter to them, which
is not applied here.

The file stores no m/z values for profile points, and it's unknown how
the vendor's library computes them. Profile points are instead spread
evenly over the method's m/z range, from `lo` to `hi`, so that point `j`
(0-based) of `n` lies at `lo + (j - 12) * (hi - lo)/(n - 27)`. Against a
`msconvert` export of the one file checked (150-2000 at 15 points per
m/z), these m/z values are up to 0.012 low, about a fifth of the point
spacing. The offsets 12 and 27 come from that file alone. Profile scans
with any other number of points than 15 per m/z of the range plus 26
give an error.

Polarity is read from bit 2 of the event flags. In the one method seen,
which alternates positive and negative scans, the method's event table
stores the same flags beside a signed interface voltage, and bit 2 is
set on the event with the negative voltage. `msconvert`, which reads the
file through the vendor's library, reports the same polarity as this
reader for every scan.

No file with SIM events has been seen, and no header field is known to
give the scan type. A SIM event whose records match the layout below
would be returned in `MS1` as its own event, with nothing marking it as
SIM.

## File format

Values are little-endian unsigned integers.

- `Mass Raw Data/Spectrum Index`: the byte offset of each scan in
  `MS Raw Data` (uint32).

- `Mass Raw Data/TIC Data`: the total ion current of each scan (uint64).

- `Mass Raw Data/MS Raw Data`: one block per scan, a 64-byte header
  followed by one record per centroid. Header:

  - 0: scan index, 0-based (uint32).

  - 4: retention time in milliseconds, as in `Retention Time` (uint32).

  - 8: acquisition event, 0-based (uint16).

  - 44: event flags; bit 2 is the polarity, 0 positive and 1 negative
    (uint16).

  - 52: intensity width code, w: the width in bytes, 2 or 3, or 1 for 4
    bytes (uint16).

  - 54: number of centroids, n (uint16).

  - 60: number of profile points, 0 in centroid scans (uint32).

  Each of the n records is the m/z times 20 (uint16) followed by the
  intensity. Bit 31 of a 4-byte intensity is a flag and is masked off.
  Scans with bit 4 of the event flags set hold profile data instead: n
  is 0 and the block holds the profile points (uint16) with no m/z
  values. A point with bit 15 set holds its lower 15 bits times 32.
  Bytes not listed are not read.

- `Mass Raw Data/Status`: the m/z range of the method (uint16 at bytes
  14 and 16).

## Author

Ethan Bass
