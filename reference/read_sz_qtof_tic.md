# Read 'Shimadzu' QTOF TIC stream

A parser to read total ion chromatogram data streams from the quadrupole
time-of-flight (`QTFL RawData`) container of 'Shimadzu' `.lcd` files.
Triple quadrupole files store their TIC differently and are read by
`read_sz_tlm_tic` instead. LCD files are encoded as 'Microsoft' OLE
documents. The parser relies on the
[olefile](https://pypi.org/project/olefile/) package in Python to unpack
the files. The TIC data is encoded in a stream called `Centroid SumTIC`,
which opens with an 8-byte header and holds one 16-byte record per scan:
three 4-byte little-endian integers — the retention time in
milliseconds, the scan number and the intensity — followed by a 4-byte
spacer (`00000000`). Retention times are converted to minutes, as
elsewhere in the package.

## Usage

``` r
read_sz_qtof_tic(
  path,
  format_out = "data.frame",
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = "shimadzu_lcd"
)
```

## Arguments

- path:

  Path to 'Shimadzu' `.lcd` file.

- format_out:

  Matrix or data.frame.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata.

## Value

A 2D chromatogram from the SumTIC stream in `matrix` or `data.frame`
format, according to the value of `format_out`. The chromatograms will
be returned in `wide` or `long` format according to the value of
`data_format`.

## Author

Ethan Bass
