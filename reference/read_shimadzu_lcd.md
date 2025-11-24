# Read 'Shimadzu' LCD

Read 3D PDA or 2D chromatogram streams from 'Shimadzu' `.lcd` files.

## Usage

``` r
read_shimadzu_lcd(
  path,
  what,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  scale = TRUE,
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Shimadzu' `.lcd` file.

- what:

  What stream to get: current options are `pda`, chromatograms
  (`chroms`), `tic`, or peak lists (`peak_table`). If a stream is not
  specified, the function will default to `pda` if the PDA stream is
  present.

- format_out:

  Matrix or data.frame.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- scale:

  Whether to scale the data by the value factor. Defaults to `TRUE`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

## Value

A chromatogram or list of chromatograms in the format specified by
`data_format` and `format_out`. If `data_format` is `wide`, the
chromatogram(s) will be returned with retention times as rows and a
single column for the intensity. If `long` format is requested, two
columns will be returned: one for the retention time and one for the
intensity. The `format_out` argument determines whether chromatograms
are returned as a `matrix`, `data.frame`, or `data.table`. Metadata can
be attached to the chromatogram as
[`attributes`](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.

## Details

A parser to read data from 'Shimadzu' `.lcd` files. LCD files are
encoded as 'Microsoft' OLE documents. The parser relies on the
[olefile](https://pypi.org/project/olefile/) package in Python to unpack
the files. The PDA data is encoded in a stream called
`PDA 3D Raw Data:3D Raw Data`. The PDA data stream contains a segment
for each retention time, beginning with a 24-byte header.

The 24 byte header consists of the following fields:

- 4 bytes: segment label (`17234`).

- 4 bytes: Little-endian integer specifying the sampling rate along the
  time axis for 2D streams or along the spectral axis (?) for PDA
  streams.

- 4 bytes: Little-endian integer specifying the number of values in the
  file (for 2D data) or the number of wavelength values in each segment
  (for 3D data).

- 4 bytes: Little-endian integer specifying the total number of bytes in
  the segment.

- 8 bytes of `00`.

For 3D data, Each time point is divided into two sub-segments, which
begin and end with an integer specifying the length of the sub-segment
in bytes. 2D data are structured similarly but with more segments. All
known values in this the LCD data streams are little-endian and the data
are delta-encoded. The first hexadecimal digit of each value is a sign
digit specifying the number of bytes in the delta and whether the value
is positive or negative. The sign digit represents the number of
hexadecimal digits used to encode each value. Even numbered sign digits
correspond to positive deltas, whereas odd numbers indicate negative
deltas. Positive values are encoded as little-endian integers, while
negative values are encoded as two's complements. The value at each
position is derived by subtracting the delta at each position from the
previous value.

## Note

My parsing of the date-time format seems to be a little off, since the
acquisition times diverge slightly from the ASCII file.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_shimadzu_lcd(path)
} # }
```
