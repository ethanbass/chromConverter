# Read 'Shimadzu' LCD 2D data

Reads 2D PDA data stream from 'Shimadzu' `.lcd` files.

## Usage

``` r
read_sz_lcd_2d(
  path,
  format_out = "data.frame",
  data_format = "wide",
  read_metadata = TRUE,
  metadata_format = "shimadzu_lcd",
  scale = TRUE
)
```

## Arguments

- path:

  Path to 'Shimadzu' `.lcd` 2D data file.

- format_out:

  Matrix or data.frame.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- scale:

  Whether to scale the data by the value factor.

## Value

One or more 2D chromatograms from the chromatogram streams in `matrix`
or `data.frame` format, according to the value of `format_out`. If
multiple chromatograms are found, they will be returned as a list of
matrices or data.frames. The chromatograms will be returned in `wide` or
`long` format according to the value of `data_format`.

## Details

A parser to read chromatogram data streams from 'Shimadzu' `.lcd` files.
LCD files are encoded as 'Microsoft' OLE documents. The parser relies on
the [olefile](https://pypi.org/project/olefile/) package in Python to
unpack the files. The chromatogram data is encoded in streams titled
`LSS Raw Data:Chromatogram Ch<#>`. The chromatogram data streams begin
with a 24-byte header.

The 24 byte header consists of the following fields:

- 4 bytes: segment label (`17234`).

- 4 bytes: Little-endian integer specifying the sampling rate (in
  milliseconds).

- 4 bytes: Little-endian integer specifying the number of values in the
  file.

- 4 bytes: Little-endian integer specifying the total number of bytes in
  the file.

- 8 bytes of `00`s

Each segment is divided into multiple sub-segments, which begin and end
with an integer specifying the length of the sub-segment in bytes. All
known values in this data stream are little-endian and the data are
delta-encoded. The first hexadecimal digit of each value is a sign digit
specifying the number of bytes in the delta and whether the value is
positive or negative. The sign digit represents the number of
hexadecimal digits used to encode each value. Even numbered sign digits
correspond to positive deltas, whereas odd numbers indicate negative
deltas. Positive values are encoded as little-endian integers, while
negative values are encoded as two's complements. The value at each
position is derived by subtracting the delta at each position from the
previous value.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md)

## Author

Ethan Bass
