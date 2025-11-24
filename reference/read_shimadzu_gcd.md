# Read 'Shimadzu' GCD

Read chromatogram data streams from 'Shimadzu' `.gcd` files.

## Usage

``` r
read_shimadzu_gcd(
  path,
  what = "chroms",
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Shimadzu' `.gcd` file.

- what:

  What stream to get: current options are chromatograms (`chroms`)
  and/or peak lists (`peak_table`). If a stream is not specified, the
  function will default to `chroms`.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

## Value

A 2D chromatogram from the chromatogram stream in `matrix` or
`data.frame` format, according to the value of `format_out`. The
chromatograms will be returned in `wide` or `long` format according to
the value of `data_format`.

## Details

A parser to read chromatogram data streams from 'Shimadzu' `.gcd` files.
GCD files are encoded as 'Microsoft' OLE documents. The parser relies on
the [olefile](https://pypi.org/project/olefile/) package in Python to
unpack the files. The PDA data is encoded in a stream called
`PDA 3D Raw Data:3D Raw Data`. The GCD data stream contains a segment
for each retention time, beginning with a 24-byte header.

The 24 byte header consists of the following fields:

- 4 bytes: segment label (`17234`).

- 4 bytes: Little-endian integer specifying the sampling interval in
  milliseconds.

- 4 bytes: Little-endian integer specifying the number of values in the
  file.

- 4 bytes: Little-endian integer specifying the total number of bytes in
  the file (However, this seems to be off by a few bytes?).

- 8 bytes of `00`s

After the header, the data are simply encoded as 64-bit (little-endian)
floating-point numbers. The retention times can be (approximately?)
derived from the number of values and the sampling interval encoded in
the header.

## Note

This parser is experimental and may still need some work. It is not yet
able to interpret much metadata from the files.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md)

## Author

Ethan Bass
