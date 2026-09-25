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

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A 2D chromatogram in the format specified by `data_format` and
`format_out`. If `data_format` is `wide`, the chromatogram will be
returned with retention times as row names and a single column for the
intensity. If `long` format is requested, two columns will be returned:
one for the retention time and one for the intensity. The `format_out`
argument determines whether the chromatogram is returned as a `matrix`,
`data.frame`, or `data.table`. Metadata are attached to the chromatogram
as [attributes](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.

## Details

A parser to read chromatogram data streams from 'Shimadzu' `.gcd` files.
GCD files are encoded as 'Microsoft' OLE documents. The parser relies on
the [olefile](https://pypi.org/project/olefile/) package in Python to
unpack the files. The chromatogram data is encoded in streams titled
`LSS Raw Data:Chromatogram Ch<#>`. Each stream begins with a 24-byte
header:

- 4 bytes: segment label (`17234`).

- 4 bytes: Little-endian integer specifying the sampling interval in
  milliseconds.

- 4 bytes: Little-endian integer specifying the number of values in the
  stream.

- 4 bytes: Little-endian integer specifying a byte count, which does not
  match the size of the stream exactly.

- 8 bytes of `00`s

After the header, the data are encoded as 64-bit (little-endian)
floating-point numbers. Retention times are derived from the number of
values and the sampling interval encoded in the header, rather than read
from the file: the `n`th value is placed at `n` times the sampling
interval.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md),
[`read_sz_tables()`](https://ethanbass.github.io/chromConverter/reference/read_sz_tables.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_shimadzu_gcd("path/to/file.gcd")
} # }
```
