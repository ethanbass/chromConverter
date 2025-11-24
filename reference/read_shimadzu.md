# Read 'Shimadzu' ASCII

Reads 'Shimadzu' ASCII (`.txt`) files. These files can be exported from
'Shimadzu LabSolutions' by right clicking on samples in the sample list
and selecting `File Conversion:Convert to ASCII`.

## Usage

``` r
read_shimadzu(
  path,
  what = "chroms",
  format_in = NULL,
  include = c("fid", "lc", "dad", "uv", "tic"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  peaktable_format = c("chromatographr", "original"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  ms_format = c("data.frame", "list"),
  collapse = TRUE,
  scale = TRUE
)
```

## Arguments

- path:

  Path to Shimadzu `.txt` ASCII file.

- what:

  Whether to extract chromatograms (`chroms`), `peak_table`, and/or
  `ms_spectra`. Accepts multiple arguments.

- format_in:

  This argument is deprecated and is no longer required.

- include:

  Which chromatograms to include. Options are `fid`, `dad`, `uv`, `tic`,
  and `status`.

- format_out:

  R format. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- peaktable_format:

  Whether to return peak tables in `chromatographr` or `original`
  format.

- read_metadata:

  Whether to read metadata from file.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- ms_format:

  Whether to return mass spectral data as a (long) `data.frame` or a
  `list`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

- scale:

  Whether to scale the data by the scaling factor present in the file.
  Defaults to `TRUE`.

## Value

A nested list of elements from the specified `file`, where the top
levels are chromatograms, peak tables, and/or mass spectra according to
the value of `what`. Chromatograms are returned in the format specified
by `format_out` (retention time x wavelength).

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
path <- "tests/testthat/testdata/ladder.txt"
read_shimadzu(path)
}
```
