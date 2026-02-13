# Read files from 'Agilent ChemStation' .D directories

Reads files from 'Agilent' `.D` directories.

## Usage

``` r
read_agilent_d(
  path,
  what = c("dad", "chroms", "peak_table"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Agilent' `.D` directory.

- what:

  Whether to extract chromatograms (`chroms`), DAD data (`dad`) and/or
  peak tables `peak_table`. Accepts multiple arguments. `ms_spectra`.
  Accepts multiple arguments.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

## Value

A list of chromatograms in the format specified by `data_format` and \#'
`format_out`. If `data_format` is `wide`, the chromatograms will be
returned with retention times as rows and columns containing signal
intensity for each signal. If `long` format is requested, retention
times will be in the first column. The `format_out` argument determines
whether the chromatogram is returned as a `matrix`, `data.frame` or
`data.table`. Metadata can be attached to the chromatogram as
[`attributes`](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.

## Details

Currently this function is limited to reading `.uv`, `.ch` and
`peak_table` elements.

## See also

Other 'Agilent' parsers:
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
read_agilent_d("tests/testthat/testdata/RUTIN2.D")
}
```
