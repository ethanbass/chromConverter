# Read 'Agilent ChemStation' MS files

Reads 'Agilent ChemStation MSD Spectral Files' beginning with
`x01/x32/x00/x00`.

## Usage

``` r
read_chemstation_ms(
  path,
  what = c("MS1", "BPC", "TIC"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = "long",
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Agilent' `.ms` file.

- what:

  Which streams to return: `MS1`, `BPC` and/or `TIC`. Defaults to all
  three.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return the `BPC` and `TIC` in `long` (default) or `wide`
  format. Mass spectra are always returned in `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A list of the streams in `what`, in the format specified by
`format_out`. In wide format, the `BPC` and `TIC` have retention times
as rows and a single intensity column; in long format, a retention time
column and an intensity column. MS data will always be returned in long
format. The `format_out` argument determines whether the chromatogram is
returned as a `matrix`, `data.frame`, or `data.table`. Metadata are
attached as [attributes](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`. With `collapse = TRUE`, a list of one stream
is replaced by that stream.

## Note

Many thanks to Evan Shi and Eugene Kwan for providing helpful
information on the structure of these files in the [rainbow
documentation](https://rainbow-api.readthedocs.io/en/latest/agilent/ms.html).

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_chemstation_ms(path)
} # }
```
