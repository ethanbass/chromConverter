# Read 'Agilent ChemStation' MS file.

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

  What stream to get: current options are `MS1`, `BPC` and/or `TIC`. If
  a stream is not specified, the function will return all streams.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`. This argument applies only to TIC
  data, since MS and BPC data will always be returned in long format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A 2D chromatogram in the format specified by `data_format` and
`format_out`. If `data_format` is `wide`, the chromatogram will be
returned with retention times as rows and a single column for the
intensity. If `long` format is requested, two columns will be returned:
one for the retention time and one for the intensity. The `format_out`
argument determines whether the chromatogram is returned as a `matrix`,
`data.frame`, or `data.table`. Metadata can be attached to the
chromatogram as [`attributes`](https://rdrr.io/r/base/attributes.html)
if `read_metadata` is `TRUE`.

## Note

Many thanks to Evan Shi and Eugene Kwan for providing helpful
information on the structure of these files in the [rainbow
documentation](https://rainbow-api.readthedocs.io/en/latest/agilent/ms.html).

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
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
