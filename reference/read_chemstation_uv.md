# Read 'Agilent ChemStation' DAD files

Agilent `.uv` files come in several different formats. This parser can
automatically detect and read several versions of these files from
'Agilent ChemStation' and 'Agilent OpenLab', including versions `31` and
`131`.

## Usage

``` r
read_chemstation_uv(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  scale = TRUE
)
```

## Arguments

- path:

  Path to 'Agilent' `.uv` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- scale:

  Whether to scale the data by the scaling factor present in the file.
  Defaults to `TRUE`.

## Value

A 3D chromatogram in the format specified by `data_format` and
`format_out`. If `data_format` is `wide`, the chromatogram will be
returned with retention times as rows and wavelengths as columns. If
`long` format is requested, three columns will be returned: one for the
retention time, one for the wavelength and one for the intensity. The
`format_out` argument determines whether the chromatogram is returned as
a `matrix` or `data.frame`. Metadata can be attached to the chromatogram
as [`attributes`](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.

## Note

This function was adapted from the parser in the rainbow project
licensed under GPL 3 by Evan Shi
<https://rainbow-api.readthedocs.io/en/latest/agilent/uv.html>.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
read_chemstation_uv("tests/testthat/testdata/dad1.uv")
}
```
