# Read 'Agilent ChemStation' CH files

Reads 'Agilent ChemStation' `.ch` files.

## Usage

``` r
read_chemstation_ch(
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

  Path to 'Agilent' `.ch` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- scale:

  Whether to scale the data by the scaling factor present in the file.
  Defaults to `TRUE`. 'MassHunter' seems to ignore the scaling factor in
  at least some types of 'ChemStation' files.

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

## Details

'Agilent' `.ch` files come in several different formats. This parser can
automatically detect and read several versions of these files from
'Agilent ChemStation' and 'Agilent OpenLab', including versions `30` and
`130`, which are generally produced by ultraviolet detectors, as well as
`81`, `179`, and `181` which are generally produced by flame ionization
(FID) detectors.

## Note

This function was adapted from the [Chromatography
Toolbox](https://github.com/chemplexity/chromatography) (© James Dillon
2014).

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
read_chemstation_ch("tests/testthat/testdata/chemstation_130.ch")
}
```
