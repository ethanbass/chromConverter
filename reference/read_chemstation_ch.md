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
  scale = TRUE,
  source_file = NULL
)
```

## Arguments

- path:

  Path to 'Agilent' `.ch` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- scale:

  Whether to multiply the data by the file's scaling factor and add its
  intercept. Defaults to `TRUE`. 'MassHunter' seems to ignore the
  scaling factor in at least some types of 'ChemStation' files.

- source_file:

  Source file from which chromatogram data was originally derived.

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

'Agilent' `.ch` files come in several formats. This parser detects the
version from the file and reads versions `8`, `30`, `130`, `81`, `179`
and `181` from 'Agilent ChemStation' and 'Agilent OpenLab'; any other
version is an error. Versions `30` and `130` are generally produced by
ultraviolet detectors, and `81`, `179` and `181` by flame ionization
detectors (FID).

## Note

This function was adapted from the [Chromatography
Toolbox](https://github.com/chemplexity/chromatography) (© James Dillon
2014).

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
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
