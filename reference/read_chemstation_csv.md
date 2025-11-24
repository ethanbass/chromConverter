# Read 'Agilent ChemStation' CSV files

Reads 'Agilent Chemstation' `.csv` files.

## Usage

``` r
read_chemstation_csv(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE
)
```

## Arguments

- path:

  Path to 'Agilent' `.csv` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`. There is no
  instrumental metadata saved in the CSV files so this will only attach
  metadata about the settings used by chromConverter to parse the file.

## Value

A chromatogram in the format specified by `format_out` (retention time x
wavelength) and `data_format`.

## Details

'Agilent Chemstation' CSV files are encoded in UTF-16.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
read_chemstation_csv("tests/testthat/testdata/dad1.csv")
}
```
