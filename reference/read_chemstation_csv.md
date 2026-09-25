# Read 'Agilent ChemStation' CSV files

Reads 'Agilent ChemStation' `.csv` files.

## Usage

``` r
read_chemstation_csv(
  path,
  format_out = "matrix",
  data_format = "wide",
  read_metadata = TRUE
)
```

## Arguments

- path:

  Path to 'Agilent' `.csv` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`. The files
  record no instrument metadata, so only the settings chromConverter
  used to parse the file are attached.

## Value

A chromatogram in the format specified by `format_out` and
`data_format`.

## Details

'Agilent ChemStation' CSV files are encoded in UTF-16.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
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
