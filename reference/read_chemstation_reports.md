# Read 'Agilent ChemStation' report files

Reads peak tables from 'Agilent ChemStation' `Report.TXT` files.

## Usage

``` r
read_chemstation_reports(
  paths,
  peaktable_format = c("chromatographr", "original"),
  metadata_format = c("chromconverter", "raw"),
  data_format = NULL
)
```

## Arguments

- paths:

  Paths to 'ChemStation' report files.

- peaktable_format:

  Whether to return peak tables in `chromatographr` or `original`
  format.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- data_format:

  Deprecated. Use `peaktable_format` instead.

## Value

A `peak_list`: a list with one element per report, named by its `.D`
directory, each holding one peak table per signal, named by wavelength.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_chemstation_reports("path/to/report.txt")
} # }
```
