# Read 'Agilent ChemStation' report files.

Reads 'Agilent ChemStation' reports into R.

## Usage

``` r
read_chemstation_reports(
  paths,
  data_format = c("chromatographr", "original"),
  metadata_format = c("chromconverter", "raw")
)
```

## Arguments

- paths:

  Paths to 'ChemStation' report files.

- data_format:

  Format to output data. Either `chromatographr` or `chemstation`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

## Value

A data.frame containing the information from the specified 'ChemStation'
report.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass
