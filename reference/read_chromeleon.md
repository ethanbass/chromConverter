# Read 'Chromeleon' ASCII files

Reads 'Thermo Fisher Chromeleon™ CDS' ASCII (`.txt`) files.

## Usage

``` r
read_chromeleon(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  decimal_mark = NULL
)
```

## Arguments

- path:

  Path to 'Chromeleon' ASCII file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- read_metadata:

  Whether to read metadata from file.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- decimal_mark:

  Which character is used as the decimal separator in the file. By
  default, decimal mark will be detected automatically, but it can also
  be manually set as `"."` or `","`.

## Value

A chromatogram in the format specified by `format_out`. (retention time
x wavelength).

## Author

Ethan Bass
