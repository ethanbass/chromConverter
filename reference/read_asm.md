# Read 'Allotrope Simple Model' (ASM) 2D chromatograms

Reads ['Allotrope Simple Model'](https://www.allotrope.org/asm) files
into R.

## Usage

``` r
read_asm(
  path,
  data_format = c("wide", "long"),
  format_out = c("matrix", "data.frame", "data.table"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to ASM `.json` file.

- data_format:

  Whether to return data in `wide` or `long` format.

- format_out:

  Matrix or data.frame.

- read_metadata:

  Logical. Whether to attach metadata.

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
argument determines whether the chromatogram is returned as a `matrix`
or `data.frame`. Metadata can be attached to the chromatogram as
[`attributes`](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.

## Author

Ethan Bass
