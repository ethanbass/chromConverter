# Read 'Allotrope Simple Model' (ASM) 2D chromatograms

Reads ['Allotrope Simple
Model'](https://allotropefoundation.org/our-technology/) files into R.

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

  Whether to return data in `wide` (default) or `long` format.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A 2D chromatogram in the format specified by `format_out` and
`data_format`, or a list of them named by detection type if the file
holds more than one (or `collapse = FALSE`). Metadata are attached as
[attributes](https://rdrr.io/r/base/attributes.html) if `read_metadata`
is `TRUE`.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_asm("path/to/file.json")
} # }
```
