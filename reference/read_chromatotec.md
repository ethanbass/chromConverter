# Read 'Chromatotec' file

Reads 'Chromatotec' `.Chrom` files.

## Usage

``` r
read_chromatotec(
  path,
  what = c("chrom", "peak_table"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Chromatotec' `.Chrom` file.

- what:

  Whether to extract chromatograms (`chrom`) and/or `peak_table` data.
  Accepts multiple arguments.

- format_out:

  R format. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long` format.

- read_metadata:

  Whether to read metadata from file.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

## Value

A chromatogram and/or peak table from the specified `path`, according to
the value of `what`. Chromatograms are returned in the format specified
by `format_out`.

## Note

This function is a work in progress and the accuracy of the results is
not guaranteed.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_chromatotec(path)
} # }
```
