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

  The path to 'Chromatotec' `.Chrom` file.

- what:

  Whether to extract chromatograms (`chrom`) and/or `peak_table` data.
  Accepts multiple arguments.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A chromatogram and/or peak table from the specified `path`, according to
the value of `what`. Chromatograms are returned in the format specified
by `format_out`.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_chromatotec(path)
} # }
```
