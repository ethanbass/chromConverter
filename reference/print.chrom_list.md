# Print a chrom_list object

Prints a summary of a `chrom_list` without displaying the underlying
chromatographic data. Attributes that are constant across all
chromatograms are collapsed into a single header line, while varying
attributes are shown as a table truncated to the first `n` rows.

## Usage

``` r
# S3 method for class 'chrom_list'
print(
  x,
  n = 5,
  cols = c("sample_name", "run_datetime", "method", "detector"),
  ...
)
```

## Arguments

- x:

  A `chrom_list` object.

- n:

  Integer. Maximum number of chromatograms to show in the table.
  Defaults to `10`.

- cols:

  Character vector of attribute names to extract and display. Defaults
  to `c("sample_name", "run_datetime", "method", "detector")`.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)
