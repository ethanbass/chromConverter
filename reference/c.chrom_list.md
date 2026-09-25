# Combine `chrom_list` objects

Combines `chrom_list` objects, or a mix of `chrom_list` objects and
plain lists of chromatograms, into a single `chrom_list`, preserving the
class.

## Usage

``` r
# S3 method for class 'chrom_list'
c(...)
```

## Arguments

- ...:

  `chrom_list` objects or lists of chromatograms to combine. Wrap a
  single chromatogram in [`list()`](https://rdrr.io/r/base/list.html),
  since a bare matrix is split into its individual values and a bare
  data frame into its columns.

## Value

A `chrom_list` containing all elements.
