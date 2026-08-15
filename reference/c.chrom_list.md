# Combine `chrom_list` objects

Combines multiple `chrom_list` objects (or a mix of `chrom_list` and
plain lists/matrices) into a single `chrom_list`, preserving the class.

## Usage

``` r
# S3 method for class 'chrom_list'
c(...)
```

## Arguments

- ...:

  One or more `chrom_list` objects (or objects coercible via
  [`c()`](https://rdrr.io/r/base/c.html)) to combine.

## Value

A `chrom_list` containing all elements.
