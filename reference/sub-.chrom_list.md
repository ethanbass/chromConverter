# Subset a `chrom_list` object

Extracts a subset of a `chrom_list` while preserving its class, so the
result remains a `chrom_list` rather than a plain `list`.

## Usage

``` r
# S3 method for class 'chrom_list'
x[i, ...]
```

## Arguments

- x:

  A `chrom_list` object.

- i:

  Indices specifying elements to extract.

- ...:

  Additional arguments passed to the default `[` method.

## Value

A `chrom_list` containing the selected elements.
