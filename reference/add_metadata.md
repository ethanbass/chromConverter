# Add metadata to a list of chromatograms

Attaches the columns of a table to the chromatograms of a `chrom_list`
as metadata fields, matching each row to a sample by name. The new
fields can then be used by
[subset.chrom_list](https://ethanbass.github.io/chromConverter/reference/subset.chrom_list.md)
and requested from
[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md).
Where an element holds several chromatograms, such as the traces
[read_agilent_d](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md)
returns for each `.D` directory, every one of them gets the sample's
values.

## Usage

``` r
add_metadata(chrom_list, metadata, by = "name", overwrite = FALSE)
```

## Arguments

- chrom_list:

  A `chrom_list` object.

- metadata:

  A `data.frame`, `tibble` or `data.table` with one row per sample.

- by:

  The column of `metadata` holding the sample names, matched to
  `names(chrom_list)`. Defaults to `name`, the column
  [extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)
  identifies samples by.

- overwrite:

  Whether a column may replace a field that chromConverter reads from
  the file, such as `sample_name`. Defaults to `FALSE`, in which case
  such a column is an error.

## Value

`chrom_list` with the columns of `metadata` attached to its
chromatograms, and their names recorded in an `added_metadata`
attribute, from which chromatographR's `get_peaktable` fills its
`sample_meta`. A sample without a row in `metadata` is left unchanged,
with a warning.

## See also

[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md),
[subset.chrom_list](https://ethanbass.github.io/chromConverter/reference/subset.chrom_list.md)

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chrom <- read_chroms(path, format_in = "shimadzu_ascii",
                     find_files = FALSE, progress_bar = FALSE)
# three copies stand in for the samples of a sequence
chroms <- c(chrom, chrom, chrom)
names(chroms) <- c("s1", "s2", "s3")
meta <- data.frame(name = c("s1", "s2", "s3"),
                   treatment = c("control", "drought", "drought"))
chroms <- add_metadata(chroms, meta)
extract_metadata(chroms, what = "treatment")
#>   name treatment
#> 1   s1   control
#> 2   s2   drought
#> 3   s3   drought
names(subset(chroms, treatment == "drought"))
#> [1] "s2" "s3"
```
