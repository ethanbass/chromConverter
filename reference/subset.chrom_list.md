# Select chromatograms by their metadata

Retains the chromatograms of a `chrom_list` whose metadata meet the
specified condition, such as `sample_name == "blank"` or
`run_datetime > as.POSIXct("2024-01-01", tz = "UTC")`. The condition can
refer to any field
[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)
reports, such as `sample_name` or `method`, as well as the
chromatogram's name in the list.

## Usage

``` r
# S3 method for class 'chrom_list'
subset(x, subset, ...)
```

## Arguments

- x:

  A `chrom_list` object.

- subset:

  An expression giving a single `TRUE` or `FALSE` for each chromatogram.

- ...:

  Ignored.

## Value

A `chrom_list` containing the selected chromatograms.

## Details

Numeric fields such as `time_range` and `sample_injection_volume`
compare as numbers wherever the file's value parses as one, and a field
with several values can be indexed (e.g. `time_range[2]`). A field a
chromatogram does not carry is `NA`, and a chromatogram for which
`subset` is `NA` is dropped, as in
[`subset()`](https://rdrr.io/r/base/subset.html) for data frames. Where
an element holds several chromatograms, such as the traces
[read_agilent_d](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md)
returns for each `.D` directory, it is kept or dropped as a whole, and
`subset` sees the fields its chromatograms agree on, ignoring those that
leave a field empty; a field they disagree on is `NA`.

## See also

[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chroms <- read_chroms(path, format_in = "shimadzu_ascii",
                      find_files = FALSE, progress_bar = FALSE)
subset(chroms, sample_name == "FS19_214")
#> A chrom_list with 1 chromatogram
#> name: ladder  |  sample_name: FS19_214  |  run_datetime: 2019-07-18 19:45:56
#>   method: C:\LabSolutions\Data\A Legan\Method files\SPME_sample_1.gcm
subset(chroms, grepl("ladder", source_file))
#> A chrom_list with 1 chromatogram
#> name: ladder  |  sample_name: FS19_214  |  run_datetime: 2019-07-18 19:45:56
#>   method: C:\LabSolutions\Data\A Legan\Method files\SPME_sample_1.gcm
```
