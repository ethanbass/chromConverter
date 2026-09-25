# Summarize a chrom_list object

Returns what
[print.chrom_list](https://ethanbass.github.io/chromConverter/reference/print.chrom_list.md)
displays as a table, with one row per chromatogram: the sample it
belongs to, its size, and the metadata fields in `cols`. Unlike `print`,
nothing is collapsed into a header, abbreviated or truncated to the
first few rows, so the result can be filtered and joined against.

## Usage

``` r
# S3 method for class 'chrom_list'
summary(
  object,
  cols = chrom_summary_cols(),
  format_out = c("data.frame", "data.table", "tibble"),
  ...
)
```

## Arguments

- object:

  A `chrom_list` object.

- cols:

  Character vector of attribute names to report. Defaults to:
  `sample_name`, `run_datetime`, `method`, `detector`, `wavelength`,
  `detector_range`, `scan_type`, `polarity`, `precursor_mz`,
  `product_mz`, `mz_range`. A field that no chromatogram carries, or
  that all of them leave empty, is omitted rather than filled with `NA`.

- format_out:

  Format of object. Either `data.frame`, `data.table` or `tibble`.

- ...:

  Additional arguments (currently ignored).

## Value

A `data.frame`, `data.table` or `tibble` (according to the value of
`format_out`) with one row per chromatogram. The first columns describe
where the chromatogram sits and how large it is — `sample`, `trace`
(only when a sample holds more than one), `n_rows` and `n_cols` —
followed by one column per metadata field found. A field no chromatogram
records, or that every one of them leaves empty, is dropped rather than
filled with `NA`. A field holding more than one value, such as the
`product_mz` of an MRM event monitoring several transitions, is
collapsed to a comma-separated string so that it occupies one column.

## See also

[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md),
[print.chrom_list](https://ethanbass.github.io/chromConverter/reference/print.chrom_list.md)

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chroms <- read_chroms(path, format_in = "shimadzu_ascii",
                      find_files = FALSE, progress_bar = FALSE)
summary(chroms)
#>   sample n_rows n_cols sample_name        run_datetime
#> 1 ladder  66255      1    FS19_214 2019-07-18 19:45:56
#>                                                             method
#> 1 C:\\LabSolutions\\Data\\A Legan\\Method files\\SPME_sample_1.gcm
```
