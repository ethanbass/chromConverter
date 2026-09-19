# Print a chrom_list object

Prints a summary of a `chrom_list` without displaying the underlying
chromatographic data. Attributes that are constant across all
chromatograms are collapsed into a single header line, while varying
attributes are shown as a table truncated to the first `n` rows. When a
sample holds more than one chromatogram, its traces are printed as a
block headed by the sample, and any attribute they all share is shown in
that block's header rather than repeated down it.

## Usage

``` r
# S3 method for class 'chrom_list'
print(x, n = 10, cols = chrom_summary_cols(), ...)
```

## Arguments

- x:

  A `chrom_list` object.

- n:

  Integer. Maximum number of chromatograms to show in the table.
  Defaults to `10`.

- cols:

  Character vector of attribute names to report. Defaults to:
  `sample_name`, `run_datetime`, `method`, `detector`, `wavelength`,
  `detector_range`, `scan_type`, `polarity`, `precursor_mz`,
  `product_mz`, `mz_range`.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chroms <- read_chroms(path, format_in = "shimadzu_ascii",
                      find_files = FALSE, progress_bar = FALSE)
print(chroms)
#> A chrom_list with 1 chromatogram
#> name: ladder  |  sample_name: FS19_214  |  run_datetime: 2019-07-18 19:45:56
#>   method: C:\LabSolutions\Data\A Legan\Method files\SPME_sample_1.gcm
```
