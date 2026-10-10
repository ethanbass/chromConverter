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
  digits = NULL,
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

- digits:

  Number of significant digits for the numbers in a field collapsed into
  a string, or `NULL` (the default) to keep them in full.

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
path <- system.file("extdata", "benzoxazinoid_standards",
                    package = "chromConverter")
chroms <- read_chroms(path, format_in = "chemstation_ch", pattern = "dad1A",
                      parser = "chromconverter", progress_bar = FALSE)
summary(chroms)
#>             sample n_rows n_cols     sample_name        run_datetime
#> 1             MEOH   9000      1            MEOH 2023-06-15 16:46:23
#> 2   BENZOS_1000PPM   9001      1  benzos_1000ppm 2023-06-20 19:29:41
#> 3    BENZOS_500PPM   9001      1   benzos_500ppm 2023-06-21 15:23:43
#> 4    BENZOS_250PPM   9000      1   benzos_250ppm 2023-06-21 16:46:40
#> 5    BENZOS_125PPM   9001      1   benzos_125ppm 2023-06-21 18:02:48
#> 6   BENZOS_62,5PPM   9001      1  benzos_62,5ppm 2023-06-21 19:19:00
#> 7  BENZOS_31,25PPM   9000      1 benzos_31,25ppm 2023-06-21 20:35:12
#> 8     BENZOS_16PPM   9001      1    benzos_16ppm 2023-06-21 21:51:21
#> 9      BENZOS_8PPM   9001      1     benzos_8ppm 2023-06-21 23:07:28
#> 10     BENZOS_4PPM   9001      1     benzos_4ppm 2023-06-22 00:23:39
#>                   method detector wavelength
#> 1  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 2  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 3  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 4  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 5  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 6  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 7  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 8  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 9  ETHAN_DT_MEOH_L16-6.M      DAD        254
#> 10 ETHAN_DT_MEOH_L16-6.M      DAD        254
```
