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
print(
  x,
  n = 10,
  cols = chrom_summary_cols(),
  digits = getOption("digits"),
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

  Character vector of attribute names to report. Defaults to:
  `sample_name`, `run_datetime`, `method`, `detector`, `wavelength`,
  `detector_range`, `scan_type`, `polarity`, `precursor_mz`,
  `product_mz`, `mz_range`.

- digits:

  Number of significant digits for numeric metadata. Defaults to
  `getOption("digits")`, as for
  [print.data.frame](https://rdrr.io/r/base/print.dataframe.html).

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)

## Examples

``` r
path <- system.file("extdata", "benzoxazinoid_standards",
                    package = "chromConverter")
chroms <- read_chroms(path, format_in = "chemstation_ch", pattern = "dad1A",
                      parser = "chromconverter", progress_bar = FALSE)
print(chroms)
#> A chrom_list with 10 chromatograms
#> method: ETHAN_DT_MEOH_L16-6.M  |  detector: DAD  |  wavelength: 254
#>               name     sample_name        run_datetime
#> 1             MEOH            MEOH 2023-06-15 16:46:23
#> 2   BENZOS_1000PPM  benzos_1000ppm 2023-06-20 19:29:41
#> 3    BENZOS_500PPM   benzos_500ppm 2023-06-21 15:23:43
#> 4    BENZOS_250PPM   benzos_250ppm 2023-06-21 16:46:40
#> 5    BENZOS_125PPM   benzos_125ppm 2023-06-21 18:02:48
#> 6   BENZOS_62,5PPM  benzos_62,5ppm 2023-06-21 19:19:00
#> 7  BENZOS_31,25PPM benzos_31,25ppm 2023-06-21 20:35:12
#> 8     BENZOS_16PPM    benzos_16ppm 2023-06-21 21:51:21
#> 9      BENZOS_8PPM     benzos_8ppm 2023-06-21 23:07:28
#> 10     BENZOS_4PPM     benzos_4ppm 2023-06-22 00:23:39
```
