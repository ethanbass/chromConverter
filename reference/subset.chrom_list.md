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
path <- system.file("extdata", "benzoxazinoid_standards",
                    package = "chromConverter")
chroms <- read_chroms(path, format_in = "chemstation_ch", pattern = "dad1A",
                      parser = "chromconverter", progress_bar = FALSE)
subset(chroms, sample_name != "MEOH")
#> A chrom_list with 9 chromatograms
#> method: ETHAN_DT_MEOH_L16-6.M  |  detector: DAD  |  wavelength: 254
#>              name     sample_name        run_datetime
#> 1  BENZOS_1000PPM  benzos_1000ppm 2023-06-20 19:29:41
#> 2   BENZOS_500PPM   benzos_500ppm 2023-06-21 15:23:43
#> 3   BENZOS_250PPM   benzos_250ppm 2023-06-21 16:46:40
#> 4   BENZOS_125PPM   benzos_125ppm 2023-06-21 18:02:48
#> 5  BENZOS_62,5PPM  benzos_62,5ppm 2023-06-21 19:19:00
#> 6 BENZOS_31,25PPM benzos_31,25ppm 2023-06-21 20:35:12
#> 7    BENZOS_16PPM    benzos_16ppm 2023-06-21 21:51:21
#> 8     BENZOS_8PPM     benzos_8ppm 2023-06-21 23:07:28
#> 9     BENZOS_4PPM     benzos_4ppm 2023-06-22 00:23:39
subset(chroms, run_datetime < as.POSIXct("2023-06-21", tz = "UTC"))
#> A chrom_list with 2 chromatograms
#> method: ETHAN_DT_MEOH_L16-6.M  |  detector: DAD  |  wavelength: 254
#>             name    sample_name        run_datetime
#> 1           MEOH           MEOH 2023-06-15 16:46:23
#> 2 BENZOS_1000PPM benzos_1000ppm 2023-06-20 19:29:41
```
