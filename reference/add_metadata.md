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
path <- system.file("extdata", "benzoxazinoid_standards",
                    package = "chromConverter")
chroms <- read_chroms(path, format_in = "chemstation_ch", pattern = "dad1A",
                      parser = "chromconverter", progress_bar = FALSE)
meta <- data.frame(name = c("MEOH", "BENZOS_1000PPM", "BENZOS_500PPM",
                            "BENZOS_250PPM", "BENZOS_125PPM",
                            "BENZOS_62,5PPM", "BENZOS_31,25PPM",
                            "BENZOS_16PPM", "BENZOS_8PPM", "BENZOS_4PPM"),
                   ppm = c(0, 1000 / 2^(0:8)))
chroms <- add_metadata(chroms, meta)
extract_metadata(chroms, what = "ppm")
#>               name        ppm
#> 1             MEOH    0.00000
#> 2   BENZOS_1000PPM 1000.00000
#> 3    BENZOS_500PPM  500.00000
#> 4    BENZOS_250PPM  250.00000
#> 5    BENZOS_125PPM  125.00000
#> 6   BENZOS_62,5PPM   62.50000
#> 7  BENZOS_31,25PPM   31.25000
#> 8     BENZOS_16PPM   15.62500
#> 9      BENZOS_8PPM    7.81250
#> 10     BENZOS_4PPM    3.90625
names(subset(chroms, ppm >= 250))
#> [1] "BENZOS_1000PPM" "BENZOS_500PPM"  "BENZOS_250PPM" 
```
