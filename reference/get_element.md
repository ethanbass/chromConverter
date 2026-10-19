# Get one element of each sample

Extracts the same element from every sample of a `chrom_list` whose
samples are themselves lists, such as the output of
[read_chroms](https://ethanbass.github.io/chromConverter/reference/read_chroms.md)
with several `what` values (e.g. `c("dad", "instrument")`). The result
has one chromatogram per sample, which packages that expect a list of
chromatograms, such as chromatographR, accept as is.

## Usage

``` r
get_element(x, what)
```

## Arguments

- x:

  A `chrom_list` whose samples are lists of elements.

- what:

  The element to extract: a single name (e.g. `"dad"`), or a vector of
  names for a nested element (e.g. `c("instrument", "PMP1, Pressure")`).
  A nested element can also be written as one string split at the first
  colon, as in `"instrument:PMP1, Pressure"`, unless that string is
  itself an element name.

## Value

A list of the extracted chromatograms, with the names and class of `x`.

## See also

[read_chroms](https://ethanbass.github.io/chromConverter/reference/read_chroms.md),
[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)

## Examples

``` r
path <- system.file("extdata", "benzoxazinoid_standards", "BENZOS_250PPM.D",
                    package = "chromConverter")
run <- read_chroms(path, format_in = "agilent_d",
                   what = c("chroms", "instrument"), progress_bar = FALSE)
pressure <- get_element(run, "instrument:PMP1, Pressure")
head(pressure[[1]])
#>       intensity
#> 0        169.77
#> 0.005    157.60
#> 0.01     157.73
#> 0.015    158.26
#> 0.02     158.66
#> 0.025    158.99
```
