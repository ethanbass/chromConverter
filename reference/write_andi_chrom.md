# Write ANDI chrom CDF file from chromatogram

Exports a chromatogram in ANDI (Analytical Data Interchange)
chromatography format (ASTM E1947-98). The format holds a single trace,
so a 3D chromatogram must be reduced to one: name the column to export
with the `lambda` argument, or the first column is exported with a
warning.

## Usage

``` r
write_andi_chrom(x, path_out, sample_name = NULL, lambda = NULL, force = FALSE)
```

## Arguments

- x:

  A chromatogram in (wide) format.

- path_out:

  The path to write the file.

- sample_name:

  The name of the file. If a name is not provided, the name will be
  derived from the `sample_name` attribute.

- lambda:

  The wavelength to export, for a 3D chromatogram. Either a string
  matching one of the columns of `x` or the index of the column to
  export.

- force:

  Whether to overwrite existing files at the specified path. Defaults to
  `FALSE`.

## Value

Invisibly returns the path to the written CDF file.

## Details

Retention times are written in the unit the chromatogram reports,
declared in the file's mandatory `retention_unit` attribute as `Minutes`
or `Seconds`. A chromatogram whose `time_unit` is missing or
unrecognized is taken to be in minutes, since the attribute cannot be
left unset. The run length, delay time and sampling interval are derived
from the retention times, and `detector_maximum_value` and
`detector_minimum_value` report the range of the exported trace.
`actual_sampling_interval` is the mean of the intervals, and
`uniform_sampling_flag` reports whether every interval matches that
mean. The per-point times are written to `raw_data_retention` either
way, which is what a reader needs where the flag is `N`.

## Side effects

Exports a chromatogram in ANDI chromatography format (netCDF) in the
directory specified by `path_out`. The file will be named according to
the value of `sample_name`. If no `sample_name` is provided, the
`sample_name` attribute will be used if it exists.

## See also

Other write functions:
[`write_andi_ms()`](https://ethanbass.github.io/chromConverter/reference/write_andi_ms.md),
[`write_chroms()`](https://ethanbass.github.io/chromConverter/reference/write_chroms.md),
[`write_mzml()`](https://ethanbass.github.io/chromConverter/reference/write_mzml.md)

## Author

Ethan Bass

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chrom <- read_shimadzu(path, what = "chroms")
# the file is named for the `sample_name` attribute unless one is supplied
write_andi_chrom(chrom, path_out = tempdir(), force = TRUE)
```
