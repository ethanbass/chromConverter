# Write ANDI MS CDF file from chromatogram

Exports mass spectrometry data in ANDI (Analytical Data Interchange) MS
format (ASTM E1947-98). A total ion chromatogram is derived from the MS1
scans if the supplied object does not already carry one.

## Usage

``` r
write_andi_ms(
  x,
  path_out,
  sample_name = NULL,
  force = FALSE,
  ms_params = list()
)
```

## Arguments

- x:

  A list of chromatograms containing an element whose name includes `MS`
  (e.g. `MS1`), or a single chromatogram of MS1 scans, in `wide` or
  `long` format.

- path_out:

  The path to write the file.

- sample_name:

  The name of the file. If a name is not provided, the name will be
  derived from the `sample_name` attribute.

- force:

  Whether to overwrite existing files at the specified path. Defaults to
  `FALSE`.

- ms_params:

  A list of settings recorded in the file: `ionization_mode`,
  `ionization_polarity`, `detector_type`, `scan_function` and
  `experiment_type`. A setting that is not given is taken from the
  chromatogram's metadata where it records one: the polarity from its
  `polarity` attribute, the scan function from `scan_type`, and the
  ionization mode and detector type from its `ms_params` attribute.
  Failing that, the defaults are `"Electron Impact"`,
  `"Positive Polarity"` and `"Electron Multiplier"`, and a scan function
  is left out of the file. `experiment_type` is not read from metadata
  and defaults to `"Centroided Mass Spectrum"`.

## Value

Invisibly returns the path to the written CDF file.

## Details

Retention times are written in seconds, the only time unit the
specification suggests, and declared in `raw_data_time_units`. A
chromatogram is converted unless its `time_unit` already reports
seconds, so one whose unit is missing or unrecognized is taken to be in
minutes.

## Side effects

Exports mass spectrometry data in ANDI MS format (netCDF) in the
directory specified by `path_out`. The file will be named according to
the value of `sample_name`. If no `sample_name` is provided, the
`sample_name` attribute will be used if it exists.

## See also

Other write functions:
[`write_andi_chrom()`](https://ethanbass.github.io/chromConverter/reference/write_andi_chrom.md),
[`write_chroms()`](https://ethanbass.github.io/chromConverter/reference/write_chroms.md),
[`write_mzml()`](https://ethanbass.github.io/chromConverter/reference/write_mzml.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
write_andi_ms(chrom, path_out = "path/to/directory")
} # }
```
