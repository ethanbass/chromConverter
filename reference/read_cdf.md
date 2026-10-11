# Read CDF

Reads 'Analytical Data Interchange' (ANDI) netCDF (`.cdf`) files.

## Usage

``` r
read_cdf(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  what = NULL,
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE,
  peaktable_format = c("chromatographr", "original"),
  ...
)
```

## Arguments

- path:

  Path to ANDI netCDF file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format. `"long"` returns
  the retention time as the first column of the data.frame or matrix,
  while `"wide"` returns it as the rownames. The mass spectra of an ANDI
  MS file are always long, so there it applies only to the TIC.

- what:

  For ANDI chrom files, `chroms` and/or `peak_table`. For ANDI MS files,
  MS1 scans (`MS1`) and/or the total ion chromatogram (`TIC`).

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

- peaktable_format:

  Whether to return peak tables in the standard `chromatographr` layout
  (the default) or with all the columns the file provides (`original`).
  A `chromatographr` table has the columns `rt`, `start`, `end`, `area`,
  `height` and `FWHM` (the full width at half maximum), with times in
  the same unit as the format's chromatograms and `NA` where the file
  does not record a value.

- ...:

  Additional arguments to the parser. For ANDI MS files, pass
  `ms_format` to return the mass spectra as a `data.frame` (the default)
  or a `list`.

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments.

## Details

Retention times are returned in minutes, converted from the unit the
file declares. An ANDI chrom file declares it in its `retention_unit`
attribute, which also governs the peak table. Seconds is both what the
template uses and what all but one of its conformance files declare, so
a file that declares no unit is read as seconds, unless chromConverter
wrote it, in which case it is read as minutes.

An ANDI MS file has no mandatory unit attribute, since the specification
never formally defined its axes units, so `scan_acquisition_time` is
read as seconds, the only unit the specification suggests, unless a
`raw_data_time_units` attribute says otherwise.

Either kind of file warns about a unit it does not recognize and reads
it as seconds.

Some writers that do not record peak heights fill `peak_height` with
`-1` or `0` for every peak. A `chromatographr` peak table returns `NA`
heights in these cases.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_cdf("path/to/file.cdf")
} # }
```
