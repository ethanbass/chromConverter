# Read 'Chromatotec' file

Reads 'Chromatotec' `.Chrom` files.

## Usage

``` r
read_chromatotec(
  path,
  what = c("chrom", "peak_table"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE,
  peaktable_format = c("chromatographr", "original")
)
```

## Arguments

- path:

  The path to 'Chromatotec' `.Chrom` file.

- what:

  Whether to extract chromatograms (`chrom`) and/or `peak_table` data.
  Accepts multiple arguments, and defaults to both.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

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

## Value

A chromatogram and/or peak table from the specified `path`, according to
the value of `what`, with times in minutes. Chromatograms are returned
in the format specified by `format_out`. The rows of the peak table that
report a total over most of the run rather than a peak, such as
`NMTHC-EqC6` and `THC_PAMS`, are only kept with
`peaktable_format = "original"`. In the original table, `Start` and
`End` are where integration of the peak starts and ends, and
`SubstanceIndex` appears to be the position of its substance in the
run's substance table (`0` for an unnamed peak); `U1`, `U3` and `U4`
have not been identified.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_chromatotec(path)
} # }
```
