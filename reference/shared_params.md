# Shared params 2D chromatogram

Shared params 2D chromatogram

## Arguments

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if `pbapply`
  is installed.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of parallel workers to use or a cluster object
  created by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html)
  (a set of parallel R worker processes). Defaults to `1`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

- scale:

  Whether to scale the data by the scaling factor present in the file.
  Defaults to `TRUE`.

- peaktable_format:

  Whether to return peak tables in the standard `chromatographr` layout
  (the default) or with all the columns the file provides (`original`).
  A `chromatographr` table has the columns `rt`, `start`, `end`, `area`,
  `height` and `FWHM` (the full width at half maximum), with times in
  the same unit as the format's chromatograms and `NA` where the file
  does not record a value.

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments.
