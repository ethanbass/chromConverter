# Shared params 2D chromatogram

Shared params 2D chromatogram

## Arguments

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if `pbapply`
  is installed.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of clusters to use or a cluster object created
  by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html).
  Defaults to `1`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

- scale:

  Whether to scale the data by the scaling factor present in the file.
  Defaults to `TRUE`.

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments.
