# Read 'Agilent' ACAML files from directory.

Extracts injection metadata from 'Agilent Common Analytical Markup
Language' (ACAML) files into an R object.

## Usage

``` r
read_acaml(
  path,
  find_files,
  format_out = c("data.frame", "data.table", "tibble"),
  progress_bar = TRUE,
  cl = 1
)
```

## Arguments

- path:

  Path(s) to ACAML files or to folders that contain the files.

- find_files:

  Logical. Set to `TRUE` (default) if you are providing the function
  with a folder or vector of folders containing the files. Otherwise,
  set to `FALSE`.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if `pbapply`
  is installed.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of clusters to use or a cluster object created
  by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html).
  Defaults to `1`.

## Value

A `data.frame`, `data.table` or `tibble` (according to the value of
`format_out`) containing sample metadata derived from the supplied ACAML
files.

## Details

ACAML is an XML-based format used by Agilent OpenLab to store sequence
and sample metadata. This function extracts information from the
`InjectionMetaData` nodes embedded in the `InjectionMetaDataItems`
custom field files, which do not seem to be readily accessible through
other means.
