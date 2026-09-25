# Read 'Agilent' ACAML files

Extracts injection metadata from 'Agilent Common Analytical Markup
Language' (ACAML) files into a table.

## Usage

``` r
read_acaml(
  path,
  find_files,
  format_out = c("data.frame", "data.table", "tibble"),
  progress_bar = FALSE,
  cl = 1
)
```

## Arguments

- path:

  Path(s) to ACAML files or to folders that contain the files.

- find_files:

  Logical. Whether to treat the supplied paths as directories to search
  for files. Inferred if not supplied, by testing whether every path is
  a file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- progress_bar:

  Logical. Whether to show a progress bar. Defaults to `FALSE`, unlike
  [read_chroms](https://ethanbass.github.io/chromConverter/reference/read_chroms.md),
  because an ACAML file usually accompanies a sequence rather than
  arriving in bulk:
  [read_agilent_rslt](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md)
  calls this function on the single `.acaml` file in a `.rslt`
  directory, where a progress bar over one element is just noise.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of parallel workers to use or a cluster object
  created by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html)
  (a set of parallel R worker processes). Defaults to `1`.

## Value

A `data.frame`, `data.table` or `tibble` (according to the value of
`format_out`) with one row per injection, and a `SourceFile` column
naming the ACAML file it came from.

## Details

ACAML is an XML-based format used by Agilent OpenLab to store sequence
and sample metadata. This function extracts information from the
`InjectionMetaData` nodes embedded in the `InjectionMetaDataItems`
custom field files, which do not seem to be readily accessible through
other means.

## Examples

``` r
if (FALSE) { # \dontrun{
read_acaml(path)
} # }
```
