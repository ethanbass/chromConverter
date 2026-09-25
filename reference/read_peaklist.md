# Read peak lists

Reads peak lists from specified folders or vector of paths.

## Usage

``` r
read_peaklist(
  paths,
  find_files,
  format_in = c("chemstation", "shimadzu_fid", "shimadzu_dad", "shimadzu_lcd",
    "shimadzu_gcd", "chromatotec"),
  pattern = NULL,
  peaktable_format = c("chromatographr", "original"),
  metadata_format = c("chromconverter", "raw"),
  read_metadata = TRUE,
  progress_bar,
  cl = 1,
  data_format = NULL
)
```

## Arguments

- paths:

  Paths to files or folders containing peak list files.

- find_files:

  Logical. Whether to treat the supplied paths as directories to search
  for files. Inferred if not supplied, by testing whether every path is
  a file.

- format_in:

  Format of files to be imported/converted. One of `chemstation` (the
  default), `shimadzu_fid`, `shimadzu_dad`, `shimadzu_lcd`,
  `shimadzu_gcd`, or `chromatotec`.

- pattern:

  A pattern (e.g. a file extension). Defaults to `NULL`, in which case
  the file extension will be deduced from `format_in`.

- peaktable_format:

  Whether to return peak tables in `chromatographr` or `original`
  format.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if `pbapply`
  is installed.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of parallel workers to use or a cluster object
  created by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html)
  (a set of parallel R worker processes). Defaults to `1`.

- data_format:

  Deprecated. Use `peaktable_format` instead.

## Value

A `peak_list`: a list with one element per sample, holding its peak
table, or a list of peak tables named by signal where the file records
more than one. Each row is a peak.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
path <- "tests/testthat/testdata/RUTIN2.D"
peak_list <- read_peaklist(path)
peak_list[["RUTIN2"]][["254"]]
}
```
