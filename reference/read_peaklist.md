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
  data_format = c("chromatographr", "original"),
  metadata_format = c("chromconverter", "raw"),
  read_metadata = TRUE,
  progress_bar,
  cl = 1
)
```

## Arguments

- paths:

  Paths to files or folders containing peak list files.

- find_files:

  Logical. Set to `TRUE` (default) if you are providing the function
  with a folder or vector of folders containing the files. Otherwise,
  set to`FALSE`.

- format_in:

  Format of files to be imported/converted. Current options include:
  `chemstation`, `shimadzu_fid`, `shimadzu_dad`, `shimadzu_lcd`, and
  `shimadzu_gcd`.

- pattern:

  pattern (e.g. a file extension). Defaults to NULL, in which case file
  extension will be deduced from `format_in`.

- data_format:

  Either `chromatographr` or `original`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- read_metadata:

  Logical, whether to attach metadata (if it's available). Defaults to
  TRUE.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if
  [`pbapply`](https://peter.solymos.org/pbapply/reference/pbapply.html)
  is installed.

- cl:

  Argument to
  [`pbapply`](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of clusters to use or a cluster object created
  by [`makeCluster`](https://rdrr.io/r/parallel/makeCluster.html).
  Defaults to 1.

## Value

A list of chromatograms in `matrix` or `data.frame` format, according to
the value of `format_out`.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
path <- "tests/testthat/testdata/dad1.uv"
chr <- read_chroms(path, find_files = FALSE, format_in = "chemstation_uv")
}
```
