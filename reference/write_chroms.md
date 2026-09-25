# Write chromatograms

Writes chromatograms to disk in the format specified by `export_format`:
either `mzml`, `cdf`, `csv`, or `arw`.

## Usage

``` r
write_chroms(
  chrom_list,
  path_out,
  export_format = c("mzml", "cdf", "csv", "arw"),
  what = "",
  force = FALSE,
  show_progress = TRUE,
  verbose = getOption("verbose"),
  ...
)
```

## Arguments

- chrom_list:

  A list of chromatograms.

- path_out:

  Path to directory for writing files.

- export_format:

  Format to export files: either `"mzml"`, `"cdf"`, `"csv"`, `"arw"`.

- what:

  What to write. Used by the `cdf` and `mzml` exporters only. For
  `export_format = "cdf"`, either `"MS1"` (written by `write_andi_ms`)
  or `"chrom"` (written by `write_andi_chrom`). For
  `export_format = "mzml"`, any of `"MS1"`, `"MS2"`, `"TIC"`, `"BPC"`
  and/or `"DAD"`. If it is not specified, the streams to write are
  inferred from the supplied data.

- force:

  Logical. Whether to overwrite existing files. Defaults to `FALSE`.

- show_progress:

  Logical. Whether to show progress bar. Defaults to `TRUE`.

- verbose:

  Logical. Whether to print verbose output.

- ...:

  Additional arguments to write function.

## Value

No return value. The function is called for its side effects.

## Side effects

Exports a chromatogram in the file format specified by `export_format`
in the directory specified by `path_out`.

## See also

Other write functions:
[`write_andi_chrom()`](https://ethanbass.github.io/chromConverter/reference/write_andi_chrom.md),
[`write_andi_ms()`](https://ethanbass.github.io/chromConverter/reference/write_andi_ms.md),
[`write_mzml()`](https://ethanbass.github.io/chromConverter/reference/write_mzml.md)

## Author

Ethan Bass

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chroms <- read_chroms(path, format_in = "shimadzu_ascii",
                      find_files = FALSE, progress_bar = FALSE)
write_chroms(chroms, path_out = tempdir(), export_format = "csv",
             show_progress = FALSE)
```
