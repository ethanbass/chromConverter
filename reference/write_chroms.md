# Write chromatograms

Writes chromatograms to disk in the format specified by `export_format`:
either (`mzml`), `cdf` or `csv`.

## Usage

``` r
write_chroms(
  chrom_list,
  path_out,
  export_format = c("mzml", "cdf", "csv"),
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

  Format to export files: either `mzml`, `cdf`, or `csv`.

- what:

  What to write. Either `MS1` or `chrom`.

- force:

  Logical. Whether to overwrite existing files. Defaults to `TRUE`.

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
[`write_mzml()`](https://ethanbass.github.io/chromConverter/reference/write_mzml.md)

## Author

Ethan Bass
