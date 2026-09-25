# Read mzML files

Extracts data from `mzML` files using parsers from either RaMS or mzR.

## Usage

``` r
read_mzml(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  parser = c("RaMS", "mzR"),
  what = c("MS1", "MS2", "BPC", "TIC", "DAD", "chroms", "metadata", "everything"),
  verbose = FALSE,
  ...
)
```

## Arguments

- path:

  Path to `.mzml` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`. With
  RaMS, applies only to the `TIC` and `BPC` in wide format.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- parser:

  What parser to use. Either `RaMS` or `mzR`.

- what:

  What types of data to return (argument to
  [RaMS::grabMSdata](https://rdrr.io/pkg/RaMS/man/grabMSdata.html)).
  Options include `MS1`, `MS2`, `BPC`, `TIC`, `DAD`, `chroms`,
  `metadata`, or `everything`. Defaults to all of them.

- verbose:

  Argument to `grabMSdata` controlling verbosity.

- ...:

  Additional arguments to `grabMSdata`.

## Value

With RaMS, a named list of the streams in `what`. With mzR, a DAD
chromatogram in the format specified by `format_out` and `data_format`.

## Details

The RaMS parser (default) returns a list with one element per stream in
`what`. Mass spectra are always long. With `data_format = "wide"` (the
default), `TIC` and `BPC` are returned as 2D chromatograms of class
`format_out`, and `DAD` as a wide chromatogram of class `format_out`;
with `"long"`, every stream is a long `data.table`. The mzR parser
returns only the DAD data, as a single chromatogram.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_mzml("path/to/file.mzML")
} # }
```
