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

  Class of output. Either `matrix`, `data.frame`, or `data.table`.
  Applies only to the `TIC`, `BPC`, `DAD` and `chroms` in wide format.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- parser:

  What parser to use. Either `RaMS` or `mzR`.

- what:

  What types of data to return (argument to
  [RaMS::grabMSdata](https://rdrr.io/pkg/RaMS/man/grabMSdata.html)).
  Options include `MS1`, `MS2`, `BPC`, `TIC`, `DAD`, `chroms`,
  `metadata`, or `everything`. Defaults to all of them. `EIC` and
  `EIC_MS2` return extracted ion chromatograms from the MS1 or MS2 data
  for the masses given by the `mz` and `ppm` arguments of
  [RaMS::grabMSdata](https://rdrr.io/pkg/RaMS/man/grabMSdata.html),
  passed through `...`.

- verbose:

  Argument to `grabMSdata` controlling verbosity. Ignored by mzR.

- ...:

  Additional arguments to `grabMSdata`. Ignored by mzR.

## Value

A named list of the streams in `what`.

## Details

Both parsers return a list with one element per stream in `what`. Mass
spectra and EICs are always long. With `data_format = "wide"` (the
default), `TIC` and `BPC` are returned as 2D chromatograms of class
`format_out`, and `DAD` as a wide chromatogram of class `format_out`;
with `"long"`, every stream is a long `data.table`.

The `chroms` stream holds the chromatograms stored in the file, such as
the SIM and SRM traces of a triple quadrupole run. In wide format it is
a list of 2D chromatograms named by their ID in the file (or
`Chromatogram 1`, `Chromatogram 2` and so on where it has none), each
with the `precursor_mz` and `product_mz` it monitors as attributes. If
any trace repeats a retention time, as in some malformed files, the
whole stream is returned as the long `data.table` instead, with a
warning.

The mzR parser reads files that RaMS cannot, such as those compressed
with MS-Numpress. It returns only `MS1`, `MS2`, `BPC`, `TIC` and `DAD`,
and no file metadata. Its `TIC` and `BPC` are the per-scan values
recorded in the MS1 spectra.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_mzml("path/to/file.mzML")
} # }
```
