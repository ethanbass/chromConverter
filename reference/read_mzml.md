# Read mzML files

Extracts data from `mzML` files using parsers from either RaMS or mzR.
The RaMS parser (default) will only return data in tidy (long) format.
The mzR parser will return data in wide format. Currently the mzR-based
parser is configured to return only DAD data.

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

  Class of output. Only applies if `mzR` is selected. Either `matrix`,
  `data.frame`, or `data.table`. `RaMS` will return a list of
  data.tables regardless of what is selected here.

- data_format:

  Whether to return data in `wide` or `long` format.

- parser:

  What parser to use. Either `RaMS` or `mzR`.

- what:

  What types of data to return (argument to
  [`grabMSdata`](https://rdrr.io/pkg/RaMS/man/grabMSdata.html). Options
  include `MS1`, `MS2`, `BPC`, `TIC`, `DAD`, `chroms`, `metadata`, or
  `everything`).

- verbose:

  Argument to
  [`grabMSdata`](https://rdrr.io/pkg/RaMS/man/grabMSdata.html)
  controlling `verbosity`.

- ...:

  Additional arguments to
  [`grabMSdata`](https://rdrr.io/pkg/RaMS/man/grabMSdata.html).

## Value

If `RaMS` is selected, the function will return a list of "tidy"
`data.table` objects. If `mzR` is selected, the function will return a
chromatogram in `matrix` or `data.frame` format according to the value
of `format_out`.

## Author

Ethan Bass
