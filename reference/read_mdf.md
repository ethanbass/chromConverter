# Read 'Lumex' MDF

Reads 'Lumex' `.mdf` files.

## Usage

``` r
read_mdf(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE
)
```

## Arguments

- path:

  The path to a 'Lumex' `.mdf` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- read_metadata:

  Whether to read metadata from file.

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments (retention time x wavelength).

## Author

Ethan Bass
