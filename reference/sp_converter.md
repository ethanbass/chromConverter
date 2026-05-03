# Converter for 'Agilent MassHunter' UV files

Converts a single chromatogram from MassHunter `.sp` format to R
`data.frame` using the [Aston](https://github.com/bovee/aston) file
parser.

## Usage

``` r
sp_converter(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw")
)
```

## Arguments

- path:

  Path to file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`uv_converter()`](https://ethanbass.github.io/chromConverter/reference/uv_converter.md)
