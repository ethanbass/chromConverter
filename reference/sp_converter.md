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

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments.

## Deprecation

The 'aston' parser is deprecated and will be removed in a future
release. 'Aston' has been unmaintained since 2020. This is the only
remaining 'aston' binding, and
[read_chroms](https://ethanbass.github.io/chromConverter/reference/read_chroms.md)
selects it automatically only as a last resort, when no other parser can
read the file. Please use the 'entab' parser (by the same author as
'Aston') instead, e.g.
`read_chroms(path, format_in = "masshunter_dad", parser = "entab")`.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`uv_converter()`](https://ethanbass.github.io/chromConverter/reference/uv_converter.md)
