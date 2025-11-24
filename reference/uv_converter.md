# Converter for 'Agilent ChemStation' UV files

Converts a single chromatogram from ChemStation `.uv` format to R
`data.frame`.

## Usage

``` r
uv_converter(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  correction = TRUE,
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw")
)
```

## Arguments

- path:

  Path to file

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- correction:

  Logical. Whether to apply empirical correction. Defaults is TRUE.

- read_metadata:

  Logical. Whether to read metadata and attach it to the chromatogram.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

## Value

A chromatogram in `data.frame` format (retention time x wavelength).

## Details

Uses the [Aston](https://github.com/bovee/aston) file parser.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`sp_converter()`](https://ethanbass.github.io/chromConverter/reference/sp_converter.md)
