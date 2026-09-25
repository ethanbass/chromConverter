# Call Entab

Converts chromatography data files using
[entab](https://github.com/bovee/entab) parsers. 'entab' detects the
file format itself. Mass spectra are always returned in long format.

## Usage

``` r
call_entab(
  path,
  data_format = c("wide", "long"),
  format_out = c("matrix", "data.frame", "data.table"),
  format_in = "",
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw")
)
```

## Arguments

- path:

  Path to file.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- format_in:

  Format of the file, used only to read the metadata that 'entab' does
  not: those of 'ChemStation' formats (any value containing
  `chemstation`) and of `masshunter_dad`. Defaults to `""`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments.

## See also

Other external parsers:
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`sp_converter()`](https://ethanbass.github.io/chromConverter/reference/sp_converter.md),
[`uv_converter()`](https://ethanbass.github.io/chromConverter/reference/uv_converter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
call_entab("path/to/file.uv", format_in = "chemstation_uv")
} # }
```
