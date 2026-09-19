# Converter for 'Agilent ChemStation' UV files

Defunct. Use
[read_chemstation_uv](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)
instead.

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

  Whether to return data in `wide` (default) or `long` format.

- correction:

  Logical. Whether to apply empirical correction. Defaults is TRUE.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

## Value

There is no return value. Calling this function is an error.

## Details

This function wrapped 'Aston”s generic `TraceFile` reader, which imports
`scipy.io.netcdf.NetCDFFile`. That symbol was removed in scipy v1.14, so
`TraceFile` cannot be used without pinning `scipy < 1.14` for the whole
Python session. Since `.uv` files are read by chromConverter's internal
parser,
[read_chemstation_uv](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md),
this wrapper was retired rather than constrain scipy for everyone.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`sp_converter()`](https://ethanbass.github.io/chromConverter/reference/sp_converter.md)
