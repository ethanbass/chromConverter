# Call 'rainbow' parsers Parse 'Agilent' or 'Waters' files with rainbow parsers

Uses [rainbow](https://rainbow-api.readthedocs.io) parsers to read in
Agilent (.D) and Waters (.raw) files. If `format_in` is `"agilent_d"` or
`"waters_raw"`, a directory of the appropriate format (`.d` or `.raw`)
should be provided to the `file` argument. If `format_in` is
`"chemstation_uv"` a `.uv` file should be provided. Data can be filtered
by detector type using the `what` argument.

## Usage

``` r
call_rainbow(
  path,
  format_in = c("agilent_d", "waters_raw", "masshunter", "chemstation", "chemstation_uv",
    "chemstation_fid", "chemstation_ms"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  by = c("detector", "name"),
  what = NULL,
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE,
  precision = 1,
  sparse = TRUE
)
```

## Arguments

- path:

  Path to file.

- format_in:

  Format of the supplied files. Either `agilent_d`, `waters_raw`, or
  `chemstation`.

- format_out:

  R format. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in wide or long format.

- by:

  How to order the list that is returned. Either `detector` (default) or
  `name`.

- what:

  What types of data to return (e.g. `MS`, `UV`, `CAD`, `ELSD`). This
  argument only applies if `by == "detector"`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to TRUE.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

- precision:

  Number of decimals to round mz values. Defaults to 1.

- sparse:

  Logical. Whether to return MS data in sparse format (excluding zeros).
  Defaults to `TRUE`. Applies only when data are requested in `long`
  format.

## Value

Returns a (nested) list of `matrices` or `data.frames` according to the
value of `format_out`. Data is ordered according to the value of `by`.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`sp_converter()`](https://ethanbass.github.io/chromConverter/reference/sp_converter.md),
[`uv_converter()`](https://ethanbass.github.io/chromConverter/reference/uv_converter.md)

## Author

Ethan Bass
