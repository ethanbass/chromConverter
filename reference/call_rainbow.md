# Call 'rainbow' parsers

Parse 'Agilent' or 'Waters' files with rainbow parsers.

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
  sparse = TRUE,
  bin_width = NULL
)
```

## Arguments

- path:

  Path to file.

- format_in:

  Format of the supplied files. Either `agilent_d`, `waters_raw`,
  `masshunter`, `chemstation`, `chemstation_uv`, `chemstation_fid`, or
  `chemstation_ms`.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- by:

  How to group the returned list: by `detector` (default), or by `name`,
  one element per data file in the directory.

- what:

  Which detectors to return (e.g. `MS`, `UV`, `CAD`, `ELSD`). Applies
  only when `by = "detector"`. Defaults to `NULL`, which returns all of
  them.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

- precision:

  Number of decimals of the m/z grid, which is spaced `10^-precision`
  apart. Defaults to `1`. Ignored if `bin_width` is supplied.

- sparse:

  Logical. Whether to drop zero intensities from MS data. Applies only
  to `long` format. Defaults to `TRUE`.

- bin_width:

  Width of the m/z grid, in daltons. An alternative to `precision` for
  grids that are not a power of ten (e.g. `0.5`). Defaults to `NULL`, in
  which case the grid is derived from `precision` as `10^-precision`.

## Value

A (nested) list of chromatograms, or a single chromatogram for the
`chemstation` formats, in the class given by `format_out`. The list is
grouped according to the value of `by`.

## Details

Uses [rainbow](https://rainbow-api.readthedocs.io) parsers to read in
Agilent (`.D`) and Waters (`.raw`) files. For `agilent_d`, `waters_raw`
and `masshunter`, `path` is the data directory (`.D` or `.raw`), and the
result is a list grouped according to `by`. For the `chemstation`
formats, `path` is a single file (e.g. `.uv`), the result is a single
chromatogram, and `by` and `what` are ignored. Otherwise, data can be
filtered by detector type using the `what` argument.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`sp_converter()`](https://ethanbass.github.io/chromConverter/reference/sp_converter.md),
[`uv_converter()`](https://ethanbass.github.io/chromConverter/reference/uv_converter.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
call_rainbow("path/to/file.D", format_in = "agilent_d")
} # }
```
