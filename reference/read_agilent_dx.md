# Read 'Agilent' DX files

Reads 'Agilent' `.dx` files.

## Usage

``` r
read_agilent_dx(
  path,
  what = c("chroms", "dad"),
  path_out = NULL,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to Agilent `.dx` file.

- what:

  Whether to extract chromatograms (`chroms`), DAD data (`dad`) and/or
  auxiliary instrumental data (`instrument`), such as temperature,
  pressure or solvent composition. Accepts multiple arguments, and
  defaults to `chroms` and `dad`. When more than one is requested, any
  the archive does not contain are left out; a single one that is
  missing is an error.

- path_out:

  A directory to export unzipped files. If a path is not specified, a
  temporary directory is used. The files are extracted into a folder
  named for `path`, overwriting any files of the same name already
  there.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A list with one element per type in `what`, each a chromatogram or a
list of chromatograms named by signal, in the format specified by
`format_out` and `data_format`. With `collapse = TRUE`, a list of one
element is replaced by that element.

## Details

The archive is extracted to `path_out`, or to a temporary directory that
is deleted afterwards, and its `.ch`, `.uv` and `.it` files are read
with
[read_chemstation_ch](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[read_chemstation_uv](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)
and an internal reader for `.it` files.

Where the archive holds an `injection.acmd` file, the `run_datetime` and
`sample_injection_volume` attributes are taken from it. Its run time
records the offset from UTC, while the `.ch` and `.uv` headers give only
the local time.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_agilent_dx(path)
} # }
```
