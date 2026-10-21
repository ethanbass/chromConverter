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

  Whether to extract chromatograms (`chroms`), DAD data (`dad`),
  auxiliary instrumental data (`instrument`), such as temperature,
  pressure or solvent composition, and/or mass spectra (`MS1`) with the
  total ion chromatogram (`TIC`) and base peak chromatogram (`BPC`).
  Accepts multiple arguments, and defaults to `chroms` and `dad`. When
  more than one is requested, any the archive does not contain are left
  out; a single one that is missing is an error.

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
`format_out` and `data_format`. Mass spectra and the `BPC` are always in
long format, with `rt`, `mz` and `intensity` columns, and are returned
as a `data.table` when `format_out` is `matrix`. With `collapse = TRUE`,
a list of one element is replaced by that element.

## Details

The archive is extracted to `path_out`, or to a temporary directory that
is deleted afterwards, and its `.ch`, `.uv` and `.it` files are read
with
[read_chemstation_ch](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[read_chemstation_uv](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)
and an internal reader for `.it` files.

The mass spectra of an MS injection are not in the archive. They are in
the `.MSScan.bin` and `.MSPeak.bin` files of the same name beside it.
The first is needed for `MS1`, `TIC` and `BPC`, the second for `MS1`.
`MSScan.bin` holds one record per scan, laid out as the archive's
`MSData/MSScan.xsd` describes, which gives the retention time, TIC and
base peak of the scan and the location of its spectrum in `MSPeak.bin`.
Each spectrum there is the scan's m/z values followed by as many
intensities, as 32- or 64-bit floats or 32-bit integers depending on the
scan's spectrum format and size. Only scans at MS level 1 are read into
`MS1`, while the `TIC` and `BPC` cover every scan.

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
[`read_chemstation_logs()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_logs.md),
[`read_chemstation_method()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_method.md),
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
