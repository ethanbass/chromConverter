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

  Path to `.dx` file.

- what:

  Whether to extract chromatograms (`chroms`), DAD data (`dad`) and/or
  auxiliary instrumental data `instrument` (e.g., temperature, pressure,
  solvent composition, etc.). Accepts multiple arguments.

- path_out:

  A directory to export unzipped files. If a path is not specified, the
  files will be written to a temp directory on the disk. The function
  will overwrite existing folders in the specified directory that share
  the basename of the file specified by `path`.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

## Value

A chromatogram in the format specified by `format_out` (retention time x
wavelength).

## Details

This function unzips 'Agilent' `.dx` into a temporary directory using
[`unzip`](https://rdrr.io/r/utils/unzip.html) and calls
[`read_chemstation_ch`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md).

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass
