# Read files from 'Agilent' .rslt directories

Reads a sequence of injections from an 'Agilent' `.rslt` directory.
Combines raw chromatogram data from `.dx` files with injection-level
metadata parsed from the accompanying `.acaml` file.

## Usage

``` r
read_agilent_rslt(
  path,
  what = c("chroms", "dad"),
  path_out = NULL,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE,
  sample_names = c("basename", "sample_name"),
  progress_bar = FALSE,
  cl = 1
)
```

## Arguments

- path:

  Path to 'Agilent' `.rslt` directory.

- what:

  Whether to extract chromatograms (`chroms`), DAD data (`dad`) and/or
  auxiliary instrumental data (`instrument`) (e.g., temperature,
  pressure, solvent composition, etc.). Accepts multiple arguments.

- path_out:

  A directory to export unzipped files. If a path is not specified, the
  files will be written to a temp directory on the disk. The function
  will overwrite existing folders in the specified directory that share
  the basename of the file specified by `path`.

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

- sample_names:

  How to name the chromatograms that are returned. Either `basename`
  (default), to use the name of the source `.dx` file, or `sample_name`,
  to use the sample name field from the metadata.

- progress_bar:

  Logical. Whether to show a progress bar while reading the sequence's
  `.dx` files. Defaults to `FALSE`, since
  [read_chroms](https://ethanbass.github.io/chromConverter/reference/read_chroms.md)
  already reports progress over the directories it found and a second
  bar inside each one would be redrawn per directory. Set to `TRUE` when
  calling this function directly on a sequence with many injections.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of clusters to use or a cluster object created
  by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html).
  Defaults to `1`.

## Value

A list of chromatograms (one `read_agilent_dx`-style result per
injection in the sequence), in the format specified by `data_format` and
`format_out`. If `read_metadata` is `TRUE`, injection-level metadata
parsed from the `.acaml` file is attached to each chromatogram as an
attribute.

## Details

Currently this function only reads `.dx` chromatogram files. Peak tables
stored in `.rx` files are not yet supported.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
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
read_agilent_rslt("path/to/sequence.rslt")
} # }
```
