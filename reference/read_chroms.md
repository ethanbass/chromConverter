# Read Chromatograms

Reads chromatograms from specified folders or vector of paths using
either an internal parser or bindings to an external library, such as
[Aston](https://github.com/bovee/aston),
[Entab](https://github.com/bovee/entab),
[ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser),
[OpenChrom](https://lablicate.com/platform/openchrom),
[rainbow](https://rainbow-api.readthedocs.io/).

## Usage

``` r
read_chroms(
  paths,
  format_in = supported_formats(),
  find_files,
  pattern = NULL,
  parser = c("", "chromconverter", "aston", "entab", "thermoraw", "openchrom", "rainbow"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  path_out = NULL,
  export_format = c("", "csv", "chemstation_csv", "cdf", "mzml", "animl", "arw"),
  force = FALSE,
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  progress_bar,
  cl = 1,
  verbose = getOption("verbose"),
  sample_names = c("basename", "sample_name"),
  sort_by = c("none", "acquisition_time", "file_time"),
  dat = NULL,
  ...
)
```

## Arguments

- paths:

  Paths to data files or directories containing the files.

- format_in:

  Format of the files to be imported or converted. One of: `agilent_d`,
  `agilent_dx` (or `openlab_dx`), `agilent_rslt` (or `rslt`, `sirslt`),
  `asm` (or `allotrope`), `cdf` (or `andi`), `chemstation_ch` (or
  `chemstation_fid`), `chemstation_csv`, `chemstation_ms`,
  `chemstation_uv`, `chromatotec`, `chromeleon_uv`, `csd`, `csv`,
  `masshunter_dad`, `mdf`, `msd`, `mzml`, `mzxml`, `other`,
  `shimadzu_ascii`, `shimadzu_dad`, `shimadzu_fid`, `shimadzu_gcd`,
  `shimadzu_lcd`, `shimadzu_qgd`, `thermoraw`, `varian_sms`,
  `waters_arw`, `waters_raw`, `wsd`. A name in parentheses is an alias,
  which behaves exactly like the format it follows. Version-specific
  names for the 'Agilent ChemStation' formats (`chemstation_130`, for
  instance) are accepted as well, but are normally supplied by
  chromConverter's own file-type detection rather than being provided by
  the user.

- find_files:

  Logical. Whether to treat `paths` as directories to search for data
  files. Inferred from `paths` if not supplied: anything that is not a
  file is searched as a directory, except for the formats that are
  themselves directories (e.g. 'Agilent' `.d`), which are recognized by
  their extension.

- pattern:

  Regular expression that file names must match (e.g. a file extension).
  Defaults to `NULL`, in which case the extension is deduced from
  `format_in`.

- parser:

  What parser to use (optional). Current options are `chromconverter`,
  `aston`, `entab`, `thermoraw`, `openchrom`, `rainbow`.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to output data in wide or long format. Either `wide` (default)
  or `long`.

- path_out:

  Path for exporting files. If it is not specified, the user is asked
  whether to export to a `temp` directory in the working directory. A
  directory that does not exist is created after asking.

- export_format:

  Export format: `csv`, `chemstation_csv` (UTF-16 encoding), `cdf`,
  `mzml`, `arw`, or `animl`, which requires an `openchrom` parser.

- force:

  Logical. Whether to overwrite files when exporting. Defaults to
  `FALSE`.

- read_metadata:

  Logical, whether to attach metadata (if it's available). Defaults to
  `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if `pbapply`
  is installed.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of parallel workers to use or a cluster object
  created by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html).
  Defaults to `1`.

- verbose:

  Logical. Whether to print status messages, and the output of external
  parsers, to the R console.

- sample_names:

  Which sample names to use. Options are `basename` to use the filename
  (default) or `sample_name` to use the sample name encoded in the file
  metadata. A sample with no `sample_name`, or with conflicting ones, is
  named for its file with a warning.

- sort_by:

  How to sort the chromatograms. Either `none` (default), which keeps
  them in the order of `paths`, with files found in a directory in
  alphabetical order; `acquisition_time`, which sorts by the
  `run_datetime` attribute, oldest first, placing chromatograms without
  one last with a warning (requires `read_metadata = TRUE`); or
  `file_time`, which sorts the files by modification time before
  reading, oldest first.

- dat:

  Deprecated. Existing list of chromatograms to append results to. Use
  [`c()`](https://rdrr.io/r/base/c.html) on the returned `chrom_list`
  objects instead. Defaults to `NULL`.

- ...:

  Additional arguments to the parser. Where the parser does not take
  `...`, arguments it does not accept are dropped with a warning.

## Value

A `chrom_list` of chromatograms in `matrix`, `data.frame`, or
`data.table` format, according to the value of `format_out`.
Chromatograms may be returned in either `wide` or `long` format
according to the value of `data_format`.

## Details

Provides a unified interface to all chromConverter parsers. The formats
it recognizes are listed under the `format_in` argument. It also wraps
the 'OpenChrom' parsers, which cover many additional formats but require
'OpenChrom' 1.4 or earlier (see
[call_openchrom](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md)).
The 'Entab', 'ThermoRawFileParser' and 'OpenChrom' parsers must be
installed separately; see the instructions in the
[README](https://ethanbass.github.io/chromConverter/).

If paths to individual files are provided, `read_chroms` infers the file
format from the first file and selects a parser for it. When providing
paths to directories, the file format must be specified using the
`format_in` argument.

## Side effects

If `export_format` is provided, chromatograms are written to the folder
given by `path_out` in that format. The options are `csv`,
`chemstation_csv`, `cdf`, `mzml` and `arw`, as well as `animl` (AnIML)
when an `openchrom` parser is selected. Files are also written to
`path_out` whenever the `thermoraw` or `openchrom` parser is used, as
these parsers convert the files before reading them: `thermoraw` to
mzML, and `openchrom` to `export_format` (`mzml` by default).

## Author

Ethan Bass

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chroms <- read_chroms(path, format_in = "shimadzu_ascii",
                      find_files = FALSE, progress_bar = FALSE)
```
