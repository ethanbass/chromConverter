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
  format_in = c("agilent_d", "agilent_dx", "asm", "chemstation", "chemstation_fid",
    "chemstation_ch", "chemstation_csv", "chemstation_ms", "chemstation_uv",
    "masshunter_dad", "chromeleon_uv", "chromatotec", "mzml", "mzxml", "mdf",
    "shimadzu_ascii", "shimadzu_dad", "shimadzu_fid", "shimadzu_gcd", "shimadzu_qgd",
    "shimadzu_lcd", "thermoraw", "varian_sms", "waters_arw", "waters_raw", "msd", "csd",
    "wsd", "other"),
  find_files,
  pattern = NULL,
  parser = c("", "chromconverter", "aston", "entab", "thermoraw", "openchrom", "rainbow"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  path_out = NULL,
  export_format = c("", "csv", "chemstation_csv", "cdf", "mzml", "animl"),
  force = FALSE,
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  progress_bar,
  cl = 1,
  verbose = getOption("verbose"),
  sample_names = c("basename", "sample_name"),
  dat = NULL,
  ...
)
```

## Arguments

- paths:

  Paths to data files or directories containing the files.

- format_in:

  Format of files to be imported/converted. Current options include:
  `agilent_d`, `agilent_dx`, `chemstation`, `chemstation_uv`,
  `chemstation_ch`, `chemstation_csv`, `chemstation_ms`, `masshunter`,
  `masshunter_dad`, `chromeleon_uv`, `shimadzu_ascii`, `shimadzu_fid`,
  `shimadzu_dad`, `thermoraw`, `waters_arw`, `waters_raw`, `mzml`,
  `mzxml`, `cdf`, `mdf`, `msd`, `csd`, `wsd`, or `other`.

- find_files:

  Logical. Set to `TRUE` (default) if you are providing the function
  with a folder or vector of folders containing the files. Otherwise,
  set to`FALSE`.

- pattern:

  pattern (e.g. a file extension). Defaults to NULL, in which case file
  extension will be deduced from `format_in`.

- parser:

  What parser to use (optional). Current option are `chromconverter`,
  `aston`, `entab`, `thermoraw`, `openchrom`, or `rainbow`.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

- data_format:

  Whether to output data in wide or long format. Either `wide` or
  `long`.

- path_out:

  Path for exporting files. If path not specified, files will export to
  current working directory.

- export_format:

  Export format. Currently the options include `.csv`, `chemstation_csv`
  (utf-16 encoding), and `cdf`, unless you are using OpenChrom parsers,
  where there are two additional options: `mzml`, and `animl`.

- force:

  Logical. Whether to overwrite files when exporting. Defaults to
  `FALSE`.

- read_metadata:

  Logical, whether to attach metadata (if it's available). Defaults to
  TRUE.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if
  [`pbapply`](https://peter.solymos.org/pbapply/reference/pbapply.html)
  is installed.

- cl:

  Argument to
  [`pbapply`](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of clusters to use or a cluster object created
  by [`makeCluster`](https://rdrr.io/r/parallel/makeCluster.html).
  Defaults to 1.

- verbose:

  Logical. Whether to print output from external parsers to the R
  console.

- sample_names:

  Which sample names to use. Options are `basename` to use the filename
  (minus the extension) or `sample_name` to use the sample name encoded
  in the file metadata. Sample names default to the
  [`basename`](https://rdrr.io/r/base/basename.html) of the specified
  files.

- dat:

  Existing list of chromatograms to append results. (Defaults to NULL).

- ...:

  Additional arguments to parser.

## Value

A list of chromatograms in `matrix`, `data.frame`, or `data.table`
format, according to the value of `format_out`. Chromatograms may be
returned in either `wide` or `long` format according to the value of
`data_format`.

## Details

Provides a unified interface to all chromConverter parsers. Currently
recognizes 'Agilent ChemStation' (`.uv`, `.ch`, `.dx`), 'Agilent
MassHunter' (`.dad`), 'Thermo RAW' (`.raw`), 'Waters ARW' (`.arw`),
'Waters RAW' (`.raw`), 'Chromeleon ASCII' (`.txt`), 'Shimadzu ASCII'
(`.txt`), 'Shimadzu GCD', 'Shimadzu LCD' (DAD and chromatogram streams)
and 'Shimadzu QGD' files. Also, wraps 'OpenChrom' parsers, which include
many additional formats. To use 'Entab', 'ThermoRawFileParser', or
'OpenChrom' parsers, they must be manually installed. Please see the
instructions in the
[README](https://ethanbass.github.io/chromConverter/) for further
details.

If paths to individual files are provided, `read_chroms` will try to
infer the file format and select an appropriate parser. However, when
providing paths to directories, the file format must be specified using
the `format_in` argument.

## Side effects

If an `export_format` is provided, chromatograms will be exported in the
specified format specified into the folder specified by `path_out`.
Files can currently be converted to `csv`, `mzml`, or `cdf` format. If
an `openchrom` parser is selected, ANIML is available as an additional
option.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
path <- "tests/testthat/testdata/dad1.uv"
chr <- read_chroms(path, find_files = FALSE, format_in = "chemstation_uv")
}
```
