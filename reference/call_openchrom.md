# Parse files with OpenChrom

Writes `xml` batch-files and calls OpenChrom file parsers using a system
call to the command-line interface. Unfortunately, the command-line
interface is no longer supported in newer versions of OpenChrom
(starting with version 1.5.0) and older versions of OpenChrom that do
support the command line interface are no longer available from
Lablicate. Thus, this function is deprecated since it will only work if
you happen to have access to OpenChrom version 1.4.0, which has been
scrubbed from the internet.

## Usage

``` r
call_openchrom(
  files,
  path_out = NULL,
  format_in,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  export_format = c("mzml", "csv", "cdf", "animl"),
  return_paths = FALSE,
  verbose = getOption("verbose")
)
```

## Arguments

- files:

  Path to files.

- path_out:

  Directory to export converted files.

- format_in:

  Either `msd` for mass spectrometry data, `csd` for flame ionization
  data, or `wsd` for DAD/UV data.

- format_out:

  R format. Either `matrix`, `data.frame` or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- export_format:

  Either `mzml`, `csv`, `cdf`, `animl`. Defaults to `mzml`.

- return_paths:

  Logical. If TRUE, the function will return a character vector of paths
  to the newly created files.

- verbose:

  Logical. Whether to print output from OpenChrom to the console.

## Value

If `return_paths` is `FALSE`, the function will return a list of
chromatograms (if an appropriate parser is available to import the files
into R). The chromatograms will be returned in `matrix` or `data.frame`
format according to the value of `format_out`. If `return_paths` is
`TRUE`, the function will return a character vector of paths to the
newly created files.

## Details

The `call_openchrom` function works by creating an `xml` batchfile and
feeding it to the OpenChrom command-line interface. OpenChrom batchfiles
consist of `InputEntries` (the files you want to convert) and
`ProcessEntries` (what you want to do to the files). The parsers are
organized into broad categories by detector-type and output format. The
detector-types are `msd` (mass selective detectors), `csd` (current
selective detectors, e.g., FID, ECD, NPD), and `wsd` (wavelength
selective detectors, e.g., DAD, and UV/VIS). Thus, when calling the
OpenChrom parsers, you must select one of these three options for the
input format (`format_in`).

## Note

Activating the OpenChrom command-line will deactivate the graphical user
interface (GUI). Thus, if you wish to continue using the OpenChrom GUI,
it is recommended to create a separate command-line version of OpenChrom
to call from R.

## Side effects

Chromatograms will be exported in the format specified by
`export_format` in the folder specified by `path_out`.

## References

Wenig, Philip and Odermatt, Juergen. OpenChrom: A Cross-Platform Open
Source Software for the Mass Spectrometric Analysis of Chromatographic
Data. *BMC Bioinformatics* **11**, no. 1 (July 30, 2010): 405.
[doi:10.1186/1471-2105-11-405](https://doi.org/10.1186/1471-2105-11-405)
.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md),
[`read_thermoraw()`](https://ethanbass.github.io/chromConverter/reference/read_thermoraw.md),
[`sp_converter()`](https://ethanbass.github.io/chromConverter/reference/sp_converter.md),
[`uv_converter()`](https://ethanbass.github.io/chromConverter/reference/uv_converter.md)

## Author

Ethan Bass
