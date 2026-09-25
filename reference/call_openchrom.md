# Parse files with OpenChrom

Converts files with the OpenChrom command-line interface, then reads the
converted files back into R. OpenChrom removed the command-line
interface in version 1.5.0, and Lablicate no longer distributes older
versions, so this function works only with an existing installation of
OpenChrom 1.4 or earlier. It is deprecated for that reason.

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

  Directory to export converted files. Defaults to `NULL`, in which case
  the files go to a temporary directory that is deleted when the
  function returns.

- format_in:

  Either `msd` for mass spectrometry data, `csd` for FID, ECD or NPD
  data, or `wsd` for DAD/UV data.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- export_format:

  Either `mzml`, `csv`, `cdf`, or `animl`. Defaults to `mzml`. `mzml`
  and `cdf` are available only for `msd`.

- return_paths:

  Logical. If `TRUE`, the function will return a character vector of
  paths to the newly created files.

- verbose:

  Logical. Whether to print output from OpenChrom to the console.

## Value

If `return_paths` is `FALSE`, a list with one chromatogram per file,
read back with
[read_mzml](https://ethanbass.github.io/chromConverter/reference/read_mzml.md),
[read_cdf](https://ethanbass.github.io/chromConverter/reference/read_cdf.md)
or [read.csv](https://rdrr.io/r/utils/read.table.html) according to
`export_format`. `format_out` and `data_format` apply only to `cdf`
files, except that `format_out = "matrix"` also converts `csv` files.
`animl` files cannot be read back, so use `return_paths = TRUE` for
them. If `return_paths` is `TRUE`, a character vector of paths to the
newly created files.

## Details

The `call_openchrom` function works by creating an xml batchfile and
feeding it to the OpenChrom command-line interface. OpenChrom batchfiles
consist of `InputEntries` (specifying the files you want to convert) and
`ProcessEntries` (specifying what you want to do to the files). The
parsers are organized into broad categories by detector-type and output
format. The detector-types are `msd` (mass selective detectors), `csd`
(current selective detectors, e.g., FID, ECD, NPD), and `wsd`
(wavelength selective detectors, e.g., DAD, and UV/VIS). Thus, when
calling the OpenChrom parsers, one of these three options must be
specified using the `format_in` argument.

## Note

Activating the OpenChrom command-line deactivates the graphical user
interface (GUI). To keep using the GUI, install a second copy of
OpenChrom and call that one from R.

## Side effects

Chromatograms are exported in the format specified by `export_format` to
the folder specified by `path_out`.

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

## Examples

``` r
if (FALSE) { # \dontrun{
call_openchrom("path/to/file.RAW", format_in = "msd", export_format = "mzml")
} # }
```
