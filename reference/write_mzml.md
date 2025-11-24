# Write mzML

This function constructs mzML files by writing XML strings directly to a
file connection. While this approach is fast, it may be less flexible
than methods based on an explicit Document Object Model (DOM).

## Usage

``` r
write_mzml(
  data,
  path_out,
  sample_name = NULL,
  what = NULL,
  instrument_info = NULL,
  compress = TRUE,
  indexed = TRUE,
  force = FALSE,
  show_progress = TRUE,
  verbose = getOption("verbose")
)
```

## Arguments

- data:

  List of data.frames or data.tables containing spectral data.

- path_out:

  The path to write the file.

- sample_name:

  The name of the file. If a name is not provided, the name will be
  derived from the `sample_name` attribute.

- what:

  Which streams to write to mzML: `"MS1"`, `"MS2"`, `"TIC"`, `"BPC"`,
  and/or `"DAD"`.

- instrument_info:

  Instrument info to write to mzML file.

- compress:

  Logical. Whether to use zlib compression. Defaults to `TRUE`.

- indexed:

  Logical. Whether to write indexed mzML. Defaults to `TRUE`.

- force:

  Logical. Whether to overwrite existing files at `path_out`. Defaults
  to `FALSE`.

- show_progress:

  Logical. Whether to show progress bar. Defaults to `TRUE`.

- verbose:

  Logical. Whether or not to print status messages.

## Value

Invisibly returns the path to the written mzML file.

## Details

The function supports writing various types of spectral data including
MS1, TIC (Total Ion Current), BPC (Base Peak Chromatogram), and DAD
(Diode Array Detector) data. DAD spectra are written as electromagnetic
radiation spectra (MS:1000804) using Thermo's naming convention with
`controllerType=4` in the spectrum ID for compatibility with existing
tools. Support for MS2 may be added in a future release.

If `indexed = TRUE`, the function will generate an indexed mzML file,
which allows faster random access to spectra.

## See also

Other write functions:
[`write_andi_chrom()`](https://ethanbass.github.io/chromConverter/reference/write_andi_chrom.md),
[`write_chroms()`](https://ethanbass.github.io/chromConverter/reference/write_chroms.md)

## Author

Ethan Bass
