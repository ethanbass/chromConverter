# Write mzML

Writes spectra and chromatograms to an mzML file.

## Usage

``` r
write_mzml(
  data,
  path_out,
  sample_name = NULL,
  what = NULL,
  instrument_info = NULL,
  centroided = TRUE,
  compress = TRUE,
  indexed = TRUE,
  force = FALSE,
  show_progress = TRUE,
  verbose = getOption("verbose")
)
```

## Arguments

- data:

  A named list of `data.frame`s or `data.table`s, keyed by stream
  (`MS1`, `MS2`, `TIC`, `BPC`, `DAD`), or a single chromatogram carrying
  a `detector` attribute that says which stream it is.

- path_out:

  The path to write the file.

- sample_name:

  The name of the file. If a name is not provided, the name will be
  derived from the `sample_name` attribute, and it is an error if there
  is no such attribute.

- what:

  Which streams to write to mzML: `"MS1"`, `"MS2"`, `"TIC"`, `"BPC"`,
  and/or `"DAD"`. Defaults to every element of `data` that holds any
  rows.

- instrument_info:

  Controlled-vocabulary terms describing the instrument, as a list of
  lists with elements `cvRef`, `accession`, `name` and `value`, each
  written as one `cvParam` of the `instrumentConfiguration`. Defaults to
  `NULL`, in which case `MS:1000031` ("instrument model") is written
  with the chromatogram's `detector_model` or `instrument` as its value,
  or bare where it records neither.

- centroided:

  Logical. Whether the spectra are centroided, written as `MS:1000127`
  or, when `FALSE`, `MS:1000128` ("profile spectrum"). Defaults to
  `TRUE`. Set it to `FALSE` for the profile scan types of a triple
  quadrupole (a full scan or a product-ion scan, as opposed to SIM or
  MRM).

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

Mass spectra and DAD spectra are written to the `spectrumList`, while
the total ion current (`TIC`) and the base peak chromatogram (`BPC`) go
to the `chromatogramList`, since the controlled vocabulary has terms for
those two summaries. DAD spectra are written as electromagnetic
radiation spectra (`MS:1000804`) using Thermo's naming convention, with
`controllerType=4` in the spectrum ID for compatibility with existing
tools.

Asking for both `MS1` and `MS2` writes them into one `spectrumList`,
interleaved in acquisition order: on the `scan` column they share, or on
retention time where neither has one. Each spectrum is then named for
its scan (`scan=417`) rather than for its position in the list, which
keeps the names unique across the levels. An MS2 spectrum carries the
precursor it came from as `MS:1000744` ("selected ion m/z"), and a
`spectrumRef` to the MS1 spectrum that precedes it. Collision energy,
isolation window and precursor charge are not written, as no parser in
the package reads them.

Retention times are written in minutes (`UO:0000031`), as chromConverter
reports them, rather than converted to seconds as
[write_andi_ms](https://ethanbass.github.io/chromConverter/reference/write_andi_ms.md)
does.

The streams to write come from the names of `data`, so a bare
chromatogram has to say what it holds through its `detector` attribute:
`UV` and `DAD` are written as a DAD stream, and `MS` as `MS1`, or as
`MS2` where the table also carries an `ms_level` attribute above 1. Any
other `detector`, including a missing or `NA` one (which is how several
parsers report an unknown detector), is an error: the function stops
rather than guess, and asks for a named list instead.

A one-dimensional DAD stream (a single wavelength) is refused: mzML has
no axis to write it along, so it would become one single-point spectrum
per retention time. Use
[write_andi_chrom](https://ethanbass.github.io/chromConverter/reference/write_andi_chrom.md)
for a single trace. If it is the only stream requested this is an error;
otherwise it is dropped with a warning and the rest is written.

The file's metadata are taken from the `MS1` stream if it is written,
and otherwise from the first stream requested. That stream's
`sample_name` attribute names the file unless `sample_name` is supplied.

If `indexed = TRUE`, the function will generate an indexed mzML file,
which allows faster random access to spectra. The file is assembled by
writing XML strings straight to a connection rather than by building a
document in memory.

## See also

Other write functions:
[`write_andi_chrom()`](https://ethanbass.github.io/chromConverter/reference/write_andi_chrom.md),
[`write_andi_ms()`](https://ethanbass.github.io/chromConverter/reference/write_andi_ms.md),
[`write_chroms()`](https://ethanbass.github.io/chromConverter/reference/write_chroms.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
chrom <- read_chroms("path/to/file.qgd", progress_bar = FALSE)
write_mzml(chrom[[1]], path_out = "path/to/directory")
} # }
```
