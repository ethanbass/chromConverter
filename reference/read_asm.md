# Read 'Allotrope Simple Model' (ASM) files

Reads chromatograms, mass spectra and peak lists from ['Allotrope Simple
Model'](https://allotropefoundation.org/our-technology/) chromatography
files into R.

## Usage

``` r
read_asm(
  path,
  what = c("chroms", "MS1"),
  data_format = c("wide", "long"),
  format_out = c("matrix", "data.frame", "data.table"),
  peaktable_format = c("chromatographr", "original"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to ASM `.json` file.

- what:

  What to read: 2D chromatograms (`chroms`), mass spectra (`MS1`), peak
  lists (`peak_table`) and/or instrument traces (`instrument`), such as
  pump pressure, flow rate or temperature. Defaults to `chroms` and
  `MS1`, dropping whichever the file does not contain.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- peaktable_format:

  Whether to return peak tables in `chromatographr` format (`rt`,
  `start`, `end`, `area` and `height`) or `original` format, with every
  field the file records under its ASM name.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A 2D chromatogram in the format specified by `format_out` and
`data_format`, or a list of them named by detection type if the file
holds more than one (or `collapse = FALSE`). Mass spectra are returned
as a `data.frame` (or `data.table`) with columns `rt`, `mz` and
`intensity`, and peak tables as a `data.frame` (or `data.table`) per
measurement. Instrument traces are returned as 2D chromatograms named by
trace. Where more than one of these is returned, they are combined in a
list named by `what`. A file with several injections returns a
`chrom_list` with one such element per injection. Metadata are attached
as [attributes](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.

## Details

Retention times are returned in the unit the file declares, which is
recorded in the `time_unit` attribute.

A file can hold several injections, such as a sequence exported from a
chromatography data system. Each injection is then returned as a
separate sample, named by its sample name, and
[read_chroms](https://ethanbass.github.io/chromConverter/reference/read_chroms.md)
adds each one to the list of samples it returns.

Mass spectra have so far only been tested against the example files
published by the Allotrope Foundation.

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_asm("path/to/file.json")
read_asm("path/to/file.json", what = "peak_table")
} # }
```
