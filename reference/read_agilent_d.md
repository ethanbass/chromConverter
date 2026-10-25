# Read files from 'Agilent ChemStation' .D directories

Reads the `.ch`, `.uv`, `Report.TXT` (or `Result.xml`) and `LCDIAG.REG`
files in an 'Agilent' `.D` directory. Other files in the directory are
ignored.

## Usage

``` r
read_agilent_d(
  path,
  what = c("dad", "chroms", "peak_table", "MS1", "MS2", "TIC", "BPC"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Agilent' `.D` directory.

- what:

  Whether to extract chromatograms (`chroms`), DAD data (`dad`), peak
  tables (`peak_table`) and/or instrument traces (`instrument`), such as
  pump pressure, flow, solvent composition and temperature, read from
  `LCDIAG.REG`, and/or mass spectra (`MS1`, `MS2`) with the total ion
  chromatogram (`TIC`) and base peak chromatogram (`BPC`). Accepts
  multiple arguments, and defaults to all of these but `instrument`.
  Types the directory does not contain are left out, and it is an error
  if it contains none of them. With the default, mass spectrometry data
  that cannot be read are also left out, with a warning, if the
  directory holds other data. The `TIC` and `BPC` are read from the scan
  records, so they are returned even when the spectra cannot be.

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

A list with one element per type found, each a chromatogram or a list of
them named by file, in the format specified by `format_out` and
`data_format`. If `data_format` is `wide`, the chromatograms will be
returned with retention times as rows and columns containing signal
intensity for each signal. If `long` format is requested, retention
times will be in the first column. The `format_out` argument determines
whether the chromatogram is returned as a `matrix`, `data.frame` or
`data.table`. Peak tables are returned in the `chromatographr` layout
described in
[read_peaklist](https://ethanbass.github.io/chromConverter/reference/read_peaklist.md),
named by wavelength (e.g. `"254"`), and keep the report's name for the
signal as `signal_descriptor`. They are read from `Result.xml` where the
directory has one, since unlike `Report.TXT` it records where each peak
starts and ends. Mass spectra and the `BPC` are always in long format,
with `rt`, `mz` and `intensity` columns, and a `precursor_mz` column
before `mz` for `MS2`. They are returned as a `data.table` when
`format_out` is `matrix`. Each table of spectra has a `scan_info`
attribute with one row per scan, giving its `scan`, `rt`, `ms_level` and
`polarity`, and for `MS2` its `precursor_mz` and `collision_energy`.
Metadata are attached as
[attributes](https://rdrr.io/r/base/attributes.html) if `read_metadata`
is `TRUE`. With `collapse = TRUE`, a list of one element is replaced by
that element.

## Details

Instrument traces are named from `LCDIAG.REG`, so the names differ
between 'ChemStation' revisions and/or instruments (e.g.
`"PMP1, Pressure"` and `"PMP1, PMP1A, Pressure"`). Parts of the file
that cannot be read are skipped with a warning.

Mass spectra are read from the `AcqData` folder of a 'MassHunter' `.d`
directory. Its `MSScan.bin` and `MSScan.xsd` describe every scan, as in
[read_agilent_dx](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
and the spectra are in `MSPeak.bin`, or in `MSProfile.bin` for profiles.
Each profile starts with its first m/z and the step between points, as
two doubles or two 32-bit floats, followed by the intensities as 32-bit
floats or integers respectively. The points are spaced by the scan's
`SamplingPeriod` rather than the stored step, which can be slightly
smaller. Spectra can be read only from unit-resolution instruments, such
as single quadrupole GC-MS and LC-MS, and triple quadrupole LC-MS. `MS1`
holds the scans at MS level 1 and `MS2` the product ion and MRM scans,
each with the m/z it selected in the first quadrupole as `precursor_mz`.
Neutral loss and precursor ion scans are skipped with a warning. The
`TIC` and `BPC` come from the record of every scan, so they are also
returned for runs whose spectra cannot be read. For high-resolution
spectra, try `parser = "rainbow"` with `centroid` or `hrms`.

## See also

Other 'Agilent' parsers:
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
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
path <- system.file("extdata", "benzoxazinoid_standards", "BENZOS_250PPM.D",
                    package = "chromConverter")
run <- read_agilent_d(path)
names(run$chroms)
#> [1] "dad1A" "dad1B" "dad1C" "dad1D" "dad1E"
pump <- read_agilent_d(path, what = "instrument")
names(pump)
#> [1] "PMP1, Pressure"  "PMP1, Flow"      "PMP1, Solvent A" "PMP1, Solvent B"
#> [5] "PMP1, Solvent C" "PMP1, Solvent D"
```
