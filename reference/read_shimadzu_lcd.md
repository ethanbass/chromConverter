# Read 'Shimadzu' LCD

Read PDA, chromatogram, mass spectrometry and peak table streams from
'Shimadzu' `.lcd` files.

## Usage

``` r
read_shimadzu_lcd(
  path,
  what,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  scale = TRUE,
  sparse = TRUE,
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Shimadzu' `.lcd` file.

- what:

  What stream to get: current options are `DAD` (for which `PDA` is
  accepted as a synonym, since that is what 'Shimadzu' calls the same
  detector), chromatograms (`chroms`), `TIC`, mass spectra (`MS1`,
  `MS2`, or `MS` for both), and/or peak lists (`peak_table`). `MS1` and
  `MS2` count stages of mass selection rather than name a scan type, so
  a triple quadrupole acquisition can land on either: an MRM scan
  selects a precursor in Q1 (the first quadrupole) and a product in Q3
  (the third), so it is `MS2`, while a SIM scan sets Q1 and Q3 to the
  same mass, selecting nothing after the collision cell, and so is
  `MS1`.

  If a stream is not specified, the richest one the file contains is
  returned: `DAD` if there is a PDA stream, otherwise `chroms`, and
  otherwise `MS` for a file whose only detector is the mass
  spectrometer. The mass spectrometry streams are read from whichever
  container the file uses: `QTFL RawData` (centroided quadrupole
  time-of-flight data) or `TLM Raw Data` (triple quadrupole full scan,
  product-ion scan, MRM and SIM data).

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- scale:

  Whether to scale the data by the scaling factor present in the file.
  Defaults to `TRUE`.

- sparse:

  Logical. Whether to return mass spectra in sparse format (excluding
  zeros), as
  [`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md)
  does. Defaults to `TRUE`. Applies only to triple quadrupole profile
  spectra, whose m/z grid is largely empty; ignored for every other
  stream.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A named list with one element per stream requested, or the element
itself where there is only one and `collapse` is `TRUE`. Chromatograms
and the PDA data are returned in the format specified by `data_format`
and `format_out`. In `wide` format, retention times are the rows, and
each 2D chromatogram has a single intensity column while the PDA data
have one column per wavelength. If `long` format is requested, the
retention time and the intensity are returned as columns, alongside the
detector, channel, wavelength and unit for a 2D chromatogram. The
`format_out` argument determines whether they are returned in `matrix`,
`data.frame`, or `data.table` format. Mass spectra and peak tables are
always long. Metadata are attached as
[attributes](https://rdrr.io/r/base/attributes.html) when
`read_metadata` is `TRUE`.

## Details

A parser to read data from 'Shimadzu' `.lcd` files. LCD files are
encoded as 'Microsoft' OLE documents. The parser relies on the
[olefile](https://pypi.org/project/olefile/) package in Python to unpack
the files. Each detector writes its own storage, and this function
dispatches over them:

- **PDA** (`PDA 3D Raw Data:3D Raw Data`), requested as `DAD`: one
  delta-encoded segment per retention time, each holding a full
  spectrum. Read by
  [`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md),
  which documents the segment header and the delta encoding.

- **Chromatograms** (`LSS Raw Data:Chromatogram Ch<#>`): one stream per
  channel, delta-encoded in the same way. Read by
  [`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md).

- **Quadrupole time-of-flight mass spectra** (`QTFL RawData`):
  centroided scans stored as a scan header, a block of flight times and
  a block of intensities, with the mass axis reconstructed from the
  calibration table in the file. Read by
  [`read_sz_qtof()`](https://ethanbass.github.io/chromConverter/reference/read_sz_qtof.md),
  which documents the scan header, the flight-time conversion and the
  intensity scaling. The total ion current is held separately, in
  `Centroid SumTIC`
  ([`read_sz_qtof_tic()`](https://ethanbass.github.io/chromConverter/reference/read_sz_qtof_tic.md)).

- **Triple quadrupole mass spectra** (`TLM Raw Data`): zlib-compressed
  scan records located through a spectrum index, covering full scan,
  product-ion scan, MRM and SIM acquisitions. Read by
  [`read_sz_tlm()`](https://ethanbass.github.io/chromConverter/reference/read_sz_tlm.md),
  which documents the index, the scan header and the layout of each scan
  type. The total ion current is again held separately, in `TIC Data`.

- **Peak tables** (`Peak Table`): integration results as reported by
  'LabSolutions', one stream per channel. Read by
  [`read_sz_tables()`](https://ethanbass.github.io/chromConverter/reference/read_sz_tables.md),
  which documents the two record layouts.

The two mass spectrometry containers are mutually exclusive: a file
holds one or the other, according to the instrument that wrote it.

A mass spectrometry run is divided into **acquisition events**: the scan
functions defined by the method, each with its own polarity, MS level
and mass range, which the instrument cycles through as the run proceeds.
The mass spectrometry streams are reported per event — `TIC` returns one
chromatogram for each, and the `scan_info` attribute of a table of
spectra names the event every scan came from.

## Note

Times are stored as a 'Windows' `FILETIME`, which is always UTC, so
`run_datetime` is reported in UTC. The files also record the offset of
the local time zone (e.g. `+01'00'`), but this is the standard offset of
the zone rather than the offset that was in force, and it seems that no
daylight saving information is stored anywhere in the file. The local
times displayed by 'LabSolutions' therefore cannot be reconstructed from
the recorded offset alone: where daylight saving time applied, they are
an hour ahead of it. Rendering `run_datetime` in the zone where the data
were acquired, e.g.
`format(attr(x, "run_datetime"), tz = "Europe/Paris")`, recovers them
exactly.

As of `v0.10.0`, 2D chromatograms are scaled by the calibration factor
and the value factor recorded for each channel, so their intensities
match those reported by 'LabSolutions'. An absorbance axis can be
reported in `uAU`, `mAU` or `AU`, and the file records the size of each
as a **value factor**: `1`, `1000` and `1e6`, since one `mAU` is a
thousand `uAU` and one `AU` a million. The smallest of them is the
**base unit**, while 'LabSolutions' displays the data in whichever unit
the method selected, usually `mAU`. Other detectors work the same way: a
refractive index axis measures in `nRI` and displays `uRI`.

Two factors separate the stored integers from the displayed value. The
calibration factor converts an integer into base units, and the value
factor converts base units into the displayed unit; an intensity as
'LabSolutions' reports it is the integer times the one divided by the
other. The calibration factor is `1` on some channels and not on others
(e.g. about `0.024` for an SPD-20A and `0.0032` for an RID-10A), which
suggests the integers are detector counts whose size varies by module,
though the file does not say so.

For a 2D chromatogram, `scale = TRUE` applies both factors.
`scale = FALSE` returns the stored integers. Where the calibration
factor is `1`, their `detector_y_unit` is the base unit (e.g. `uAU`),
and the values and the attribute agree. Where it is not, the integers
are in no unit we can name, so `detector_y_unit` is `NA`; the `scaled`
attribute records which of the two was returned.

PDA data is instead returned as it is encoded in the file, matching the
`[PDA 3D]` section of a 'LabSolutions' ASCII export, which declares no
intensity unit or multiplier. The `3D Data Item` describes the
absorbance axis in `mAU` with a value factor of `1000`, which would
imply scaling the values by `0.001`, but the `[PDA Multi Chromatogram]`
traces in the ASCII export, which are extracted from the same data,
report values on the same scale as the raw data with a multiplier of
`1`. Until this can be resolved, PDA data is left unscaled and the
`scale` argument is ignored.

`Max Plot` is the maximum absorbance over the wavelength range at each
point in time, so the value at one time can come from a different
wavelength than the value at the next, and no single wavelength
describes the trace. It is derived from the PDA data but is read as a 2D
chromatogram, so it is scaled and currently differs from the PDA data by
a factor of `1000`. The wavelength range it was taken over is not
recorded in the `2D Data Item`, whose nanometer axis spans `0` to `0`,
so its `wavelength` attribute is `NA` rather than the acquisition range
of the PDA stream.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md),
[`read_sz_tables()`](https://ethanbass.github.io/chromConverter/reference/read_sz_tables.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_shimadzu_lcd(path)
} # }
```
