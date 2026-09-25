# Read 'Shimadzu' QGD files

Reads 'Shimadzu GCMSsolution' `.qgd` GC-MS data files.

## Usage

``` r
read_shimadzu_qgd(
  path,
  what = c("MS1", "TIC"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
```

## Arguments

- path:

  Path to 'Shimadzu' `.qgd` file.

- what:

  What stream to get: current options are `MS1` and/or `TIC`. If a
  stream is not specified, the function will return both streams.

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

A list holding the `MS1` spectra and the `TIC`, or the one requested
where `collapse` is `TRUE`. The TIC is returned in the format specified
by `data_format` and `format_out`. If `data_format` is `wide`, it is
returned with retention times as rows and a single column for the
intensity. If `long` format is requested, two columns are returned: one
for the retention time and one for the intensity. The `format_out`
argument determines whether it is returned as a `matrix`, `data.frame`,
or `data.table`. The spectra are always long, with columns `scan`, `rt`,
`mz` and `intensity`, and a `matrix` resolves to a `data.table`.
Metadata are attached as
[attributes](https://rdrr.io/r/base/attributes.html) if `read_metadata`
is `TRUE`.

## Details

The MS data is stored in the `GCMS Raw Data` storage, which contains an
`MS Raw Data` stream with MS scans, a `TIC Data` stream containing the
total ion chromatogram, and a `Retention Time` stream containing the
retention times. All known values are little-endian. The retention time
stream is an array of 4-byte integers giving the time of each scan in
milliseconds, and the TIC stream an array of 8-byte integers, one per
retention time.

The MS Raw Data stream holds one block per scan, located through the
`Spectrum Index` stream. Each block begins with a 32-byte header:

- scan number (4-byte integer)

- retention time in milliseconds (4-byte integer)

- unknown (12 bytes)

- number of bytes in each intensity value (2-byte integer)

- number of m/z values in the block (2-byte integer)

- unknown (8 bytes)

After the header, each m/z value is followed by its intensity. The m/z
values are 2-byte integers holding m/z times 20. Intensities are
unsigned integers of the width given in the header, except that the top
bit of a 4-byte intensity is dropped.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md),
[`read_sz_tables()`](https://ethanbass.github.io/chromConverter/reference/read_sz_tables.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_shimadzu_qgd("path/to/file.qgd")
} # }
```
