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

  Matrix or data.frame.

- data_format:

  Either `wide` (default) or `long`. This argument applies only to TIC
  and BPC data, since MS data will always be returned in long format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A 2D chromatogram from the chromatogram stream in `matrix`,
`data.frame`, or `data.table` format, according to the value of
`format_out`. The chromatograms will be returned in `wide` or `long`
format according to the value of `data_format`.

A chromatogram or list of chromatograms in the format specified by
`data_format` and `format_out`. If `data_format` is `wide`, the
chromatogram(s) will be returned with retention times as rows and a
single column for the intensity. If `long` format is requested, two
columns will be returned: one for the retention time and one for the
intensity. The `format_out` argument determines whether chromatograms
are returned as a `matrix`, `data.frame`, or `data.table`. Metadata can
be attached to the chromatogram as
[`attributes`](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.

## Details

The MS data is stored in the "GCMS Raw Data" storage, which contains a
`MS Raw Data` stream with MS scans, a `TIC Data` stream containing the
total ion chromatogram, and a `Retention Time` stream containing the
retention times. All known values are little-endian. The retention time
stream is a simple array of 4-byte integers. The TIC stream is a simple
array of 8-byte integers corresponding to retention times stored in the
retention time stream. The MS Raw Data stream is blocked by retention
time. Each block begins with a header consisting of the following
elements:

- scan number (4-byte integer)

- retention time (4-byte integer)

- unknown (12-bytes)

- number of bytes in intensity values (2-byte integer)

- unknown (8-bytes)

After the header, the rest of the block consists of an array of mz
values and intensities. The mz values are encoded as 2-byte integers
where each mz value is scaled by a factor of 20. Intensities are encoded
as (unsigned) integers with variable byte-length defined by the value in
the header.

## Note

This parser is experimental and may still need some work. It is not yet
able to interpret much metadata from the files.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md)

## Author

Ethan Bass
