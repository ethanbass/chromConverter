# Read CDF

Reads 'Analytical Data Interchange' (ANDI) netCDF (`.cdf`) files.

## Usage

``` r
read_cdf(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  what = NULL,
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE,
  ...
)
```

## Arguments

- path:

  Path to ANDI netCDF file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html).

- data_format:

  Whether to return data in `wide` or `long` format. For 2D files,
  "long" format returns the retention time as the first column of the
  data.frame or matrix while "wide" format returns the retention time as
  the rownames of the object. This argument applies only to 2D
  chromatograms, since MS data will always be returned in long format.

- what:

  For `ANDI chrom` files, whether to extract `chroms` and/or
  `peak_table`. For `ANDI ms` files, whether to extract MS1 scans
  (`MS1`) or the total ion chromatogram (`TIC`).

- read_metadata:

  Whether to read metadata from file.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

- ...:

  Additional arguments to parser. The `ms_format` argument can be used
  here to specify whether to return mass spectra in `list` format or as
  a `data.frame`.

## Value

A chromatogram in the format specified by the `format_out` and
`data_format` arguments.

## Author

Ethan Bass
