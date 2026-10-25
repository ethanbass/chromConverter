# Read TIC from 'Shimadzu' single quadrupole stream

Reads the total ion current of every scan from `Mass Raw Data/TIC Data`
(uint64, see `read_sz_sq`). Each acquisition event is returned as a
separate chromatogram, named `Event 1`, `Event 2` and so on.

## Usage

``` r
read_sz_sq_tic(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = "shimadzu_lcd"
)
```

## Arguments

- path:

  Path to `.lcd` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata.

## Value

A named list of chromatograms, one per acquisition event.

## Author

Ethan Bass
