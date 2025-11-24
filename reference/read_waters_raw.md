# Read 'Waters' RAW

Reads 'Waters MassLynx' (`.raw`) files into R.

## Usage

``` r
read_waters_raw(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw")
)
```

## Arguments

- path:

  Path to Waters `.raw` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

## Value

A chromatogram in the format specified by `format_out` (retention time x
wavelength).

## Note

For now this parser only reads 1D chromatograms (not mass spectra or DAD
data) and does not support parsing of metadata from 'Waters' RAW files.

## See also

Other 'Waters' parsers:
[`read_waters_arw()`](https://ethanbass.github.io/chromConverter/reference/read_waters_arw.md)

## Author

Ethan Bass
