# Read 'Waters' ASCII (.arw)

Reads 'Waters' ASCII `.arw` files.

## Usage

``` r
read_waters_arw(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw")
)
```

## Arguments

- path:

  Path to Waters `.arw` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `wide` or `long` format.

- read_metadata:

  Whether to read metadata from file.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

## Value

A chromatogram in the format specified by `format_out` (retention time x
wavelength).

## Details

For help exporting files from Empower, you can consult the official
documentation:
[How_to_export_3D_raw_data_from_Empower](https://support.waters.com/KB_Inf/Empower_Breeze/WKB77571_How_to_export_3D_raw_data_from_Empower_to_a_Microsoft_Excel_spreadsheet).

## See also

Other 'Waters' parsers:
[`read_waters_raw()`](https://ethanbass.github.io/chromConverter/reference/read_waters_raw.md)

## Author

Ethan Bass
