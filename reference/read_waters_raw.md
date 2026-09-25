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

  Whether to return data in `wide` (default) or `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

## Value

A list of 2D chromatograms, one per trace and named by it, in the format
specified by `format_out` and `data_format`.

## Note

This parser reads only 2D chromatograms (retention time and intensity),
not mass spectra or DAD data.

## See also

Other 'Waters' parsers:
[`read_waters_arw()`](https://ethanbass.github.io/chromConverter/reference/read_waters_arw.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_waters_raw("path/to/file.raw")
} # }
```
