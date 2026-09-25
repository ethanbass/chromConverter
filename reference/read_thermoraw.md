# Read ThermoRaw

Converts a 'Thermo' `.raw` file to `mzML` with the
[ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser)
and reads the result with
[read_mzml](https://ethanbass.github.io/chromConverter/reference/read_mzml.md).

## Usage

``` r
read_thermoraw(
  path,
  path_out = NULL,
  format_out = c("matrix", "data.frame", "data.table"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  verbose = getOption("verbose")
)
```

## Arguments

- path:

  Path to 'Thermo' `.raw` file.

- path_out:

  Path to directory to export `mzML` files. If `path_out` isn't
  specified, a temporary directory is used and deleted afterwards.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- verbose:

  Logical. Whether to print output from ThermoRawFileParser to the
  console. Ignored on Windows, where the output is always printed.

## Value

A list of chromatograms, as returned by
[read_mzml](https://ethanbass.github.io/chromConverter/reference/read_mzml.md),
in the format specified by `format_out`.

## Details

The ThermoRawFileParser must be installed separately.

## Side effects

If `path_out` is specified, the `mzML` file (and, if `read_metadata` is
`TRUE`, a metadata `.txt` file) is left there.

## References

Hulstaert Niels, Jim Shofstahl, Timo Sachsenberg, Mathias Walzer, Harald
Barsnes, Lennart Martens, and Yasset Perez-Riverol. ThermoRawFileParser:
Modular, Scalable, and Cross-Platform RAW File Conversion. *Journal of
Proteome Research* **19**, no. 1 (January 3, 2020): 537–42.
[doi:10.1021/acs.jproteome.9b00328](https://doi.org/10.1021/acs.jproteome.9b00328)
.

## See also

Other external parsers:
[`call_entab()`](https://ethanbass.github.io/chromConverter/reference/call_entab.md),
[`call_openchrom()`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md),
[`call_rainbow()`](https://ethanbass.github.io/chromConverter/reference/call_rainbow.md),
[`sp_converter()`](https://ethanbass.github.io/chromConverter/reference/sp_converter.md),
[`uv_converter()`](https://ethanbass.github.io/chromConverter/reference/uv_converter.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_thermoraw(path)
} # }
```
