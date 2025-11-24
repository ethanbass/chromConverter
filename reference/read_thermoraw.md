# Read ThermoRaw

Converts ThermoRawFiles to `mzML` by calling the
[ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser)
from the command-line.

## Usage

``` r
read_thermoraw(
  path,
  path_out = NULL,
  format_out = c("matrix", "data.frame"),
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
  specified, a temp directory will be used.

- format_out:

  R format. Either `matrix` or `data.frame`.

- read_metadata:

  Whether to read metadata from file.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- verbose:

  Logical. Whether to print output from OpenChrom to the console.

## Value

A chromatogram in the format specified by `format_out`.

## Details

To use this function, the ThermoRawFileParser must be manually
installed.

## Side effects

Exports chromatograms in `mzML` format to the folder specified by
`path_out`.

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
