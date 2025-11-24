# Write ANDI chrom CDF file from chromatogram

Exports a chromatogram in ANDI (Analytical Data Interchange)
chromatography format (ASTM E1947-98). This format can only accommodate
unidimensional data. For two-dimensional chromatograms, the column to
export can be specified using the `lambda` argument. Otherwise, a
warning will be generated and the first column of the chromatogram will
be exported.

## Usage

``` r
write_andi_chrom(x, path_out, sample_name = NULL, lambda = NULL, force = FALSE)
```

## Arguments

- x:

  A chromatogram in (wide) format.

- path_out:

  The path to write the file.

- sample_name:

  The name of the file. If a name is not provided, the name will be
  derived from the `sample_name` attribute.

- lambda:

  The wavelength to export (for 2-dimensional chromatograms). Must be a
  string matching one the columns in `x` or the index of the column to
  export.

- force:

  Whether to overwrite existing files at the specified path. Defaults to
  `FALSE`.

## Value

Invisibly returns the path to the written CDF file.

## Side effects

Exports a chromatogram in ANDI chromatography format (netCDF) in the
directory specified by `path_out`. The file will be named according to
the value of `sample_name`. If no `sample_name` is provided, the
`sample_name` attribute will be used if it exists.

## See also

Other write functions:
[`write_chroms()`](https://ethanbass.github.io/chromConverter/reference/write_chroms.md),
[`write_mzml()`](https://ethanbass.github.io/chromConverter/reference/write_mzml.md)

## Author

Ethan Bass
