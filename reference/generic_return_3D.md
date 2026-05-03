# Generic return (3D)

Generic return (3D)

## Value

A 3D chromatogram in the format specified by `data_format` and
`format_out`. If `data_format` is `wide`, the chromatogram will be
returned with retention times as rows and wavelengths as columns. If
`long` format is requested, three columns will be returned: one for the
retention time, one for the wavelength and one for the intensity. The
`format_out` argument determines whether the chromatogram is returned as
a `matrix`, `data.frame`, or `data.table`. Metadata will be attached to
the chromatogram as [attributes](https://rdrr.io/r/base/attributes.html)
if `read_metadata` is `TRUE`.
