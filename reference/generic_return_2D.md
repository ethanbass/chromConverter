# Generic return (2D)

Generic return (2D)

## Value

A 2D chromatogram in the format specified by `data_format` and
`format_out`. If `data_format` is `wide`, the chromatogram will be
returned with retention times as rows and a single column for the
intensity. If `long` format is requested, two columns will be returned:
one for the retention time and one for the intensity. The `format_out`
argument determines whether the chromatogram is returned as a `matrix`,
`data.frame`, or `data.table`. Metadata can be attached to the
chromatogram as [attributes](https://rdrr.io/r/base/attributes.html) if
`read_metadata` is `TRUE`.
