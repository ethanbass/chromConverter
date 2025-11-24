# Extract metadata

Extract metadata as a `data.frame` or `tibble` from a list of
chromatograms.

## Usage

``` r
extract_metadata(
  chrom_list,
  what = c("instrument", "detector", "detector_id", "software", "method", "batch",
    "operator", "run_datetime", "sample_name", "sample_id", "injection_volume",
    "time_range", "time_interval", "time_unit", "detector_range", "detector_y_unit",
    "detector_x_unit", "intensity_multiplier", "scaled", "source_file",
    "source_file_format", "source_sha1", "data_format", "parser", "format_out"),
  format_out = c("data.frame", "data.table", "tibble")
)
```

## Arguments

- chrom_list:

  A list of chromatograms with attached metadata (as returned by
  `read_chroms` with `read_metadata = TRUE`).

- what:

  A character vector specifying the metadata elements to extract.

- format_out:

  Format of object. Either `data.frame`, `tibble`, or `data.table`.

## Value

A `data.frame`, `tibble`, or `data.table` (according to the value of
`format_out`), with samples as rows and the specified metadata elements
as columns.
