# Extract metadata

Extract metadata as a `data.frame`, `data.table` or `tibble` from a list
of chromatograms.

## Usage

``` r
extract_metadata(
  chrom_list,
  what = chrom_metadata_fields(),
  detector = NULL,
  format_out = c("data.frame", "data.table", "tibble"),
  collapse = FALSE,
  expand = FALSE,
  by = c("sample", "chromatogram"),
  digits = NULL
)
```

## Arguments

- chrom_list:

  A list of chromatograms with attached metadata (as returned by
  `read_chroms` with `read_metadata = TRUE`), or a single chromatogram.

- what:

  A character vector specifying the metadata elements to extract.
  Defaults to every field chromConverter attaches; no format records all
  of them, so the elements a format does not provide are absent from the
  result. Superseded names (e.g. `injection_volume`, `software_name`,
  `time_start`) are accepted and mapped to the names that replaced them.
  An element of a nested field (see `expand`) may be named too, either
  by the column it is reported under (`SampleLabel`) or in full
  (`acaml_metadata.SampleLabel`), to report it without the rest of its
  field. A field requested by name that no chromatogram carries produces
  a warning.

- detector:

  A character vector of detectors to include (e.g. `"UV"` or
  `c("UV", "MS")`), matched case-insensitively against each
  chromatogram's `detector` attribute. Defaults to `NULL`, in which case
  all chromatograms are included. Useful for lists containing more than
  one detector per sample. It is an error if no chromatogram matches.

- format_out:

  Format of object. Either `data.frame`, `data.table` or `tibble`.

- collapse:

  Logical. Whether to collapse a field holding more than one value
  (`time_range`, or the `product_mz` of an MRM event monitoring several
  transitions) into a single comma-separated string. Defaults to
  `FALSE`, in which case such a field is spread across numbered columns
  (`time_range1`, `time_range2`).

- expand:

  Whether to include the nested metadata fields, whose value is itself a
  list or table rather than a single value per chromatogram: the
  `ms_params` and `method_params` instrument settings, the
  `acaml_metadata` injection record that `read_agilent_rslt` reads from
  the `.acaml` file, or the whole vendor list that
  `metadata_format = "raw"` passes through. Either `TRUE`, to include
  every nested field the chromatograms carry, a character vector naming
  the ones to include, or `FALSE` (the default) to include none. Each
  element becomes a column of its own, named for itself (`SampleName`)
  unless that name is already taken, in which case it carries the field
  it came from (`ms_params.polarity`, since `polarity` is a metadata
  field in its own right).

- by:

  Whether to return one row per `sample` (the default), that is per
  element of `chrom_list`, or one row per `chromatogram`. The two differ
  only for nested lists, such as the traces
  [read_agilent_d](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md)
  returns for each `.D` directory. With `by = "sample"`, each field
  holds the value that a sample's chromatograms agree on, ignoring those
  that leave it empty, and is `NA` where they disagree; with the default
  `what`, fields that are then empty for every sample are left out.
  Fields that belong to each trace, such as `detector` or `source_file`,
  need `by = "chromatogram"`.

- digits:

  Number of significant digits for the numbers in a field collapsed into
  a string (see `collapse`), or `NULL` (the default) to keep them in
  full. Numeric fields spread across columns are returned as numbers
  either way.

## Value

A `data.frame`, `tibble`, or `data.table` (according to the value of
`format_out`), with one row per sample or chromatogram (see `by`) and
the specified metadata elements as columns, or `NA` if none of the
specified elements could be found. For a list, the first column, `name`,
identifies each row: by the sample's name, or with `by = "chromatogram"`
by the chromatogram's path through the list (e.g. `blue.UV`).

## Examples

``` r
path <- system.file("extdata", "benzoxazinoid_standards",
                    package = "chromConverter")
chroms <- read_chroms(path, format_in = "chemstation_ch", pattern = "dad1A",
                      parser = "chromconverter", progress_bar = FALSE)
extract_metadata(chroms, what = c("sample_name", "run_datetime", "time_range"))
#>               name     sample_name        run_datetime time_range1 time_range2
#> 1             MEOH            MEOH 2023-06-15 16:46:23 -0.03716667    59.95617
#> 2   BENZOS_1000PPM  benzos_1000ppm 2023-06-20 19:29:41 -0.04333333    59.95667
#> 3    BENZOS_500PPM   benzos_500ppm 2023-06-21 15:23:43 -0.04233333    59.95767
#> 4    BENZOS_250PPM   benzos_250ppm 2023-06-21 16:46:40 -0.03783333    59.95550
#> 5    BENZOS_125PPM   benzos_125ppm 2023-06-21 18:02:48 -0.04166667    59.95833
#> 6   BENZOS_62,5PPM  benzos_62,5ppm 2023-06-21 19:19:00 -0.04016667    59.95983
#> 7  BENZOS_31,25PPM benzos_31,25ppm 2023-06-21 20:35:12 -0.03716667    59.95617
#> 8     BENZOS_16PPM    benzos_16ppm 2023-06-21 21:51:21 -0.03933333    59.96067
#> 9      BENZOS_8PPM     benzos_8ppm 2023-06-21 23:07:28 -0.04050000    59.95950
#> 10     BENZOS_4PPM     benzos_4ppm 2023-06-22 00:23:39 -0.03933333    59.96067
```
