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
  expand = FALSE
)
```

## Arguments

- chrom_list:

  A list of chromatograms with attached metadata (as returned by
  `read_chroms` with `read_metadata = TRUE`), or a single chromatogram.
  Nested lists are flattened, one row per chromatogram.

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
  `ms_params` instrument settings, the `acaml_metadata` injection record
  that `read_agilent_rslt` reads from the `.acaml` file, or the whole
  vendor list that `metadata_format = "raw"` passes through. Either
  `TRUE`, to include every nested field the chromatograms carry, a
  character vector naming the ones to include, or `FALSE` (the default)
  to include none. Each element becomes a column of its own, named for
  itself (`SampleName`) unless that name is already taken, in which case
  it carries the field it came from (`ms_params.polarity`, since
  `polarity` is a metadata field in its own right).

## Value

A `data.frame`, `tibble`, or `data.table` (according to the value of
`format_out`), with one row per chromatogram and the specified metadata
elements as columns, or `NA` if none of the specified elements could be
found. For a list, the first column, `name`, identifies each
chromatogram by its path through the list (e.g. `blue.UV`).

## Examples

``` r
path <- system.file("extdata/ladder.txt", package = "chromConverter")
chroms <- read_chroms(path, format_in = "shimadzu_ascii",
                      find_files = FALSE, progress_bar = FALSE)
extract_metadata(chroms, what = c("sample_name", "instrument", "run_datetime"))
#>     name sample_name instrument        run_datetime
#> 1 ladder    FS19_214    GC-2014 2019-07-18 19:45:56
```
