# Read the metadata of chromatograms

Reads the metadata of chromatogram files into a table, without returning
the chromatograms. It takes the same paths and formats as
[read_chroms](https://ethanbass.github.io/chromConverter/reference/read_chroms.md)
and gives the table that
[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md)
would give for the chromatograms `read_chroms` returns.

## Usage

``` r
read_metadata(
  paths,
  format_in = supported_formats(),
  find_files,
  pattern = NULL,
  parser = c("", "chromconverter", "aston", "entab", "thermoraw", "openchrom", "rainbow"),
  what = chrom_metadata_fields(),
  format_out = c("data.frame", "data.table", "tibble"),
  metadata_format = c("chromconverter", "raw"),
  collapse = FALSE,
  expand = FALSE,
  digits = NULL,
  progress_bar,
  cl = 1,
  verbose = getOption("verbose"),
  sample_names = c("basename", "sample_name"),
  sort_by = c("auto", "none", "acquisition_time", "file_time"),
  ...
)
```

## Arguments

- paths:

  Paths to data files or directories containing the files.

- format_in:

  Format of the files to be imported or converted. One of: `agilent_d`,
  `agilent_dx` (or `openlab_dx`), `agilent_rslt` (or `rslt`, `sirslt`),
  `asm` (or `allotrope`), `cdf` (or `andi`), `chemstation_ch` (or
  `chemstation_fid`), `chemstation_csv`, `chemstation_ms`,
  `chemstation_uv`, `chromatotec`, `chromeleon_uv`, `csd`, `csv`,
  `masshunter_dad`, `mdf`, `msd`, `mzml`, `mzxml`, `other`,
  `shimadzu_ascii`, `shimadzu_dad`, `shimadzu_fid`, `shimadzu_gcd`,
  `shimadzu_lcd`, `shimadzu_qgd`, `thermoraw`, `varian_sms`,
  `waters_arw`, `waters_raw`, `wsd`. A name in parentheses is an alias,
  which behaves exactly like the format it follows. Version-specific
  names for the 'Agilent ChemStation' formats (`chemstation_130`, for
  instance) are accepted as well, but are normally supplied by
  chromConverter's own file-type detection rather than being provided by
  the user.

- find_files:

  Logical. Whether to treat `paths` as directories to search for data
  files. Inferred from `paths` if not supplied: anything that is not a
  file is searched as a directory, except for the formats that are
  themselves directories (e.g. 'Agilent' `.d`), which are recognized by
  their extension.

- pattern:

  Regular expression that file names must match (e.g. a file extension).
  Defaults to `NULL`, in which case the extension is deduced from
  `format_in`.

- parser:

  What parser to use (optional). Current options are `chromconverter`,
  `aston`, `entab`, `thermoraw`, `openchrom`, `rainbow`.

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

- format_out:

  Class of the table. Either `data.frame`, `data.table` or `tibble`.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

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

- digits:

  Number of significant digits for the numbers in a field collapsed into
  a string (see `collapse`), or `NULL` (the default) to keep them in
  full. Numeric fields spread across columns are returned as numbers
  either way.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if `pbapply`
  is installed.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of parallel workers to use or a cluster object
  created by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html).
  Defaults to `1`.

- verbose:

  Logical. Whether to print status messages, and the output of external
  parsers, to the R console.

- sample_names:

  Which sample names to use. Options are `basename` to use the filename
  (default) or `sample_name` to use the sample name encoded in the file
  metadata. A sample with no `sample_name`, or with conflicting ones, is
  named for its file with a warning. Several files in one 'Agilent' `.D`
  directory that would share a name also get their file name, e.g.
  `RUTIN_2.dad1A`.

- sort_by:

  How to sort the chromatograms: `auto` (default) sorts files by
  acquisition time unless `paths` lists the files explicitly or any
  acquisition time is missing; `none` keeps files in the order given, or
  in alphabetical order if `find_files = TRUE`; `acquisition_time` sorts
  by the acquisition time recorded in each file (`run_datetime`);
  `file_time` sorts by the time when each file was last modified.

- ...:

  Additional arguments to the parser. Arguments that a format's metadata
  reader does not take are ignored.

## Value

A `data.frame`, `data.table` or `tibble` with one row per sample and one
column per metadata field, as described in
[extract_metadata](https://ethanbass.github.io/chromConverter/reference/extract_metadata.md).
A table with no rows if none of the files could be read.

## Details

The formats that are slow to read in full are read without decoding the
data: 'Agilent' `.uv`, 'Chromeleon', 'OpenLab' `.dx`, 'Shimadzu' `.lcd`,
`.gcd`, `.qgd` and ASCII, 'Thermo' RAW, 'Varian' SMS and 'Waters' `.arw`
files, and ANDI (`.cdf`), mzML and mzXML. 'OpenLab' 131 `.uv` files are
the exception: their scaling is worked out from the data, so they are
decoded in full. Other formats are read in full and their metadata
extracted.

The 'OpenLab' `.dx` and 'Shimadzu' formats report only the fields that
describe the whole file, leaving out those of its traces, such as
`detector` or `wavelength`. 'Thermo' RAW files leave out the fields that
come from the mzML they are converted to, such as `detector_range`.

If `parser` is not given, each format is read with the parser that can
skip the data, which for `.uv` files is `chromconverter` even where
'Entab' is installed.

## Author

Ethan Bass

## Examples

``` r
path <- system.file("extdata", "benzoxazinoid_standards",
                    package = "chromConverter")
read_metadata(path, format_in = "chemstation_ch", pattern = "dad1A",
              parser = "chromconverter", progress_bar = FALSE,
              what = c("sample_name", "run_datetime", "method"))
#>               name     sample_name        run_datetime                method
#> 1             MEOH            MEOH 2023-06-15 16:46:23 ETHAN_DT_MEOH_L16-6.M
#> 2   BENZOS_1000PPM  benzos_1000ppm 2023-06-20 19:29:41 ETHAN_DT_MEOH_L16-6.M
#> 3    BENZOS_500PPM   benzos_500ppm 2023-06-21 15:23:43 ETHAN_DT_MEOH_L16-6.M
#> 4    BENZOS_250PPM   benzos_250ppm 2023-06-21 16:46:40 ETHAN_DT_MEOH_L16-6.M
#> 5    BENZOS_125PPM   benzos_125ppm 2023-06-21 18:02:48 ETHAN_DT_MEOH_L16-6.M
#> 6   BENZOS_62,5PPM  benzos_62,5ppm 2023-06-21 19:19:00 ETHAN_DT_MEOH_L16-6.M
#> 7  BENZOS_31,25PPM benzos_31,25ppm 2023-06-21 20:35:12 ETHAN_DT_MEOH_L16-6.M
#> 8     BENZOS_16PPM    benzos_16ppm 2023-06-21 21:51:21 ETHAN_DT_MEOH_L16-6.M
#> 9      BENZOS_8PPM     benzos_8ppm 2023-06-21 23:07:28 ETHAN_DT_MEOH_L16-6.M
#> 10     BENZOS_4PPM     benzos_4ppm 2023-06-22 00:23:39 ETHAN_DT_MEOH_L16-6.M
```
