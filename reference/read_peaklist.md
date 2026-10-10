# Read peak lists

Reads peak lists from specified folders or vector of paths.

## Usage

``` r
read_peaklist(
  paths,
  find_files,
  format_in = c("chemstation", "shimadzu_fid", "shimadzu_dad", "shimadzu_lcd",
    "shimadzu_gcd", "chromatotec", "asm"),
  pattern = NULL,
  peaktable_format = c("chromatographr", "original"),
  metadata_format = c("chromconverter", "raw"),
  read_metadata = TRUE,
  progress_bar,
  cl = 1,
  sort_by = c("auto", "none", "acquisition_time", "file_time"),
  data_format = NULL
)
```

## Arguments

- paths:

  Paths to files or folders containing peak list files.

- find_files:

  Logical. Whether to treat the supplied paths as directories to search
  for files. Inferred if not supplied, by testing whether every path is
  a file.

- format_in:

  Format of files to be imported/converted. One of `chemstation` (the
  default), `shimadzu_fid`, `shimadzu_dad`, `shimadzu_lcd`,
  `shimadzu_gcd`, `chromatotec`, or `asm`.

- pattern:

  A pattern (e.g. a file extension). Defaults to `NULL`, in which case
  the file extension will be deduced from `format_in`.

- peaktable_format:

  Whether to return peak tables in `chromatographr` or `original`
  format. 'Chromatotec' peak tables are always returned in their
  original format.

- metadata_format:

  Format to output metadata. Either `chromconverter` (standardized field
  names) or `raw` (vendor field names, unmapped).

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- progress_bar:

  Logical. Whether to show progress bar. Defaults to `TRUE` if `pbapply`
  is installed.

- cl:

  Argument to
  [pbapply](https://peter.solymos.org/pbapply/reference/pbapply.html)
  specifying the number of parallel workers to use or a cluster object
  created by [makeCluster](https://rdrr.io/r/parallel/makeCluster.html)
  (a set of parallel R worker processes). Defaults to `1`.

- sort_by:

  How to sort the samples: `auto` (default) sorts files by acquisition
  time unless `paths` lists the files explicitly or any acquisition time
  is missing; `none` keeps files in the order given, or in alphabetical
  order if `find_files = TRUE`; `acquisition_time` sorts by the
  acquisition time recorded in each file (`run_datetime`); `file_time`
  sorts by the time when each file was last modified.

- data_format:

  Deprecated. Use `peaktable_format` instead.

## Value

A `peak_list`: a list with one element per sample, holding its peak
table, or a list of peak tables where the file records more than one.
The tables are named by wavelength (e.g. `"254"`) where the file records
one, and otherwise by the file's own name for the signal; a table
identical to another at the same wavelength is a copy and is left out.
Each row is a peak. Every table starts with a `sample` column and a
`lambda` column giving the signal's wavelength, which is `NA` for a
detector without one or a file that does not record it.

## Author

Ethan Bass

## Examples

``` r
path <- system.file("extdata", "benzoxazinoid_standards",
                    package = "chromConverter")
peak_list <- read_peaklist(path, progress_bar = FALSE)
names(peak_list)
#>  [1] "MEOH"            "BENZOS_1000PPM"  "BENZOS_500PPM"   "BENZOS_250PPM"  
#>  [5] "BENZOS_125PPM"   "BENZOS_62,5PPM"  "BENZOS_31,25PPM" "BENZOS_16PPM"   
#>  [9] "BENZOS_8PPM"     "BENZOS_4PPM"    
peak_list[["BENZOS_250PPM"]][["254"]]
#>           sample lambda     rt  width        area    height type
#> 1  BENZOS_250PPM    254  1.948 0.2483   140.95924   7.07890   BB
#> 2  BENZOS_250PPM    254  2.373 0.1433   161.74519  14.59622   BV
#> 3  BENZOS_250PPM    254  2.517 0.0912   108.11900  16.77651   VV
#> 4  BENZOS_250PPM    254  2.669 0.0752   102.54382  21.65732   VV
#> 5  BENZOS_250PPM    254  2.769 0.1612   389.25211  31.26716   VB
#> 6  BENZOS_250PPM    254  3.898 0.7973  1211.76770  18.42384   BV
#> 7  BENZOS_250PPM    254  5.063 0.4211   382.35251  11.64398   VV
#> 8  BENZOS_250PPM    254  5.319 0.2244   160.09975  10.35239   VV
#> 9  BENZOS_250PPM    254  5.615 0.2633   192.40610   9.96399   VV
#> 10 BENZOS_250PPM    254  6.108 0.4398   335.43140   9.27820   VV
#> 11 BENZOS_250PPM    254  6.621 0.4360   283.18048   7.90439   VB
#> 12 BENZOS_250PPM    254  7.223 0.3060   158.82758   6.81299   BV
#> 13 BENZOS_250PPM    254  7.916 0.7946   356.50873   5.75635   VV
#> 14 BENZOS_250PPM    254 12.541 0.3142  7078.15283 349.55142   VB
#> 15 BENZOS_250PPM    254 14.203 0.6119   136.62770   2.82697   BV
#> 16 BENZOS_250PPM    254 17.754 0.4023    78.47887   2.87611   BV
#> 17 BENZOS_250PPM    254 19.068 0.3721 10954.20000 456.22156   VB
#> 18 BENZOS_250PPM    254 20.979 0.3652  1806.64539  76.05057   BB
#> 19 BENZOS_250PPM    254 28.225 0.3587   637.83038  27.49332   BB
#> 20 BENZOS_250PPM    254 36.643 0.5246    61.25155   1.62191   BB
```
