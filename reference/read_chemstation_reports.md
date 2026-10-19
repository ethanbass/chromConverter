# Read 'Agilent ChemStation' report files

Reads peak tables from 'Agilent ChemStation' `Report.TXT` files, or from
the `Result.xml` files 'ChemStation' writes when set up to export its
results. Only `Result.xml` records where each peak starts and ends, so
it is read in preference to a `Report.TXT` in the same directory.

## Usage

``` r
read_chemstation_reports(
  paths,
  peaktable_format = c("chromatographr", "original"),
  metadata_format = c("chromconverter", "raw"),
  data_format = NULL
)
```

## Arguments

- paths:

  Paths to 'ChemStation' `Report.TXT` or `Result.xml` files.

- peaktable_format:

  Whether to return peak tables in the standard `chromatographr` layout
  (the default) or with all the columns the file provides (`original`).
  A `chromatographr` table has the columns `rt`, `start`, `end`, `area`,
  `height` and `FWHM` (the full width at half maximum), with times in
  the same unit as the format's chromatograms and `NA` where the file
  does not record a value.

- metadata_format:

  Format to output metadata. Either `chromconverter` or `raw`.

- data_format:

  Deprecated. Use `peaktable_format` instead.

## Value

A `peak_list`: a list with one element per report, named by its `.D`
directory, each holding one peak table per signal, named by wavelength.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_logs()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_logs.md),
[`read_chemstation_method()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_method.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
path <- system.file("extdata", "benzoxazinoid_standards", "BENZOS_250PPM.D",
                    "Report.TXT", package = "chromConverter")
read_chemstation_reports(path)
#> $BENZOS_250PPM
#> $BENZOS_250PPM$`254`
#>           sample lambda     rt start end        area    height   FWHM
#> 1  BENZOS_250PPM    254  1.948    NA  NA   140.95924   7.07890 0.2483
#> 2  BENZOS_250PPM    254  2.373    NA  NA   161.74519  14.59622 0.1433
#> 3  BENZOS_250PPM    254  2.517    NA  NA   108.11900  16.77651 0.0912
#> 4  BENZOS_250PPM    254  2.669    NA  NA   102.54382  21.65732 0.0752
#> 5  BENZOS_250PPM    254  2.769    NA  NA   389.25211  31.26716 0.1612
#> 6  BENZOS_250PPM    254  3.898    NA  NA  1211.76770  18.42384 0.7973
#> 7  BENZOS_250PPM    254  5.063    NA  NA   382.35251  11.64398 0.4211
#> 8  BENZOS_250PPM    254  5.319    NA  NA   160.09975  10.35239 0.2244
#> 9  BENZOS_250PPM    254  5.615    NA  NA   192.40610   9.96399 0.2633
#> 10 BENZOS_250PPM    254  6.108    NA  NA   335.43140   9.27820 0.4398
#> 11 BENZOS_250PPM    254  6.621    NA  NA   283.18048   7.90439 0.4360
#> 12 BENZOS_250PPM    254  7.223    NA  NA   158.82758   6.81299 0.3060
#> 13 BENZOS_250PPM    254  7.916    NA  NA   356.50873   5.75635 0.7946
#> 14 BENZOS_250PPM    254 12.541    NA  NA  7078.15283 349.55142 0.3142
#> 15 BENZOS_250PPM    254 14.203    NA  NA   136.62770   2.82697 0.6119
#> 16 BENZOS_250PPM    254 17.754    NA  NA    78.47887   2.87611 0.4023
#> 17 BENZOS_250PPM    254 19.068    NA  NA 10954.20000 456.22156 0.3721
#> 18 BENZOS_250PPM    254 20.979    NA  NA  1806.64539  76.05057 0.3652
#> 19 BENZOS_250PPM    254 28.225    NA  NA   637.83038  27.49332 0.3587
#> 20 BENZOS_250PPM    254 36.643    NA  NA    61.25155   1.62191 0.5246
#> 
#> $BENZOS_250PPM$`320`
#>           sample lambda     rt start end      area   height   FWHM
#> 1  BENZOS_250PPM    320  2.370    NA  NA  96.84458 11.83100 0.1115
#> 2  BENZOS_250PPM    320  2.502    NA  NA 107.63147 16.68902 0.0992
#> 3  BENZOS_250PPM    320  2.664    NA  NA 103.81037 22.40874 0.0740
#> 4  BENZOS_250PPM    320  2.763    NA  NA 248.60339 29.45256 0.1165
#> 5  BENZOS_250PPM    320  3.881    NA  NA 289.27115  6.79315 0.5888
#> 6  BENZOS_250PPM    320  5.066    NA  NA 132.71829  5.38092 0.3278
#> 7  BENZOS_250PPM    320  5.602    NA  NA  59.05860  3.18974 0.2620
#> 8  BENZOS_250PPM    320 12.508    NA  NA  25.50664  1.10318 0.3397
#> 9  BENZOS_250PPM    320 14.201    NA  NA 142.02574  3.56716 0.5565
#> 10 BENZOS_250PPM    320 19.066    NA  NA 195.75963  8.10717 0.3617
#> 
#> $BENZOS_250PPM$`360`
#>           sample lambda     rt start end       area   height   FWHM
#> 1  BENZOS_250PPM    360  0.516    NA  NA   96.44804  2.89953 0.4541
#> 2  BENZOS_250PPM    360  2.369    NA  NA  574.00183 20.06062 0.3498
#> 3  BENZOS_250PPM    360  2.500    NA  NA  131.14888 20.44745 0.0968
#> 4  BENZOS_250PPM    360  2.664    NA  NA  126.34191 26.98949 0.0746
#> 5  BENZOS_250PPM    360  2.760    NA  NA  392.27814 37.21136 0.1390
#> 6  BENZOS_250PPM    360  3.868    NA  NA 1322.81958 20.69460 0.8197
#> 7  BENZOS_250PPM    360  5.070    NA  NA  409.06174 12.16724 0.4322
#> 8  BENZOS_250PPM    360  5.600    NA  NA  334.84705 10.31378 0.4008
#> 9  BENZOS_250PPM    360  6.622    NA  NA  510.97223  7.16129 0.8464
#> 10 BENZOS_250PPM    360 14.216    NA  NA  117.29733  2.52298 0.6144
#> 
#> $BENZOS_250PPM$`210`
#>           sample lambda     rt start end        area      height   FWHM
#> 1  BENZOS_250PPM    210  1.950    NA  NA    65.69608    5.745110 0.1454
#> 2  BENZOS_250PPM    210  2.133    NA  NA    84.84812    7.271900 0.1521
#> 3  BENZOS_250PPM    210  2.372    NA  NA    70.46403    7.173740 0.1306
#> 4  BENZOS_250PPM    210  2.528    NA  NA    52.72128    8.513370 0.0882
#> 5  BENZOS_250PPM    210  2.668    NA  NA    51.72952   11.215840 0.0738
#> 6  BENZOS_250PPM    210  2.769    NA  NA   165.68953   16.845010 0.1308
#> 7  BENZOS_250PPM    210  3.855    NA  NA   291.84818    8.058200 0.4505
#> 8  BENZOS_250PPM    210  4.133    NA  NA   114.18034    7.664050 0.1978
#> 9  BENZOS_250PPM    210  5.061    NA  NA   169.26221   13.544000 0.1858
#> 10 BENZOS_250PPM    210  5.601    NA  NA    54.17995    4.207650 0.1942
#> 11 BENZOS_250PPM    210  6.061    NA  NA   288.09384    7.110250 0.5607
#> 12 BENZOS_250PPM    210  7.882    NA  NA   244.28961    5.905900 0.5226
#> 13 BENZOS_250PPM    210  9.119    NA  NA   399.80270    7.620720 0.6681
#> 14 BENZOS_250PPM    210  9.591    NA  NA   178.11578    7.189960 0.3410
#> 15 BENZOS_250PPM    210 10.153    NA  NA   394.09256    6.719370 0.7363
#> 16 BENZOS_250PPM    210 11.895    NA  NA   171.44041    6.975670 0.3467
#> 17 BENZOS_250PPM    210 12.541    NA  NA 19569.60000  952.719240 0.3196
#> 18 BENZOS_250PPM    210 14.184    NA  NA   595.64838   16.776040 0.5262
#> 19 BENZOS_250PPM    210 15.338    NA  NA    27.37215    1.095760 0.3234
#> 20 BENZOS_250PPM    210 17.748    NA  NA   160.59375    6.482190 0.3970
#> 21 BENZOS_250PPM    210 19.069    NA  NA 29204.10000 1202.623540 0.3793
#> 22 BENZOS_250PPM    210 20.978    NA  NA 13595.20000  578.575740 0.3621
#> 23 BENZOS_250PPM    210 24.907    NA  NA    42.35772    0.962402 0.5695
#> 24 BENZOS_250PPM    210 28.225    NA  NA 13109.10000  560.928890 0.3647
#> 25 BENZOS_250PPM    210 36.622    NA  NA    91.77909    3.710720 0.3766
#> 
#> $BENZOS_250PPM$`230`
#>           sample lambda     rt start end        area    height   FWHM
#> 1  BENZOS_250PPM    230  0.488    NA  NA    68.80820   1.56110 0.5542
#> 2  BENZOS_250PPM    230  1.050    NA  NA   220.37210   4.62333 0.5681
#> 3  BENZOS_250PPM    230  2.130    NA  NA   276.37167  11.37646 0.3034
#> 4  BENZOS_250PPM    230  2.375    NA  NA   143.48412  13.19617 0.1429
#> 5  BENZOS_250PPM    230  2.524    NA  NA    93.12847  13.96627 0.1111
#> 6  BENZOS_250PPM    230  2.670    NA  NA    76.98081  16.60342 0.0741
#> 7  BENZOS_250PPM    230  2.772    NA  NA   376.92337  25.19552 0.1905
#> 8  BENZOS_250PPM    230  3.901    NA  NA  1044.86316  15.04501 0.8662
#> 9  BENZOS_250PPM    230  5.063    NA  NA   342.35373  11.03793 0.4019
#> 10 BENZOS_250PPM    230  5.600    NA  NA   268.37524   9.23586 0.3790
#> 11 BENZOS_250PPM    230  6.079    NA  NA   583.56812  10.26005 0.6996
#> 12 BENZOS_250PPM    230  7.892    NA  NA   671.10590   8.78248 0.9595
#> 13 BENZOS_250PPM    230  9.124    NA  NA   548.67072   8.08717 0.8355
#> 14 BENZOS_250PPM    230  9.593    NA  NA   217.67584   7.66411 0.3794
#> 15 BENZOS_250PPM    230 10.164    NA  NA   386.70090   7.08682 0.6846
#> 16 BENZOS_250PPM    230 11.278    NA  NA   142.06198   5.90577 0.3126
#> 17 BENZOS_250PPM    230 12.541    NA  NA  4126.13525 181.33946 0.3455
#> 18 BENZOS_250PPM    230 14.202    NA  NA   438.22104   6.59593 0.8711
#> 19 BENZOS_250PPM    230 15.471    NA  NA    82.90569   1.72794 0.5798
#> 20 BENZOS_250PPM    230 17.752    NA  NA    40.14959   1.41113 0.3679
#> 21 BENZOS_250PPM    230 19.068    NA  NA  5640.98242 235.13190 0.3759
#> 22 BENZOS_250PPM    230 20.978    NA  NA 10423.30000 449.60464 0.3585
#> 23 BENZOS_250PPM    230 28.225    NA  NA 11725.60000 505.16583 0.3628
#> 24 BENZOS_250PPM    230 36.660    NA  NA    54.50668   1.01376 0.6673
#> 
#> attr(,"instrument")
#> [1] "Instrument 1"
#> attr(,"detector")
#> [1] NA
#> attr(,"software")
#> [1] NA
#> attr(,"method")
#> [1] "ETHAN_DT_MEOH_L16-6.M"
#> attr(,"batch")
#> [1] NA
#> attr(,"operator")
#> [1] "Mario"
#> attr(,"run_datetime")
#> [1] "2023-06-21 16:46:40 UTC"
#> attr(,"sample_name")
#> [1] "benzos_250ppm"
#> attr(,"sample_injection_volume")
#> [1] 15
#> attr(,"sample_amount")
#> [1] NA
#> attr(,"time_range")
#> [1] NA
#> attr(,"time_interval")
#> [1] NA
#> attr(,"time_unit")
#> [1] NA
#> attr(,"detector_range")
#> [1] NA
#> attr(,"detector_y_unit")
#> [1] NA
#> attr(,"sample_injection_volume_unit")
#> [1] "µl"
#> attr(,"source_file")
#> [1] "/home/runner/work/_temp/Library/chromConverter/extdata/benzoxazinoid_standards/BENZOS_250PPM.D/Report.TXT"
#> attr(,"source_file_format")
#> [1] "chemstation_peaklist"
#> attr(,"source_sha1")
#> [1] "a896810f0154fa1183beedc4684f2b56ee00caca"
#> attr(,"format_out")
#> [1] "data.frame"
#> attr(,"data_format")
#> [1] "chromatographr"
#> attr(,"parser")
#> [1] NA
#> 
#> attr(,"chrom_list")
#> [1] NA
#> attr(,"lambdas")
#> [1] "254" "320" "360" "210" "230"
#> attr(,"fit")
#> [1] "chemstation"
#> attr(,"sd.max")
#> [1] NA
#> attr(,"max.iter")
#> [1] NA
#> attr(,"time.units")
#> [1] "min"
#> attr(,"class")
#> [1] "peak_list"
```
