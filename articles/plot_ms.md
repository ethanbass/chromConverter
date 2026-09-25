# Intro to plotting mass spectrometry data

## Loading example data

``` r

library(chromConverter)
library(ggplot2)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
```

Mass spectra are returned in `long` format (one row per scan–m/z pair)
with three columns: retention time, m/z, and intensity.

The examples use a ‘Varian’ SMS file from the `chromConverterExtraTests`
repository.

``` r

# download example Varian SMS file from the web
path_sms <- tempfile(fileext = ".sms")
download.file("https://raw.github.com/ethanbass/chromConverterExtraTests/master/inst/STRD15.SMS", 
              destfile = path_sms, mode = "wb")

dat <- read_chroms(path_sms, format_in = "varian_sms", format_out = "data.frame")
```

## Plot TIC and mass spectra using base R syntax

``` r

x <- dat[[1]]$MS1

# derive TIC using aggregate
tic <- aggregate(intensity ~ rt, data = x, FUN = sum)

# plot TIC
matplot(tic$rt, tic$intensity, type = 'l',
        ylab = "Total intensity", xlab = "Time (min)")
```

![Total ion chromatogram of the example Varian SMS file, summed
intensity plotted against retention time in
minutes.](plot_ms_files/figure-html/plot_tic_base-1.png)

A plot function for mass spectra, using base R graphics:

``` r

plot_spec <- function(spec, lab_int=0.2, digits=1){
  plot(spec, type = "h", xlab = "m/z", ylab = "Intensity")
  lab.idx <- which(spec$intensity > lab_int * max(spec$intensity))
  text(spec$mz[lab.idx], spec$intensity[lab.idx], round(spec$mz[lab.idx], 
                    digits), offset = 0.25, pos = 3, cex = 0.5)
}
```

Mass spectra can be extracted by filtering on the time column. For
example, to get the mass spectrum of the hundredth scan:

``` r

times <- unique(x$rt)
rt_spec <- times[100]
spec <- x[x$rt == rt_spec, -1]
plot_spec(spec)
```

![Mass spectrum of the hundredth scan, plotted as vertical lines from
m/z on the x axis to intensity on the y axis, with the most intense
peaks labeled by
m/z.](plot_ms_files/figure-html/plot_spectrum_base-1.png)

## Plot TIC and mass spectra using *dplyr* syntax

Plot TIC with dplyr:

``` r

tic <- x |> dplyr::group_by(rt) |> dplyr::summarize_at("intensity", sum)

plot(intensity ~ rt, data=tic, type = 'l',
        ylab = "Total intensity", xlab = "Time (min)")
```

![The same total ion chromatogram, computed with
dplyr.](plot_ms_files/figure-html/plot_tic_dplyr-1.png)

Plot spectrum with dplyr:

``` r

dplyr::filter(x, rt == rt_spec) |> 
  dplyr::select(mz, intensity) |> 
  plot_spec()
```

![The same mass spectrum, extracted with dplyr and drawn with
plot_spec.](plot_ms_files/figure-html/plot_spectrum_dplyr-1.png)

## Plot TIC and mass spectra using *data.table* syntax

Convert to `data.table`:

``` r

x <- data.table::as.data.table(x)
```

chromConverter can also return chromatograms in data.table format
directly:

``` r

dat <- read_chroms(path_sms, format_in = "varian_sms", format_out = "data.table")
```

Extract the total ion chromatogram:

``` r

tic <- x[, .(intensity = sum(intensity)), by = rt]
matplot(tic$rt, tic$intensity, type = 'l',
        ylab = "Total intensity", xlab = "Time (min)")
```

![The same total ion chromatogram, computed with
data.table.](plot_ms_files/figure-html/tic_dt-1.png)

Extract the base peak chromatogram:

``` r

bpc <- x[, .(intensity = max(intensity)), by = rt]
matplot(bpc$rt, bpc$intensity, type = 'l',
        ylab = "Maximum intensity", xlab = "Time (min)")
```

![Base peak chromatogram of the example Varian SMS file: the most
intense signal at each retention time, plotted against time in
minutes.](plot_ms_files/figure-html/bpc_dt-1.png)

To obtain a mass spectrum, filter by retention time as before:

``` r

plot_spec(x[rt == rt_spec, c('mz', 'intensity')])
```

![The same mass spectrum, extracted with
data.table.](plot_ms_files/figure-html/spectrum_dt-1.png)

## Plot TIC and mass spectra using *ggplot*

``` r

ggplot(data = tic, aes(x=rt, y=intensity)) + 
  geom_line() + 
  xlab("Retention time (min)") +
  ylab("Intensity")  +
  theme_minimal()
```

![The same total ion chromatogram, drawn with ggplot2 as a line in the
minimal theme.](plot_ms_files/figure-html/plot_tic_ggplot-1.png)

Plot mass spectrum with ggplot:

``` r

lab_int <- 0.2
digits <- 1
dplyr::filter(x, rt == rt_spec) |> 
  dplyr::select(mz, intensity) |> 
  ggplot(aes(x = mz, y = intensity)) +
  geom_segment(aes(xend = mz, yend = 0), linewidth = 0.5) +
  geom_text(data = subset(spec, intensity > lab_int * max(intensity)),
            aes(label = round(mz, digits)),
            vjust = -0.5, size = 2) +
  labs(x = "m/z", y = "Intensity") +
  theme_minimal()
```

![The same mass spectrum, drawn with ggplot2 as vertical segments from
zero to each intensity, with the most intense peaks labeled by
m/z.](plot_ms_files/figure-html/plot_spectrum_ggplot-1.png)

## Session Information

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] data.table_1.18.6.1   ggplot2_4.0.3         chromConverter_0.10.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10        generics_0.1.4     bitops_1.1-0       xml2_1.6.0        
#>  [5] stringi_1.8.9      lattice_0.22-9     digest_0.6.39      magrittr_2.0.5    
#>  [9] evaluate_1.0.5     grid_4.6.1         RColorBrewer_1.1-3 fastmap_1.2.0     
#> [13] cellranger_1.1.0   jsonlite_2.0.0     Matrix_1.7-5       purrr_1.2.2       
#> [17] scales_1.4.0       RaMS_1.4.3         pbapply_1.7-5      textshaping_1.0.5 
#> [21] jquerylib_0.1.4    cli_3.6.6          rlang_1.3.0        bit64_4.8.6       
#> [25] withr_3.0.3        base64enc_0.1-6    cachem_1.1.0       yaml_2.3.12       
#> [29] otel_0.2.0         parallel_4.6.1     tools_4.6.1        dplyr_1.2.1       
#> [33] reticulate_1.47.0  vctrs_0.7.3        R6_2.6.1           png_0.1-9         
#> [37] lifecycle_1.0.5    stringr_1.6.0      fs_2.1.0           bit_4.6.0         
#> [41] ragg_1.5.2         pkgconfig_2.0.3    desc_1.4.3         pkgdown_2.2.1     
#> [45] bslib_0.12.0       pillar_1.11.1      gtable_0.3.6       glue_1.8.1        
#> [49] Rcpp_1.1.2         systemfonts_1.3.2  tidyselect_1.2.1   tibble_3.3.1      
#> [53] xfun_0.61          knitr_1.52         farver_2.1.2       htmltools_0.5.9   
#> [57] labeling_0.4.3     rmarkdown_2.32     compiler_4.6.1     entab_0.3.1       
#> [61] S7_0.2.2           readxl_1.5.0.1
```
