# `export_cdf` used to take no `...`, so the arguments that only the underlying
# writers accept -- `ms_params` for `write_andi_ms` and `lambda` for
# `write_andi_chrom` -- could not be reached through `write_chroms` at all.

make_ms <- function(n_scan = 10, n_pt = 4){
  ms <- data.frame(rt = rep(seq_len(n_scan) * 0.1, each = n_pt),
                   mz = rep(seq_len(n_pt) + 99, times = n_scan),
                   intensity = seq_len(n_scan * n_pt))
  attr(ms, "data_format") <- "long"
  attr(ms, "time_unit") <- "Minutes"
  attr(ms, "detector") <- "MS"
  ms
}

test_that("write_chroms forwards `...` to `write_andi_ms`", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  x <- list(sample_a = make_ms(), sample_b = make_ms())
  write_chroms(x, path_out = tmp, export_format = "cdf", what = "MS1",
               force = TRUE, show_progress = FALSE,
               ms_params = list(ionization_mode = "Chemical Ionization",
                                ionization_polarity = "Negative Polarity",
                                detector_type = "Electron Multiplier"))
  expect_setequal(list.files(tmp), c("sample_a.cdf", "sample_b.cdf"))

  nc <- ncdf4::nc_open(fs::path(tmp, "sample_a", ext = "cdf"))
  meta <- ncdf4::ncatt_get(nc, 0)
  ncdf4::nc_close(nc)
  expect_equal(meta$test_ionization_mode, "Chemical Ionization")
  expect_equal(meta$test_ionization_polarity, "Negative Polarity")
  expect_equal(meta$test_detector_type, "Electron Multiplier")
})

test_that("write_chroms uses the `write_andi_ms` defaults when `...` is empty", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  write_chroms(list(sample_a = make_ms()), path_out = tmp,
               export_format = "cdf", what = "MS1", force = TRUE,
               show_progress = FALSE)

  nc <- ncdf4::nc_open(fs::path(tmp, "sample_a", ext = "cdf"))
  meta <- ncdf4::ncatt_get(nc, 0)
  ncdf4::nc_close(nc)
  expect_equal(meta$test_ionization_mode, "Electron Impact")
  expect_equal(meta$test_ionization_polarity, "Positive Polarity")
})

test_that("write_chroms forwards `...` to `write_andi_chrom`", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- system.file("extdata/ladder.txt", package = "chromConverter")
  x <- read_chroms(path, format_in = "shimadzu_ascii", find_files = FALSE,
                   progress_bar = FALSE)
  # `lambda` selects the column to export; a one-column chromatogram only has
  # the one, so this asserts the argument arrives rather than what it picks
  expect_silent(write_chroms(x, path_out = tmp, export_format = "cdf",
                             what = "chrom", force = TRUE,
                             show_progress = FALSE, lambda = 1))
  expect_equal(length(list.files(tmp, pattern = "\\.cdf$")), 1L)
})

test_that("write_chroms writes mzML with its default `what`", {
  skip_on_cran()

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # `write_chroms` defaults to `what = ""`, which `export_cdf` reads as "not
  # specified". Passed through verbatim it reached the `match.arg` in
  # `write_mzml` and was rejected there, so every file failed.
  x <- list(sample_a = list(MS1 = make_ms()))
  expect_silent(write_chroms(x, path_out = tmp, export_format = "mzml",
                             force = TRUE, show_progress = FALSE))
  expect_equal(list.files(tmp), "sample_a.mzML")

  # naming the stream explicitly still works
  tmp2 <- tempfile()
  dir.create(tmp2)
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)
  expect_silent(write_chroms(x, path_out = tmp2, export_format = "mzml",
                             what = "MS1", force = TRUE,
                             show_progress = FALSE))
  expect_equal(list.files(tmp2), "sample_a.mzML")
})

make_chrom <- function(n = 20, time_unit = "Minutes"){
  x <- matrix(seq_len(n) * 10, ncol = 1,
              dimnames = list(seq_len(n) * 0.5, "intensity"))
  attr(x, "data_format") <- "wide"
  attr(x, "sample_name") <- "chrom_a"
  if (!is.null(time_unit)) attr(x, "time_unit") <- time_unit
  x
}

test_that("write_andi_chrom always declares a `retention_unit`", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  unit <- function(x){
    p <- write_andi_chrom(x, path_out = tmp, force = TRUE)
    nc <- ncdf4::nc_open(p)
    on.exit(ncdf4::nc_close(nc))
    ncdf4::ncatt_get(nc, 0)$retention_unit
  }
  expect_equal(unit(make_chrom(time_unit = "Seconds")), "Seconds")
  expect_equal(unit(make_chrom(time_unit = "min")), "Minutes")
  # the unit is mandatory, so an absent or unrecognized `time_unit` has to fall
  # back to one rather than writing an empty string
  expect_equal(unit(make_chrom(time_unit = NULL)), "Minutes")
  expect_equal(unit(make_chrom(time_unit = "ms")), "Minutes")
})

test_that("write_andi_chrom records the range of the data it wrote", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  x <- make_chrom()
  x[1] <- -5
  p <- write_andi_chrom(x, path_out = tmp, force = TRUE)
  nc <- ncdf4::nc_open(p)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_equal(ncdf4::ncvar_get(nc, "detector_maximum_value"), max(x[, 1]))
  expect_equal(ncdf4::ncvar_get(nc, "detector_minimum_value"), -5)
})

test_that("a chromatogram round-trips through ANDI chrom in minutes", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  x <- make_chrom()
  p <- write_andi_chrom(x, path_out = tmp, force = TRUE)
  y <- read_cdf(p, format_out = "matrix", what = "chroms")
  expect_equal(as.numeric(rownames(y)), as.numeric(rownames(x)))
  expect_equal(attr(y, "time_unit"), "Minutes")
})

test_that("a one-point chromatogram round-trips through ANDI chrom", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # one point has no interval to average and no range to take
  x <- make_chrom(n = 1)
  p <- write_andi_chrom(x, path_out = tmp, force = TRUE)
  y <- read_cdf(p, format_out = "matrix", what = "chroms")
  expect_equal(as.numeric(rownames(y)), 0.5)
  expect_equal(as.numeric(y), 10)
})

test_that("read_andi_chrom converts the times to minutes", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # a file that declares seconds, as the ANDI chrom template does and as all
  # but one of its conformance files do
  x <- make_chrom(time_unit = "Seconds")
  p <- write_andi_chrom(x, path_out = tmp, force = TRUE)
  y <- read_cdf(p, format_out = "matrix", what = "chroms")
  expect_equal(as.numeric(rownames(y)), as.numeric(rownames(x)) / 60)
  expect_equal(attr(y, "time_unit"), "Minutes")

  # `ncatt_get` reports a missing attribute as `0`, which must not be read as
  # a unit called "0". Two of the ANDI chrom conformance files declare no
  # `retention_unit`, and `write_andi_chrom` always writes one, so the file has
  # to be built here.
  p2 <- fs::path(tmp, "no_unit", ext = "cdf")
  pt <- ncdf4::ncdim_def("point_number", "", vals = seq_len(nrow(x)),
                         create_dimvar = FALSE)
  vars <- c(list(ncdf4::ncvar_def("ordinate_values", "", dim = pt)),
            lapply(c("actual_delay_time", "actual_run_time_length",
                     "actual_sampling_interval"),
                   function(v) ncdf4::ncvar_def(v, "", list())))
  nc <- ncdf4::nc_create(p2, vars)
  ncdf4::ncvar_put(nc, "ordinate_values", x[, 1])
  ncdf4::ncvar_put(nc, "actual_delay_time", as.numeric(rownames(x))[1])
  ncdf4::ncvar_put(nc, "actual_run_time_length", max(as.numeric(rownames(x))))
  ncdf4::ncvar_put(nc, "actual_sampling_interval", 0.5)
  ncdf4::nc_close(nc)

  expect_no_warning(
    z <- read_cdf(p2, format_out = "matrix", what = "chroms",
                  read_metadata = FALSE)
  )
  expect_equal(as.numeric(rownames(z)), as.numeric(rownames(x)) / 60)
})
