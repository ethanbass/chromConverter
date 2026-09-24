# `read_cdf` checks that need no external file: an ANDI MS file is written with
# `write_andi_ms` and read back. Every scan in it has the same `point_count`,
# which is the case the MS1 reader used to get wrong.

write_equal_count_ms <- function(dir, n_scan = 20, n_pt = 5){
  ms <- data.frame(rt = rep(seq_len(n_scan) * 0.1, each = n_pt),
                   mz = rep(seq_len(n_pt) + 99, times = n_scan),
                   intensity = seq_len(n_scan * n_pt))
  attr(ms, "data_format") <- "long"
  attr(ms, "time_unit") <- "Minutes"
  attr(ms, "detector") <- "MS"
  write_andi_ms(ms, path_out = dir, sample_name = "equal_counts", force = TRUE)
}

test_that("read_cdf reads MS1 scans of equal length", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- write_equal_count_ms(tmp)

  # every scan holds the same number of points
  nc <- ncdf4::nc_open(path)
  expect_equal(length(unique(ncdf4::ncvar_get(nc, "point_count"))), 1)
  ncdf4::nc_close(nc)

  # `unlist(sapply(...))` used to retain the `dim` that `sapply` adds in this
  # case, so the retention times came back as a matrix and `data.frame` split
  # them into one column per scan (`rt.1`, `rt.2`, ...)
  x <- read_cdf(path, format_out = "data.frame")
  expect_equal(colnames(x$MS1), c("rt", "mz", "intensity"))
  expect_equal(dim(x$MS1), c(100L, 3L))
  # ANDI MS records the scan time in seconds, so the round-trip scales by 60
  expect_equal(x$MS1$rt, rep(seq_len(20) * 0.1 * 60, each = 5),
               tolerance = 1e-6)
  expect_equal(x$MS1$mz, rep(100:104, times = 20), tolerance = 1e-6)
  expect_equal(x$MS1$intensity, seq_len(100), tolerance = 1e-6)

  # and `mapply` used to simplify the equally sized scans into one matrix,
  # which `c()` then spliced into a list of scalars
  y <- read_cdf(path, ms_format = "list")
  expect_type(y$MS1, "list")
  expect_equal(length(y$MS1), 20)
  expect_true(all(vapply(y$MS1, is.matrix, logical(1))))
  expect_equal(colnames(y$MS1[[1]]), c("mz", "int"))
  expect_equal(dim(y$MS1[[1]]), c(5L, 2L))
})

# An 'ANDI chrom' file shaped like the conformance file that ships with the AIA
# template (HERBIC.DAT): times in seconds, a peak table carrying `peak_name`,
# and the sampling interval declared alongside the length of the run.

write_andi_chrom_fixture <- function(dir, n = 10, delay = 0, interval = 1,
                                     uniform = TRUE,
                                     retention_unit = "time in seconds",
                                     converter_name = NULL){
  point_number <- ncdf4::ncdim_def("point_number", "", vals = seq_len(n),
                                   create_dimvar = FALSE)
  peak_number <- ncdf4::ncdim_def("peak_number", "", vals = 1:2,
                                  create_dimvar = FALSE)
  str32 <- ncdf4::ncdim_def("_32_byte_string", "", vals = seq_len(32),
                            create_dimvar = FALSE)
  vars <- list(
    ncdf4::ncvar_def("ordinate_values", "", dim = point_number),
    ncdf4::ncvar_def("raw_data_retention", "", dim = point_number),
    ncdf4::ncvar_def("actual_delay_time", "", list()),
    ncdf4::ncvar_def("actual_run_time_length", "", list()),
    ncdf4::ncvar_def("actual_sampling_interval", "", list()),
    ncdf4::ncvar_def("peak_retention_time", "", dim = peak_number),
    ncdf4::ncvar_def("peak_area", "", dim = peak_number),
    ncdf4::ncvar_def("peak_name", "", dim = list(str32, peak_number),
                     prec = "char")
  )
  path <- fs::path(dir, "fixture", ext = "cdf")
  nc <- ncdf4::nc_create(path, vars)
  on.exit(ncdf4::nc_close(nc))
  rt <- if (uniform) delay + (seq_len(n) - 1) * interval else delay + seq_len(n)^2
  ncdf4::ncvar_put(nc, "ordinate_values", seq_len(n) * 100)
  ncdf4::ncvar_put(nc, "raw_data_retention", rt)
  ncdf4::ncvar_put(nc, "actual_delay_time", delay)
  ncdf4::ncvar_put(nc, "actual_run_time_length", interval * n)
  ncdf4::ncvar_put(nc, "actual_sampling_interval", interval)
  ncdf4::ncvar_put(nc, "peak_retention_time", c(30, 120))
  ncdf4::ncvar_put(nc, "peak_area", c(10, 20))
  ncdf4::ncvar_put(nc, "peak_name", c("peak A", "peak B"))
  ncdf4::ncatt_put(nc, "ordinate_values", "uniform_sampling_flag",
                   ifelse(uniform, "Y", "N"))
  if (!is.null(retention_unit)){
    ncdf4::ncatt_put(nc, 0, "retention_unit", retention_unit)
  }
  if (!is.null(converter_name)){
    ncdf4::ncatt_put(nc, 0, "converter_name", converter_name)
  }
  path
}

test_that("read_cdf spaces ANDI chrom retention times by the sampling interval", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # `actual_run_time_length` is the length of the run, so spreading the points
  # up to it put them 10/9 s apart instead of 1 s
  path <- write_andi_chrom_fixture(tmp, n = 10, interval = 1)
  x <- read_cdf(path, format_out = "data.frame", data_format = "long")
  expect_equal(x$rt, (0:9)/60, tolerance = 1e-6)
})

test_that("read_cdf reads ANDI chrom times without a unit as seconds, unless
          chromConverter wrote them", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- write_andi_chrom_fixture(tmp, n = 10, retention_unit = NULL)
  x <- read_cdf(path, format_out = "data.frame", data_format = "long")
  expect_equal(x$rt, (0:9)/60, tolerance = 1e-6)

  unlink(path)
  path <- write_andi_chrom_fixture(tmp, n = 10, retention_unit = NULL,
                                   converter_name = "chromconverter")
  x <- read_cdf(path, format_out = "data.frame", data_format = "long")
  expect_equal(x$rt, 0:9, tolerance = 1e-6)
})

test_that("read_cdf uses `raw_data_retention` when sampling is not uniform", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- write_andi_chrom_fixture(tmp, n = 10, uniform = FALSE)
  x <- read_cdf(path, format_out = "data.frame", data_format = "long")
  expect_equal(x$rt, (seq_len(10)^2)/60, tolerance = 1e-6)
})

test_that("read_cdf converts the ANDI chrom peak table to minutes and keeps its
          character columns", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- write_andi_chrom_fixture(tmp)
  pt <- read_cdf(path, what = "peak_table")
  expect_setequal(names(pt), c("peak_retention_time", "peak_area", "peak_name"))
  expect_equal(pt$peak_retention_time, c(0.5, 2), tolerance = 1e-6)
  expect_equal(pt$peak_name, c("peak A", "peak B"))
  expect_equal(pt$peak_area, c(10, 20), tolerance = 1e-6)
})

test_that("write_andi_chrom round-trips unevenly spaced retention times", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  rt <- c(2, 2.1, 2.3, 2.6, 3, 3.5)
  x <- matrix(seq_along(rt), ncol = 1, dimnames = list(rt, "220"))
  attr(x, "data_format") <- "wide"
  attr(x, "time_unit") <- "Minutes"
  path <- write_andi_chrom(x, path_out = tmp, sample_name = "uneven",
                           force = TRUE)

  nc <- ncdf4::nc_open(path)
  flag <- ncdf4::ncatt_get(nc, "ordinate_values", "uniform_sampling_flag")$value
  # the run length is the length of the run, not the last retention time
  run_length <- ncdf4::ncvar_get(nc, "actual_run_time_length")
  delay <- ncdf4::ncvar_get(nc, "actual_delay_time")
  ncdf4::nc_close(nc)
  expect_equal(flag, "N")
  expect_equal(delay, 2, tolerance = 1e-6)
  expect_equal(run_length, mean(diff(rt)) * length(rt), tolerance = 1e-6)

  y <- read_cdf(path, format_out = "data.frame", data_format = "long")
  expect_equal(y$rt, rt, tolerance = 1e-6)
})
