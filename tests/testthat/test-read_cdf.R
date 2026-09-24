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
  # the file records the scan time in seconds; `read_cdf` reports minutes
  expect_equal(x$MS1$rt, rep(seq_len(20) * 0.1, each = 5), tolerance = 1e-6)
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

# An 'ANDI MS' file in which each scan carries flagged peaks. The toolkit that
# accompanies the specification writes the flags onto the end of each scan's
# points, so the arrays are longer than the points they describe.

write_flagged_ms_fixture <- function(dir, n_scan = 3, n_pt = 4, n_flag = 2,
                                     with_times = TRUE){
  point_number <- ncdf4::ncdim_def("point_number", "",
                                   vals = seq_len(n_scan * (n_pt + n_flag)),
                                   create_dimvar = FALSE)
  scan_number <- ncdf4::ncdim_def("scan_number", "", vals = seq_len(n_scan),
                                  create_dimvar = FALSE)
  vars <- list(
    ncdf4::ncvar_def("mass_values", "", dim = point_number),
    ncdf4::ncvar_def("intensity_values", "", dim = point_number),
    ncdf4::ncvar_def("scan_index", "", dim = scan_number, prec = "integer"),
    ncdf4::ncvar_def("point_count", "", dim = scan_number, prec = "integer"),
    ncdf4::ncvar_def("flag_count", "", dim = scan_number, prec = "integer")
  )
  if (with_times){
    vars <- c(vars, list(
      ncdf4::ncvar_def("scan_acquisition_time", "", dim = scan_number,
                       prec = "double"),
      ncdf4::ncvar_def("total_intensity", "", dim = scan_number,
                       prec = "double")))
  }
  path <- fs::path(dir, "flagged", ext = "cdf")
  nc <- ncdf4::nc_create(path, vars)
  on.exit(ncdf4::nc_close(nc))
  # the flagged entries carry values no real point has, so they are obvious
  # if they leak into the spectra
  scans <- lapply(seq_len(n_scan), function(i){
    list(mz = c(100 + seq_len(n_pt), rep(-1, n_flag)),
         int = c(seq_len(n_pt) * i, rep(-2, n_flag)))
  })
  ncdf4::ncvar_put(nc, "mass_values", unlist(lapply(scans, `[[`, "mz")))
  ncdf4::ncvar_put(nc, "intensity_values", unlist(lapply(scans, `[[`, "int")))
  ncdf4::ncvar_put(nc, "scan_index", (seq_len(n_scan) - 1) * (n_pt + n_flag))
  ncdf4::ncvar_put(nc, "point_count", rep(n_pt, n_scan))
  ncdf4::ncvar_put(nc, "flag_count", rep(n_flag, n_scan))
  if (with_times){
    ncdf4::ncvar_put(nc, "scan_acquisition_time", seq_len(n_scan) * 60)
    ncdf4::ncvar_put(nc, "total_intensity", seq_len(n_scan) * 10)
  }
  path
}

test_that("read_cdf leaves flagged peaks out of ANDI MS scans", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- write_flagged_ms_fixture(tmp)
  x <- read_cdf(path, format_out = "data.frame", what = "MS1")
  expect_equal(nrow(x), 12)
  expect_equal(x$mz, rep(101:104, times = 3), tolerance = 1e-6)
  expect_equal(x$intensity, c(1:4, (1:4) * 2, (1:4) * 3), tolerance = 1e-6)
  expect_equal(x$rt, rep(1:3, each = 4), tolerance = 1e-6)

  y <- read_cdf(path, what = "MS1", ms_format = "list")
  expect_equal(length(y), 3)
  expect_true(all(vapply(y, nrow, numeric(1)) == 4))
  expect_false(any(vapply(y, function(s) any(s[, "mz"] < 0), logical(1))))
})

test_that("read_cdf reads ANDI MS files that record no scan times", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # library spectra have no retention times, so there is no chromatogram
  path <- write_flagged_ms_fixture(tmp, with_times = FALSE)
  expect_warning(x <- read_cdf(path, format_out = "data.frame"),
                 "scan acquisition times")
  expect_equal(nrow(x), 12)
  expect_true(all(is.na(x$rt)))
  expect_error(read_cdf(path, what = "TIC"), "scan acquisition times")
})

test_that("write_andi_ms writes the scan elements the specification defines", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- write_equal_count_ms(tmp, n_scan = 20, n_pt = 5)
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE, after = FALSE)

  expect_true("flag_count" %in% names(nc$var))
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "flag_count")), rep(0, 20))
  # the mass range is the range of each scan, not a fixed 0-1000
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "mass_range_min")), rep(100, 20))
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "mass_range_max")), rep(104, 20))
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "time_range_min")),
               as.numeric(ncdf4::ncvar_get(nc, "scan_acquisition_time")))

  meta <- ncdf4::ncatt_get(nc, 0)
  expect_equal(meta$experiment_type, "Centroided Mass Spectrum")
  expect_equal(meta$raw_data_mass_units, "M/Z")
  expect_false(any(grepl("factor", names(meta))))
})

test_that("write_andi_ms takes the instrument settings from the data", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  ms <- data.frame(rt = rep((1:4) * 0.1, each = 3), mz = rep(c(100, 200, 300), 4),
                   intensity = 1:12)
  attr(ms, "data_format") <- "long"
  attr(ms, "time_unit") <- "Minutes"
  attr(ms, "detector") <- "MS"
  attr(ms, "polarity") <- "negative"
  attr(ms, "scan_type") <- "MRM"
  attr(ms, "instrument") <- "LCMS8040"
  attr(ms, "detector_model") <- "LCMS-3030"
  attr(ms, "software_version") <- "5.65"

  path <- write_andi_ms(ms, path_out = tmp, sample_name = "wired", force = TRUE)
  nc <- ncdf4::nc_open(path)
  meta <- ncdf4::ncatt_get(nc, 0)
  expect_equal(meta$test_ionization_polarity, "Negative Polarity")
  expect_equal(meta$test_scan_function, "Selected Ion Detection")
  expect_equal(as.character(ncdf4::ncvar_get(nc, "instrument_name")), "LCMS8040")
  expect_equal(as.character(ncdf4::ncvar_get(nc, "instrument_model")), "LCMS-3030")
  expect_equal(as.character(ncdf4::ncvar_get(nc, "instrument_sw_version")), "5.65")
  ncdf4::nc_close(nc)

  # `ms_params` wins over the data
  path <- write_andi_ms(ms, path_out = tmp, sample_name = "given", force = TRUE,
                        ms_params = list(ionization_polarity = "Positive Polarity",
                                         scan_function = "Other"))
  nc <- ncdf4::nc_open(path)
  meta <- ncdf4::ncatt_get(nc, 0)
  expect_equal(meta$test_ionization_polarity, "Positive Polarity")
  expect_equal(meta$test_scan_function, "Other")
  # the settings that were not given keep their defaults
  expect_equal(meta$test_ionization_mode, "Electron Impact")
  expect_equal(meta$test_detector_type, "Electron Multiplier")
  ncdf4::nc_close(nc)

  # data that records none of them
  for (a in c("polarity", "scan_type", "instrument", "detector_model",
              "software_version")){
    attr(ms, a) <- NULL
  }
  path <- write_andi_ms(ms, path_out = tmp, sample_name = "bare", force = TRUE)
  nc <- ncdf4::nc_open(path)
  meta <- ncdf4::ncatt_get(nc, 0)
  expect_equal(meta$test_ionization_polarity, "Positive Polarity")
  expect_false("test_scan_function" %in% names(meta))
  expect_equal(as.character(ncdf4::ncvar_get(nc, "instrument_name")), "")
  ncdf4::nc_close(nc)
})

test_that("write_andi_ms records the intensity unit where read_cdf finds it", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  ms <- data.frame(rt = rep((1:4) * 0.1, each = 3), mz = rep(c(100, 200, 300), 4),
                   intensity = 1:12)
  attr(ms, "data_format") <- "long"
  attr(ms, "time_unit") <- "Minutes"
  attr(ms, "detector") <- "MS"

  units <- c(counts = "Total Counts", cps = "Counts Per Second",
             V = "Volts", mAU = "Arbitrary Intensity Units")
  for (u in names(units)){
    attr(ms, "detector_y_unit") <- u
    path <- write_andi_ms(ms, path_out = tmp, sample_name = u, force = TRUE)
    nc <- ncdf4::nc_open(path)
    expect_equal(ncdf4::ncatt_get(nc, "intensity_values", "units")$value,
                 units[[u]])
    expect_equal(ncdf4::ncatt_get(nc, "total_intensity", "units")$value,
                 units[[u]])
    ncdf4::nc_close(nc)
    x <- read_cdf(path, format_out = "data.frame")
    expect_equal(attr(x$MS1, "detector_y_unit"), units[[u]])
    expect_equal(attr(x$MS1, "detector_x_unit"), "M/Z")
  }

  attr(ms, "detector_y_unit") <- NULL
  path <- write_andi_ms(ms, path_out = tmp, sample_name = "none", force = TRUE)
  x <- read_cdf(path, format_out = "data.frame")
  expect_equal(attr(x$MS1, "detector_y_unit"), "Arbitrary Intensity Units")
})

test_that("write_andi_ms writes the MS1 element of a list that also holds MS2", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  ms1 <- data.frame(rt = rep((1:4) * 0.1, each = 3),
                    mz = rep(c(100, 200, 300), 4), intensity = 1:12)
  attr(ms1, "data_format") <- "long"
  ms2 <- data.frame(rt = c(0.15, 0.25), mz = c(50, 60), intensity = c(7, 8))
  attr(ms2, "data_format") <- "long"

  expect_warning(
    path <- write_andi_ms(list(MS2 = ms2, MS1 = ms1), path_out = tmp,
                          sample_name = "levels", force = TRUE),
    "cannot hold 'MS2' scans")
  x <- read_cdf(path, what = "MS1", format_out = "data.frame")
  expect_equal(x$mz, ms1$mz)
  expect_equal(x$intensity, ms1$intensity)
})
