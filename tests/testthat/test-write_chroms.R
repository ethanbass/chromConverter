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
