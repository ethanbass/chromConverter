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
