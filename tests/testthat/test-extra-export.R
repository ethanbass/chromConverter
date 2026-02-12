# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_chroms can export 'Agilent' MS files as ANDI MS cdf", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")
  skip_if_not_installed("ncdf4")

  path <- system.file("chemstation_MSD.MS",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # export as CDF
  tmp <- tempdir()
  x2 <- read_chroms(path, parser = "entab", format_out = "data.table",
                    progress_bar = FALSE,
                    export_format = "cdf", path_out = tmp, force=TRUE)[[1]]
  x2 <- x2[order(rt, mz)]

  path_cdf <- fs::path(tmp, gsub(" ", "_", attr(x2,"sample_name")), ext = "cdf")
  on.exit(unlink(path_cdf))

  xx <- read_cdf(path_cdf, format_out = "data.table")
  expect_equal(x2$rt, xx$MS1$rt/60, ignore_attr = TRUE)
  expect_equal(x2$intensity, xx$MS1$intensity, ignore_attr = TRUE)
  expect_equal(x2$mz, xx$MS1$mz, ignore_attr = TRUE, tolerance = .000001)
})

test_that("read_chroms can write mzML files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")

  path <- system.file("chemstation_MSD.MS",
                      package = "chromConverterExtraTests")

  tmp <- tempdir()

  x <- read_chroms(path, parser = "entab", progress_bar = FALSE,
                   format_out = "data.table",
                   export_format = "mzml", path_out = tmp, force = TRUE)[[1]]

  path_mzml <- fs::path(tmp, gsub(" ", "_",
                                  attr(x, "sample_name")), ext = "mzML")
  on.exit(unlink(path_mzml))

  x1 <- read_mzml(path_mzml, what = c("MS1", "metadata"))

  expect_equal(x1$MS1[,c(1:3)], x, ignore_attr = TRUE)
  # expect_equal(x1$metadata$timestamp, attr(x,"run_datetime"))
  expect_equal(x1$metadata$source_file, basename(attr(x,"source_file")))

  # only works with `data_format == "long"`
  # time zone discrepancy between rainbow and entab
  # why doesn't work with format == data.table
  path_mzml_rb <- fs::path(tmp, fs::path_ext_remove(basename(path)),
                           ext = "mzML")
  on.exit(unlink(path_mzml_rb))

  x <- read_chroms(path, parser = "rainbow",
                   data_format = "long",
                   format_out = "data.frame",
                   progress_bar = FALSE,
                   export_format = "mzml", path_out = tmp,
                   force = TRUE)[[1]]
  x1 <- read_mzml(path_mzml_rb, what = c("MS1", "metadata"))
  expect_equal(x1$MS1[, c(1:3)], as.data.frame(x), ignore_attr = TRUE)
  expect_equal(x1$metadata$source_file, basename(attr(x,"source_file")))
  # expect_equal(x1$metadata$timestamp, attr(x,"run_datetime"))
})

test_that("read_chroms can convert CDF to mzML", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("HP_MS.CDF", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tmp <- tempdir()

  x <- read_chroms(path, progress_bar = FALSE, export_format = "mzml",
                   path_out = tmp, force = TRUE)[[1]]

  mzml_path <- fs::path(tmp, attr(x$MS1,"sample_name"), ext = "mzML")
  on.exit(unlink(mzml_path))

  x1 <- read_mzml(mzml_path, what = c("MS1", "metadata"))
  expect_equal(x1$MS1[,c(1:3)], x[[1]], ignore_attr = TRUE)
  expect_equal(x1$metadata$source_file, basename(attr(x$MS1, "source_file")))
})

test_that("read_chroms can convert Agilent 131 to ARW", {
  skip_on_cran()
  path_uv <- test_path("testdata/dad1.uv")
  tmp <- tempdir()
  x <- read_chroms(path_uv, format_in = "chemstation_uv", export_format="arw",
              path_out = tmp, force = TRUE, progress_bar=FALSE)[[1]]
  arw_path <- fs::path(tmp, attr(x,"sample_name"), ext = "arw")
  on.exit(unlink(arw_path))
  x1 <- read_chroms(arw_path, format_in = "waters_arw", progress_bar = FALSE)[[1]]
  expect_equal(x, x1, ignore_attr = TRUE)
})
