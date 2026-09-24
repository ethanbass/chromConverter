# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_chroms can read 'Thermo' RAW files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_thermorawfileparser()

  path <- system.file("CirA.raw",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tmp <- tempdir()
  on.exit(unlink(tmp))
  x <- read_chroms(path, progress_bar = FALSE, path_out = tmp)[[1]]
  expect_type(x, "list")
  expect_equal(names(x), c("MS1", "MS2", "DAD", "BPC",
                           "TIC", "chroms", "metadata"))

  # `format_out` is forwarded to `read_mzml`, which accepts all three classes
  y <- read_chroms(path, progress_bar = FALSE, path_out = tmp,
                   format_out = "data.table")[[1]]
  expect_s3_class(y$TIC, "data.table")
  expect_equal(attr(y$TIC, "format_out"), "data.table")
})

test_that("read_chroms can use 'OpenChrom' parsers", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_openchrom()

  path <- system.file("DCM1.SMS", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))
  tmp <- tempdir()
  on.exit(unlink(tmp))
  x <- read_chroms(path, format_in = "msd", progress_bar = FALSE,
                   verbose = FALSE, export_format = "csv",
                   path_out = tmp)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(3032, 297))

  x <- read_chroms(path, format_in = "msd", progress_bar = FALSE,
                   verbose = FALSE, path_out = tmp)[[1]]
  expect_type(x, "list")
  expect_equal(dim(x$MS1), c(469732,4))
})

test_that("read_cdf function can read peak tables", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("VARIAN1.CDF", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_cdf(path, what = "peak_table")
  # what about chromatograms
  expect_s3_class(x, "data.frame")
  expect_equal(dim(x), c(8,6))
  expect_true("peak_name" %in% names(x))
})

test_that("read_chroms can read ANDI MS files", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("HP_MS.CDF", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]

  expect_equal(names(x), c("MS1", "TIC"))
  expect_equal(colnames(x$MS1), c("rt","mz","intensity"))
  expect_equal(colnames(x$TIC),"intensity")
  expect_s3_class(x$MS1, "data.frame")
  expect_equal(class(x$TIC)[1], "matrix")
  expect_true(all(dim(x$TIC) == c(621, 1)))
  expect_true(all(dim(x$MS1) == c(7638, 3)))

  x1 <- read_chroms(path, what = c("TIC"), data_format="long",
                    progress_bar = FALSE)[[1]]
  expect_equal(ncol(x1), 2)
  expect_equal(colnames(x1), c("rt", "intensity"))

  x2 <- read_chroms(path, what=c("MS1"), ms_format = "list",
                    progress_bar = FALSE)[[1]]
  expect_type(x2, "list")
  expect_equal(length(x2), length(unique(x$MS1$rt)))
})

test_that("get_filetype works as expected", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  expect_equal(get_filetype(system.file("chemstation_MSD.MS",
                                        package = "chromConverterExtraTests")),
               "chemstation_ms"
  )
  expect_equal(get_filetype(system.file("B4NF.7_C23.qgd",
                                        package="chromConverterExtraTests")),
               "shimadzu_qgd")
  expect_equal(get_filetype(system.file("chemstation_181.D/FID1A.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_181")
  expect_equal(get_filetype(system.file("chemstation_179_mustang.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_179")
  expect_equal(get_filetype(system.file("openlab_131.uv",
                                        package="chromConverterExtraTests")),
               "openlab_131")
  expect_equal(get_filetype(system.file("chemstation_81.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_81")
  expect_equal(get_filetype(system.file("chemstation_30.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_30")
  expect_equal(get_filetype(system.file("chemstation_31.uv",
                                        package="chromConverterExtraTests")),
               "chemstation_31")
  expect_equal(get_filetype(system.file("small.RAW",
                                        package="chromConverterExtraTests")),
               "thermoraw")
  expect_equal(get_filetype(system.file("FS19_214.gcd",
                                        package="chromConverterExtraTests")),
               "shimadzu_gcd")
  expect_equal(get_filetype(system.file("DCM1.SMS",
                                        package="chromConverterExtraTests")),
               "varian_sms")
  expect_equal(get_filetype(system.file("VARIAN1.CDF",
                                        package="chromConverterExtraTests")),
               "cdf")
  expect_equal(get_filetype(system.file("agilent.dx",
                                        package="chromConverterExtraTests")),
               "agilent_dx")
})
