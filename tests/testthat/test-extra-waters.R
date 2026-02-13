# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_chroms can read 'Waters ARW' PDA files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("waters_pda.arw", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "waters_arw", progress_bar = FALSE)
  expect_equal(class(x[[1]])[1], "matrix")
  expect_equal(dim(x[[1]]), c(6001, 489))
  expect_equal(attr(x[[1]], "parser"), "chromconverter")
  expect_equal(attr(x[[1]], "data_format"), "wide")

  x1 <- read_chroms(path, format_in = "waters_arw", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_equal(colnames(x1), c("rt", "lambda", "intensity"))
  expect_s3_class(x1[1], "data.frame")
  expect_equal(attr(x1, "data_format"), "long")
})

test_that("read_chroms can read 'Waters RAW' files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("waters_blue.raw", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                   precision = 0)[[1]]
  expect_equal(names(x), c("MS", "UV", "CAD"))
  expect_equal(dim(x$MS), c(725, 740))
  expect_equal(attr(x$MS, "parser"), "rainbow")
  expect_equal(attr(x$MS, "data_format"), "wide")
  expect_equal(attr(x$MS,"polarity"), "+")
  expect_equal(attr(x$MS,"vial"), "2:A,11")

  expect_equal(attr(x$CAD,"vial"), "2:A,11")
  expect_equal(attr(x$CAD,"detector_y_unit"), "mV")
  expect_equal(attr(x$CAD,"parser"), "rainbow")

  x1 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    parser = "chromconverter")[[1]]
  expect_equal(class(x1$CAD)[1], "matrix")
  expect_equal(x$CAD, x1$CAD, ignore_attr = TRUE)
  # attr(x1$CAD, "parser")

  x2 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    what = "MS", data_format = "long", precision = 0,
                    sparse = FALSE)[[1]]
  expect_equal(nrow(x2$MS), nrow(x$MS)*ncol(x$MS))
  expect_equal(colnames(x2$MS), c("rt", "mz", "intensity"))
})
