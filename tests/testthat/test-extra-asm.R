# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_chroms can read ASM LC format", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-liquid-chromatography.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "asm", format_out = "data.table",
                   progress_bar = FALSE)[[1]]
  expect_equal(names(x), c("single channel", "UV spectrum"))
  expect_equal(nrow(x[[1]]), 36000)
  expect_s3_class(x[[1]], c("data.table","data.frame"))
  expect_equal(attr(x[[1]], "sample_name"), "Sample 1")
  expect_equal(attr(x[[1]], "instrument"), "LC344")
  expect_equal(attr(x[[1]], "detector_range"), 210)
  expect_equal(attr(x[[1]], "detector_y_unit"), "mAU")
  expect_equal(attr(x[[1]], "run_datetime"), as.POSIXct("2016-10-20 06:33:54",
                                                        tz = "UTC"))
  expect_equal(as.character(attr(x[[1]], "time_unit")), "s")
  expect_equal(as.character(attr(x[[1]], "data_format")), "wide")
})

test_that("read_chroms can read ASM GC format", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-gas-chromatography.tabular.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "asm", format_out = "data.frame",
                   data_format = "long", progress_bar = FALSE)[[1]]

  expect_equal(nrow(x), 36000)
  expect_s3_class(x, "data.frame")
  expect_equal(attr(x, "sample_name"), "22-00465-1")
  expect_equal(attr(x, "instrument"), "GC65")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "run_datetime"), as.POSIXct("2022-05-12 11:24:28",
                                                   tz = "UTC"))
  expect_equal(as.character(attr(x, "time_unit")), "s")
  expect_equal(as.character(attr(x, "data_format")), "long")
})
