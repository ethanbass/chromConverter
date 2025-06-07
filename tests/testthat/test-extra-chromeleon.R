# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_chroms can read 'Chromeleon' comma-separated files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chromeleon_comma.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "chromeleon_uv", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(3241, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")

  x1 <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_s3_class(x1[1], "data.frame")
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
  expect_equal(attr(x1, "data_format"), "long")
})

test_that("read_chroms can read 'Chromeleon' period-separated files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chromeleon_period.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(10, 1))
  expect_equal(attr(x, "parser"), "chromconverter")

  x1 <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_s3_class(x1[1], "data.frame")
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
})

test_that("read_chroms can read 'Chromeleon' 3D data files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chromeleon_3D.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "chromeleon_uv", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(6000, 301))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")
  expect_equal(attr(x, "detector"), "UV")
  expect_equal(attr(x, "sample_name"), "MeOH_Blank")
  expect_equal(attr(x, "vial"), "GA1")
  expect_equal(attr(x, "sample_injection_volume"), "1.000")
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "detector_y_unit"), "mAU")

  x1 <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_s3_class(x1[1], "data.frame")
  expect_equal(colnames(x1), c("rt", "lambda", "intensity"))
  expect_equal(as.numeric(rownames(x)), x1[x1$lambda==200,1])
  expect_equal(nrow(x1), ncol(x)*nrow(x))
  expect_equal(attr(x1, "data_format"), "long")
})
