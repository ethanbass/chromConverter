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
  # a 2D file records no scan range. `NULL == "3DFIELD"` is `logical(0)`, so
  # the same `ifelse` set a zero-length attribute, which `unlist` then dropped
  # from `extract_metadata` rather than reporting as `NA`
  expect_equal(attr(x, "detector_range"), NA)

  x1 <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_s3_class(x1[1], "data.frame")
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
  expect_equal(attr(x1, "data_format"), "long")

  expect_equal(sum(x),
    25631.528997)
  expect_equal(range(x),
    c(-0.7749, 210.061603))
  expect_equal(which.max(x),
    799L)
  expect_equal(unname(head(x[, 1], 5)),
    c(-0.0896, -0.03875, -0.0199, -0.0107, -0.0044))
  expect_equal(unname(tail(x[, 1], 5)),
    c(0.62745, 0.6253, 0.6306, 0.63075, 0.62815))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(-0.0896, -0.01275, 6.94385, 5.90415, 7.08265, 8.47905, 9.69805, 
    10.91685, 11.9916, 12.9037, 13.895, 6.6609, 0.94275, 0.4297, 0.62815))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(0, 0.016667, 0.033333))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(53.966667, 53.983333, 54)) 
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

  expect_equal(sum(x),
    -0.23485)
  expect_equal(range(x),
    c(-0.0896, -0.0044))
  expect_equal(which.max(x),
    5L)
  expect_equal(unname(head(x[, 1], 5)),
    c(-0.0896, -0.03875, -0.0199, -0.0107, -0.0044))
  expect_equal(unname(tail(x[, 1], 5)),
    c(-0.01355, -0.00705, -0.0116, -0.0142, -0.0251))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(0, 0.016667, 0.033333))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(0.116667, 0.133333, 0.15)) 
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
  expect_equal(attr(x, "sample_position"), "GA1")
  expect_equal(attr(x, "sample_injection_volume"), "1.000")
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "detector_y_unit"), "mAU")
  # the scan range was assembled with `ifelse()`, whose result is shaped like
  # its *test*, so only the lower bound survived
  expect_length(attr(x, "detector_range"), 2)
  expect_equal(as.numeric(attr(x, "detector_range")), c(200, 800))

  x1 <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_s3_class(x1[1], "data.frame")
  expect_equal(colnames(x1), c("rt", "lambda", "intensity"))
  expect_equal(as.numeric(rownames(x)), x1[x1$lambda==200,1])
  expect_equal(nrow(x1), ncol(x)*nrow(x))
  expect_equal(attr(x1, "data_format"), "long")

  expect_equal(sum(x),
    -1759379.88521591)
  expect_equal(range(x),
    c(-395.993972, 221.548094))
  expect_equal(unname(colSums(x)[c(1, 151, 301)]),
    c(-788845.459228999, 1621.582252, -7921.156766))
  expect_equal(unname(x[1, 1:5]),
    c(-0.001371, -0.000417, 0, 0.000179, -0.000656))
  expect_equal(unname(x[nrow(x), 297:301]),
    c(0.035107, 0.011384, 0.010073, 0.046074, 0.044167))
  expect_equal(unname(x[3000, c(1, 151, 301)]),
    c(-200.909269, 0.459433, -2.27046))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 12)), 151]),
    c(0.000238, 0.02569, -0.078201, -0.191271, 0.13274, 0.446737, 0.407219, 
    0.350237, 0.499487, 0.180602, 0.363827, 0.016391)) 
})

test_that("read_chroms can return raw 'Chromeleon' metadata", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chromeleon_3D.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # `metadata_format = "raw"` used to error, because the `switch` resolving it
  # had an unquoted `raw` on the right-hand side, which evaluates to
  # `base::raw`. The chromatogram was dropped with a warning.
  x <- read_chroms(path, format_in = "chromeleon_uv", metadata_format = "raw",
                   progress_bar = FALSE)[[1]]
  expect_false(is.null(x))
  meta <- attr(x, "metadata")
  expect_type(meta, "list")
  expect_true("Detector" %in% names(meta))
  # the chromConverter vocabulary is not applied in raw mode
  expect_null(attr(x, "detector"))
})
