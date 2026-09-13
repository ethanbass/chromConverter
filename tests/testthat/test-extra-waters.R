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

  expect_equal(sum(x[[1]]),
    37775.669732916)
  expect_equal(range(x[[1]]),
    c(-0.0326082, 2.335802))
  expect_equal(unname(colSums(x[[1]])[c(1, 245, 489)]),
    c(1903.4079729, -6.49555660009999, -23.1140699741))
  expect_equal(unname(x[[1]][1, 1:5]),
    c(0, 0, 0, 0, 0))
  expect_equal(unname(x[[1]][nrow(x[[1]]), 485:489]),
    c(-0.0098628, -0.0098831, -0.0092422, -0.0093695, -0.0095033))
  expect_equal(unname(x[[1]][3000, c(1, 245, 489)]),
    c(0.2262086, -0.0014038, -0.0037171))
  expect_equal(unname(x[[1]][round(seq(1, nrow(x[[1]]), length.out = 12)), 245]),
    c(0, 0.0017719, -0.0014524, -0.0014026, -0.0010072, -0.0012787, 
    -0.0015119, -0.0017052, -0.0018358, -0.0019263, -0.0020249, -0.0019739)) 
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
  expect_equal(attr(x$MS,"sample_position"), "2:A,11")

  expect_equal(attr(x$CAD,"sample_position"), "2:A,11")
  expect_equal(attr(x$CAD,"detector_y_unit"), "mV")
  expect_equal(attr(x$CAD,"parser"), "rainbow")

  x1 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    parser = "chromconverter")[[1]]
  expect_equal(class(x1$CAD)[1], "matrix")
  expect_equal(x$CAD, x1$CAD, ignore_attr = TRUE)

  x2 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    what = "MS", data_format = "long", precision = 0,
                    sparse = FALSE)[[1]]
  expect_equal(nrow(x2$MS), nrow(x$MS)*ncol(x$MS))
  expect_equal(colnames(x2$MS), c("rt", "mz", "intensity"))

  expect_equal(sum(x1$CAD),
    11141.2282361984)
  expect_equal(range(x1$CAD),
    c(11.0376472473145, 24.6099243164062))
  expect_equal(which.max(x1$CAD),
    80L)
  expect_equal(unname(head(x1$CAD[, 1], 5)),
    c(11.0376472473145, 11.1447868347168, 11.2513303756714, 11.3565330505371, 
    11.4600954055786))
  expect_equal(unname(tail(x1$CAD[, 1], 5)),
    c(12.7757186889648, 12.7682685852051, 12.7696094512939, 12.7807855606079, 
    12.803882598877))
  expect_equal(unname(x1$CAD[round(seq(1, nrow(x1$CAD), length.out = 15)), 1]),
    c(11.0376472473145, 14.2595767974854, 19.4326629638672, 20.321964263916, 
    19.7731552124023, 16.5281295776367, 14.0557289123535, 12.8301086425781, 
    12.2373399734497, 11.3967657089233, 11.3832054138184, 11.5787086486816, 
    11.5755796432495, 11.7000045776367, 12.803882598877))
  expect_equal(head(as.numeric(rownames(x1$CAD)), 3),
    c(0, 0.00333333341404796, 0.00666666682809591))
  expect_equal(tail(as.numeric(rownames(x1$CAD)), 3),
    c(2.53999996185303, 2.54333329200745, 2.54666662216187)) 
})

test_that("read_waters_arw decodes 2D values exactly", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("waters.arw", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "waters_arw", progress_bar = FALSE)[[1]]

  expect_equal(dim(x),
    c(6601L, 1L))
  expect_equal(sum(x),
    144023.407497391)
  expect_equal(range(x),
    c(-0.7805328, 366.7791))
  expect_equal(which.max(x),
    3324L)
  expect_equal(unname(head(x[, 1], 5)),
    c(0, 0, 0, 0, 0))
  expect_equal(unname(tail(x[, 1], 5)),
    c(0.4577484, 0.4567719, 0.4557953, 0.4557953, 0.4577484))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(0, -0.197525, -0.4289703, -0.6008453, -0.7795563, -0.5647125, 18.26537, 
    357.6794, 39.01439, 14.50072, 5.28978, 2.854233, 1.702866, 0.9733734, 
    0.4577484))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(0, 0.008333333, 0.01666667))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(54.98333, 54.99167, 55))
})
