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
  expect_equal(as.character(attr(x[[1]], "data_format")), "long")

  expect_equal(sum(x[[1]]$intensity),
    270881.911277771)
  expect_equal(range(x[[1]]$intensity),
    c(-5.16033172607422, 496.427059173584))
  expect_equal(which.max(x[[1]]$intensity),
    7344L)
  expect_equal(head(x[[1]]$intensity, 5),
    c(1.29270553588867, 1.28221511840821, 1.26743316650391, 1.251220703125, 
    1.2350082397461))
  expect_equal(tail(x[[1]]$intensity, 5),
    c(-4.44841384887696, -4.46367263793946, -4.48083877563477, 
    -4.5003890991211, -4.52375411987305))
  expect_equal(x[[1]]$intensity[round(seq(1, nrow(x[[1]]), length.out = 15))],
    c(1.29270553588867, -0.171184539794922, -0.422000885009766, 
    4.95052337646485, -2.2120475769043, -1.94931030273438, -3.46899032592774, 
    -3.62539291381836, -3.1423568725586, -3.33356857299805, 
    -3.49760055541992, -4.41122055053711, -4.68301773071289, 
    -4.14466857910156, -4.52375411987305))
  expect_equal(head(x[[1]]$rt, 3),
    c(0, 0.000833333, 0.001666666)) 
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

  expect_equal(sum(x[[2]]),
    270881.911277771)
  expect_equal(range(x[[2]]),
    c(-5.16033172607422, 496.427059173584))
  expect_equal(which.max(x[[2]]),
    7344L)
  expect_equal(head(x[[1]], 3),
    c(0, 0.000833333, 0.001666666))
  expect_equal(head(x[[2]], 5),
    c(1.29270553588867, 1.28221511840821, 1.26743316650391, 1.251220703125, 
    1.2350082397461))
  expect_equal(tail(x[[2]], 5),
    c(-4.44841384887696, -4.46367263793946, -4.48083877563477, 
    -4.5003890991211, -4.52375411987305))
  expect_equal(x[[2]][round(seq(1, nrow(x), length.out = 15))],
    c(1.29270553588867, -0.171184539794922, -0.422000885009766, 
    4.95052337646485, -2.2120475769043, -1.94931030273438, -3.46899032592774, 
    -3.62539291381836, -3.1423568725586, -3.33356857299805, 
    -3.49760055541992, -4.41122055053711, -4.68301773071289, 
    -4.14466857910156, -4.52375411987305)) 
})

