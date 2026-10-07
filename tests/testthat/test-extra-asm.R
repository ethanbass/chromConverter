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


test_that("read_chroms can read ASM LC-MS format", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-lc-ms.tabular.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "asm", format_out = "data.frame",
                   data_format = "long", progress_bar = FALSE)[[1]]
  expect_equal(names(x), c("single channel", "UV spectrum",
                           "MS ES- : TIC Smooth (SG, 2x2)",
                           "MS ES+ :TIC Smooth (SG, 2x2)",
                           "MS ES+ :195.2+217.2 1.0000DA Smooth (SG, 2x2)"))
  expect_equal(unname(sapply(x, nrow)), c(2401, 2401, 347, 347, 347))
  expect_equal(attr(x[[1]], "sample_name"), "Sample 1")
  expect_equal(attr(x[[1]], "instrument"), "ACQ-SQD#K06SQD061N")
  expect_equal(attr(x[[1]], "operator"), "Chemist, Joe")
  expect_equal(attr(x[[1]], "run_datetime"), as.POSIXct("2023-01-31 14:26:30",
                                                        tz = "UTC"))
  expect_equal(attr(x[[1]], "detector_y_unit"), "mAU")
  expect_equal(attr(x[[3]], "detector_y_unit"), "Counts")
  expect_equal(unique(x[[3]]$detector), "MS ES- : TIC Smooth (SG, 2x2)")

  expect_equal(sum(x[[1]]$intensity), 289847233.52752)
  expect_equal(sum(x[[3]]$intensity), 168719.854554)
  expect_equal(head(x[[3]]$rt, 3), c(0.0029, 0.0087, 0.01447))
})

test_that("read_chroms can read ASM GC-MS mass spectra", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-gc-ms-tiny.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "asm", progress_bar = FALSE)[[1]]
  expect_s3_class(x, "data.table")
  expect_equal(names(x), c("rt", "mz", "intensity"))
  expect_equal(nrow(x), 40)
  expect_equal(unique(x$rt), c(353.43, 359.43, 42.05))
  expect_equal(x$mz[1:3], c(0, 1, 2))
  expect_equal(sum(x$intensity), 350)
  expect_equal(attr(x, "time_unit"), "s")
  expect_equal(attr(x, "sample_name"), "tiny.pwiz.1.1")
})

test_that("read_chroms can read ASM GC-MS mass spectra and peak lists", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-gc-ms.tabular.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "asm", progress_bar = FALSE)[[1]]
  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 78182)
  expect_equal(length(unique(x$rt)), 646)
  expect_equal(range(x$rt), c(3502.856, 3691.025))
  expect_equal(sum(x$intensity), 1223813621)
  expect_equal(attr(x, "sample_name"), "212 SEXY")

  y <- read_asm(path, what = "peak_table", peaktable_format = "original",
                format_out = "data.frame")
  expect_equal(y[["written name"]], c("Ethyl Vanillin", "Vanillin"))
  expect_equal(y[["retention time"]], c(3567.329, 3636.762))
})

test_that("read_chroms splits ASM sequences into samples", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-empower-sequence.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "asm", format_out = "data.frame",
                   data_format = "long", progress_bar = FALSE)
  expect_equal(names(x), c("S0102", "S0103"))
  expect_equal(nrow(x[[1]]), 720)
  expect_equal(sum(x[[1]]$intensity), 2920.4099991074)
  expect_equal(attr(x[[1]], "sample_name"), "S0102")
  expect_equal(attr(x[[1]], "instrument"), "Alliance")
  expect_equal(attr(x[[1]], "run_datetime"),
               as.POSIXct("1997-09-17 17:03:14", tz = "UTC"))

  pk <- read_asm(path, what = "peak_table", format_out = "data.frame")
  expect_equal(names(pk), c("S0102", "S0103"))
  expect_equal(pk[[1]]$rt, c(74.9226, 127.9421, 186.6237, 286.9002),
               tolerance = 1e-6)

  path <- system.file("ASM-openlab-sequence.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  y <- read_chroms(path, format_in = "asm", progress_bar = FALSE)
  expect_equal(names(y), c("ACN", "A 20"))
  expect_equal(names(y[[2]]), c("DAD1B chromatogram", "DAD1D chromatogram"))
  expect_equal(attr(y[[2]][[1]], "sample_name"), "A 20")
  expect_equal(attr(y[[2]][[1]], "run_datetime"),
               as.POSIXct("2023-09-01 11:52:56", tz = "UTC"))
  expect_equal(attr(y[[2]][[1]], "detector_model"), "G7117C")
  expect_equal(attr(y[[2]][[1]], "detector_range"), 210)
})

test_that("read_chroms can read ASM instrument traces", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-unicorn.json", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_asm(path, what = "instrument", format_out = "data.frame",
                data_format = "long")
  expect_equal(names(x), c("Conc B", "PreC pressure", "PostC pressure",
                           "Sample pressure", "System pressure",
                           "System flow (CV/h)", "Sample flow (CV/h)",
                           "System flow", "Sample flow", "Sample linear flow",
                           "Cond temp"))
  pressure <- x[["System pressure"]]
  expect_equal(pressure$rt, c(0.377792358398438, 0.382675170898438,
                              0.387557983398438))
  expect_equal(pressure$intensity, c(0.164909511804581, 0.132852107286453,
                                     0.100794687867165))
  expect_equal(attr(pressure, "time_unit"), "mL")
  expect_equal(attr(pressure, "detector_y_unit"), "MPa")
  expect_equal(attr(pressure, "sample_name"), "Sample 001")

  path <- system.file("ASM-openlab-sequence.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  y <- read_chroms(path, format_in = "asm", what = c("chroms", "instrument"),
                   format_out = "data.frame", data_format = "long",
                   progress_bar = FALSE)
  expect_equal(names(y[[2]]), c("chroms", "instrument"))
  expect_equal(y[[2]]$instrument$intensity, c(0.0036, 12.4505, 0.0096, 12.3949))
  expect_equal(attr(y[[2]]$instrument, "sample_name"), "A 20")
})

test_that("read_chroms can read ASM peak lists", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-liquid-chromatography.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "asm", what = "peak_table",
                   format_out = "data.frame", progress_bar = FALSE)[[1]]
  expect_equal(names(x), c("single channel", "UV spectrum"))
  expect_equal(names(x[[1]]), c("rt", "start", "end", "area", "height"))
  expect_equal(x[[1]]$rt, c(367.2145, 372.7502, 374.4260), tolerance = 1e-6)
  expect_equal(x[[1]]$area, c(53118.3728, 1004.5008, 408.5294),
               tolerance = 1e-6)
  expect_equal(attr(x[[1]], "time_unit"), "s")
  expect_equal(attr(x[[1]], "sample_name"), "Sample 1")

  y <- read_asm(path, what = "peak_table", peaktable_format = "original",
                format_out = "data.frame")[[1]]
  expect_equal(y[["written name"]], rep("GSK1234567", 3))
  expect_equal(y[["retention time"]], x[[1]]$rt)

  z <- read_asm(path, what = c("chroms", "peak_table"))
  expect_equal(names(z), c("chroms", "peak_table"))
})

test_that("read_peaklist can read ASM peak lists", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("ASM-gas-chromatography.tabular.json",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_peaklist(path, format_in = "asm", progress_bar = FALSE)
  expect_s3_class(x, "peak_list")
  expect_equal(names(x[[1]]), c("sample", "rt", "start", "end", "area", "height"))
  expect_equal(nrow(x[[1]]), 4)
  expect_equal(x[[1]]$rt[1], 39.80021, tolerance = 1e-6)
})
