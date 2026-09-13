# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_peaklist can read `Shimadzu` ASCII (PDA) files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzuDAD_Anthocyanin.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_peaklist(path, format_in = "shimadzu_dad",
                     progress_bar = FALSE)[[1]]
  expect_type(x, "list")
  expect_equal(length(x), 5)
  expect_s3_class(x[[1]], "data.frame")
  expect_equal(dim(x[[1]]), c(133, 6))
  expect_equal(colnames(x[[1]]), c("sample", "rt", "start",
                                   "end", "area", "height"))
})

test_that("read_chroms can read 'Shimadzu' PDA files (ASCII and LCD)", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_ascii <- system.file("shimadzuDAD_Anthocyanin.txt",
                            package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_ascii))

  path_lcd <- system.file("Anthocyanin.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_chroms(path_ascii, format_in = "shimadzu_dad",
                   progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(4689, 328))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")

  x1 <- read_chroms(path_ascii, format_in = "shimadzu_dad",
                    progress_bar = FALSE, data_format = "long",
                    format_out = "data.frame")[[1]]

  expect_s3_class(x1[1], "data.frame")
  expect_equal(dim(x1), c(4689 * 328, 3))

  x2 <- read_chroms(path_lcd, progress_bar = FALSE)[[1]]

  expect_equal(dim(x2), c(4689, 328))
  expect_equal(x, x2, ignore_attr = TRUE)

  # exact intensities from the LCD decoder, to catch drift independently of
  # the ASCII comparison above
  expect_equal(x2[2345, 100:104],
               c(1114881, 1100341, 1084494, 1069855, 1051037),
               ignore_attr = TRUE)
  expect_equal(x2[4689, 324:328], c(-374, -635, -684, -718, -917),
               ignore_attr = TRUE)
  expect_equal(sum(x2), 491929355147)
  expect_equal(range(x2), c(-454445, 4000000))
  expect_equal(as.numeric(rownames(x2))[c(1, 2, 4689)],
               c(0, 0.010666666666666699, 50.005333333333297))

  # long format
  x3 <- read_chroms(path_lcd, progress_bar = FALSE, data_format = "long",
                    format_out = "data.frame")[[1]]

  expect_s3_class(x3, "data.frame")
  expect_equal(dim(x3), c(4689 * 328, 3))
  expect_equal(colnames(x3), c("rt", "lambda", "intensity"))
  expect_equal(attr(x3, "data_format"), "long")

  # the long and wide forms must describe the same data
  expect_equal(x3$intensity, as.numeric(t(x2)), ignore_attr = TRUE)
  expect_equal(unique(x3$lambda), as.numeric(colnames(x2)))
  expect_equal(unique(x3$rt), as.numeric(rownames(x2)))

  # check metadata equivalence
  expect_equal(attr(x, "software_version"), attr(x2, "software_version"))
  expect_equal(attr(x, "method"), attr(x2, "method"))
  expect_equal(attr(x, "batch"), attr(x2, "batch"))
  expect_equal(attr(x, "operator"), attr(x2, "operator"))
  expect_equal(attr(x, "sample_name"), attr(x2, "sample_name"))
  expect_equal(attr(x, "sample_id"), attr(x2, "sample_id"))
  expect_equal(attr(x, "sample_injection_volume"),
               attr(x2, "sample_injection_volume"))
  expect_equal(as.numeric(attr(x, "time_range")),
               round(attr(x2, "time_range"), 3))
})

test_that("Shimadzu Anthocyanin peak tables match", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_ascii <- system.file("shimadzuDAD_Anthocyanin.txt",
                            package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_ascii))

  path_lcd <- system.file("Anthocyanin.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_peaklist(path_ascii, format_in = "shimadzu_dad",
                     data_format = "original",
                     progress_bar = FALSE)[[1]]

  x1 <- read_shimadzu_lcd(path_lcd, what="peak_table")
  x1 <- read_peaklist(path_lcd, format_in = "shimadzu_lcd", progress_bar=FALSE)[[1]]

  expect_equal(x[[1]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[1]][,-1],
               tolerance = .001, ignore_attr = TRUE)
  expect_equal(x[[2]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[3]][,-1],
               tolerance = .001, ignore_attr = TRUE)
  expect_equal(x[[3]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[4]][,-1],
               tolerance = .001, ignore_attr = TRUE)
  expect_equal(x[[4]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[5]][,-1],
               tolerance = .001, ignore_attr = TRUE)
  expect_equal(x[[5]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[6]][,-1],
               tolerance = .001, ignore_attr = TRUE)
})


test_that("read_chroms can read 2D chromatograms from 'Shimadzu' LCD files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_ascii <- system.file("shimadzuDAD_Anthocyanin.txt",
                            package = "chromConverterExtraTests")

  skip_if_not(file.exists(path_ascii))


  path_lcd <- system.file("Anthocyanin.lcd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_chroms(path_ascii, format_in = "shimadzu_ascii", progress_bar = FALSE,
                   what = "chroms")[[1]][["lc"]]

  x1 <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                    progress_bar = FALSE)[[1]]

  expect_equal(class(x1$AD2)[1], "matrix")
  expect_equal(dim(x1$AD2), c(30000, 1))
  expect_equal(x[-1,1], x1$AD2[,1], ignore_attr = TRUE)
  expect_equal(as.numeric(rownames(x)[-1]), as.numeric(rownames(x1$AD2)),
               tolerance = .0001)

  # unscaled
  x2 <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                    progress_bar = FALSE, scale = FALSE)[[1]]
  expect_equal(x[-1, 1], x2$AD2[, 1] * attr(x2$AD2, "intensity_multiplier"),
               ignore_attr = TRUE)

  # check metadata equivalence
  expect_equal(attr(x, "software_version"), attr(x1$AD2, "software_version"))
  expect_equal(attr(x, "method"), attr(x1$AD2, "method"))
  expect_equal(attr(x, "batch"), attr(x1$AD2, "batch"))
  expect_equal(attr(x, "operator"), attr(x1$AD2, "operator"))
  expect_equal(attr(x, "sample_name"), attr(x1$AD2, "sample_name"))
  expect_equal(attr(x, "sample_id"), attr(x1$AD2, "sample_id"))
  expect_equal(attr(x, "sample_injection_volume"),
               attr(x1$AD2, "sample_injection_volume"))
  expect_equal(as.numeric(attr(x, "time_range")),
               round(attr(x1$AD2, "time_range"), 3))
  expect_equal(attr(x, "detector_y_unit"), attr(x1$AD2, "detector_y_unit"))
  expect_equal(attr(x, "intensity_multiplier"),
               attr(x1$AD2, "intensity_multiplier"))
})

test_that("read_chroms can read 'Shimadzu' PDA comma-separated file", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_ascii <- system.file("shimadzuDAD_comma.txt",
                            package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_ascii))

  x <- read_chroms(path_ascii, format_in = "shimadzu_dad",
                   progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(6096, 171))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")
  expect_equal(attr(x, "sample_name"), "Pinoresinol Standard")
})

test_that("read_chroms can read multi-channel chromatograms from 'Shimadzu' LCD files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_asc <- system.file("multichannel_chrom.txt",
                          package = "chromConverterExtraTests")

  skip_if_not(file.exists(path_asc))

  path_lcd <- system.file("multichannel_chrom.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                   progress_bar = FALSE)[[1]]
  x1 <- read_chroms(path_asc, format_in = "shimadzu_ascii", what = "chroms",
                    progress_bar = FALSE)[[1]]

  # check intensities
  expect_equal(x[[1]],x1[[1]][-1,]*40, ignore_attr = TRUE, tolerance = .1)
  expect_equal(x[[2]],x1[[2]][-1,]*40, ignore_attr = TRUE, tolerance = .1)
  expect_equal(x[[3]],x1[[3]][-1,]*310, ignore_attr = TRUE, tolerance = .1)

  # exact values from the LCD decoder. The comparison against the ASCII export
  # above needs an ad hoc scaling factor and a loose tolerance, so pin the
  # decoded values directly as well.
  expect_equal(names(x), c("A, 260nm", "A, 210nm", "B"))
  expect_equal(dim(x[[1]]), c(3359, 1))
  expect_equal(dim(x[[2]]), c(3359, 1))
  expect_equal(dim(x[[3]]), c(3360, 1))
  expect_equal(x[[1]][1:5, 1], c(-0.029, -0.069, -0.062, 0.003, 0.074),
               ignore_attr = TRUE)
  expect_equal(x[[2]][1:5, 1], c(-0.034, 0.012, -0.053, -0.143, -0.214),
               ignore_attr = TRUE)
  expect_equal(x[[3]][1:5, 1], c(0.06, 0.007, 0, 0.023, -0.025),
               ignore_attr = TRUE)
  expect_equal(sum(x[[1]]), 7998.1729999999743)
  expect_equal(sum(x[[2]]), 529714.86499999929)
  expect_equal(sum(x[[3]]), 1708960.8040000028)

  # (the shape of the signals approximately match but the scaling is off. The values
  # in the text file may also be rounded?)

  # check retention times
  expect_equal(as.numeric(rownames(x[[1]])),
               as.numeric(rownames(x1[[1]]))[-1], tolerance = .001)
  expect_equal(as.numeric(rownames(x[[2]])),
               as.numeric(rownames(x1[[2]]))[-1], tolerance = .001)
  expect_equal(as.numeric(rownames(x[[3]])),
               as.numeric(rownames(x1[[3]]))[-1], tolerance = .001)

  # check metadata equivalence
  expect_equal(attr(x[[1]], "software_version"), attr(x1[[1]], "software_version"))
  expect_equal(attr(x[[1]], "method"), attr(x1[[1]], "method"))
  expect_equal(attr(x[[1]], "batch"), attr(x1[[1]], "batch"))
  expect_equal(attr(x[[1]], "operator"), attr(x1[[1]], "operator"))
  expect_equal(attr(x[[1]], "sample_name"), attr(x1[[1]], "sample_name"))
  expect_equal(attr(x[[1]], "sample_id"), attr(x1[[1]], "sample_id"))
  expect_equal(attr(x[[1]], "sample_injection_volume"),
               attr(x1[[1]], "sample_injection_volume"))
  expect_equal(round(as.numeric(attr(x[[1]], "time_range"))),
               round(as.numeric(attr(x1[[1]], "time_range"), 3)))
  expect_equal(attr(x[[1]], "detector_y_unit"), attr(x1[[1]], "detector_y_unit"))
  expect_equal(attr(x[[1]], "intensity_multiplier"),
               attr(x1[[1]], "intensity_multiplier"))

  # check long format
  x2 <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                    data_format = "long", progress_bar = FALSE)[[1]]
  # x3 <- read_chroms(path_asc, format_in = "shimadzu_ascii", what = "chroms",
  #                   data_format = "long", progress_bar = FALSE)[[1]]

  expect_s3_class(x2, "data.frame")

  # expect_s3_class(x3, "data.frame")

  expect_equal(nrow(x2), sum(sapply(x, nrow)))
  expect_equal(x2[x2$lambda == "260nm", "intensity"], x[["A, 260nm"]],
               ignore_attr = TRUE)
  expect_equal(x2[x2$lambda == "260nm", "rt"],
               as.numeric(rownames(x[["A, 260nm"]])))

  expect_equal(x2[x2$lambda == "210nm", "intensity"], x[["A, 210nm"]],
               ignore_attr = TRUE)
  expect_equal(x2[x2$lambda == "210nm", "rt"],
               as.numeric(rownames(x[["A, 210nm"]])))

  expect_equal(x2[x2$lambda == "", "intensity"], x[["B"]], ignore_attr = TRUE)
})

test_that("Shimadzu multichannel peak tables match", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_asc <- system.file("multichannel_chrom.txt",
                          package = "chromConverterExtraTests")

  skip_if_not(file.exists(path_asc))

  path_lcd <- system.file("multichannel_chrom.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_peaklist(path_asc, format_in = "shimadzu_dad",
                     data_format = "original",
                     progress_bar = FALSE)[[1]]

  x1 <- read_peaklist(path_lcd, format_in = "shimadzu_lcd",
                      progress_bar=FALSE)[[1]]


  expect_equal(x[[1]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[1]][,-1],
               tolerance=.01, ignore_attr = TRUE)
  expect_equal(x[[2]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[2]][,-1],
               tolerance = .001, ignore_attr = TRUE)
  expect_equal(x[[3]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[3]][,-1],
               tolerance = .001, ignore_attr = TRUE)
})


test_that("Shimadzu GCD parser works", {
  skip_on_cran()
  skip_if_missing_dependencies("olefile")
  skip_if_not_installed("chromConverterExtraTests")

  path_gcd <- system.file("FS19_214.gcd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_gcd))

  x <- read_chroms(path_gcd, format_in = "shimadzu_gcd", find_files = FALSE,
                   progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")

  path_ascii <- test_path("testdata/ladder.txt")

  txt <- read_chroms(path_ascii, format_in = "shimadzu_fid", find_files = FALSE,
                     progress_bar = FALSE)[[1]]

  expect_equal(x, txt, tolerance = .0001, ignore_attr = TRUE)

  # exact values from the GCD decoder
  expect_equal(dim(x), c(66255, 1))
  expect_equal(x[1:5, 1], c(-361.80000539124012, -361.80000539124012,
                            -361.90000539273024, -362.00000539422035,
                            -362.10000539571047), ignore_attr = TRUE)
  expect_equal(tail(x[, 1], 3),
               c(4328.00001525879, 4328.300064496696, 4328.2000644952059),
               ignore_attr = TRUE)
  expect_equal(sum(x[, 1]), 281615360.29634726)

  # check metadata equivalence
  expect_equal(attr(x, "software_version"), attr(txt, "software_version"))
  expect_equal(attr(x, "method"), attr(txt, "method"))
  expect_equal(attr(x, "operator"), attr(txt, "operator"))
  expect_equal(attr(x, "sample_name"), attr(txt, "sample_name"))
  expect_equal(attr(x, "sample_id"), attr(txt, "sample_id"))
  expect_equal(attr(x, "sample_injection_volume"), attr(txt, "sample_injection_volume"))
  expect_equal(as.numeric(attr(txt, "time_range")), round(attr(x, "time_range"), 3))
})

test_that("Shimadzu FID peak tables match", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_asc <- test_path("testdata/ladder.txt")

  path_gcd <- system.file("FS19_214.gcd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_gcd))

  x <- read_peaklist(path_asc, format_in = "shimadzu_dad",
                     data_format = "original",
                     progress_bar = FALSE)[[1]]

  x1 <- read_peaklist(path_gcd, format_in = "shimadzu_gcd", progress_bar=FALSE)

  expect_equal(x[,c(3,6:7,4:5,8:9,11,13:18,21:22)], x1[[1]][,-1], tolerance=.001,
               ignore_attr = TRUE)
})


test_that("Shimadzu QGD parser works", {
  skip_on_cran()
  skip_if_missing_dependencies("olefile")
  skip_if_not_installed("chromConverterExtraTests")

  path_qgd <- system.file("B4NF.7_C23.qgd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_qgd))

  tmp <- tempdir()
  mzml_path <- fs::path_ext_set(fs::path(tmp, basename(path_qgd)), ext = "mzML")
  on.exit(unlink(mzml_path))

  x <- read_chroms(path_qgd, find_files = FALSE, progress_bar = FALSE,
                   format_out = "data.table",
                   export_format = "mzml", path_out = tmp)[[1]]

  expect_equal(class(x[[1]])[1], "data.table")
  expect_equal(class(x[[2]])[1], "data.table")

  # check metadata equivalence
  expect_equal(attr(x$MS1, "operator"), "Admin")
  expect_equal(attr(x$MS1, "sample_name"), "B4NF.7_C23")
  expect_equal(attr(x$MS1, "sample_type"), "Unknown")
  expect_equal(attr(x$MS1, "parser"), "chromconverter")
  expect_equal(attr(x$MS1, "data_format"), "long")
  expect_equal(attr(x$TIC, "data_format"), "long")

  expect_equal(unique(x$MS1$rt), x$TIC$rt)

  # The TIC stream is a flat array of 8-byte integers, decoded independently of
  # the variable-width MS1 intensity encoding. Summing MS1 intensities per scan
  # must reproduce it exactly, which pins the MS1 intensity decoder against
  # independent data in the same file.
  expect_equal(as.numeric(tapply(x$MS1$intensity, x$MS1$scan, sum)),
               x$TIC$intensity, ignore_attr = TRUE)

  # exact m/z and intensity values. Intensities are stored with a per-scan
  # byte width; this file contains scans encoded with both 2 bytes (16372
  # scans, first is scan 1) and 3 bytes (428 scans, first is scan 8), so pin
  # one of each. Scan 8's maximum exceeds 65535, so it can only come from the
  # 3-byte path. NOTE: the 4-byte path is not exercised by any test file.
  expect_equal(dim(x$MS1), c(9508566, 4))

  i1 <- which(x$MS1$scan == 1)
  expect_equal(length(i1), 567)
  expect_equal(x$MS1$rt[i1[1]], 4.003333333333333)
  expect_equal(x$MS1$mz[i1][1:5], c(35, 36, 37, 38, 39))
  expect_equal(x$MS1$intensity[i1][1:5], c(489, 500, 557, 549, 499))
  expect_equal(max(x$MS1$intensity[i1]), 14420)

  i8 <- which(x$MS1$scan == 8)
  expect_equal(length(i8), 566)
  expect_equal(x$MS1$rt[i8[1]], 4.0266666666666664)
  expect_equal(x$MS1$mz[i8][1:5], c(35, 36, 37, 37.95, 38.95))
  expect_equal(x$MS1$intensity[i8][1:5], c(605, 866, 1112, 1567, 9248))
  expect_equal(max(x$MS1$intensity[i8]), 123490)

  iL <- which(x$MS1$scan == 16799)
  expect_equal(length(iL), 566)
  expect_equal(tail(x$MS1$intensity[iL], 3), c(525, 489, 517))
  expect_equal(tail(x$MS1$mz[iL], 3), c(597.8, 598.8, 599.8))

  expect_equal(sum(x$MS1$intensity), 9258016526)
  expect_equal(sum(x$MS1$mz), 3018199528.7796111)
  expect_equal(range(x$MS1$intensity), c(102, 4851696))

  # TIC
  expect_equal(nrow(x$TIC), 16800)
  expect_equal(x$TIC$intensity[1:5],
               c(289349, 1188395, 2133942, 1228615, 313071))
  expect_equal(sum(x$TIC$intensity), 9258016526)

  # compare mzml generated by chromconverter
  x1 <- read_mzml(mzml_path, what = c("MS1", "TIC", "metadata"))

  expect_equal(x1$MS1[,c(1:3)], as.data.frame(x$MS1[,-1]), ignore_attr = TRUE,
               tolerance = .0000001)

  expect_equal(x1$TIC[,"intensity"], x$TIC[,"intensity"][[1]],
               ignore_attr = TRUE)
  expect_equal(x1$metadata$source_file, basename(attr(x$MS1, "source_file")))
  # expect_equal(x1$metadata$timestamp, attr(x$MS1, "run_datetime"))

  # just TIC
  x2 <- read_chroms(path_qgd, format_in = "shimadzu_qgd", what='TIC',
                    format_out = "data.table", progress_bar = FALSE)[[1]]
  expect_equal(x2, x$TIC)

  x3 <- read_chroms(path_qgd, format_in = "shimadzu_qgd", what='TIC',
                    format_out = "data.table", collapse = FALSE,
                    progress_bar = FALSE)[[1]]
  expect_equal(x3$TIC, x$TIC)

  ## write CDF
  # cdf_path <- fs::path_ext_set(fs::path(tmp, basename(path_qgd)), ext = "cdf")
  # on.exit(unlink(cdf_path))
  # export_cdf(list(B4NF.7_C23 = x), path_out = tmp, force=TRUE, show_progress = FALSE)
})


test_that("Shimadzu times are converted correctly", {
  expect_equal(parse_shimadzu_tz("+05'30"), "Asia/Kolkata")
  expect_equal(parse_shimadzu_tz("UTC"), "UTC")
  expect_equal(parse_shimadzu_tz("+01'00'"), "Etc/GMT-1")
  unix_time <- 1699599676
  xx <-as.POSIXct(unix_time, origin = "1970-01-01",
                  tz = convert_fractional_timezone_offset("+03'30"))
  expect_equal(attr(xx, "tzone"), "Asia/Tehran")
  expect_equal(as.numeric(xx), 1699599676)

  xx <-as.POSIXct(unix_time, origin = "1970-01-01",
                  tz = convert_fractional_timezone_offset("+05'30"))
  expect_equal(attr(xx, "tzone"), "Asia/Kolkata")
  expect_equal(as.numeric(xx), 1699599676)
})
