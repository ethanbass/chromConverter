# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_peaklist can read `Shimadzu` ASCII (PDA) files", {
  skip_on_cran()
  skip_if_missing_dependencies()
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
  all.equal(as.numeric(rownames(x)[-1]), as.numeric(rownames(x1$AD2)),
            tolerance = .0001)

  # unscaled
  x2 <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                    progress_bar = FALSE, scale = FALSE)[[1]]
  all.equal(x[-1, 1], x2$AD2[, 1] * attr(x2$AD2, "intensity_multiplier"),
            check.attributes = FALSE)

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
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path_gcd <- system.file("FS19_214.gcd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_gcd))

  x <- read_chroms(path_gcd, format_in = "shimadzu_gcd", find_files = FALSE,
                   progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")

  path_ascii <- test_path("testdata/ladder.txt")

  txt <- read_chroms(path_ascii, format_in = "shimadzu_fid", find_files = FALSE,
                     progress_bar = FALSE)[[1]]

  all.equal(x, txt, tolerance = .0001, check.attributes = FALSE)

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
  skip_if_missing_dependencies()
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
