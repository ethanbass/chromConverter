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
  # a PDA 3D export has a field map of its own, unlike the 2D exports
  expect_equal(attr(x, "detector"), "DAD")
  expect_equal(attr(x, "detector_range"), c("190", "600"))

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
                     peaktable_format = "original",
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

test_that("'Shimadzu' ASCII exports report the acquisition time", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_us <- system.file("shimadzuDAD_comma.txt",
                         package = "chromConverterExtraTests")
  path_eu <- system.file("shimadzuDAD_Anthocyanin.txt",
                         package = "chromConverterExtraTests")
  path_chrom <- system.file("multichannel_chrom.txt",
                            package = "chromConverterExtraTests")
  skip_if_not(all(file.exists(path_us, path_eu, path_chrom)))

  us <- read_chroms(path_us, format_in = "shimadzu_dad",
                    progress_bar = FALSE)[[1]]
  eu <- read_chroms(path_eu, format_in = "shimadzu_dad",
                    progress_bar = FALSE)[[1]]
  chrom <- read_chroms(path_chrom, format_in = "shimadzu_ascii",
                       what = "chroms", progress_bar = FALSE)[[1]][[1]]

  expect_equal(format(attr(us, "run_datetime"), "%Y-%m-%d %H:%M:%S"),
               "2021-04-26 23:01:11")
  expect_equal(format(attr(eu, "run_datetime"), "%Y-%m-%d %H:%M:%S"),
               "2022-03-29 10:12:19")
  expect_equal(format(attr(chrom, "run_datetime"), "%Y-%m-%d %H:%M:%S"),
               "2023-08-02 17:08:21")

  # the ASCII export records local time, while the LCD records the UTC instant,
  # so the two agree only once the acquisition time zone is applied
  path_lcd <- system.file("multichannel_chrom.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))
  lcd <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                     progress_bar = FALSE)[[1]][[1]]
  expect_equal(format(attr(lcd, "run_datetime"), tz = "Europe/Paris",
                      "%Y-%m-%d %H:%M:%S"),
               format(attr(chrom, "run_datetime"), "%Y-%m-%d %H:%M:%S"))
})

test_that("'Shimadzu' LCD calibration factors are read from the raw data", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_lcd <- system.file("multichannel_chrom.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  # one factor per channel, indexed by the channel number in the stream name.
  # These are full scale over 2^22: 100000/2^22 for the two UV channels and
  # 13550/2^22 for the refractive index channel.
  expect_equal(read_sz_calibration_factor(path_lcd,
                                          c("LSS Raw Data", "Chromatogram Ch1")),
               100000/2^22)
  expect_equal(read_sz_calibration_factor(path_lcd,
                                          c("LSS Raw Data", "Chromatogram Ch2")),
               100000/2^22)
  expect_equal(read_sz_calibration_factor(path_lcd,
                                          c("LSS Raw Data", "Chromatogram Ch3")),
               13550/2^22)

  # the PDA max plot uses a separate status stream with a single record
  expect_equal(read_sz_calibration_factor(path_lcd,
                                          c("PDA 3D Raw Data", "Max Plot")), 1)

  # falls back on 1 when the status stream is missing
  expect_equal(read_sz_calibration_factor(path_lcd,
                                          c("No Such Storage", "Chromatogram Ch1")), 1)

  # the record also carries the gain factor and the value factor, which the
  # older 'LCsolution' files rely on since they have no `2D Data Item`
  status <- read_sz_chrom_status(path_lcd, c("LSS Raw Data", "Chromatogram Ch3"))
  expect_equal(status$CF, 13550/2^22)
  expect_equal(status$GF, 1)
  expect_equal(status$VF, 1000)
  expect_null(read_sz_chrom_status(path_lcd,
                                  c("No Such Storage", "Chromatogram Ch1")))

  # the record also names the unit selected for display, which is the only
  # source of it in files that have no `2D Data Item`
  expect_equal(status$unit, "mV")

  # the PDA max plot keeps its factors in a status stream of its own
  path_pda <- system.file("Anthocyanin.lcd",
                          package = "chromConverterExtraTests")
  if (file.exists(path_pda)){
    max_plot <- read_sz_chrom_status(path_pda, c("PDA 3D Raw Data", "Max Plot"))
    expect_equal(max_plot$unit, "mAU")
    expect_equal(max_plot$CF, 1)
    expect_equal(max_plot$VF, 1000)
  }

  # these must agree with the data item accompanying the raw data
  DI <- read_sz_2DDI(path_lcd, idx = 3)
  expect_equal(status$VF, 1/DI$detector.vf)
  expect_equal(status$unit, DI$detector.unit)

  # all three of this file's channels hold data, so all three have a record
  expect_equal(read_sz_chrom_status(path_lcd,
                                    c("LSS Raw Data", "Chromatogram Ch1"))$CF,
               100000/2^22)
  # a channel with no data has an empty record, reported as NA rather than zeros
  empty <- read_sz_chrom_status(path_lcd, c("LSS Raw Data", "Chromatogram Ch4"))
  expect_true(all(is.na(unlist(empty))))
  # reading past the end of the table gives nothing at all
  expect_null(read_sz_chrom_status(path_lcd,
                                   c("LSS Raw Data", "Chromatogram Ch10")))
})

test_that("'Shimadzu' LCD files report the container format version", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_lcd <- system.file("multichannel_chrom.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                   progress_bar = FALSE)
  meta <- extract_metadata(x, what = c("file_version", "software_version"))
  expect_equal(nrow(meta), 3)
  expect_equal(unique(meta$file_version), "5.01")
  expect_equal(unique(meta$software_version), "5.54 SP2")
})

test_that("'Shimadzu' LCD chromatograms report the unit of the values returned", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_lcd <- system.file("multichannel_chrom.lcd",
                          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  scaled <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                        progress_bar = FALSE)[[1]]
  expect_equal(unname(sapply(scaled, attr, "detector_y_unit")),
               rep("mV", 3))
  expect_true(all(sapply(scaled, attr, "scaled")))

  # each chromatogram must describe a single detector. A vector here used to
  # make the naming step generate more names than there are chromatograms
  expect_equal(vapply(scaled, function(x) length(attr(x, "detector")),
                      integer(1)),
               rep(1L, 3), ignore_attr = TRUE)
  expect_length(names(scaled), 3)

  # every channel in this file has a calibration factor, so the unscaled values
  # are converter counts rather than the base unit, and the unit reported for
  # display is left in place
  unscaled <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chroms",
                          scale = FALSE, progress_bar = FALSE)[[1]]
  expect_equal(unname(sapply(unscaled, attr, "detector_y_unit")),
               rep("mV", 3))
  expect_false(any(sapply(unscaled, attr, "scaled")))
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

  # check intensities. The intensities in the ASCII export are rounded to three
  # decimal places, so compare them on an absolute scale: they agree with the
  # decoded values to within half of the last retained digit.
  expect_lt(max(abs(as.numeric(x[[1]]) - as.numeric(x1[[1]][-1,]))), 5e-4)
  expect_lt(max(abs(as.numeric(x[[2]]) - as.numeric(x1[[2]][-1,]))), 5e-4)
  expect_lt(max(abs(as.numeric(x[[3]]) - as.numeric(x1[[3]][-1,]))), 5e-4)

  # exact values from the LCD decoder. The comparison against the ASCII export
  # above is limited by the rounding in the export, so pin the decoded values
  # directly as well.
  expect_equal(names(x), c("A, 260nm", "A, 210nm", "B"))
  expect_equal(dim(x[[1]]), c(3359, 1))
  expect_equal(dim(x[[2]]), c(3359, 1))
  expect_equal(dim(x[[3]]), c(3360, 1))
  expect_equal(x[[1]][1:5, 1],
               c(-0.00069141387939453125, -0.0016450881958007815,
                 -0.0014781951904296875, 0.00007152557373046875,
                 0.0017642974853515625),
               ignore_attr = TRUE)
  expect_equal(x[[2]][1:5, 1],
               c(-0.00081062316894531261, 0.000286102294921875,
                 -0.00126361846923828125, -0.0034093856811523442,
                 -0.0051021575927734375),
               ignore_attr = TRUE)
  expect_equal(x[[3]][1:5, 1],
               c(1.9383430480957031e-04, 2.2614002227783205e-05, 0,
                 7.4303150177001948e-05, -8.0764293670654297e-05),
               ignore_attr = TRUE)
  expect_equal(sum(x[[1]]), 190.69130420684814)
  expect_equal(sum(x[[2]]), 12629.3865442276)
  expect_equal(sum(x[[3]]), 5520.9204898357493)

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
                     peaktable_format = "original",
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
                     peaktable_format = "original",
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
