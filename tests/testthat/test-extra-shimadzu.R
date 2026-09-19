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

test_that("read_shimadzu_lcd can read 'Shimadzu' TLM (triple quadrupole) data", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_tlm_dda.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  expect_equal(get_sz_ms_format(path), "tlm")

  ms <- read_shimadzu_lcd(path, what = "MS", format_out = "data.frame")

  # one table per MS level, as from `read_mzml` and `read_cdf`
  expect_named(ms, c("MS1", "MS2"))
  x <- ms$MS1
  expect_s3_class(x, "data.frame")
  expect_equal(dim(x), c(22806403, 4))
  expect_equal(names(x), c("scan", "rt", "mz", "intensity"))
  expect_type(x$intensity, "integer")
  # only the product-ion scans have a precursor to report
  expect_equal(dim(ms$MS2), c(8791, 5))
  expect_equal(names(ms$MS2), c("scan", "rt", "precursor_mz", "mz",
                                "intensity"))
  expect_equal(attr(ms$MS1, "ms_level"), 1)
  expect_equal(attr(ms$MS2, "ms_level"), 2)

  # (asking for one level, and the `format_out` resolution, are checked on the
  # MRM file, which decodes in a tenth of a second rather than five)

  # per-scan summary, describing the level it is attached to
  scan_info <- attr(x, "scan_info")
  expect_s3_class(scan_info, "data.frame")
  expect_equal(dim(scan_info), c(4793, 7))
  expect_equal(names(scan_info), c("scan", "rt", "event", "ms_level",
                                   "polarity", "precursor_mz", "n_points"))
  expect_equal(unique(scan_info$ms_level), 1)
  expect_equal(nrow(attr(ms$MS2, "scan_info")), 273)
  expect_equal(unique(attr(ms$MS2, "scan_info")$ms_level), 2)
  expect_equal(sort(unique(c(scan_info$event,
                             attr(ms$MS2, "scan_info")$event))), 1:4)
  expect_equal(as.vector(table(scan_info$polarity)), c(2396, 2397))
  # the MS1 grid holds 7920 points and the product-ion grid 9520
  expect_equal(unique(scan_info$n_points), 7920)
  expect_equal(unique(attr(ms$MS2, "scan_info")$n_points), 9520)

  # precursors are recorded for product ion scans only
  expect_true(all(is.na(scan_info$precursor_mz)))
  expect_false(anyNA(attr(ms$MS2, "scan_info")$precursor_mz))
  # and the table reports the same precursor as the scan it came from
  info2 <- attr(ms$MS2, "scan_info")
  expect_equal(ms$MS2$precursor_mz,
               info2$precursor_mz[match(ms$MS2$scan, info2$scan)])

  # the final scan carries a truncated scan type, but the MS level is taken
  # from the low half of the field and still resolves
  expect_false(anyNA(scan_info$ms_level))

  # metadata: the instrument comes from `SystemInformation`, since the `Status`
  # record names the control platform the whole line shares
  expect_equal(attr(x, "instrument"), "LCMS-8030")
  expect_equal(attr(x, "instrument_config"), "TQ8030-60_M1.66")
  expect_equal(attr(x, "detector"), "MS")
  expect_equal(attr(x, "sample_name"), "flav_3D_=L39")
  expect_equal(attr(x, "time_range"), c(1, 27.99368), tolerance = 1e-6)
  expect_equal(attr(x, "source_file_format"), "shimadzu_lcd")
  expect_equal(attr(x, "data_format"), "long")

  # --- the spectra sum to the total ion current stream ----------------------
  # Decoding this file takes about five seconds, so the checks that need every
  # scan of it share the read above rather than repeating it.
  # the levels stay separate here too: this run is 22.8 million rows, and the
  # sums below need only the scan and intensity columns of each
  scan_info <- do.call(rbind, lapply(ms, attr, "scan_info"))
  tic <- read_shimadzu_lcd(path, what = "TIC")

  # The instrument stores its own TIC per spectrum. It agrees with the decoded
  # spectra only if the m/z grid is trimmed correctly at both ends and the
  # saturation flag is masked off, so this is the load-bearing check on the
  # profile decoding.
  totals <- rowsum(as.numeric(c(ms$MS1$intensity, ms$MS2$intensity)),
                   c(ms$MS1$scan, ms$MS2$scan))
  for (event in sort(unique(scan_info$event))){
    scans <- scan_info$scan[scan_info$event == event]
    expected <- as.numeric(totals[match(as.character(scans), rownames(totals))])
    expected[is.na(expected)] <- 0
    observed <- as.numeric(tic[[paste("Event", event)]][, "intensity"])
    expect_equal(expected, observed)
  }

  # the stored grid overhangs the acquired range by 10 bins at the bottom and
  # 9 at the top; untrimmed, MS1 scans would run from 209.0 to 1000.9
  expect_equal(min(x$mz), 210)
  expect_equal(max(x$mz), 1000)
  expect_true(all(abs(ms$MS1$mz*10 - round(ms$MS1$mz*10)) < 1e-6))
  expect_true(all(abs(ms$MS2$mz*10 - round(ms$MS2$mz*10)) < 1e-6))
})

test_that("read_shimadzu_lcd can read 'Shimadzu' TLM total ion currents", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_tlm_dda.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tic <- read_shimadzu_lcd(path, what = "tic")

  # one chromatogram per acquisition event
  expect_type(tic, "list")
  expect_equal(names(tic), paste("Event", 1:4))
  expect_equal(sapply(tic, nrow), c(2397, 165, 2396, 108),
               ignore_attr = TRUE)
  expect_true(all(sapply(tic, function(x) inherits(x, "matrix"))))
  expect_equal(colnames(tic[[1]]), "intensity")
  expect_equal(attr(tic[[1]], "instrument"), "LCMS-8030")

  tic_long <- read_shimadzu_lcd(path, what = "tic", data_format = "long",
                                format_out = "data.frame")
  expect_equal(names(tic_long[[1]]), c("rt", "intensity"))
  expect_equal(nrow(tic_long[[1]]), 2397)
  expect_equal(tic_long[[1]]$intensity, as.numeric(tic[[1]][, "intensity"]))

  # the per-cycle curve computed by the instrument
  sumtic <- read_sz_tlm_tic(path, what = "sumtic",
                                             read_metadata = FALSE)
  expect_equal(dim(sumtic), c(2397, 1))
})

test_that("'Shimadzu' TLM acquisition events are summarized", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_tlm_dda.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  events <- read_tlm_events(path)
  expect_s3_class(events, "data.frame")
  expect_equal(dim(events), c(4, 8))
  expect_equal(events$event, 1:4)
  expect_equal(events$n_scans, c(2397, 165, 2396, 108))
  expect_equal(events$ms_level, c(1, 2, 1, 2))
  expect_equal(events$polarity, c("positive", "positive",
                                  "negative", "negative"))
  expect_equal(events$scan_type, c("scan", "product ion scan",
                                   "scan", "product ion scan"),
               ignore_attr = TRUE)
  # stored ranges are the untrimmed grid bounds
  expect_equal(events$mz_min, c(209, 49, 209, 49))
  expect_equal(events$mz_max, c(1001, 1001, 1001, 1001))

  meta <- read_tlm_metadata(path)
  expect_equal(meta$instrument_config, "TQ8030-60_M1.66")
  expect_equal(read_sz_system_info(path)$SI.IN, "LCMS-8030")
  expect_equal(meta$firmware_version, "5.98SP1")
  expect_equal(meta$n_scans, 5066)
  expect_equal(meta$n_events, 4)
})

test_that("'Shimadzu' OLE files name the instrument from `SystemInformation`", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path_pda <- system.file("Anthocyanin.lcd",
                          package = "chromConverterExtraTests")
  path_gcd <- system.file("FS19_214.gcd",
                          package = "chromConverterExtraTests")
  path_qtof <- system.file("shimadzu_qtof.lcd",
                           package = "chromConverterExtraTests")
  path_tlm <- system.file("shimadzu_tlm_scan.lcd",
                          package = "chromConverterExtraTests")
  path_qgd <- system.file("B4NF.7_C23.qgd",
                          package = "chromConverterExtraTests")
  skip_if_not(all(file.exists(path_pda, path_gcd, path_qtof, path_tlm,
                              path_qgd)))

  si <- read_sz_system_info(path_pda)
  expect_equal(si$SI.IN, "Instrument2")
  expect_equal(si$SI.units, c(LC = "CBM-20A", PDA = "SPD-M20A"))

  # the instrument describes the file, and the detector the trace
  pda <- read_shimadzu_lcd(path_pda, what = "pda")
  expect_equal(attr(pda, "instrument"), "Instrument2")
  expect_equal(attr(pda, "detector_model"), "SPD-M20A")
  expect_equal(attr(pda, "channel_id"), "PDA.1.1.PDA.1.3D")

  # which is the name the ascii export of the same acquisition reports
  path_ascii <- system.file("shimadzuDAD_Anthocyanin.txt",
                            package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_ascii))
  ascii <- read_chroms(path_ascii, format_in = "shimadzu_dad",
                       progress_bar = FALSE)[[1]]
  expect_equal(attr(ascii, "instrument"), attr(pda, "instrument"))

  gcd <- read_shimadzu_gcd(path_gcd)
  expect_equal(attr(gcd, "instrument"), "GC-2014")
  expect_equal(attr(gcd, "detector_model"), "SFID1")

  # a mass spectrometry stream names no unit, so the mass spectrometer slot
  # from `SystemInformation` stands in as the detector
  expect_equal(read_sz_system_info(path_qtof)$SI.units[["LCMS-QP"]],
               "LCMS-9030")
  qtof <- read_shimadzu_lcd(path_qtof, what = "tic")
  expect_equal(attr(qtof, "instrument"), "LCMS-9030")
  expect_equal(attr(qtof, "detector_model"), "LCMS-9030")
  expect_null(attr(qtof, "channel_id"))

  tlm <- read_shimadzu_lcd(path_tlm, what = "tic")[[1]]
  expect_equal(attr(tlm, "instrument"), "LCMS8040")
  expect_equal(attr(tlm, "instrument_config"), "TQ8030-50_M1.13")
  # older software registers every triple quadrupole under the platform name
  expect_equal(attr(tlm, "detector_model"), "LCMS-3030")

  # the stream is absent from a `.qgd` file, which leaves the field empty
  expect_equal(read_sz_system_info(path_qgd), list())
  expect_true(is.na(attr(read_shimadzu_qgd(path_qgd, what = "tic"),
                         "instrument")))
})

test_that("`SystemInformation` is unpacked once per read, not once per trace", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("multichannel_chrom.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  unpacked <- 0L
  read_si <- read_sz_system_info
  local_mocked_bindings(read_sz_system_info = function(...){
    unpacked <<- unpacked + 1L
    read_si(...)
  })

  # three channels, each attaching metadata of its own, off one stream read:
  # the stream describes the file, so it is read beside the file properties
  # rather than from the field map, which runs once per trace
  x <- read_shimadzu_lcd(path, what = "chroms")
  expect_length(x, 3)
  expect_equal(unpacked, 1L)
})

test_that("every trace from one 'Shimadzu' file reports the same instrument", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_tlm_dda.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # a file holding both PDA and MS data used to name the PDA module on one
  # trace and the system on the other
  x <- read_shimadzu_lcd(path, what = c("pda", "tic"))
  meta <- extract_metadata(x, what = c("instrument", "detector",
                                       "detector_model"))
  expect_equal(unique(meta$instrument), "LCMS-8030")
  expect_equal(meta$detector, c("DAD", rep("MS", 4)))
  expect_equal(meta$detector_model, c("SPD-M20A", rep("LCMS-3030", 4)))

  # the multi-channel case is the same story without a mass spectrometer
  path_lc <- system.file("multichannel_chrom.lcd",
                         package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lc))
  ch <- read_shimadzu_lcd(path_lc, what = "chroms")
  meta_lc <- extract_metadata(ch, what = c("instrument", "detector_model",
                                           "channel_id"))
  expect_equal(unique(meta_lc$instrument), "HPLC RID")
  expect_equal(meta_lc$detector_model, c("SPD-20A", "SPD-20A", "RID-10A"))

  # the channel is what ties a trace to its peak table
  pt <- read_shimadzu_lcd(path_lc, what = "peak_table")
  expect_equal(names(pt), paste0("PT-", meta_lc$channel_id))
})

test_that("read_shimadzu_lcd can read 'Shimadzu' QTOF data", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_qtof.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  expect_equal(get_sz_ms_format(path), "qtof")

  # the TOF calibration warning is covered by its own test below

  ms <- read_shimadzu_lcd(path, what = "MS", format_out = "data.frame")
  expect_named(ms, c("MS1", "MS2"))

  # `precursor_mz` belongs to the product-ion spectra, so only `MS2` has it
  expect_equal(dim(ms$MS1), c(182972, 4))
  expect_equal(names(ms$MS1), c("scan", "rt", "mz", "intensity"))
  expect_equal(dim(ms$MS2), c(11325, 5))
  expect_equal(names(ms$MS2), c("scan", "rt", "precursor_mz", "mz",
                                "intensity"))
  expect_false(anyNA(ms$MS2$precursor_mz))

  # 13,929 of the 16,018 scans contain peaks; the rest are empty
  x <- sz_stitch_ms(ms)
  expect_equal(length(unique(x$scan)), 13929)
  expect_equal(range(x$scan), c(0, 16017))
  expect_equal(range(x$rt), c(0, 13.99997), tolerance = 1e-6)

  expect_equal(attr(ms$MS1, "sample_name"), "20190607_NM16")
  expect_equal(attr(ms$MS1, "detector"), "MS")
  expect_equal(attr(ms$MS1, "data_format"), "long")
  expect_equal(attr(ms$MS1, "source_file_format"), "shimadzu_lcd")

  # the TIC shares the file's time axis, in minutes like every other reader
  tic <- read_shimadzu_lcd(path, what = "tic")
  expect_true(is.matrix(tic))
  expect_equal(dim(tic), c(9091, 1))
  expect_equal(range(as.numeric(rownames(tic))), c(0, 13.99897),
               tolerance = 1e-6)
  expect_equal(attr(tic, "time_unit"), "Minutes")
  expect_equal(attr(tic, "detector"), "MS")
  expect_equal(attr(tic, "sample_name"), "20190607_NM16")

  # --- MS level, event and precursor ----------------------------------------
  # sharing the decode above rather than reading the file again
  si <- attr(x, "scan_info")

  # one row per spectrum, including those that hold no peaks
  expect_s3_class(si, "data.frame")
  expect_equal(nrow(si), 16018)
  expect_equal(names(si), c("scan", "rt", "event", "ms_level", "cycle",
                            "polarity", "precursor_mz", "n_peaks"))
  expect_equal(si$scan, 0:16017)
  expect_equal(sum(si$n_peaks), nrow(x))

  # MS level is the high word of the flags field; these counts are what
  # 'ProteoWizard' reports for this file
  expect_equal(as.vector(table(si$ms_level)), c(9091L, 6927L))
  expect_equal(sort(unique(si$event)), c(1, 2, 3, 4))
  # event 1 collects the survey scans, 2-4 the product-ion scans
  expect_equal(unique(si$ms_level[si$event == 1]), 1)
  expect_equal(unique(si$ms_level[si$event > 1]), 2)
  expect_true(all(si$polarity == "positive"))

  # every product-ion scan has a selected precursor, no survey scan does
  expect_true(all(!is.na(si$precursor_mz[si$ms_level == 2])))
  expect_true(all(is.na(si$precursor_mz[si$ms_level == 1])))
  dda <- read_qtof_dda(path)
  expect_equal(nrow(dda), sum(si$ms_level == 2))
  expect_equal(sort(dda$scan), sort(si$scan[si$ms_level == 2] + 1))

  # the precursor should be a real peak in the survey scan of its own cycle
  expect_true("precursor_mz" %in% names(ms$MS2))
  ms2 <- si[si$ms_level == 2 & si$n_peaks > 0, ]
  found <- vapply(head(seq_len(nrow(ms2)), 40), function(i){
    parent <- si$scan[si$ms_level == 1 & si$cycle == ms2$cycle[i]][1]
    peaks <- ms$MS1$mz[ms$MS1$scan == parent]
    length(peaks) > 0 &&
      min(abs(peaks - ms2$precursor_mz[i]))/ms2$precursor_mz[i] < 2e-5
  }, logical(1))
  expect_true(all(found))
})

test_that("'Shimadzu' QTOF calibration is read from the file", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_qtof.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # `Mass Parameters` records a voltage whose sign follows the ion polarity
  expect_equal(read_qtof_polarity(path), "positive")

  # `TOF Calibration Table` stores 5 calibrants per polarity in 3 replicate
  # sets; the second set is a factory default rather than a measurement
  cal <- read_sz_qtof_calibration(path)
  expect_named(cal, c("A", "B"))
  expect_equal(cal[["A"]], 4.6906089e+13, tolerance = 1e-6)
  expect_equal(cal[["B"]], 6.7674066e+11, tolerance = 1e-6)

  # including the factory set moves `B` by a factor of ~1.7
  bad <- read_sz_qtof_calibration(path, drop_rep = integer(0))
  expect_gt(bad[["B"]], 1e12)

  # one 24-byte index record per spectrum, including the empty ones
  offsets <- read_qtof_spectrum_index(path)
  expect_length(offsets, 16018)
  expect_true(all(diff(offsets) > 0))

  # the reference compounds infused to correct the mass axis
  expect_equal(read_qtof_lock_mass(path),
               c(121.050873, 922.009798))
})

test_that("'Shimadzu' QTOF m/z are checked against the acquisition range", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_qtof.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # the m/z range the instrument was told to scan, from `Mass Parameters`
  window <- read_qtof_mz_range(path)
  expect_equal(window, c(100, 2000))

  x <- sz_read_ms(path)
  expect_gte(min(x$mz), window[1])
  expect_lte(max(x$mz), window[2])
  # a correctly calibrated file raises nothing
  expect_no_warning(check_qtof_mz_range(x$mz, window))

  # using the calibration block for the wrong polarity is a ~3% error, which
  # on a 100-1700 window puts the top of the range near 1750
  expect_warning(
    check_qtof_mz_range(c(103.008, 1750.464), c(100, 1700)),
    "outside the acquisition range")
  # and a few ppm of slack does not
  expect_no_warning(
    check_qtof_mz_range(c(100.04, 1699.998), c(100, 1700)))
  # a missing window skips the check rather than erroring
  expect_no_warning(check_qtof_mz_range(x$mz, NULL))

  # --- the polarity and scan window are reported ----------------------------
  expect_equal(attr(x, "polarity"), "positive")
  expect_equal(attr(x, "mz_range"), c(100, 2000))
  expect_gte(min(x$mz), 100)
  expect_lte(max(x$mz), 2000)

  # the TIC describes the same acquisition
  tic <- read_shimadzu_lcd(path, what = "tic")
  expect_equal(attr(tic, "polarity"), "positive")
  expect_equal(attr(tic, "mz_range"), c(100, 2000))
})

test_that("'Shimadzu' QTOF intensities are scaled by the accumulation count", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzu_qtof.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))


  # Raw detector counts are accumulated over all TOF transients in a scan and
  # normalized to a nominal 100. The divisor is read from the `Status` stream,
  # which records 376 accumulations per scan for this acquisition.
  expect_equal(read_qtof_int_scale(path), 3.76)

  x <- sz_read_ms(path)
  raw <- sz_read_ms(path, scale = FALSE)

  # Checked against a 'ProteoWizard' conversion of this file rather than
  # against this parser, so the assertion is independent of it. The first scan
  # is spelled out for readability; the slice covers 200 spectra.
  vendor_mz <- c(129.054300, 130.967500, 141.958800, 147.064400,
                 158.961600, 182.984800, 184.985000, 201.112700)
  expect_equal(head(x$mz, 8), vendor_mz, tolerance = 2e-6)

  gt <- sz_ground_truth("shimadzu_qtof")
  err <- vapply(unique(gt$scan), function(s){
    g <- gt[gt$scan == s, ]
    o <- x[x$scan == s, ]
    if (nrow(o) != nrow(g)) return(NA_real_)
    max(abs(sort(o$mz) - sort(g$mz))/sort(g$mz))
  }, numeric(1))
  expect_false(anyNA(err))
  # the file's own calibration and mass correction reproduce the reported m/z
  # to a fraction of a ppm across every spectrum in the slice
  expect_lt(stats::median(err), 1e-6)
  expect_lt(max(err), 5e-6)
  # intensities are exact, unlike the profile formats
  for (s in head(unique(gt$scan), 60)){
    g <- gt[gt$scan == s, ]
    o <- x[x$scan == s, ]
    expect_equal(o$intensity[order(o$mz)], g$intensity[order(g$mz)])
  }

  # without the mass correction the tune-time calibration alone is ~5 ppm low
  uncorrected <- sz_read_ms(path, lock_mass = FALSE)
  expect_false(isTRUE(all.equal(uncorrected$mz, x$mz)))
  expect_gt(median(abs(head(uncorrected$mz, 8) - vendor_mz)/vendor_mz), 4e-6)
  expect_lt(median(abs(head(x$mz, 8) - vendor_mz)/vendor_mz), 1e-6)
  # the correction moves only the mass axis
  expect_equal(uncorrected$intensity, x$intensity)
  expect_equal(uncorrected$rt, x$rt)
  expect_equal(head(x$intensity, 8),
               c(768, 269, 615, 234, 1050, 356, 228, 1769))
  expect_equal(head(raw$intensity, 8),
               c(2886, 1013, 2314, 880, 3947, 1340, 856, 6650))

  # scaling is round-half-up of raw / 3.76 across every peak in the file
  expect_equal(x$intensity, floor(raw$intensity/3.76 + 0.5))
  expect_true(all(x$intensity == as.integer(x$intensity)))

  # `scale = FALSE` leaves the stored counts untouched
  expect_equal(x$mz, raw$mz)
  expect_false(isTRUE(all.equal(x$intensity, raw$intensity)))
})

test_that("read_shimadzu_lcd can read 'Shimadzu' MRM data", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("shimadzu_tlm_mrm.lcd",
          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- sz_read_ms(path)
  si <- attr(x, "scan_info")

  # 1165 scans, each monitoring two transitions of one precursor
  expect_equal(dim(x), c(2330, 5))
  expect_equal(names(x), c("scan", "rt", "precursor_mz", "mz", "intensity"))
  expect_equal(nrow(si), 1165)
  expect_equal(unique(si$event), 1)
  expect_equal(unique(si$ms_level), 2)
  expect_equal(unique(si$polarity), "positive")
  expect_equal(unique(x$precursor_mz), 544.2)
  expect_equal(sort(unique(x$mz)), c(320.95, 397.3))
  expect_false(anyNA(x$precursor_mz))

  expect_equal(read_tlm_events(path)$scan_type, "MRM")

  # This file decodes in a fraction of a second, so the checks that need a
  # decode of their own are done here rather than on one of the large files.
  # Asking for a level returns that table on its own, whatever case it is
  # written in, and spectra are long, so a `matrix` request resolves to a
  # `data.table`.
  expect_equal(read_shimadzu_lcd(path, what = "ms2",
                                 format_out = "data.frame"),
               x, ignore_attr = c("scan_info", "source_sha1"))
  expect_s3_class(read_shimadzu_lcd(path, what = "MS2"), "data.table")
  expect_s3_class(read_shimadzu_lcd(path, what = "MS2",
                                    format_out = "data.table"), "data.table")
  expect_warning(read_shimadzu_lcd(path, what = "MS1"), "MS1 data not found")

  # against a 'ProteoWizard' conversion of the same file
  gt <- sz_ground_truth("shimadzu_tlm_mrm")
  for (s in head(unique(gt$scan), 50)){
    g <- gt[gt$scan == s, ]
    o <- x[x$scan == s, ]
    expect_equal(nrow(o), nrow(g))
    expect_equal(sort(o$mz), sort(g$mz))
    expect_equal(o$intensity[order(o$mz)], g$intensity[order(g$mz)])
  }
})

test_that("read_shimadzu_lcd can read MRM data with many events and both polarities", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("shimadzu_tlm_mrm_multi.lcd",
          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- sz_read_ms(path)
  si <- attr(x, "scan_info")

  expect_equal(dim(x), c(119023, 5))
  expect_equal(nrow(si), 104232)
  # 179 retention-time scheduled events, each covering about two minutes
  expect_length(unique(si$event), 179)
  spans <- vapply(split(si$rt, si$event), function(z) diff(range(z)), numeric(1))
  expect_lt(max(spans), 4.01)
  # 158 events are negative and 21 positive; where their windows overlap the
  # instrument alternates polarity from one scan to the next
  expect_equal(sort(unique(si$polarity)), c("negative", "positive"))
  pol <- si$polarity[order(si$scan)]
  expect_gt(length(rle(pol)$lengths), 1000)
  expect_equal(unique(si$ms_level), 2)
  expect_equal(unique(read_tlm_events(path)$scan_type), "MRM")
  # most scans hold one transition, some up to four
  expect_equal(sort(unique(as.vector(table(x$scan)))), c(1, 2, 3, 4))

  gt <- sz_ground_truth("shimadzu_tlm_mrm_multi")
  for (s in head(unique(gt$scan), 100)){
    g <- gt[gt$scan == s, ]
    o <- x[x$scan == s, ]
    expect_equal(sort(o$mz), sort(g$mz))
    expect_equal(o$intensity[order(o$mz)], g$intensity[order(g$mz)])
    expect_equal(si$ms_level[si$scan == s], unique(g$ms_level))
  }
})

test_that("read_shimadzu_lcd describes each MRM event in its TIC metadata", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path_mrm <- system.file("shimadzu_tlm_mrm.lcd",
              package = "chromConverterExtraTests")
  path_multi <- system.file("shimadzu_tlm_mrm_multi.lcd",
                package = "chromConverterExtraTests")
  skip_if_not(all(file.exists(path_mrm, path_multi)))

  # a single-event acquisition collapses to one chromatogram, which still
  # carries the transitions it monitors
  x <- read_shimadzu_lcd(path_mrm, what = "tic")
  expect_equal(nrow(x), 1165)
  expect_equal(attr(x, "scan_type"), "MRM")
  expect_equal(attr(x, "ms_level"), 2)
  expect_equal(attr(x, "polarity"), "positive")
  expect_equal(attr(x, "precursor_mz"), 544.2)
  expect_equal(attr(x, "product_mz"), c(320.95, 397.3))
  # `time_range` describes the event, not the run
  expect_equal(attr(x, "time_range"), c(2.978, 6.9744))

  y <- read_shimadzu_lcd(path_multi, what = "tic")
  expect_length(y, 179)
  expect_equal(names(y)[c(1, 179)], c("Event 1", "Event 179"))
  expect_equal(unique(vapply(y, attr, character(1), "scan_type")), "MRM")
  # a scheduled acquisition gives every event its own retention-time window
  expect_equal(length(unique(vapply(y, function(z)
    paste(attr(z, "time_range"), collapse = "/"), character(1)))), 179)
  expect_equal(sort(table(vapply(y, attr, character(1), "polarity"))),
               structure(c(positive = 21L, negative = 158L), dim = 2L,
                         dimnames = list(c("positive", "negative")),
                         class = "table"), ignore_attr = TRUE)

  expect_equal(attr(y[["Event 1"]], "precursor_mz"), 626.4)
  expect_equal(attr(y[["Event 1"]], "product_mz"), 308.2)
  expect_equal(attr(y[["Event 1"]], "time_range"), c(11.14325, 13.13675))
  expect_equal(attr(y[["Event 179"]], "polarity"), "negative")

  # an event monitoring several transitions reports all of them, and the two
  # precursors of event 175 are not collapsed to one
  expect_equal(attr(y[["Event 16"]], "product_mz"), c(104.2, 184.1))
  expect_equal(attr(y[["Event 175"]], "precursor_mz"), c(279.35, 281.35))
  expect_equal(attr(y[["Event 175"]], "product_mz"), c(59.1, 279.35, 281.35))

  # the event table pairs each precursor with its product, which the two
  # attributes above cannot do
  ev <- read_tlm_events(path_multi)
  expect_equal(nrow(ev), 179)
  expect_equal(ev$transitions[[16]],
               data.frame(precursor_mz = c(482.3, 482.3),
                          product_mz = c(104.2, 184.1)))
  expect_equal(ev$transitions[[175]],
               data.frame(precursor_mz = c(281.35, 279.35, 279.35, 281.35),
                          product_mz = c(59.1, 59.1, 279.35, 281.35)))
  # the summary column is NA where an event has more than one precursor
  expect_true(is.na(ev$precursor_mz[175]))
  expect_equal(sum(is.na(ev$precursor_mz)), 1)
})

test_that("read_shimadzu_lcd reports a scanned m/z range rather than a product", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path_scan <- system.file("shimadzu_tlm_scan.lcd",
               package = "chromConverterExtraTests")
  path_sim <- system.file("shimadzu_tlm_sim.lcd",
              package = "chromConverterExtraTests")
  path_mrm <- system.file("shimadzu_tlm_mrm.lcd",
              package = "chromConverterExtraTests")
  skip_if_not(all(file.exists(path_scan, path_sim, path_mrm)))

  # a full scan and a product-ion scan sweep a range of masses, so neither
  # names a product
  x <- read_shimadzu_lcd(path_scan, what = "tic")
  expect_length(x, 2)
  expect_equal(vapply(x, attr, character(1), "scan_type"),
               c("Event 1" = "scan", "Event 2" = "product ion scan"))
  expect_equal(attr(x[["Event 1"]], "mz_range"), c(49, 2001))
  expect_equal(attr(x[["Event 2"]], "mz_range"), c(99, 2001))
  expect_null(attr(x[["Event 1"]], "product_mz"))
  expect_null(attr(x[["Event 2"]], "product_mz"))
  # neither event reports one precursor: the survey scan has none, and the
  # product-ion event is data-dependent, retuning Q1 every cycle (43 distinct
  # precursors across its 621 scans, the most common covering a quarter of
  # them), so no single value describes it
  expect_true(is.na(attr(x[["Event 1"]], "precursor_mz")))
  expect_true(is.na(attr(x[["Event 2"]], "precursor_mz")))
  # the per-scan precursors are still there, in `scan_info`
  si <- attr(read_sz_tlm(path_scan)$MS2, "scan_info")
  expect_gt(length(unique(si$precursor_mz)), 1)
  expect_false(anyNA(si$precursor_mz))

  # SIM passes its ions through undissociated, so they are precursors and the
  # Q3 masses that repeat them are not reported
  y <- read_shimadzu_lcd(path_sim, what = "tic")
  expect_equal(attr(y[["Event 1"]], "scan_type"), "SIM")
  expect_equal(attr(y[["Event 1"]], "precursor_mz"),
               c(1041.3, 1044.2, 1058.2, 1200.4, 1448))
  expect_null(attr(y[["Event 1"]], "product_mz"))
  expect_null(attr(y[["Event 1"]], "mz_range"))
  expect_equal(attr(y[["Event 2"]], "mz_range"), c(39, 1101))

  # an MRM event is the other way round: fixed transitions, no scanned range
  z <- read_shimadzu_lcd(path_mrm, what = "tic")
  expect_equal(attr(z, "product_mz"), c(320.95, 397.3))
  expect_null(attr(z, "mz_range"))
})

test_that("extract_metadata can collapse a multi-valued field", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("shimadzu_tlm_mrm.lcd",
          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))
  x <- read_shimadzu_lcd(path, what = "tic")

  # by default a field holding two values is spread over two columns
  m <- extract_metadata(x, c("scan_type", "product_mz"))
  expect_equal(as.numeric(m$product_mz1), 320.95)
  expect_equal(as.numeric(m$product_mz2), 397.3)
  # and is not reported missing on account of the renaming
  expect_no_warning(extract_metadata(x, c("scan_type", "product_mz")))

  m <- extract_metadata(x, c("scan_type", "product_mz"), collapse = TRUE)
  expect_equal(m$product_mz, "320.95, 397.3")
  expect_warning(extract_metadata(x, c("scan_type", "not_a_field")),
                 "not found")
})

test_that("read_shimadzu_lcd can read 'Shimadzu' SIM data", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("shimadzu_tlm_sim.lcd",
          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- sz_read_ms(path)
  si <- attr(x, "scan_info")

  # one SIM event monitoring 5 ions, plus five product-ion scan events
  expect_equal(dim(x), c(56589, 5))
  expect_equal(nrow(si), 8765)
  expect_length(unique(si$event), 6)
  expect_equal(as.vector(table(si$ms_level)), c(1461L, 7304L))
  expect_equal(sort(table(read_tlm_events(path)$scan_type)),
               structure(c(SIM = 1L, `product ion scan` = 5L),
                         dim = 2L, dimnames = list(c("SIM", "product ion scan")),
                         class = "table"), ignore_attr = TRUE)
  # SIM is reported as MS1: its Q1 and Q3 are equal, so nothing is selected
  # after the collision cell
  expect_equal(unique(si$ms_level[si$event == 1]), 1)
  # every scan reports the precursor it came from: the SIM events their Q1,
  # the product-ion events the precursor they were told to isolate
  expect_false(anyNA(x$precursor_mz))
  expect_equal(sum(x$precursor_mz %in% unique(x$precursor_mz[x$scan %in%
                     si$scan[si$ms_level == 1]])), 1461 * 5)

  gt <- sz_ground_truth("shimadzu_tlm_sim")
  sim <- gt[gt$ms_level == 1, ]
  for (s in unique(sim$scan)){
    g <- sim[sim$scan == s, ]
    o <- x[x$scan == s, ]
    expect_equal(nrow(o), 5)
    expect_equal(sort(o$mz), sort(g$mz))
    expect_equal(o$intensity[order(o$mz)], g$intensity[order(g$mz)])
  }
})

test_that("read_shimadzu_lcd can read a 'Shimadzu' full scan and product-ion run", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("shimadzu_tlm_scan.lcd",
          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  ms <- read_shimadzu_lcd(path, what = "MS", format_out = "data.frame",
                          sparse = FALSE, read_metadata = FALSE)
  # Kept as two tables rather than stitched: with the zeros kept this run is
  # ~24 million rows, and copying and sorting that to put the levels back
  # together costs more than the decode itself.
  si <- do.call(rbind, lapply(ms, attr, "scan_info"))
  si <- si[order(si$scan), ]
  row.names(si) <- NULL

  expect_equal(nrow(si), 2391)
  expect_equal(as.vector(table(si$ms_level)), c(1770L, 621L))
  expect_equal(sort(read_tlm_events(path)$scan_type),
               c("product ion scan", "scan"))
  expect_equal(unique(si$polarity), "negative")
  # a full scan has no precursor to report; the product-ion scans do
  expect_false("precursor_mz" %in% names(ms$MS1))
  expect_true("precursor_mz" %in% names(ms$MS2))

  # The m/z grid matches 'ProteoWizard' exactly, but the intensities do not:
  # the vendor returns a ringing-suppressed profile, ~7% high on total ion
  # current and ~20% low at the apex. The raw values are validated against the
  # file's own `TIC Data` instead.
  gt <- sz_ground_truth("shimadzu_tlm_scan")
  # the slice covers a handful of scans, so the rows for those are taken out
  # once instead of the whole table being searched inside the loop
  slice <- do.call(rbind, lapply(ms, function(x){
    x[x$scan %in% unique(gt$scan), c("scan", "mz")]
  }))
  for (s in unique(gt$scan)){
    g <- gt[gt$scan == s, ]
    o <- slice[slice$scan == s, ]
    expect_equal(nrow(o), nrow(g))
    expect_equal(o$mz[order(o$mz)], g$mz[order(g$mz)])
  }
  tic <- matrix(read_ole_uint32(path, c("TLM Raw Data", "TIC Data")),
                ncol = 2L, byrow = TRUE)[, 1]
  totals <- rowsum(as.numeric(c(ms$MS1$intensity, ms$MS2$intensity)),
                   c(ms$MS1$scan, ms$MS2$scan))
  expect_equal(as.numeric(totals), as.numeric(tic))
})

test_that("read_shimadzu_lcd can read negative-mode 'Shimadzu' QTOF data", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("shimadzu_qtof_neg.lcd",
          package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # the calibration table holds a block per polarity; this file needs the
  # negative one, whose coefficients differ from the positive block by ~1.5%
  expect_equal(read_qtof_polarity(path), "negative")
  cal <- read_sz_qtof_calibration(path)
  expect_equal(cal[["A"]], 4.7595880e+13, tolerance = 1e-6)
  expect_equal(read_qtof_lock_mass(path),
               c(112.985587, 601.978977, 1033.988109, 1633.949786))
  expect_equal(read_qtof_mz_range(path), c(100, 1700))

  ms <- read_shimadzu_lcd(path, what = "MS", format_out = "data.frame")
  x <- sz_stitch_ms(ms)
  si <- attr(x, "scan_info")
  # the shared columns of the two levels; `precursor_mz` is on `MS2` alone
  expect_equal(dim(x), c(1627195, 4))
  expect_true("precursor_mz" %in% names(ms$MS2))
  expect_equal(nrow(si), 8383)
  expect_equal(unique(si$polarity), "negative")
  expect_equal(as.vector(table(si$ms_level)), c(3172L, 5211L))
  # this file also exercises the high byte on the intensity-width field
  expect_gte(min(x$mz), 100 * (1 - 1e-5))
  expect_lte(max(x$mz), 1700 * (1 + 1e-5))

  gt <- sz_ground_truth("shimadzu_qtof_neg")
  for (s in head(unique(gt$scan), 15)){
    g <- gt[gt$scan == s, ]
    o <- x[x$scan == s, ]
    expect_equal(nrow(o), nrow(g))
    om <- sort(o$mz); gm <- sort(g$mz)
    # the file's own calibration reproduces the reported m/z to well under 1 ppm
    expect_lt(max(abs(om - gm)/gm), 1e-6)
    expect_equal(o$intensity[order(o$mz)], g$intensity[order(g$mz)])
  }
})
