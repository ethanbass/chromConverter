# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_chroms can read 'Agilent MS' files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")

  path <- system.file("chemstation_MSD.MS",
                      package = "chromConverterExtraTests")

  skip_if_not(file.exists(path))

  x <- read_chroms(path, parser = "entab", progress_bar = FALSE)[[1]]
  expect_equal(class(x), "data.frame")
  expect_equal(dim(x), c(95471, 3))
  expect_equal(attr(x, "parser"), "entab")
  expect_equal(colnames(x), c("rt", "mz", "intensity"))

  x1 <- read_chroms(path, parser = "rainbow",
                    progress_bar = FALSE, precision = 0)[[1]]
  expect_equal(class(x1)[1], "matrix")
  expect_equal(dim(x1), c(2534, 841))

  x2 <- read_chroms(path, parser = "rainbow",
                    progress_bar = FALSE, data_format = "long",
                    precision = 0)[[1]]
  expect_equal(class(x2)[1], "matrix")
  expect_equal(dim(x2), c(2131094, 3))
  expect_equal(colnames(x2), c("rt", "mz", "intensity"))
})

test_that("read_chroms can read 'Agilent Chemstation' version 30 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_30.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, parser="chromconverter", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(38405, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "NVAC-6B1-S3R1")
  expect_equal(attr(x, "detector"), "G1315B")
  expect_equal(attr(x, "detector_unit"), "mAU")
  expect_equal(attr(x, "method"), "JCMONO1.M")
  expect_equal(attr(x, "time_unit"), "Minutes")

  x1 <- read_chroms(path, parser = "chromconverter", format_out = "data.frame",
                    data_format = "long", progress_bar = FALSE)[[1]]
  expect_equal(class(x1)[1], "data.frame")
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
})

test_that("read_chroms can read 'Agilent Chemstation' 31 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")

  path <- system.file("chemstation_31.uv", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE, parser = "chromconverter")[[1]]
  x1 <- read_chroms(path, progress_bar = FALSE, parser = "entab")[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(class(x1)[1], "matrix")

  expect_equal(dim(x), c(27659, 176))
  expect_equal(dim(x1), c(27659, 177))

  # check metadata
  expect_equal(attr(x1, "parser"), "entab")
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "NVAC-6B1-S3R1")
  expect_equal(attr(x, "detector"), "G1315B")
  expect_equal(attr(x, "detector_range"), c(250,600))
  expect_equal(attr(x, "method"), "JCMONO1.M")
  expect_equal(attr(x, "time_unit"), "Minutes")
})

test_that("read_chroms can read 'Agilent Chemstation' version 81 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_81.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(2699, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "detector_unit"), "pA")
  expect_equal(attr(x, "detector"), "HP G1530A")
  expect_equal(attr(x, "sample_name"), "5970 mix 10nG")
  expect_equal(attr(x, "time_unit"), "Minutes")

  x1 <- read_chroms(path, progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_equal(class(x1)[1], "data.frame")
  expect_equal(dim(x1), c(2699, 2))
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
})

test_that("read_chroms can read 'Agilent Chemstation' version 130 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_130.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(12750, 1))
  expect_equal(attr(x, "sample_name"), "0-CN-6-6-PU")
  expect_equal(attr(x, "detector_unit"), "mAU")
  expect_equal(attr(x, "method"), "Phenolics_new2.M")
  expect_equal(attr(x, "time_unit"), "Minutes")

  x <- read_chroms(path, data_format = "long", format_out = "data.frame",
                   progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "data.frame")
  expect_equal(dim(x), c(12750, 2))
})


test_that("read_chroms can read 'Agilent OpenLab' 179 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab_179.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(10000, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "STD_1_1mM-1MKHCO3")
  expect_equal(attr(x, "detector_unit"), "nRIU")
  expect_equal(attr(x, "time_unit"), "Minutes")

  x1 <- read_chroms(path, progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_equal(class(x1)[1], "data.frame")
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
})

test_that("read_chroms can read 'Agilent ChemStation' 179 files (8-byte format)", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_179_mustang.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(54704, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "393006_A1_diol_Al")
  expect_equal(attr(x, "detector_unit"), "pA")
  expect_equal(attr(x, "software"), "Mustang ChemStation")
  expect_equal(attr(x, "method"), "NGS Default Edit.M")
  expect_equal(attr(x, "time_unit"), "Minutes")

  # test scale argument
  x1 <- read_chroms(path, progress_bar = FALSE, scale=FALSE)[[1]]
  expect_equal(x, x1*attr(x1,"intensity_multiplier"))
})

test_that("read_chroms can read 'Agilent ChemStation' 179 (4-byte format)", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_179_asterix.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(22800, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "NI cat")
  expect_equal(attr(x, "detector_unit"), "pA")
  expect_equal(attr(x, "software"), "Asterix ChemStation")
  expect_equal(attr(x, "method"), "Sine14.M")
  expect_equal(attr(x, "time_unit"), "Minutes")
})

test_that("read_chroms can read 'Agilent Masshunter' dad files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")

  path <- system.file("masshunter.d/AcqData/DAD1.sp",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "masshunter_dad", parser = "entab",
                   progress_bar = FALSE)
  x1 <- read_chroms(path, format_in = "masshunter_dad", parser = "aston",
                    progress_bar = FALSE)

  expect_equal(dim(x[[1]]), c(240, 276))
  expect_equal(class(x[[1]])[1], "matrix")
  expect_equal(x[[1]], x1[[1]], ignore_attr = TRUE)
  expect_equal(attr(x[[1]], "parser"), "entab")
  expect_equal(attr(x1[[1]], "parser"), "aston")

  x <- read_chroms(path, format_in = "masshunter_dad", parser = "entab",
                    data_format = "long", format_out = "data.frame",
                    progress_bar = FALSE)
  x1 <- read_chroms(path, format_in = "masshunter_dad", parser = "aston",
                    data_format = "long", format_out = "data.frame",
                    progress_bar = FALSE)
  expect_equal(dim(x[[1]]), c(66240, 3))
  expect_equal(class(x[[1]]), "data.frame")
  # expect_equal(x[[1]], x1[[1]], ignore_attr = TRUE)
  expect_equal(attr(x[[1]], "parser"), "entab")
  expect_equal(attr(x1[[1]], "parser"), "aston")
})

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
                    format_out = "data.frame", data_format = "long")
  expect_equal(class(x1[[1]])[1], "data.frame")
  expect_equal(attr(x1[[1]], "data_format"), "long")
})

test_that("read_chroms can read 'Waters RAW' files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("waters_blue.raw", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                   precision = 0)[[1]]
  expect_equal(names(x), c("MS","UV","CAD"))
  expect_equal(dim(x$MS), c(725, 740))
  expect_equal(attr(x$MS, "parser"), "rainbow")
  expect_equal(attr(x$MS, "data_format"), "wide")
  expect_equal(attr(x$MS, "metadata")$polarity, "+")

  x1 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    parser = "chromconverter")[[1]]
  expect_equal(class(x1$CAD)[1], "matrix")
  expect_equal(x$CAD, x1$CAD, ignore_attr = TRUE)

  x2 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    what = "MS", data_format = "long", precision = 0)[[1]]
  expect_equal(nrow(x2$MS), nrow(x$MS)*ncol(x$MS))
  expect_equal(colnames(x2$MS), c("rt", "mz", "intensity"))
  # no metadata
  # expect_equal(attr(x1$CAD, "parser"), "chromconverter")
})

test_that("read_chroms can read 'Chromeleon' comma-separated files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chromeleon_comma.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "chromeleon_uv", progress_bar = FALSE)
  expect_equal(class(x[[1]])[1], "matrix")
  expect_equal(dim(x[[1]]), c(3241, 1))
  expect_equal(attr(x[[1]], "parser"), "chromconverter")
  expect_equal(attr(x[[1]], "data_format"), "wide")

  x1 <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")
  expect_equal(class(x1[[1]])[1], "data.frame")
  expect_equal(as.numeric(rownames(x[[1]])), x1[[1]][,1])
  expect_equal(x[[1]][,1], x1[[1]][,2], ignore_attr = TRUE)
  expect_equal(attr(x1[[1]], "data_format"), "long")
})

test_that("read_chroms can read 'Chromeleon' period-separated files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chromeleon_period.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE)
  expect_equal(class(x[[1]])[1], "matrix")
  expect_equal(dim(x[[1]]), c(10, 1))
  expect_equal(attr(x[[1]], "parser"), "chromconverter")

  x1 <- read_chroms(path, format_in = "chromeleon", progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")
  expect_equal(class(x1[[1]])[1], "data.frame")
  expect_equal(as.numeric(rownames(x[[1]])), x1[[1]][,1])
  expect_equal(x[[1]][,1], x1[[1]][,2], ignore_attr = TRUE)
})

test_that("read_peaklist can read `Shimadzu` ascii (PDA) files", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("shimadzuDAD_Anthocyanin.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_peaklist(path, format_in="shimadzu_dad", progress_bar = FALSE)[[1]]
  expect_equal(class(x), "list")
  expect_equal(length(x), 5)
  expect_equal(class(x[[1]]), "data.frame")
  expect_equal(dim(x[[1]]), c(133, 6))
  expect_equal(colnames(x[[1]]), c("sample", "rt", "start", "end", "area", "height"))
})

test_that("read_chroms can read 'Shimadzu' PDA files (ascii and LCD)", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_ascii <- system.file("shimadzuDAD_Anthocyanin.txt",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_ascii))

  path_lcd <- system.file("Anthocyanin.lcd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_chroms(path_ascii, format_in = "shimadzu_dad", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(4689, 328))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")

  x1 <- read_chroms(path_ascii, format_in = "shimadzu_dad", progress_bar = FALSE,
                    data_format = "long", format_out = "data.frame")[[1]]
  expect_equal(class(x1)[1], "data.frame")
  expect_equal(dim(x1), c(4689*328, 3))


  x2 <- read_chroms(path_lcd, progress_bar = FALSE)[[1]]
  expect_equal(dim(x2),c(4689,328))
  expect_equal(x, x2, ignore_attr = TRUE)

  # check metadata equivalence
  expect_equal(attr(x, "software_version"), attr(x2, "software_version"))
  expect_equal(attr(x, "method"), attr(x2, "method"))
  expect_equal(attr(x, "batch"), attr(x2, "batch"))
  expect_equal(attr(x, "operator"), attr(x2, "operator"))
  expect_equal(attr(x, "sample_name"), attr(x2, "sample_name"))
  expect_equal(attr(x, "sample_id"), attr(x2, "sample_id"))
  expect_equal(attr(x, "sample_injection_volume"), attr(x2, "sample_injection_volume"))
  expect_equal(as.numeric(attr(x, "time_range")), round(attr(x2, "time_range"), 3))
})

test_that("read_chroms can read 'Shimadzu' chromatograms from LCD files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_ascii <- system.file("shimadzuDAD_Anthocyanin.txt",
                            package = "chromConverterExtraTests")

  skip_if_not(file.exists(path_ascii))


  path_lcd <- system.file("Anthocyanin.lcd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_lcd))

  x <- read_chroms(path_ascii, format_in="shimadzu_ascii", progress_bar = FALSE,
                   what="chromatogram")[[1]][["lc"]]

  x1 <- read_chroms(path_lcd, format_in="shimadzu_lcd", what="chromatogram",
                    progress_bar = FALSE)[[1]]

  expect_equal(class(x1)[1], "matrix")
  expect_equal(dim(x1), c(30000, 1))
  all.equal(x[-1,1], x1[,1], check.attributes = FALSE)

  # unscaled
  x2 <- read_chroms(path_lcd, format_in = "shimadzu_lcd", what = "chromatogram",
                    progress_bar = FALSE, scale = FALSE)[[1]]
  all.equal(x[-1,1], x2[,1]*attr(x2, "intensity_multiplier"),
            check.attributes = FALSE)

  # check metadata equivalence
  expect_equal(attr(x, "software_version"), attr(x1, "software_version"))
  expect_equal(attr(x, "method"), attr(x1, "method"))
  expect_equal(attr(x, "batch"), attr(x1, "batch"))
  expect_equal(attr(x, "operator"), attr(x1, "operator"))
  expect_equal(attr(x, "sample_name"), attr(x1, "sample_name"))
  expect_equal(attr(x, "sample_id"), attr(x1, "sample_id"))
  expect_equal(attr(x, "sample_injection_volume"),
               attr(x1, "sample_injection_volume"))
  expect_equal(as.numeric(attr(x, "time_range")),
               round(attr(x1, "time_range"), 3))
  expect_equal(attr(x, "detector_unit"), attr(x1, "detector_unit"))
  expect_equal(attr(x, "intensity_multiplier"),
               attr(x1, "intensity_multiplier"))
})

test_that("read_chroms can read 'Shimadzu' PDA comma-separated file", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_ascii <- system.file("shimadzuDAD_comma.txt",
                            package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_ascii))

  x <- read_chroms(path_ascii, format_in = "shimadzu_dad", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(6096, 171))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")
  expect_equal(attr(x, "sample_name"), "Pinoresinol Standard")
})

test_that("read_chroms can read 'Agilent' dx files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("agilent.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "agilent_dx", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(10000, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")

  x1 <- read_chroms(path, format_in="agilent_dx", progress_bar = FALSE,
                    data_format = "long", format_out = "data.frame")[[1]]
  expect_equal(class(x1)[1], "data.frame")
  expect_equal(dim(x1), c(10000, 2))
})

test_that("read_chroms can read 'Thermo' RAW files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_thermorawfileparser()

  path <- system.file("20220404_CirA_D2_04.raw",
                      package = "chromConverterExtraTests")
  x <- read_chroms(path, progress_bar = FALSE)[[1]]
  expect_equal(class(x), "list")
  expect_equal(names(x), c("MS1", "MS2", "DAD", "BPC",
                           "TIC", "chroms", "metadata"))
})

test_that("read_chroms can use 'OpenChrom' parsers", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_openchrom()

  path <- system.file("DCM1.SMS", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "msd", progress_bar = FALSE,
                   verbose = FALSE, export_format = "csv")[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(3032, 297))

  x <- read_chroms(path, format_in = "msd", progress_bar = FALSE,
                   verbose = FALSE)[[1]]
  expect_equal(class(x), "list")
  expect_equal(dim(x$MS1), c(469732,4))
})

test_that("read_varian_peaklist function works", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("varian_peaklist.csv",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_varian_peaklist(path)
  expect_s3_class(x, "data.frame")
  expect_equal(dim(x), c(46476, 15))
})

test_that("read_cdf function can read peak tables", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("VARIAN1.CDF", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_cdf(path, what = "peak_table")
  # what about chromatograms
  expect_s3_class(x, "data.frame")
  expect_equal(dim(x), c(8,5))
})

test_that("read_chroms can read ANDI MS files", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("HP_MS.CDF", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, what=c("chromatogram","ms_spectra"), progress_bar = FALSE)[[1]]
  expect_equal(names(x), c("chromatogram", "ms_spectra"))
  expect_s3_class(x$ms_spectra, "data.frame")
  expect_equal(class(x$chromatogram)[1], "matrix")
  expect_true(all(dim(x$chromatogram) == c(621, 1)))
  expect_true(all(dim(x$ms_spectra) == c(7638, 3)))

  x1 <- read_chroms(path, what=c("chromatogram"), data_format="long",
                    progress_bar = FALSE)[[1]]
  expect_equal(ncol(x1), 2)

  x2 <- read_chroms(path, what=c("ms_spectra"), ms_format="list",
                    progress_bar = FALSE)[[1]]
  expect_type(x2, "list")
  expect_equal(length(x2), length(unique(x$ms_spectra$rt)))
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

