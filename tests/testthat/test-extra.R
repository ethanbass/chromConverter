# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

test_that("read_chroms can read 'Agilent' MS files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")

  path <- system.file("chemstation_MSD.MS",
                      package = "chromConverterExtraTests")

  skip_if_not(file.exists(path))

  x <- read_chroms(path, parser = "entab", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(95471, 3))
  expect_equal(attr(x, "parser"), "entab")
  expect_equal(colnames(x), c("rt", "mz", "intensity"))

  # export as mzML
  tmp <- tempdir()
  path_mzml <- fs::path(tmp, gsub(" ", "_", attr(x, "sample_name")),
                        ext = "mzML")
  on.exit(unlink(path_mzml))

  x1 <- read_chroms(path, parser = "chromconverter", what = "MS1",
                    format_out = "data.table",
                   progress_bar = FALSE,
                   export_format = "mzML", path_out = tmp)[[1]]
  expect_s3_class(x1, "data.table")
  expect_equal(attr(x1, "format_out"), "data.table")

  xx <- read_mzml(path_mzml)
  expect_equal(x1, xx$MS1[,-4], ignore_attr = TRUE)
  expect_equal(x1, as.data.frame(x), ignore_attr = TRUE)
  expect_equal(attr(x1,"sample_name"), attr(x1,"sample_name"))
  expect_equal(attr(x1,"source_sha1"), attr(x1,"source_sha1"))
  expect_equal(attr(x1,"time_unit"), attr(x1,"time_unit"))
  expect_equal(attr(x1,"run_datetime"), attr(x1,"run_datetime"))
  expect_equal(attr(x1,"operator"), attr(x1,"operator"))
  expect_equal(attr(x1,"method"), attr(x1,"method"))
  expect_equal(attr(x1,"detector"), attr(x1,"detector"))

  # rainbow
  x1 <- read_chroms(path, parser = "rainbow",
                    progress_bar = FALSE, precision = 0)[[1]]
  expect_equal(class(x1)[1], "matrix")
  expect_equal(dim(x1), c(2534, 841))
  expect_equal(attr(x1,"run_datetime"), attr(x1,"run_datetime"))
  expect_equal(attr(x1,"method"), attr(x1,"method"))
  expect_equal(attr(x1,"detector"), attr(x1,"detector"))

  x2 <- read_chroms(path, parser = "rainbow",
                    progress_bar = FALSE, data_format = "long",
                    format_out = "data.table",
                    precision = 0)[[1]]
  expect_s3_class(x2, "data.table")
  expect_equal(dim(x2), c(2131094, 3))
  expect_equal(colnames(x2), c("rt", "mz", "intensity"))
})

test_that("read_chroms can export 'Agilent' MS files as ANDI MS cdf", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")
  skip_if_not_installed("ncdf4")

  path <- system.file("chemstation_MSD.MS",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # export as CDF
  tmp <- tempdir()
  x2 <- read_chroms(path, parser = "entab", format_out = "data.table",
                    progress_bar = FALSE,
                    export_format = "cdf", path_out = tmp)[[1]]
  x2 <- x2[order(rt, mz)]

  path_cdf <- fs::path(tmp, gsub(" ", "_", attr(x2,"sample_name")), ext = "cdf")
  on.exit(unlink(path_cdf))

  xx <- read_cdf(path_cdf, format_out = "data.table")
  expect_equal(x2$rt, xx$MS1$rt/60, ignore_attr = TRUE)
  expect_equal(x2$intensity, xx$MS1$intensity, ignore_attr = TRUE)
  expect_equal(x2$mz, xx$MS1$mz, ignore_attr = TRUE, tolerance = .000001)
})

test_that("read_chroms can write mzML files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")

  path <- system.file("chemstation_MSD.MS",
                      package = "chromConverterExtraTests")

  tmp <- tempdir()

  x <- read_chroms(path, parser = "entab", progress_bar = FALSE,
                   format_out = "data.table",
                   export_format = "mzml", path_out = tmp, force = TRUE)[[1]]

  path_mzml <- fs::path(tmp, gsub(" ", "_", attr(x,"sample_name")), ext = "mzML")
  on.exit(unlink(path_mzml))

  x1 <- read_mzml(path_mzml, what = c("MS1","metadata"))
  expect_equal(x1$MS1[,c(1:3)], x, ignore_attr = TRUE)
  # expect_equal(x1$metadata$timestamp, attr(x,"run_datetime"))
  expect_equal(x1$metadata$source_file, basename(attr(x,"source_file")))

  # only works with `data_format == "long"`
  # time zone discrepancy between rainbow and entab
  # why doesn't work with format == data.table
  path_mzml_rb <- fs::path(tmp, fs::path_ext_remove(basename(path)),
                        ext = "mzML")
  on.exit(unlink(path_mzml_rb))

  x <- read_chroms(path, parser = "rainbow",
                   data_format = "long",
                  format_out = "data.frame",
                   progress_bar = FALSE,
                   export_format = "mzml", path_out = tmp,
                   force = TRUE)[[1]]
  x1 <- read_mzml(path_mzml_rb, what = c("MS1", "metadata"))
  expect_equal(x1$MS1[, c(1:3)], as.data.frame(x), ignore_attr = TRUE)
  expect_equal(x1$metadata$source_file, basename(attr(x,"source_file")))
  # expect_equal(x1$metadata$timestamp, attr(x,"run_datetime"))
})

test_that("read_chroms can convert CDF to mzML", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("HP_MS.CDF", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tmp <- tempdir()

  x <- read_chroms(path, progress_bar = FALSE, export_format = "mzml",
                   path_out = tmp, force = TRUE)[[1]]

  mzml_path <- fs::path(tmp, attr(x$MS1,"sample_name"), ext = "mzML")
  on.exit(unlink(mzml_path))

  x1 <- read_mzml(mzml_path, what = c("MS1", "metadata"))
  expect_equal(x1$MS1[,c(1:3)], x[[1]], ignore_attr = TRUE)
  expect_equal(x1$metadata$source_file, basename(attr(x$MS1,"source_file")))
})

test_that("read_chroms can read 'Agilent ChemStation' version 30 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_30.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, parser = "chromconverter", progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(38405, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "NVAC-6B1-S3R1")
  expect_equal(attr(x, "detector"), "G1315B")
  expect_equal(attr(x, "detector_y_unit"), "mAU")
  expect_equal(attr(x, "method"), "JCMONO1.M")
  expect_equal(attr(x, "time_unit"), "Minutes")

  x1 <- read_chroms(path, parser = "chromconverter", format_out = "data.frame",
                    data_format = "long", progress_bar = FALSE)[[1]]
  expect_equal(colnames(x1), c("rt","intensity"))
  expect_s3_class(x1[1], "data.frame")
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
})

test_that("read_chroms can read 'Agilent ChemStation' 31 files", {
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
  expect_equal(attr(x, "sample_name"), attr(x1, "sample_name"))

  expect_equal(attr(x, "detector"), "DAD")

  expect_equal(attr(x, "detector_id"), "G1315B")
  expect_equal(attr(x, "detector"), attr(x1, "detector"))

  expect_equal(attr(x, "detector_range"), c(250, 600))

  expect_equal(attr(x, "method"), "JCMONO1.M")
  expect_equal(attr(x, "method"), attr(x1, "method"))

  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "time_unit"), attr(x1, "time_unit"))
})

test_that("read_chroms can read 'Agilent ChemStation' version 81 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_81.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]

  # check metadata
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(2699, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "detector_id"), "HP G1530A")
  expect_equal(attr(x, "sample_name"), "5970 mix 10nG")
  expect_equal(attr(x, "time_unit"), "Minutes")

  # long format
  x1 <- read_chroms(path, progress_bar = FALSE,
                    format_out = "data.table", data_format = "long")[[1]]
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_s3_class(x1[1], "data.table")
  expect_equal(dim(x1), c(2699, 2))
  expect_equal(as.numeric(rownames(x)), x1[[1]])
  expect_equal(x[,1], x1[[2]], ignore_attr = TRUE)
})

test_that("read_chroms can read 'Agilent ChemStation' version 130 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_130.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(12750, 1))

  # check metadata
  expect_equal(attr(x, "sample_name"), "0-CN-6-6-PU")
  expect_equal(attr(x, "detector_y_unit"), "mAU")
  expect_equal(attr(x, "method"), "Phenolics_new2.M")
  expect_equal(attr(x, "time_unit"), "Minutes")

  # long format
  x1 <- read_chroms(path, data_format = "long", format_out = "data.table",
                   progress_bar = FALSE)[[1]]
  expect_equal(as.numeric(rownames(x)), x1[[1]])
  expect_equal(x[,1], x1[[2]], ignore_attr = TRUE)
  expect_s3_class(x1[1], c("data.table","data.frame"))
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_equal(dim(x1), c(12750, 2))
  expect_equal(attr(x1, "sample_name"), "0-CN-6-6-PU")
  expect_equal(attr(x1, "detector_y_unit"), "mAU")
  expect_equal(attr(x1, "method"), "Phenolics_new2.M")
  expect_equal(attr(x1, "time_unit"), "Minutes")
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

  # check metadata
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "STD_1_1mM-1MKHCO3")
  expect_equal(attr(x, "detector_y_unit"), "nRIU")
  expect_equal(attr(x, "time_unit"), "Minutes")

  # long format
  x1 <- read_chroms(path, progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_equal(as.numeric(rownames(x)), x1[[1]])
  expect_equal(x[,1], x1[[2]], ignore_attr = TRUE)
  expect_s3_class(x1[1], "data.frame")
  expect_equal(colnames(x1), c("rt", "intensity"))
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

  # check metadata
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "393006_A1_diol_Al")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "software"), "Mustang ChemStation")
  expect_equal(attr(x, "method"), "NGS Default Edit.M")
  expect_equal(attr(x, "time_unit"), "Minutes")

  # test scale argument
  x1 <- read_chroms(path, progress_bar = FALSE, scale=FALSE)[[1]]
  expect_equal(x, x1*attr(x1,"intensity_multiplier"), ignore_attr = TRUE)
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

  # check metadata
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "NI cat")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "software"), "Asterix ChemStation")
  expect_equal(attr(x, "method"), "Sine14.M")
  expect_equal(attr(x, "time_unit"), "Minutes")
})

test_that("read_chroms can read 'Agilent MassHunter' dad files", {
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
                    progress_bar = FALSE)[[1]]
  x1 <- read_chroms(path, format_in = "masshunter_dad", parser = "aston",
                    data_format = "long", format_out = "data.frame",
                    progress_bar = FALSE)[[1]]
  expect_equal(dim(x), c(66240, 3))
  expect_equal(colnames(x), c("rt", "lambda", "intensity"))
  expect_s3_class(x, "data.frame")
  # expect_equal(x[[1]], x1[[1]], ignore_attr = TRUE)
  expect_equal(attr(x, "parser"), "entab")

  expect_equal(attr(x1, "parser"), "aston")
  expect_equal(colnames(x1), c("rt", "lambda", "intensity"))
})


test_that("read_chroms can read 'Agilent ChemStation' version 181 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_181.D",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "agilent_d", progress_bar = FALSE)[[1]]

  expect_type(x, "list")
  expect_equal(class(x[[1]]), c("matrix","array"))
  expect_equal(dim(x[[2]]), c(5914, 1))

  # check metadata
  expect_equal(attr(x[[1]], "sample_name"), "blanc421")
  expect_equal(attr(x[[1]], "file_version"), "181")
  expect_equal(attr(x[[1]], "detector_y_unit"), "pA")
  expect_equal(attr(x[[1]], "method"), "DET3300.M")
  expect_equal(attr(x[[1]], "run_datetime"), as.POSIXct("2022-08-23 12:16:25",
                                                        tz="UTC"))
  expect_equal(attr(x[[1]], "time_unit"), "Minutes")

  expect_equal(attr(x[[2]], "sample_name"), "140+H")
  expect_equal(attr(x[[2]], "file_version"), "181")
  expect_equal(attr(x[[2]], "detector_y_unit"), "pA")
  expect_equal(attr(x[[2]], "method"), "DET3300.M")
  expect_equal(attr(x[[2]], "run_datetime"), as.POSIXct("2022-08-23 12:48:20",
                                                        tz="UTC"))
  expect_equal(attr(x[[2]], "time_unit"), "Minutes")

  # long format
  x1 <- read_chroms(path, format_in="agilent_d", data_format = "long",
                    format_out = "data.table",
                    progress_bar = FALSE)[[1]]
  expect_type(x1,"list")
  expect_s3_class(x1[[1]],"data.table")
  expect_equal(x[[1]][,1], x1[[1]][[2]], ignore_attr=TRUE)
  expect_equal(as.numeric(rownames(x[[1]])), x1[[1]][[1]])
  expect_equal(x[[2]][,1], x1[[2]][[2]], ignore_attr=TRUE)
  expect_equal(colnames(x1[[1]]), c("rt","intensity"))

  #error
  expect_equal(extract_metadata(x)[,c(1:8)], extract_metadata(x1)[,c(1:8)])
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
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_equal(colnames(x1), c("rt", "lambda", "intensity"))
  expect_s3_class(x1[1], "data.frame")
  expect_equal(attr(x1, "data_format"), "long")
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
  expect_equal(attr(x$MS,"vial"), "2:A,11")

  expect_equal(attr(x$CAD,"vial"), "2:A,11")
  expect_equal(attr(x$CAD,"detector_y_unit"), "mV")
  expect_equal(attr(x$CAD,"parser"), "rainbow")

  x1 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    parser = "chromconverter")[[1]]
  expect_equal(class(x1$CAD)[1], "matrix")
  expect_equal(x$CAD, x1$CAD, ignore_attr = TRUE)
  # attr(x1$CAD, "parser")

  x2 <- read_chroms(path, format_in = "waters_raw", progress_bar = FALSE,
                    what = "MS", data_format = "long", precision = 0)[[1]]
  expect_equal(nrow(x2$MS), nrow(x$MS)*ncol(x$MS))
  expect_equal(colnames(x2$MS), c("rt", "mz", "intensity"))
})

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

  expect_equal(x[[1]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[1]][,-1], tolerance=.001,
            ignore_attr=TRUE)
  expect_equal(x[[2]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[3]][,-1], tolerance=.001,
               ignore_attr=TRUE)
  expect_equal(x[[3]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[4]][,-1], tolerance=.001,
               ignore_attr=TRUE)
  expect_equal(x[[4]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[5]][,-1], tolerance=.001,
               ignore_attr=TRUE)
  expect_equal(x[[5]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[6]][,-1], tolerance=.001,
               ignore_attr=TRUE)
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


  expect_equal(x[[1]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[1]][,-1], tolerance=.01,
               ignore_attr=TRUE)
  expect_equal(x[[2]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[2]][,-1], tolerance=.001,
               ignore_attr=TRUE)
  expect_equal(x[[3]][,c(3,6:7,4:5,8:9,11,13:18,21:22)],x1[[3]][,-1], tolerance=.001,
               ignore_attr=TRUE)
})


test_that("read_chroms can read 'Agilent' .dx files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("agilent.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "agilent_dx", progress_bar = FALSE)[[1]]
  expect_equal(class(x)[1], "matrix")
  expect_equal(colnames(x),"intensity")
  expect_equal(dim(x), c(10000, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "data_format"), "wide")

  x1 <- read_chroms(path, format_in="agilent_dx", progress_bar = FALSE,
                    data_format = "long", format_out = "data.frame")[[1]]
  expect_s3_class(x1[1], "data.frame")
  expect_equal(dim(x1), c(10000, 2))
})

test_that("read_chroms can read 'Thermo' RAW files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_thermorawfileparser()

  path <- system.file("CirA.raw",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tmp <- tempdir()
  on.exit(unlink(tmp))
  x <- read_chroms(path, progress_bar = FALSE, path_out = tmp)[[1]]
  expect_type(x, "list")
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
  tmp <- tempdir()
  on.exit(unlink(tmp))
  x <- read_chroms(path, format_in = "msd", progress_bar = FALSE,
                   verbose = FALSE, export_format = "csv",
                   path_out = tmp)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(3032, 297))

  x <- read_chroms(path, format_in = "msd", progress_bar = FALSE,
                   verbose = FALSE, path_out = tmp)[[1]]
  expect_type(x, "list")
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

  x <- read_chroms(path, progress_bar = FALSE)[[1]]

  expect_equal(names(x), c("MS1", "TIC"))
  expect_equal(colnames(x$MS1), c("rt","mz","intensity"))
  expect_equal(colnames(x$TIC),"intensity")
  expect_s3_class(x$MS1, "data.frame")
  expect_equal(class(x$TIC)[1], "matrix")
  expect_true(all(dim(x$TIC) == c(621, 1)))
  expect_true(all(dim(x$MS1) == c(7638, 3)))

  x1 <- read_chroms(path, what=c("TIC"), data_format="long",
                    progress_bar = FALSE)[[1]]
  expect_equal(ncol(x1), 2)
  expect_equal(colnames(x1), c("rt", "intensity"))

  x2 <- read_chroms(path, what=c("MS1"), ms_format = "list",
                    progress_bar = FALSE)[[1]]
  expect_type(x2, "list")
  expect_equal(length(x2), length(unique(x$MS1$rt)))
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
               ignore_attr=TRUE)
})


test_that("Shimadzu QGD parser works", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")

  path_gqd <- system.file("B4NF.7_C23.qgd", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_gqd))

  tmp <- tempdir()
  mzml_path <- fs::path_ext_set(fs::path(tmp, basename(path_gqd)), ext = "mzML")
  on.exit(unlink(mzml_path))

  x <- read_chroms(path_gqd, find_files = FALSE, progress_bar = FALSE,
                   export_format = "mzml", path_out = tmp)[[1]]

  expect_equal(class(x[[1]])[1], "matrix")
  expect_equal(class(x[[2]])[1], "matrix")

  # check metadata equivalence
  expect_equal(attr(x$MS1, "operator"), "Admin")
  expect_equal(attr(x$MS1, "sample_name"), "B4NF.7_C23")
  expect_equal(attr(x$MS1, "sample_type"), "Unknown")
  expect_equal(attr(x$MS1, "parser"), "chromconverter")

  x1 <- read_mzml(mzml_path, what = c("MS1", "TIC", "metadata"))
  expect_equal(x1$MS1[,c(1:3)], as.data.frame(x$MS1[,-1]), ignore_attr = TRUE,
               tolerance = .0000001)
  expect_equal(x1$TIC[,"intensity"], as.data.frame(x$TIC[,"intensity"]),
               ignore_attr = TRUE)
  expect_equal(x1$metadata$source_file, basename(attr(x$MS1, "source_file")))
  # expect_equal(x1$metadata$timestamp, attr(x$MS1, "run_datetime"))

  ## write CDF
  # cdf_path <- fs::path_ext_set(fs::path(tmp, basename(path_gqd)), ext = "cdf")
  # on.exit(unlink(cdf_path))
  # export_cdf(list(B4NF.7_C23 = x), path_out = tmp, force=TRUE, show_progress = FALSE)
})

test_that("read_chroms can read Varian SMS", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_sms <- system.file("STRD15.SMS", package = "chromConverterExtraTests")
  path_mzml <- system.file("STRD15.mzML", package = "chromConverterExtraTests")

  skip_if_not(file.exists(path_sms))
  skip_if_not(file.exists(path_mzml))

  tmp <- tempdir()

  x <- read_chroms(path_sms, progress_bar = FALSE, export_format = "mzml",
                   path_out = tmp)[[1]]

  path_mzml_cc <- fs::path(tmp, attr(x$MS1, "sample_name"), ext = "mzML")
  on.exit(unlink(path_mzml_cc))

  x1 <- read_chroms(path_mzml, format_in = "mzml", progress_bar = FALSE)[[1]]
  x2 <- read_chroms(path_mzml_cc, format_in = "mzml", progress_bar = FALSE,
                    format_out = "data.frame")[[1]]

  # check equality of mass spec scans
  ms1_mzml <- x1$MS1[,-4]
  ms1_mzml$rt <- ms1_mzml$rt / 1000
  expect_equal(as.data.frame(x$MS1), ms1_mzml, tolerance = .0000001,
               ignore_attr = TRUE)
  expect_equal(as.data.frame(x$MS1), x2$MS1[,c(1:3)], ignore_attr=TRUE)

  # check equality of TIC
  expect_equal(x$TIC[, "rt"], x1$TIC$rt/1000, tolerance = .000001)
  # there is a slight discrepancy with the TICs generated by OpenChrom but mine
  # seem to be correct...?
  expect_equal(x$TIC[, "intensity"], x1$TIC$int, tolerance = .000001)
  # expect_equal(x$TIC, x2$TIC[,-3], ignore_attr = FALSE)

  # check equality of BPC
  expect_equal(x$BPC[, "rt"], x1$BPC$rt/1000, tolerance = .000001)
  expect_equal(x$BPC[, "intensity"], x1$BPC$int)

  # check metadata
  expect_equal(attr(x$TIC, "software_name"), "MS Workstation (Upgrade)")
  expect_equal(attr(x$TIC, "no_scans"), 3432)
  expect_equal(attr(x$TIC, "sample_name"), "STRD15")
  expect_equal(attr(x$TIC, "ms_params")$max_ric_scan, 1445)
  expect_equal(attr(x$TIC, "ms_params")$max_ric_val, 39285)
  expect_equal(attr(x$TIC, "ms_params")$max_ionization_time, c(0, 25000))
  expect_equal(attr(x$TIC, "ms_params")$temp_trap, 150)
  expect_equal(attr(x$TIC, "ms_params")$temp_manifold, 50)
  expect_equal(attr(x$TIC, "ms_params")$temp_transferline, 250)
  expect_equal(attr(x$TIC, "ms_params")$axial_modulation, 4)
  # attr(x$MS1, "run_datetime") # should be 8/8/2014 8:50 PM - 9:20 PM
  expect_equal(x2$metadata$source_file, basename(attr(x$MS1,"source_file")))
  # expect_equal(x2$metadata$timestamp, attr(x$MS1,"run_datetime")[1])
})

test_that("read_chroms can write Varian SMS to CDF", {

  # write CDF
  skip_if_not_installed("ncdf4")
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path_sms <- system.file("STRD15.SMS", package = "chromConverterExtraTests")
  skip_if_not(fs::file_exists(path_sms))

  tmp <- tempdir()

  path_cdf <- fs::path(tmp, fs::path_ext_remove(basename(path_sms)), ext = "cdf")
  on.exit(unlink(path_cdf))

  x <- read_chroms(path_sms, path_out = tmp, export_format = "cdf",
               progress_bar = FALSE)[[1]]

  x3 <- read_cdf(path_cdf, data_format = "long", format_out = "data.frame")
  x3$MS1$rt <- x3$MS1$rt/60
  x3$TIC$rt <- x3$TIC$rt/60

  expect_equal(as.data.frame(x$MS1), x3$MS1, ignore_attr=TRUE, tolerance=.0000001)
  expect_equal(as.data.frame(x$TIC), x3$TIC, ignore_attr=TRUE, tolerance=.0000001)
  expect_equal(attr(x3$MS1, "run_datetime"), attr(x$MS1, "run_datetime")[1])
  expect_equal(attr(x3$MS1, "time_unit"), "Seconds")
  expect_equal(attr(x3$MS1, "detector"), "MS")
  expect_equal(attr(x3$MS1, "sample_name"), "STRD15")
})


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
  expect_equal(as.character(attr(x[[1]], "data_format")), "wide")
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
})

test_that("get_filetype works as expected", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  # expect_equal(get_filetype(system.file("chemstation_MSD.MS",
  #                                       package = "chromConverterExtraTests")),
  # )
  expect_equal(get_filetype(system.file("B4NF.7_C23.qgd",
                                        package="chromConverterExtraTests")),
               "shimadzu_qgd")
  expect_equal(get_filetype(system.file("chemstation_181.D/FID1A.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_181")
  expect_equal(get_filetype(system.file("chemstation_179_mustang.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_179")
  expect_equal(get_filetype(system.file("openlab_131.uv",
                                        package="chromConverterExtraTests")),
               "openlab_131")
  expect_equal(get_filetype(system.file("chemstation_81.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_81")
  expect_equal(get_filetype(system.file("chemstation_30.ch",
                                        package="chromConverterExtraTests")),
               "chemstation_30")
  expect_equal(get_filetype(system.file("chemstation_31.uv",
                                        package="chromConverterExtraTests")),
               "chemstation_31")
  expect_equal(get_filetype(system.file("small.RAW",
                                        package="chromConverterExtraTests")),
               "thermoraw")
  expect_equal(get_filetype(system.file("FS19_214.gcd",
                                        package="chromConverterExtraTests")),
               "shimadzu_gcd")
  expect_equal(get_filetype(system.file("DCM1.SMS",
                                        package="chromConverterExtraTests")),
               "varian_sms")
  expect_equal(get_filetype(system.file("VARIAN1.CDF",
                                        package="chromConverterExtraTests")),
               "cdf")
})
