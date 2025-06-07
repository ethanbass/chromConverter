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

test_that("read_chroms can read 'Agilent ChemStation' version 30 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_30.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, parser = "chromconverter", progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(38405, 1))
  expect_equal(head(rownames(x), 1), "-.00133333333333333")
  expect_equal(tail(rownames(x), 1), "32.002")

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
  expect_equal(head(x1$rt, 1), -.00133333333333333)
  expect_equal(tail(x1$rt, 1), 32.002)
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

  expect_equal(head(rownames(x), 1), "0.000333333333333333")
  expect_equal(head(rownames(x1), 1), "0.000333333333333333")

  expect_equal(tail(rownames(x), 1), "31.9911666666667")
  expect_equal(tail(rownames(x1), 1), "31.9911666666667")

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

  expect_equal(dim(x), c(2699, 1))
  expect_equal(head(rownames(x), 1), "3.00044479166667")
  expect_equal(tail(rownames(x), 1), "11.9971114583333")

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
  expect_equal(head(x1$rt, 1), 3.00044479166667)
  expect_equal(tail(x1$rt, 1), 11.9971114583333)
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
  expect_equal(head(rownames(x),1), "0.00583333333333333")
  expect_equal(tail(rownames(x),1), "84.9991666666667")

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
  expect_equal(head(x1$rt,1), 0.00583333333333333)
  expect_equal(tail(x1$rt,1), 84.9991666666667)

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
  expect_equal(head(rownames(x),1),"0.001125")
  expect_equal(tail(rownames(x),1),"36")

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
  expect_equal(head(x1$rt,1), 0.001125)
  expect_equal(tail(x1$rt,1), 36)
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
  expect_equal(head(rownames(x),1),"0.000326049995422363")
  expect_equal(tail(rownames(x),1),"18.2346604166667")

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
  expect_equal(head(rownames(x),1), "0.00083331667582194")
  expect_equal(tail(rownames(x),1),"19")

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
  expect_equal(head(rownames(x$FID1A), 1), "-.00181875") # is this right?
  expect_equal(tail(rownames(x$FID1A), 1), "19.7048479166667")
  expect_equal(head(rownames(x$V181), 1), "-.00126875") # is this right?
  expect_equal(tail(rownames(x$V181), 1), "19.7053979166667")

  # check metadata
  expect_equal(attr(x[[1]], "sample_name"), "blanc421")
  expect_equal(attr(x[[1]], "file_version"), "181")
  expect_equal(attr(x[[1]], "detector_y_unit"), "pA")
  expect_equal(attr(x[[1]], "method"), "DET3300.M")
  expect_equal(attr(x[[1]], "run_datetime"), as.POSIXct("2022-8-23 12:16:25",
                                                        tz = "UTC"))
  expect_equal(attr(x[[1]], "time_unit"), "Minutes")

  expect_equal(attr(x[[2]], "sample_name"), "140+H")
  expect_equal(attr(x[[2]], "file_version"), "181")
  expect_equal(attr(x[[2]], "detector_y_unit"), "pA")
  expect_equal(attr(x[[2]], "method"), "DET3300.M")
  expect_equal(attr(x[[2]], "run_datetime"), as.POSIXct("2022-8-23 12:48:20",
                                                        tz = "UTC"))
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
  expect_equal(head(x1$FID1A$rt, 1), -.00181875) # is this right?
  expect_equal(tail(x1$FID1A$rt, 1), 19.7048479166667)

  expect_equal(extract_metadata(x)[,c(1:8)], extract_metadata(x1)[,c(1:8)])

  expect_warning(read_chroms(path, format_in = "agilent_d", what = "dad",
                           progress_bar = FALSE))
  expect_error(read_agilent_d(path, what = "dad"))

})

test_that("read_chroms can read 'Agilent' .dx files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("agilent.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "agilent_dx", what = c("chroms", "instrument"),
                   progress_bar = FALSE)[[1]]
  expect_equal(class(x$chroms)[1], "matrix")
  expect_equal(colnames(x$chroms),"intensity")
  expect_equal(dim(x$chroms), c(10000, 1))
  expect_equal(attr(x$chroms, "parser"), "chromconverter")
  expect_equal(attr(x$chroms, "data_format"), "wide")
  expect_equal(head(rownames(x$chroms),1), "0.001125")
  expect_equal(tail(rownames(x$chroms),1), "36")

  # auxiliary instrumental data
  expect_equal(dim(x$instrument$`PMP1C,Solvent Ratio A`), c(43253, 1))

  expect_true(all(
    sapply(x$instrument, function(x) round(as.numeric(tail(rownames(x),1)))) == 36)
  )

  expect_true(all(
    sapply(x$instrument, function(x) round(as.numeric(head(rownames(x),1)))) == 0)
  )

  expect_true(all(x$instrument$`PMP1C,Solvent Ratio A` == 100))
  expect_true(all(x$instrument$`PMP1D,Solvent Ratio B` == 0))
  expect_true(all(round(x$instrument$`THM1B,Right Temperature`) == 45))

  expect_equal(head(names(x$instrument),5), c("RID1G,Board Temperature",
                                            "RID1F,Diode 2",
                                            "RID1E,Diode 1",
                                            "RID1D,Polarity",
                                            "RID1C,Diode Balance"))

  expect_equal(sapply(x$instrument, function(xx) attr(xx, "detector_y_unit")),
               c("\u00b0C","counts","counts","","","\u00b0C", "\u00b0C",
                 "\u00b0C","","%","%","%","%","mL/min","bar"),
               ignore_attr = TRUE)

  expect_equal(sapply(x$instrument, function(xx) attr(xx, "intensity_multiplier")),
               c(1e-3, 1e-2, 1e-2, 1, 1e-6, 1e-3, 1e-3, 1e-3, 1e-5,1e-3,1e-3,
                 1e-3,1e-3,1e-6,5e-3),
               ignore_attr = TRUE)

  expect_true(all(
    sapply(x$instrument, function(xx){
      attr(xx, "run_datetime")
      }) == 1636717143))

  x1 <- read_chroms(path, format_in="agilent_dx", what = c("chroms","instrument"),
                    progress_bar = FALSE, data_format = "long",
                    format_out = "data.frame")[[1]]
  expect_s3_class(x1$chroms[1], "data.frame")
  expect_equal(dim(x1$chroms), c(10000, 2))
  expect_equal(dim(x1$instrument$`PMP1C,Solvent Ratio A`), c(43253, 2))

  expect_warning(read_chroms(path, format_in = "agilent_dx", what = "dad",
                             progress_bar = FALSE))
  expect_error(read_agilent_dx(path, what = "dad"))

  expect_equal(x1$instrument[[1]]$intensity, x$instrument[[1]][,1],
               ignore_attr=TRUE)
  expect_equal(x1$instrument[[1]]$rt, as.numeric(rownames(x$instrument[[1]])),
               ignore_attr=TRUE)

  expect_equal(x1$instrument[[2]]$intensity, x$instrument[[2]][,1],
               ignore_attr=TRUE)
  expect_equal(x1$instrument[[2]]$rt, as.numeric(rownames(x$instrument[[2]])),
               ignore_attr=TRUE)

  expect_equal(x1$instrument[[3]]$intensity, x$instrument[[3]][,1],
               ignore_attr=TRUE)
  expect_equal(x1$instrument[[3]]$rt, as.numeric(rownames(x$instrument[[3]])),
               ignore_attr=TRUE)
})
