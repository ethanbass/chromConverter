library(testthat)

path_csv <- "testdata/dad1.csv"
path_uv <- "testdata/dad1.uv" #chemstation 131

x <- read_chroms(path_csv, format_in = "chemstation_csv", progress_bar = FALSE)

test_that("aston parser can read `Agilent Chemstation` 131 files", {
  skip_if_missing_dependecies()
  paths <- rep(path_uv, 2)
  x1 <- read_chroms(paths, format_in = "chemstation_uv", parser = "aston",
                    find_files = FALSE,
                    read_metadata = TRUE, progress_bar = FALSE)
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][, "220.0"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(length(x1), length(paths))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
  expect_equal(names(x1), c("dad1", "dad1"))
})

x1 <- read_chroms(path_uv, format_in = "chemstation_uv", parser = "chromconverter",
                  find_files = FALSE,
                  read_metadata = TRUE, progress_bar = FALSE)

test_that("read_chemstation_uv parser can read chemstation 131 files", {
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(length(x1), length(path_uv))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
})

test_that("extract_metadata function works", {
  meta <- extract_metadata(x1)
  expect_equal(class(meta), "data.frame")
  expect_equal(nrow(meta),1)
  expect_equal(meta$instrument, attr(x1[[1]],"instrument"))
  expect_equal(meta$parser, attr(x1[[1]],"parser"))
})

test_that("entab parser can read `Agilent Chemstation` 131 files", {
  skip_on_cran()
  skip_if_not_installed("entab")
  file <- "testdata/dad1.uv"
  x1 <- read_chroms(file, format_in = "chemstation_uv", parser = "entab",
                    find_files = FALSE,
                    read_metadata = TRUE, progress_bar = FALSE)
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "parser"), "entab")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
})

test_that("Shimadzu ascii parser works", {
  file <- "testdata/ladder.txt"
  x <- read_chroms(file, format_in = "shimadzu_fid", find_files = FALSE,
                   progress_bar = FALSE)
  expect_equal(class(x[[1]])[1], "matrix")
  expect_equal(attributes(x[[1]])$instrument, "GC-2014")
})

test_that("read_mzml works", {
  ext_filepath <- system.file("extdata", package = "RaMS")
  DAD_filepath <- list.files(ext_filepath, full.names = TRUE,
                             pattern = "uv_test_mini.mzML")
  dad_long <- read_mzml(DAD_filepath, what = "DAD", verbose = FALSE)
  expect_equal(dad_long,
               RaMS::grabMSdata(files = DAD_filepath, grab_what = "DAD", verbosity = FALSE)
  )
  dad_wide <- read_mzml(DAD_filepath, what = "DAD", verbose = FALSE,
                        data_format = "wide")
  expect_equal(nrow(dad_wide[[1]]), length(unique(dad_long[[1]]$rt)))
  expect_equal(ncol(dad_wide[[1]]), length(unique(dad_long[[1]]$lambda)))
  expect_equal(as.numeric(colnames(dad_wide[[1]])), unique(dad_long[[1]]$lambda))
  expect_equal(as.numeric(rownames(dad_wide[[1]])), unique(dad_long[[1]]$rt))
})

test_that("get_filetype works as expected", {
  expect_equal(get_filetype(path_uv), "chemstation_131")
})

test_that("Rainbow parser can read chemstation 131 files", {
  skip_if_missing_dependecies()
  skip_on_cran()
  skip_on_ci()
  x1 <- read_chroms(path_uv, format_in = "chemstation_uv", parser = "rainbow",
                    find_files = FALSE,
                    read_metadata = TRUE,
                    progress_bar = FALSE)
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "parser"), "rainbow")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
})

test_that("chromconverter parser can read chemstation 130 files", {
  skip_if_missing_dependecies()
  skip_on_cran()
  x1 <- read_chroms("testdata/chemstation_130.ch", progress_bar = FALSE)
  # expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
  # expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "parser"), "chromconverter")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
  expect_equal(attr(x1[[1]], "detector_unit"), "mAU")
  expect_equal(attr(x1[[1]], "file_version"), "130")
  expect_equal(ncol(x1[[1]]), 1)
  x2 <- read_chroms("testdata/chemstation_130.ch", progress_bar = FALSE,
                    data_format = "long", format_out = "data.frame")[[1]]
  expect_equal(ncol(x2), 2)
  expect_equal(class(x2), "data.frame")
  expect_equal(as.numeric(rownames(x1[[1]])), x2[,1])
})

test_that("read_chroms exports csvs correctly", {
  skip_on_cran()
  path_out <-  tempdir(check = TRUE)
  on.exit(unlink(c(fs::path(path_out, "dad1", ext = "csv"), path_out)))
  x1 <- read_chroms(paths = path_uv, export = TRUE, path_out = path_out,
                    export_format="csv", format_out = "data.frame",
                    progress_bar = FALSE)
  x1_out <- read.csv(fs::path(path_out, "dad1", ext = "csv"), row.names = 1)
  expect_equal(x1[[1]], x1_out, ignore_attr = TRUE)
})

test_that("read_chroms exports cdf files correctly", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  path_out <-  tempdir(check = TRUE)
  on.exit(unlink(c(fs::path(path_out, "ladder", ext = "cdf"), path_out)))
  file <- "testdata/ladder.txt"
  x1 <- read_chroms(paths = file, format_in = "shimadzu_fid", export = TRUE, path_out = path_out,
                    export_format = "cdf", progress_bar = FALSE)
  x1_out <- read_cdf(fs::path(path_out, "ladder", ext = "cdf"))
  expect_equal(x1[[1]], x1_out, ignore_attr = TRUE)
})

test_that("read_peaklist can read chemstation reports", {
  path <- "testdata/RUTIN2.D/Report.TXT"
  x <- read_peaklist(path, format_in = "chemstation")
  expect_equal(class(x[[1]]), "list")
  expect_equal(class(x[[1]][[1]]), "data.frame")
  expect_equal(names(x[[1]]), c("254", "320", "360", "210", "230"))
  expect_equal(x[[1]][[1]][[1,"sample"]], "RUTIN2")
  expect_equal(x[[1]][[1]][[1,"lambda"]], "254")
  expect_equal(colnames(x[[1]][[1]]),
               c("sample", "lambda", "rt", "width", "area", "height", "type"))
  expect_equal(attr(x, "fit"), "chemstation")
  expect_equal(attr(x, "class"), "peak_list")
  x <- read_peaklist(path, format_in = "chemstation", data_format = "original")
  expect_equal(class(x[[1]]), "list")
  expect_equal(class(x[[1]][[1]]), "data.frame")
  expect_equal(names(x[[1]]), c("254", "320", "360", "210", "230"))
  expect_equal(x[[1]][[1]][[1,"sample"]], "RUTIN2")
  expect_equal(x[[1]][[1]][[1,"lambda"]], "254")
  expect_equal(colnames(x[[1]][[1]]),
               c("sample", "lambda", "Peak #", "RetTime [min]", "Width [min]",
                 "Area [mAU*s]", "Height [mAU]", "Area %", "Type"))
  expect_equal(attr(x, "fit"), "chemstation")
  expect_equal(attr(x, "class"), "peak_list")
})

test_that("read_peaklist can read 'Shimadzu' fid files", {
  path <- "testdata/ladder.txt"
  x <- read_peaklist(path, format_in = "shimadzu_fid", progress_bar = FALSE)
  expect_equal(class(x[[1]]), "list")
  expect_equal(class(x[[1]][[1]]), "data.frame")
  expect_equal(x[[1]][[1]][[1,"sample"]], "ladder")
  expect_equal(colnames(x[[1]][[1]]),
               c("sample", "rt", "start", "end", "area", "height"))
  x <- read_peaklist(path, format_in = "shimadzu_fid", data_format = "original")
  expect_equal(class(x[[1]]), "list")
  expect_equal(class(x[[1]][[1]]), "data.frame")
  expect_equal(x[[1]][[1]][[1,"sample"]], "ladder")
  expect_equal(x[[1]][[1]][[1,"sample"]], "ladder")
  expect_equal(colnames(x[[1]][[1]]),
               c("sample","Peak.","R.Time","I.Time","F.Time","Area","Height",
                 "A.H","Conc.","Mark","ID.","Name", "k.", "Plate..", "Plate.Ht.",
                 "Tailing", "Resolution", "Sep.Factor", "Area.Ratio", "Height.Ratio",
                 "Conc...", "Norm.Conc."))
  expect_equal(attr(x, "class"), "peak_list")
})
