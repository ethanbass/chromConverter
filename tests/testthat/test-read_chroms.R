library(testthat)

path_csv <- test_path("testdata/dad1.csv")
path_uv <- test_path("testdata/dad1.uv") #chemstation v131

x <- read_chroms(path_csv, format_in = "chemstation_csv", progress_bar = FALSE)[[1]]

test_that("chromConverter can read `Agilent Chemstation` .csv file", {
  expect_equal(dim(x), c(1944, 1))
  expect_equal(head(x[,1], n = 3), c(-4.086018, -4.113674, -4.142761),
               tolerance = .0001, ignore_attr = TRUE)
  expect_equal(head(rownames(x), n = 3), c("0.002", "0.0086666666667",
                                           "0.0153333333333"))
  x1 <- read_chroms(path_csv, format_in = "chemstation_csv",
                    format_out = "data.table", progress_bar = FALSE)[[1]]
  expect_s3_class(x1, "data.table")

  x2 <- read_chroms(path_csv, format_in="chemstation_csv",
                    format_out="data.frame", progress_bar = FALSE)[[1]]
  expect_s3_class(x1, "data.frame")
})

x1 <- read_chroms(path_uv, format_in = "chemstation_uv",
                  parser = "chromconverter", find_files = FALSE,
                  read_metadata = TRUE, progress_bar = FALSE)

test_that("read_chemstation_uv parser can read chemstation 131 files", {
  expect_equal(as.numeric(x[,1]), as.numeric(x1[[1]][,"220"]))
  expect_equal(as.numeric(rownames(x)), as.numeric(rownames(x1[[1]])))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
  expect_named(x1, "dad1")

  x2 <- read_chroms(path_uv, format_in = "chemstation_uv",
              parser = "chromconverter", format_out="data.table",
              read_metadata = TRUE, progress_bar = FALSE,
              sample_names = "sample_name")

  expect_s3_class(x2[[1]], class = "data.table")
  expect_equal(attr(x2[[1]], "format_out"), "data.table")
  expect_equal(attr(x2[[1]], "detector_y_unit"), "mAU")
  expect_equal(attr(x2[[1]], "detector_x_unit"), "nm")
  expect_equal(attr(x2[[1]], "detector"), "DAD")
  expect_equal(attr(x2[[1]], "sample_name"), "las_bulk_hexE")
  expect_named(x2, "las_bulk_hexE")
})

test_that("extract_metadata function works", {
  meta <- extract_metadata(x1)
  expect_equal(class(meta), "data.frame")
  expect_equal(nrow(meta), 1)
  expect_equal(meta$instrument, attr(x1[[1]],"instrument"))
  expect_equal(meta$parser, attr(x1[[1]], "parser"))
  expect_equal(meta$sample_name, "las_bulk_hexE")
  expect_equal(meta$detector_id, "G1315A")
  expect_equal(meta$detector_y_unit, "mAU")
  expect_equal(meta$detector_x_unit, "nm")
  expect_equal(meta$detector, "DAD")
  expect_equal(meta$detector_range1, "200")
  expect_equal(meta$method, "ETHAN_PA_SHORT8_2_PREP_30UL.M")
  expect_equal(meta$time_unit, "Minutes")
  expect_equal(meta$run_datetime, as.POSIXct(1648668556, tz = "UTC"))

  expect_error(extract_metadata(x1, what = "instrument"))

  meta <- extract_metadata(x1, format_out = "tibble")
  expect_equal(class(meta)[1], "tbl_df")
  expect_equal(nrow(meta), 1)
  expect_equal(meta[["instrument"]], attr(x1[[1]],"instrument"))
  expect_equal(meta[["parser"]], attr(x1[[1]],"parser"))

  meta <- extract_metadata(x1, format_out = "data.table")
  expect_equal(class(meta)[1], "data.table")
  expect_equal(nrow(meta), 1)
  expect_equal(meta[["instrument"]], attr(x1[[1]],"instrument"))
  expect_equal(meta[["parser"]], attr(x1[[1]],"parser"))

  meta <- extract_metadata(x1, what = c("sample_name", "run_datetime"))
  expect_named(meta, c("name", "sample_name", "run_datetime"))

  meta <- extract_metadata(x1, what=c("sample_name"))
  expect_named(meta, c("name","sample_name"))

  x2 <- read_chroms(rep(path_uv, 2), parser="chromConverter",
                    progress_bar = FALSE)
  attr(x2[[1]],"detector") <- NULL
  meta2 <- extract_metadata(x2)
  expect_equal(nrow(meta2), length(x2))
  expect_equal(meta2$detector,c(NA,"DAD"))
  expect_equal(meta2$sample_name, rep(meta$sample_name,2))

  expect_warning(extract_metadata(x2,what=c("sample_name", "instrument")))
})

test_that("entab parser can read `Agilent Chemstation` 131 files", {
  skip_on_cran()
  skip_if_not_installed("entab")

  path <- test_path("testdata/dad1.uv")

  x1 <- read_chroms(path, format_in = "chemstation_uv", parser = "entab",
                    find_files = FALSE,
                    read_metadata = TRUE, progress_bar = FALSE)[[1]]

  expect_equal(as.numeric(x[,1]), as.numeric(x1[,"220"]))
  expect_equal(as.numeric(rownames(x)), as.numeric(rownames(x1)))
  expect_equal(class(x1)[1], "matrix")
  expect_equal(attr(x1, "parser"), "entab")
  expect_equal(attr(x1, "data_format"), "wide")
  expect_equal(attr(x1, "operator"), "Ethan")
  expect_equal(attr(x1, "detector"), "UV")
  expect_equal(attr(x1, "detector_id"), "G1315A")

  expect_equal(attr(x1, "sample_id"), 24)

  x2 <- read_chroms(path, format_in = "chemstation_uv", parser = "entab",
                    find_files = FALSE, data_format = "long",
                    format_out = "data.frame",
                    read_metadata = TRUE, progress_bar = FALSE)[[1]]
  expect_equal(colnames(x2), c("rt", "lambda", "intensity"))
  expect_equal(x[,1], x2[x2$lambda == 220, "intensity"], ignore_attr = TRUE)
  expect_equal(nrow(x2), ncol(x1)*nrow(x1))
  expect_equal(attr(x2, "data_format"), "long")
  expect_equal(attr(x2, "operator"), "Ethan")
  expect_equal(attr(x1, "detector"), "UV")
  expect_equal(attr(x1, "detector_id"), "G1315A")
  expect_equal(attr(x2, "sample_id"), 24)
  expect_equal(attr(x2, "format_out"), "data.frame")
  expect_s3_class(x2, "data.frame")
})

test_that("`Shimadzu` ASCII parser works", {
  path <- test_path("testdata/ladder.txt")

  x <- read_chroms(path, format_in = "shimadzu_fid", find_files = FALSE,
                   progress_bar = FALSE)[[1]]

  expect_equal(class(x)[1], "matrix")
  expect_equal(attr(x, "instrument"), "GC-2014")
  expect_equal(attr(x, "sample_name"), "FS19_214")
})

test_that("read_mzml works", {
  skip_on_cran()
  ext_filepath <- system.file("extdata", package = "RaMS")
  DAD_filepath <- list.files(ext_filepath, full.names = TRUE,
                             pattern = "uv_test_mini.mzML.gz")

  dad_long <- read_mzml(DAD_filepath, what = "DAD", verbose = FALSE)
  colnames(dad_long$DAD)[3] <- "int"
  expect_equal(dad_long,
               RaMS::grabMSdata(files = DAD_filepath, grab_what = "DAD",
                                verbosity = FALSE)
  )
  dad_wide <- read_mzml(DAD_filepath, what = "DAD", verbose = FALSE,
                        data_format = "wide")
  expect_equal(nrow(dad_wide[[1]]), length(unique(dad_long[[1]]$rt)))
  expect_equal(ncol(dad_wide[[1]]), length(unique(dad_long[[1]]$lambda)))
  expect_equal(as.numeric(colnames(dad_wide[[1]])), unique(dad_long[[1]]$lambda))
  expect_equal(as.numeric(rownames(dad_wide[[1]])), unique(dad_long[[1]]$rt))
})

test_that("get_filetype fnc works as expected", {
  expect_equal(get_filetype(path_uv), "chemstation_131")
})

test_that("Rainbow parser can read chemstation 131 files", {
  skip_if_missing_dependencies()
  skip_on_cran()
  skip_on_ci()

  x1 <- read_chroms(path_uv, format_in = "chemstation_uv", parser = "rainbow",
                    find_files = FALSE,
                    read_metadata = TRUE,
                    progress_bar = FALSE)

  expect_equal(as.numeric(x[,1]), as.numeric(x1[[1]][,"220"]))
  expect_equal(as.numeric(rownames(x)), as.numeric(rownames(x1[[1]])))

  # check metadata
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "parser"), "rainbow")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
  expect_equal(attr(x1$dad1, "detector_y_unit"), "mAU")
  expect_equal(attr(x1$dad1, "sample_name"), "las_bulk_hexE")

  x2 <- read_chroms(path_uv, format_in = "chemstation_uv", parser = "rainbow",
                    find_files = FALSE, data_format = "long",
                    format_out = "data.frame",
                    read_metadata = TRUE,
                    progress_bar = FALSE)
  expect_equal(nrow(x2$dad1), nrow(x1$dad1)*ncol(x1$dad1))
  expect_equal(colnames(x2$dad1), c("rt", "lambda", "intensity"))
  expect_equal(attr(x2$dad1, "detector_y_unit"), "mAU")
  expect_equal(attr(x1$dad1, "sample_name"), "las_bulk_hexE")
  expect_equal(attr(x2[[1]], "data_format"), "long")
  expect_equal(attr(x2[[1]], "format_out"), "data.frame")
  expect_s3_class(x2[[1]], "data.frame")
  expect_equal(attr(x2[[1]], "parser"), "rainbow")
})

test_that("chromConverter parser can read `ChemStation` 130 files", {
  skip_if_missing_dependencies()
  skip_on_cran()

  x1 <- read_chroms(test_path("testdata/chemstation_130.ch"), progress_bar = FALSE)
  # expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
  # expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(colnames(x1[[1]]), "intensity")
  expect_equal(attr(x1[[1]], "parser"), "chromconverter")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
  expect_equal(attr(x1[[1]], "detector_y_unit"), "mAU")
  expect_equal(attr(x1[[1]], "file_version"), "130")
  expect_equal(ncol(x1[[1]]), 1)

  x2 <- read_chroms(test_path("testdata/chemstation_130.ch"), progress_bar = FALSE,
                    data_format = "long", format_out = "data.frame")[[1]]
  expect_equal(ncol(x2), 2)
  expect_equal(colnames(x2), c("rt", "intensity"))
  expect_s3_class(x2, "data.frame")
  expect_equal(attr(x2,"format_out"), "data.frame")
  expect_equal(as.numeric(rownames(x1[[1]])), x2[,1])
})

test_that("read_chroms exports CSV files correctly", {
  skip_on_cran()
  path_out <-  tempdir(check = TRUE)
  on.exit(unlink(c(fs::path(path_out, "dad1", ext = "csv"), path_out)))
  x1 <- read_chroms(paths = path_uv, path_out = path_out,
                    export_format = "csv", format_out = "data.frame",
                    progress_bar = FALSE)
  x1_out <- read.csv(fs::path(path_out, "dad1", ext = "csv"), row.names = 1)
  expect_equal(x1[[1]], x1_out, ignore_attr = TRUE)
})

# progress bar can't be supressed when writing CDFs
test_that("read_chroms exports CDF files correctly", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  tmp <-  tempdir(check = TRUE)
  file <- test_path("testdata/ladder.txt")

  x1 <- read_chroms(paths = file, format_in = "shimadzu_fid",
                  path_out = tmp, export_format = "cdf",
                  progress_bar = FALSE)

  path_cdf <- fs::path(tmp, attr(x1[[1]], "sample_name"), ext = "cdf")
  on.exit(unlink(c(path_cdf, tmp)))

  x1_out <- read_cdf(path_cdf)

  # check metadata equivalence
  expect_equal(x1[[1]], x1_out, ignore_attr = TRUE)
  # expect_equal(attr(x1[[1]],"instrument"), attr(x1_out,"instrument"))
  # expect_equal(attr(x1[[1]],"detector"), attr(x1_out,"detector"))
  # expect_equal(attr(x1[[1]],"software_name"), attr(x1_out,"software_name")) #NA
  # expect_equal(attr(x1[[1]],"software_version"), attr(x1_out,"software_version")) #NA
  # expect_equal(attr(x1[[1]],"method"), attr(x1_out,"method")) #NA
  # expect_equal(attr(x1[[1]],"operator"), attr(x1_out,"operator")) #NA
  expect_equal(attr(x1[[1]],"run_datetime"), attr(x1_out,"run_datetime"))
  # expect_equal(attr(x1[[1]],"sample_name"), attr(x1_out,"sample_name")) #???
  expect_equal(as.numeric(attr(x1[[1]],"sample_injection_volume")),
               attr(x1_out,"sample_injection_volume"))
  expect_equal(as.numeric(attr(x1[[1]],"sample_amount")), attr(x1_out,"sample_amount"))
  # expect_equal(attr(x1[[1]],"time_interval"), attr(x1_out,"time_interval"))
  expect_equal(attr(x1[[1]],"time_unit"), attr(x1_out,"time_unit"))
  # expect_equal(attr(x1[[1]],"source_file"), attr(x1_out,"source_file")) # doesn't match
})

test_that("read_peaklist can read `ChemStation` report files", {
  path <- test_path("testdata/RUTIN2.D/")

  x <- read_peaklist(path, format_in = "chemstation")

  expect_equal(class(x[[1]]), "list")
  expect_equal(class(x[[1]][[1]]), "data.frame")
  expect_equal(names(x[[1]]), c("254", "320", "360", "210", "230"))
  expect_equal(x[[1]][[1]][[1, "sample"]], "RUTIN2")
  expect_equal(x[[1]][[1]][[1, "lambda"]], "254")
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

test_that("read_peaklist can read `Shimadzu` fid files", {
  path <- test_path("testdata/ladder.txt")
  x <- read_peaklist(path, format_in = "shimadzu_fid", progress_bar = FALSE)
  expect_equal(class(x[[1]]), "data.frame")
  expect_equal(x[[1]][[1,"sample"]], "ladder")
  expect_equal(colnames(x[[1]]),
               c("sample", "rt", "start", "end", "area", "height"))

  x <- read_peaklist(path, format_in = "shimadzu_fid", data_format = "original",
                     progress_bar = FALSE)
  expect_equal(class(x[[1]]), "data.frame")
  expect_equal(x[[1]][[1,"sample"]], "ladder")
  expect_equal(x[[1]][[1,"sample"]], "ladder")
  expect_equal(colnames(x[[1]]),
               c("sample", "Peak#", "R.Time", "I.Time", "F.Time", "Area",
                 "Height", "A/H", "Conc.", "Mark", "ID#", "Name", "k'",
                 "Plate #", "Plate Ht.", "Tailing", "Resolution", "Sep.Factor",
                 "Area Ratio", "Height Ratio", "Conc. %", "Norm Conc."))
  expect_equal(attr(x, "class"), "peak_list")
})

