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
  expect_equal(attr(x, "data_format"), "wide")
  expect_equal(attr(x, "format_out"), "matrix")

  x1 <- read_chroms(path_csv, format_in = "chemstation_csv",
                    format_out = "data.table", progress_bar = FALSE)[[1]]
  expect_s3_class(x1, "data.table")
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_equal(attr(x1, "data_format"), "long")
  expect_equal(attr(x1, "format_out"), "data.table")

  x2 <- read_chroms(path_csv, format_in="chemstation_csv",
                    format_out = "data.frame", progress_bar = FALSE)[[1]]
  expect_s3_class(x1, "data.frame")
  expect_equal(attr(x2, "data_format"), "wide")
  expect_equal(attr(x2, "format_out"), "data.frame")
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

  path_uv <- test_path("testdata/dad1.uv")

  x2 <- read_chroms(path_uv, format_in = "chemstation_uv",
              parser = "chromconverter", format_out = "data.table",
              read_metadata = TRUE, progress_bar = FALSE,
              sample_names = "sample_name")

  expect_s3_class(x2[[1]], class = "data.table")
  expect_equal(colnames(x2[[1]]), c("rt","lambda","intensity"))
  expect_equal(attr(x2[[1]], "format_out"), "data.table")
  expect_equal(attr(x2[[1]], "detector_y_unit"), "mAU")
  expect_equal(attr(x2[[1]], "detector_x_unit"), "nm")
  expect_equal(attr(x2[[1]], "detector"), "DAD")
  expect_equal(attr(x2[[1]], "sample_name"), "las_bulk_hexE")
  expect_equal(attr(x2[[1]], "data_format"), "long")
  expect_named(x2, "las_bulk_hexE")

  expect_equal(sum(x1[[1]]),
    9029434.92889404)
  expect_equal(range(x1[[1]]),
    c(-10.6658935546875, 2705.6097984314))
  expect_equal(unname(colSums(x1[[1]])[c(1, 50, 101)]),
    c(438664.104938507, 48688.8132095337, 7581.65645599365))
  expect_equal(unname(x1[[1]][1, 1:5]),
    c(-0.70953369140625, -2.12287902832031, -2.94065475463867,
    -3.44944000244141, -3.78751754760742))
  expect_equal(unname(x1[[1]][nrow(x1[[1]]), 97:101]),
    c(-2.91585922241211, -1.2969970703125, -0.599384307861328,
    -0.261783599853516, 0.839710235595703))
  expect_equal(unname(x1[[1]][972, c(1, 50, 101)]),
    c(155.673980712891, 11.4827156066895, 5.07116317749023))
  expect_equal(unname(x1[[1]][round(seq(1, nrow(x1[[1]]), length.out = 12)), 50]),
    c(-1.31607055664062, -1.26075744628906, -0.530719757080078,
    -1.93309783935547, -2.13956832885742, -0.751972198486328,
    284.765243530273, 9.3231201171875, -0.819683074951172, 1.02043151855469,
    -4.44650650024414, -7.99036026000977))
  expect_equal(attr(x1[[1]], "intensity_multiplier"),
    0.000476837158203125)
})

test_that("long format preserves the wide format values exactly", {
  path_uv <- test_path("testdata/dad1.uv")

  xw <- read_chroms(path_uv, format_in = "chemstation_uv",
                    parser = "chromconverter", progress_bar = FALSE)[[1]]
  xl <- read_chroms(path_uv, format_in = "chemstation_uv",
                    parser = "chromconverter", data_format = "long",
                    format_out = "data.frame", progress_bar = FALSE)[[1]]

  expect_equal(nrow(xl), prod(dim(xw)))

  # the long table is laid out row-major (all wavelengths for each time point),
  # so it must match the transposed matrix element-for-element. `identical`
  # rather than `expect_equal`: the reshaping step used to round every value to
  # 7 significant figures, which is within the default tolerance.
  expect_true(identical(xl$intensity, as.numeric(t(xw))))

  expect_equal(unique(xl$rt), as.numeric(rownames(xw)))
  expect_equal(unique(xl$lambda), as.numeric(colnames(xw)))
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

  meta <- extract_metadata(x1, format_out = "tibble")
  expect_equal(class(meta)[1], "tbl_df")
  expect_equal(nrow(meta), 1)
  expect_equal(meta[["instrument"]], attr(x1[[1]],"instrument"))
  expect_equal(meta[["parser"]], attr(x1[[1]], "parser"))

  meta <- extract_metadata(x1, format_out = "data.table")
  expect_equal(class(meta)[1], "data.table")
  expect_equal(nrow(meta), 1)
  expect_equal(meta[["instrument"]], attr(x1[[1]],"instrument"))
  expect_equal(meta[["parser"]], attr(x1[[1]], "parser"))

  meta <- extract_metadata(x1, what = c("sample_name", "run_datetime"))
  expect_named(meta, c("name", "sample_name", "run_datetime"))

  meta <- extract_metadata(x1, what = c("sample_name"))
  expect_named(meta, c("name", "sample_name"))

  expect_warning({
    x2 <- read_chroms(rep(path_uv, 2), parser = "chromConverter",
                    progress_bar = FALSE)
  })
  attr(x2[[1]],"detector") <- NULL
  meta2 <- extract_metadata(x2)
  expect_equal(nrow(meta2), length(x2))
  expect_equal(meta2$detector, c(NA, "DAD"))
  expect_equal(meta2$sample_name, rep(meta$sample_name, 2))

  # `instrument` is recorded in the file header, so it is found
  expect_equal(extract_metadata(x2, what = c("sample_name", "instrument"))$instrument,
               rep("LC", 2))
  # ... but a field the format does not record still warns
  expect_warning(extract_metadata(x2, what = c("sample_name", "batch")))
})

test_that("extract_metadata can filter by detector", {
  mk <- function(detector){
    m <- matrix(1:4, nrow = 2)
    attr(m, "detector") <- detector
    m
  }
  # a nested list, as returned for a sample with several detectors
  nested <- list(blue = list(MS = mk("MS"), UV = mk("UV"), CAD = mk("CAD")))

  expect_equal(extract_metadata(nested, what = "detector")$name,
               c("blue.MS", "blue.UV", "blue.CAD"))
  expect_equal(extract_metadata(nested, what = "detector",
                                detector = "UV")$name, "blue.UV")
  # matched against the `detector` attribute, case-insensitively
  expect_equal(extract_metadata(nested, what = "detector",
                                detector = "uv")$detector, "UV")
  # more than one detector can be requested
  expect_equal(extract_metadata(nested, what = "detector",
                                detector = c("CAD", "UV"))$name,
               c("blue.UV", "blue.CAD"))
  # an informative error, rather than an empty frame, when nothing matches
  err <- expect_error(extract_metadata(nested, what = "detector",
                                       detector = "FID"))
  expect_match(conditionMessage(err), "No chromatograms were found")
  expect_match(conditionMessage(err), "'MS', 'UV', 'CAD'", fixed = TRUE)

  # chromatograms with no `detector` attribute cannot match, so are dropped
  partial <- list(a = mk("UV"), b = matrix(1:4, nrow = 2))
  expect_equal(nrow(extract_metadata(partial, what = "detector",
                                     detector = "UV")), 1)
  err <- expect_error(extract_metadata(list(b = matrix(1:4, nrow = 2)),
                                       what = "detector", detector = "UV"))
  expect_match(conditionMessage(err), "do not have a 'detector' attribute",
               fixed = TRUE)

  # and it works on an ordinary flat list too
  expect_equal(nrow(extract_metadata(x1, detector = "DAD")), 1)
  expect_error(extract_metadata(x1, detector = "MS"),
               "No chromatograms were found")
})

test_that("entab parser can read `Agilent Chemstation` 131 files", {
  skip_if_not_installed("entab")
  skip_on_cran()

  path_uv <- test_path("testdata/dad1.uv")

  x1 <- read_chroms(path_uv, format_in = "chemstation_uv", parser = "entab",
                    find_files = FALSE, read_metadata = TRUE,
                    progress_bar = FALSE)[[1]]

  expect_equal(as.numeric(x[,1]), as.numeric(x1[,"220"]))
  expect_equal(as.numeric(rownames(x)), as.numeric(rownames(x1)))
  expect_equal(class(x1)[1], "matrix")
  expect_equal(attr(x1, "parser"), "entab")
  expect_equal(attr(x1, "data_format"), "wide")
  expect_equal(attr(x1, "operator"), "Ethan")
  expect_equal(attr(x1, "detector"), "UV")
  expect_equal(attr(x1, "detector_id"), "G1315A")
  expect_equal(attr(x1, "sample_position"), 24)

  x2 <- read_chroms(path_uv, format_in = "chemstation_uv", parser = "entab",
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
  expect_equal(attr(x2, "sample_position"), 24)
  expect_equal(attr(x2, "format_out"), "data.frame")
  expect_s3_class(x2, "data.frame")
})

test_that("`Shimadzu` ASCII parser works", {
  path <- test_path("testdata/ladder.txt")

  x <- read_chroms(path, format_in = "shimadzu_fid", find_files = FALSE,
                   progress_bar = FALSE)[[1]]

  expect_true(inherits(x, "matrix"))
  expect_equal(dim(x),c(66255,1))
  expect_equal(attr(x, "instrument"), "GC-2014")
  expect_equal(attr(x, "sample_name"), "FS19_214")
  expect_equal(attr(x, "data_format"), "wide")

  x1 <- read_chroms(path, format_in = "shimadzu_fid", find_files = FALSE,
                   progress_bar = FALSE, format_out="data.table")[[1]]
  expect_s3_class(x1, "data.table")
  expect_equal(attr(x1, "format_out"), "data.table")
  expect_equal(attr(x1, "data_format"), "long")
  expect_equal(dim(x1),c(66255,3))

  x2 <- read_chroms(path, format_in = "shimadzu_fid", find_files = FALSE,
                    progress_bar = FALSE, format_out = "data.frame",
                    data_format = "long")[[1]]
  expect_s3_class(x2, "data.frame")
  expect_equal(attr(x2, "format_out"), "data.frame")
  expect_equal(attr(x2, "data_format"), "long")
  expect_equal(dim(x2), c(66255,3))
  expect_equal(x1, x2, ignore_attr = TRUE)
})

test_that("read_mzml works", {
  skip_on_cran()
  ext_filepath <- system.file("extdata", package = "RaMS")
  DAD_filepath <- list.files(ext_filepath, full.names = TRUE,
                             pattern = "uv_test_mini.mzML.gz")

  dad_long <- read_mzml(DAD_filepath, what = "DAD", verbose = FALSE,
                        data_format = "long")
  colnames(dad_long$DAD)[3] <- "int"
  expect_equal(dad_long,
               RaMS::grabMSdata(files = DAD_filepath, grab_what = "DAD",
                                verbosity = FALSE), ignore_attr =TRUE
  )
  dad_wide <- read_mzml(DAD_filepath, what = "DAD", verbose = FALSE,
                        data_format = "wide")
  expect_equal(nrow(dad_wide[[1]]), length(unique(dad_long[[1]]$rt)))
  expect_equal(ncol(dad_wide[[1]]), length(unique(dad_long[[1]]$lambda)))
  expect_equal(as.numeric(colnames(dad_wide[[1]])), unique(dad_long[[1]]$lambda))
  expect_equal(as.numeric(rownames(dad_wide[[1]])), unique(dad_long[[1]]$rt))
})

test_that("get_filetype fnc works as expected", {
  path_uv <- test_path("testdata/dad1.uv")
  expect_equal(get_filetype(path_uv), "chemstation_131")
})

test_that("Rainbow parser can read chemstation 131 files", {
  skip_if_missing_dependencies("rainbow")
  skip_on_cran()
  skip_on_ci()

  path_uv <- test_path("testdata/dad1.uv")

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

  x1 <- read_chroms(test_path("testdata/chemstation_130.ch"), progress_bar = FALSE)
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
  expect_equal(attr(x2,"data_format"), "long")
  expect_equal(as.numeric(rownames(x1[[1]])), x2[,1])

  expect_equal(sum(x1[[1]]),
    27824.1186141968)
  expect_equal(range(x1[[1]]),
    c(-59.9513053894043, 2368.70050430298))
  expect_equal(which.max(x1[[1]]),
    2916L)
  expect_equal(unname(head(x1[[1]][, 1], 5)),
    c(0.384807586669922, 0.370502471923828, 0.359058380126953,
    0.360488891601562, 0.391483306884766))
  expect_equal(unname(tail(x1[[1]][, 1], 5)),
    c(-0.974655151367188, -0.974655151367188, -0.978469848632812,
    -0.982761383056641, -0.982761383056641))
  expect_equal(unname(x1[[1]][round(seq(1, nrow(x1[[1]]), length.out = 15)), 1]),
    c(0.384807586669922, 0.0309944152832031, 0.367164611816406,
    -0.70953369140625, -2.197265625, -6.44636154174805, -7.23648071289062,
    -8.31699371337891, -10.1351737976074, -9.79804992675781,
    -8.57305526733398, -5.41400909423828, -0.385284423828125,
    -3.25155258178711, -0.982761383056641))
  expect_equal(head(as.numeric(rownames(x1[[1]])), 3),
    c(-0.0421666666666667, -0.0355, -0.0288333333333333))
  expect_equal(tail(as.numeric(rownames(x1[[1]])), 3),
    c(39.9445, 39.9511666666667, 39.9578333333333))
  expect_equal(attr(x1[[1]], "intensity_multiplier"),
    0.000476837158203125)
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

test_that("read_chroms exports CDF files correctly", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  tmp <-  tempdir(check = TRUE)
  file <- test_path("testdata/ladder.txt")

  x1 <- read_chroms(paths = file, format_in = "shimadzu_fid",
                    path_out = tmp, export_format = "cdf",
                    progress_bar = FALSE, force = TRUE)

  path_cdf <- fs::path(tmp, attr(x1[[1]], "sample_name"), ext = "cdf")
  on.exit(unlink(c(path_cdf, tmp)))

  x1_out <- read_cdf(path_cdf)

  # check metadata equivalence
  expect_equal(x1[[1]], x1_out, ignore_attr = TRUE)
  expect_equal(attr(x1[[1]],"run_datetime"), attr(x1_out,"run_datetime"))
  expect_equal(attr(x1[[1]],"sample_name"), attr(x1_out,"sample_name"))
  expect_equal(as.numeric(attr(x1[[1]],"sample_injection_volume")),
               attr(x1_out,"sample_injection_volume"))
  expect_equal(as.numeric(attr(x1[[1]],"sample_amount")), attr(x1_out,"sample_amount"))
  expect_equal(attr(x1[[1]],"time_unit"), attr(x1_out,"time_unit"))
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


# --- sample-level attributes on nested chromatogram lists -------------------

# a minimal stand-in for a chromatogram: parsers attach metadata to these, not
# to the list that groups them
fake_chrom <- function(time = NULL, name = NULL){
  x <- matrix(1:4, nrow = 2)
  if (!is.null(time)) attr(x, "run_datetime") <- as.POSIXct(time, tz = "UTC")
  if (!is.null(name)) attr(x, "sample_name") <- name
  x
}

test_that("get_sample_attr finds attributes at any nesting depth", {
  flat <- fake_chrom("2020-01-01", "a")
  expect_equal(get_sample_attr(flat, "sample_name"), "a")

  # one level, as returned by `what = c("MS1", "TIC")`
  nested <- list(MS1 = fake_chrom("2020-01-01", "a"),
                 TIC = fake_chrom("2020-01-01", "a"))
  expect_equal(get_sample_attr(nested, "sample_name"), "a")

  # mixed depths within one sample, as `read_agilent_dx` returns
  mixed <- list(dad = fake_chrom("2020-01-01", "a"),
                chroms = list(fake_chrom("2020-01-01", "a"),
                              fake_chrom("2020-01-01", "a")))
  expect_equal(get_sample_attr(mixed, "sample_name"), "a")

  # only some leaves carry the attribute
  partial <- list(MS1 = fake_chrom(), TIC = fake_chrom("2020-01-01", "a"))
  expect_equal(get_sample_attr(partial, "sample_name"), "a")

  expect_null(get_sample_attr(list(MS1 = fake_chrom()), "sample_name"))
  expect_null(get_sample_attr(list(), "sample_name"))
})

test_that("get_sample_attr prefers an attribute on the element itself", {
  # `read_agilent_rslt` writes acaml attributes onto the list, not the leaves
  x <- structure(list(MS1 = fake_chrom(name = "leaf")), sample_name = "outer")
  expect_equal(get_sample_attr(x, "sample_name"), "outer")
})

test_that("get_sample_attr skips non-chromatogram elements", {
  # `read_mzml` returns a metadata table alongside its traces
  meta <- structure(data.frame(sample = "m"), class = "chromconverter_metadata")
  attr(meta, "sample_name") <- "wrong"
  x <- list(metadata = meta, TIC = fake_chrom(name = "right"))
  expect_equal(get_sample_attr(x, "sample_name"), "right")
})

test_that("get_sample_attr returns a single value", {
  x <- structure(fake_chrom(), run_datetime = as.POSIXct(c("2020-01-01",
                                                           "2020-01-02"),
                                                         tz = "UTC"))
  expect_length(get_sample_attr(x, "run_datetime"), 1)
  expect_equal(get_sample_attr(x, "run_datetime"),
               as.POSIXct("2020-01-01", tz = "UTC"))
})

test_that("sort_chroms_by_time orders nested samples oldest first", {
  data <- list(b = list(TIC = fake_chrom("2020-06-01")),
               a = list(TIC = fake_chrom("2019-01-01")),
               c = list(TIC = fake_chrom("2021-12-31")))
  expect_silent(sorted <- sort_chroms_by_time(data))
  expect_equal(names(sorted), c("a", "b", "c"))
})

test_that("sort_chroms_by_time places samples with no timestamp last", {
  data <- list(b = list(TIC = fake_chrom("2020-06-01")),
               x = list(TIC = fake_chrom()),
               a = list(TIC = fake_chrom("2019-01-01")))
  expect_warning(sorted <- sort_chroms_by_time(data), "could not be determined")
  expect_equal(names(sorted), c("a", "b", "x"))
})

test_that("sort_chroms_by_time leaves the list alone when nothing is sortable", {
  data <- list(b = list(TIC = fake_chrom()), a = list(TIC = fake_chrom()))
  expect_warning(sorted <- sort_chroms_by_time(data), "could not be determined")
  expect_equal(names(sorted), c("b", "a"))
})

test_that("sort_chroms_by_time is stable for ties and for unknowns", {
  data <- list(b = list(TIC = fake_chrom("2020-01-01")),
               a = list(TIC = fake_chrom("2020-01-01")),
               y = list(TIC = fake_chrom()),
               x = list(TIC = fake_chrom()))
  expect_warning(sorted <- sort_chroms_by_time(data), "could not be determined")
  expect_equal(names(sorted), c("b", "a", "y", "x"))
})

test_that("read_chroms sorts and names nested samples correctly", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  # both files return a nested list (MS1/TIC/BPC) and were acquired years apart
  dcm1 <- system.file("DCM1.SMS", package = "chromConverterExtraTests")
  strd15 <- system.file("STRD15.SMS", package = "chromConverterExtraTests")
  skip_if_not(all(file.exists(c(dcm1, strd15))))

  x <- read_chroms(c(dcm1, strd15), format_in = "varian_sms",
                   progress_bar = FALSE, sort_by = "acquisition_time")
  expect_equal(names(x), c("STRD15", "DCM1"))

  # sample names used to come back as the literal string "NULL"
  y <- read_chroms(c(dcm1, strd15), format_in = "varian_sms",
                   progress_bar = FALSE, sample_names = "sample_name")
  expect_equal(names(y), c("DCM1", "STRD15"))

  # a single-chromatogram (collapsed) sample must still work
  z <- read_chroms(c(dcm1, strd15), format_in = "varian_sms", what = "TIC",
                   progress_bar = FALSE, sample_names = "sample_name",
                   sort_by = "acquisition_time")
  expect_equal(names(z), c("STRD15", "DCM1"))
  expect_true(is.matrix(z[[1]]))
})
