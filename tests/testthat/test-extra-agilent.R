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
  expect_equal(attr(x, "data_format"), "long")
  expect_equal(colnames(x), c("rt", "mz", "intensity"))

  # export as mzML
  tmp <- tempdir()
  path_mzml <- fs::path(tmp, gsub(" ", "_", attr(x, "sample_name")),
                        ext = "mzML")
  on.exit(unlink(path_mzml))

  # chromConverter can read Agilent MS data and write to mzml
  x1 <- read_chroms(path, parser = "chromconverter",
                    format_out = "data.frame", data_format = "wide",
                    progress_bar = FALSE,
                    export_format = "mzML", path_out = tmp, force = TRUE)[[1]]
  expect_s3_class(x1$MS1, "data.frame")
  expect_equal(attr(x1$MS1, "format_out"), "data.frame")
  expect_equal(attr(x1$MS1, "data_format"), "long")
  expect_equal(attr(x1$TIC, "data_format"), "wide")
  expect_equal(attr(x1$BPC, "data_format"), "long")

  xx <- read_mzml(path_mzml)
  expect_equal(xx$MS1[,-4], x1$MS1, ignore_attr = TRUE)
  expect_equal(as.data.frame(xx$TIC), x1$TIC, ignore_attr = TRUE)
  expect_equal(rownames(xx$TIC), rownames(x1$TIC))
  expect_equal(xx$BPC[,1], x1$BPC[,3], ignore_attr=TRUE)
  expect_equal(as.numeric(rownames(xx$BPC)), x1$BPC$rt, tolerance = 0.00001)

  expect_equal(x1$MS1, as.data.frame(x), ignore_attr = TRUE)
  expect_equal(attr(x1$MS1,"sample_name"), attr(x,"sample_name"))
  expect_equal(attr(x1$MS1,"source_sha1"), attr(x,"source_sha1"))
  expect_equal(attr(x1$MS1, "time_unit"), attr(x, "time_unit"))
  # time zone inconsistency
  # expect_equal(attr(x1$MS1,"run_datetime"), attr(x,"run_datetime"))
  expect_equal(attr(x1$MS1,"operator"), attr(x,"operator"))
  expect_equal(attr(x1$MS1,"method"), attr(x,"method"))
  expect_equal(attr(x1$MS1,"detector"), attr(x,"detector"))
  expect_equal(attr(x1$MS1, "data_format"), "long")

  # rainbow
  x2 <- read_chroms(path, parser = "rainbow",
                    progress_bar = FALSE, precision = 0)[[1]]
  expect_equal(class(x2)[1], "matrix")
  expect_equal(dim(x2), c(2534, 841))
  expect_equal(attr(x2, "method"), attr(x1$MS1, "method"))
  expect_equal(attr(x2, "detector"), attr(x1$MS1, "detector"))
  expect_equal(attr(x2, "data_format"), "wide")

  x3 <- read_chroms(path, parser = "rainbow",
                    progress_bar = FALSE, data_format = "long",
                    format_out = "data.table",
                    precision = 0, sparse = FALSE)[[1]]
  expect_s3_class(x3, "data.table")
  expect_equal(dim(x3), c(2131094, 3))
  expect_equal(colnames(x3), c("rt", "mz", "intensity"))
  expect_equal(attr(x3, "method"), attr(x2, "method"))
  expect_equal(attr(x3, "detector"), attr(x2, "detector"))
  expect_equal(attr(x3, "data_format"), "long")

  x4 <- read_chroms(path, parser = "rainbow",
                    progress_bar = FALSE, data_format = "long",
                    format_out = "data.table",
                    precision = 0, sparse = TRUE)[[1]]
  expect_s3_class(x4, "data.table")
  expect_equal(dim(x4), c(92466.0, 3))
  expect_equal(colnames(x3), c("rt", "mz", "intensity"))
  expect_equal(attr(x3, "method"), attr(x2, "method"))
  expect_equal(attr(x3, "detector"), attr(x2, "detector"))
  expect_equal(attr(x3, "data_format"), "long")
  expect_equal(x3[intensity!=0], x4)

  expect_equal(unname(colSums(x1$MS1)),
    c(2410340.08160019, 27133679.6999996, 17657612))
  expect_equal(unname(as.matrix(x1$MS1[1:5, ])),
    structure(c(0.0791666666666667, 0.0791666666666667, 0.0791666666666667,
    0.0791666666666667, 0.0791666666666667, 915.7, 865.4, 840.4, 727.5,
    680.6, 112, 184, 157, 145, 120), dim = c(5L, 3L)))
  expect_equal(unname(as.matrix(x1$MS1[nrow(x1$MS1), ])),
    structure(c(44.9728666666667, 105.2, 313), dim = c(1L, 3L)))
  expect_equal(max(x1$MS1$intensity),
    14859)
  expect_equal(unname(colSums(x1$BPC)),
    c(57080.9160833333, 598612.000000003, 1973751))
  expect_equal(unname(as.matrix(x1$BPC[1:3, ])),
    structure(c(0.0791666666666667, 0.0968833333333333, 0.114616666666667,
    105.2, 105.2, 105.1, 644, 410, 305), dim = c(3L, 3L)))
  expect_equal(sum(x1$TIC[, 1]),
    17703817)
  expect_equal(unname(head(x1$TIC[, 1], 5)),
    c(13924, 11824, 5041, 1034, 1000))
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
  expect_equal(head(get_times(x),1), -0.001333333, tolerance = .00001)
  expect_equal(tail(get_times(x),1), 32.002, tolerance = .00001)

  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "NVAC-6B1-S3R1")
  expect_equal(attr(x, "detector_id"), "G1315B")
  expect_true(is.na(attr(x, "detector")))
  expect_equal(attr(x, "instrument"), "LC")
  expect_equal(attr(x, "detector_y_unit"), "mAU")
  expect_equal(attr(x, "method"), "JCMONO1.M")
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "data_format"), "wide")

  x1 <- read_chroms(path, parser = "chromconverter", format_out = "data.frame",
                    data_format = "long", progress_bar = FALSE)[[1]]
  expect_equal(colnames(x1), c("rt","intensity"))
  expect_s3_class(x1[1], "data.frame")
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
  expect_equal(head(x1$rt, 1), -.00133333333333333, tolerance = .00001)
  expect_equal(tail(x1$rt, 1), 32.002, tolerance = .00001)
  expect_equal(attr(x1, "data_format"), "long")

  expect_equal(sum(x),
    1389686.77091599)
  expect_equal(range(x),
    c(-23.5595703125, 2085.93797683716))
  expect_equal(which.max(x),
    2012L)
  expect_equal(unname(head(x[, 1], 5)),
    c(-0.72479248046875, -0.726222991943359, -0.720024108886719,
    -0.714302062988281, -0.710010528564453))
  expect_equal(unname(tail(x[, 1], 5)),
    c(2.08759307861328, 2.08568572998047, 2.09283828735352, 2.09903717041016,
    2.09712982177734))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(-0.72479248046875, 70.218563079834, 98.7544059753418, 20.2045440673828,
    26.1750221252441, 34.522533416748, 45.8745956420898, 20.2937126159668,
    13.2441520690918, 6.53409957885742, 2.96545028686523, 1.22594833374023,
    0.322341918945312, 5.03826141357422, 2.09712982177734))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(-0.00133333333333333, -5e-04, 0.000333333333333333))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(32.0003333333333, 32.0011666666667, 32.002))
  expect_equal(attr(x, "intensity_multiplier"),
    0.000476837158203125)
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

  expect_equal(head(get_times(x), 1), 0.000333333333333333, tolerance = .00001)
  expect_equal(head(get_times(x1), 1), 0.000333333333333333, tolerance = .00001)

  expect_equal(tail(get_times(x), 1), 31.9911666666667, tolerance = .00001)
  expect_equal(tail(get_times(x1), 1), 31.9911666666667, tolerance = .00001)

  # check metadata
  expect_equal(attr(x1, "parser"), "entab")
  expect_equal(attr(x, "parser"), "chromconverter")

  expect_equal(attr(x, "sample_name"), "NVAC-6B1-S3R1")
  expect_equal(attr(x, "sample_name"), attr(x1, "sample_name"))

  expect_equal(attr(x, "detector"), "DAD")
    expect_equal(attr(x, "detector"), attr(x1, "detector"))

  expect_equal(attr(x, "detector_id"), "G1315B")
  expect_equal(attr(x, "detector_range"), c(250, 600))
  expect_equal(attr(x, "detector_y_unit"), "mAU")

  expect_equal(attr(x, "method"), "JCMONO1.M")
  expect_equal(attr(x, "method"), attr(x1, "method"))

  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "time_unit"), attr(x1, "time_unit"))

  expect_equal(attr(x, "data_format"), "wide")
  expect_equal(attr(x, "data_format"), attr(x1, "data_format"))

  expect_equal(sum(x),
    64460273.4203339)
  expect_equal(range(x),
    c(-34.482479095459, 2320.54805755615))
  expect_equal(unname(colSums(x)[c(1, 88, 176)]),
    c(1366884.23919678, 150754.784584045, 14532.4759483337))
  expect_equal(unname(x[1, 1:5]),
    c(-0.699043273925781, -0.710487365722656, -0.657081604003906,
    -0.722885131835938, -0.674724578857422))
  expect_equal(unname(x[nrow(x), 172:176]),
    c(0.253200531005859, 0.290393829345703, 0.310897827148438,
    0.240802764892578, 0.224590301513672))
  expect_equal(unname(x[13830, c(1, 88, 176)]),
    c(38.210391998291, 1.06906890869141, 0.332355499267578))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 12)), 88]),
    c(-0.555515289306641, 4.42886352539062, 3.22723388671875,
    1.34944915771484, 1.99985504150391, 74.0513801574707, 0.875949859619141,
    0.666141510009766, -0.487327575683594, -0.648975372314453,
    -3.80277633666992, 1.29938125610352))
  expect_equal(attr(x, "intensity_multiplier"),
    0.000476837158203125)
})

test_that("read_chroms can read 'Agilent ChemStation' version 81 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_81.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, progress_bar = FALSE)[[1]]

  expect_equal(dim(x), c(2699, 1))
  expect_equal(head(get_times(x), 1), 3.00044479166667, tolerance = .00001)
  expect_equal(tail(get_times(x), 1), 11.9971114583333, tolerance = .00001)

  # check metadata
  expect_equal(class(x)[1], "matrix")
  expect_equal(dim(x), c(2699, 1))
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "detector_id"), "HP G1530A")
  expect_equal(attr(x, "sample_name"), "5970 mix 10nG")
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "data_format"), "wide")

  # long format
  x1 <- read_chroms(path, progress_bar = FALSE,
                    format_out = "data.table", data_format = "long")[[1]]
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_s3_class(x1[1], "data.table")
  expect_equal(dim(x1), c(2699, 2))
  expect_equal(as.numeric(rownames(x)), x1[[1]])
  expect_equal(x[,1], x1[[2]], ignore_attr = TRUE)
  expect_equal(head(x1$rt, 1), 3.00044479166667, tolerance = .00001)
  expect_equal(tail(x1$rt, 1), 11.9971114583333, tolerance = .00001)
  expect_equal(attr(x1, "data_format"), "long")

  expect_equal(sum(x),
    47813.2741082398)
  expect_equal(range(x),
    c(17.4561207020743, 31.3735693445924))
  expect_equal(which.max(x),
    557L)
  expect_equal(unname(head(x[, 1], 5)),
    c(17.5007821627369, 17.5005217460566, 17.5053394546412, 17.4997404960159,
    17.5023446628184))
  expect_equal(unname(tail(x[, 1], 5)),
    c(17.8859384328243, 17.8838550993823, 17.883464474362, 17.8837248910422,
    17.8859384328243))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(17.5007821627369, 17.7187509241048, 17.5089852881647, 17.4744800780318,
    17.4923186206288, 17.4674488276651, 17.4833342451602, 17.487240495364,
    17.599480084551, 17.6227873774333, 17.6053394598566, 17.6699227965582,
    17.7399748835451, 17.8773446823761, 17.8859384328243))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(3.00044479166667, 3.00377936048307, 3.00711392929948))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(11.9904423207005, 11.9937768895169, 11.9971114583333))
  expect_equal(attr(x, "intensity_multiplier"),
    0.000130208340124227)
})

test_that("read_chroms can write 'Agilent ChemStation' version 81 files to CDF", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("ncdf4")

  path <- system.file("chemstation_81.ch",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))
  path_out <- tempdir()
  x <- read_chroms(path, progress_bar = FALSE, export_format = "cdf",
                   path_out = path_out, force=TRUE)[[1]]

  xx <- read_chroms(fs::path(path_out, "5970_mix_10nG.cdf"),
                    progress_bar = FALSE)[[1]]
  expect_equal(x, xx, ignore_attr = TRUE, tolerance = 1e-7)
  expect_equal(get_times(x), get_times(xx))
  fields <-c("sample_name", "detector_id", "detector_y_unit",
             "method", "operator", "time_interval", "time_unit", "run_datetime")
  expect_equal(attributes(x)[fields], attributes(xx)[fields],
               ignore_attr = TRUE)
  # version 81 files record no detector type, and an `NA` field comes back from
  # CDF as an empty string, so `detector` is checked separately
  expect_true(is.na(attr(x, "detector")))
  expect_equal(attr(xx, "detector"), "")
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
  expect_equal(head(get_times(x),1), 0.00583333333333333, tolerance = .00001)
  expect_equal(tail(get_times(x),1), 84.9991666666667, tolerance = .00001)

  # check metadata
  expect_equal(attr(x, "sample_name"), "0-CN-6-6-PU")
  expect_equal(attr(x, "detector_y_unit"), "mAU")
  expect_equal(attr(x, "method"), "Phenolics_new2.M")
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "data_format"), "wide")

  # long format
  x1 <- read_chroms(path, data_format = "long", format_out = "data.table",
                    progress_bar = FALSE)[[1]]
  expect_equal(as.numeric(rownames(x)), x1[[1]])
  expect_equal(x[,1], x1[[2]], ignore_attr = TRUE)
  expect_s3_class(x1[1], c("data.table","data.frame"))
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_equal(dim(x1), c(12750, 2))
  expect_equal(head(x1$rt,1), 0.00583333333333333, tolerance = .00001)
  expect_equal(tail(x1$rt,1), 84.9991666666667, tolerance = .00001)

  expect_equal(attr(x1, "sample_name"), "0-CN-6-6-PU")
  expect_equal(attr(x1, "detector_y_unit"), "mAU")
  expect_equal(attr(x1, "method"), "Phenolics_new2.M")
  expect_equal(attr(x1, "time_unit"), "Minutes")
  expect_equal(attr(x1, "data_format"), "long")

  expect_equal(sum(x),
    94265.6593322754)
  expect_equal(range(x),
    c(-0.160694122314453, 482.753276824951))
  expect_equal(which.max(x),
    4625L)
  expect_equal(unname(head(x[, 1], 5)),
    c(-0.0982284545898438, -0.0691413879394531, -0.0452995300292969,
    -0.0290870666503906, -0.0233650207519531))
  expect_equal(unname(tail(x[, 1], 5)),
    c(2.59780883789062, 2.58731842041016, 2.5787353515625, 2.57301330566406,
    2.56919860839844))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(-0.0982284545898438, 3.39984893798828, 0.159263610839844,
    0.113487243652344, 2.37417221069336, 2.17580795288086, 5.96761703491211,
    6.07109069824219, 4.53329086303711, 5.401611328125, 4.80127334594727,
    53.5097122192383, 0.374794006347656, 1.57976150512695, 2.56919860839844))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(0.00583333333333333, 0.0125, 0.0191666666666667))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(84.9858333333333, 84.9925, 84.9991666666667))
  expect_equal(attr(x, "intensity_multiplier"),
    0.000476837158203125)
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
  expect_equal(head(get_times(x),1), 0.001125, tolerance = .00001)
  expect_equal(tail(get_times(x),1), 36, tolerance = .00001)

  # check metadata
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "STD_1_1mM-1MKHCO3")
  expect_equal(attr(x, "detector_y_unit"), "nRIU")
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "data_format"), "wide")

  # long format
  x1 <- read_chroms(path, progress_bar = FALSE,
                    format_out = "data.frame", data_format = "long")[[1]]
  expect_equal(as.numeric(rownames(x)), x1[[1]])
  expect_equal(x[,1], x1[[2]], ignore_attr = TRUE)
  expect_s3_class(x1[1], "data.frame")
  expect_equal(colnames(x1), c("rt", "intensity"))
  expect_equal(as.numeric(rownames(x)), x1[,1])
  expect_equal(x[,1], x1[,2], ignore_attr = TRUE)
  expect_equal(head(x1$rt,1), 0.001125, tolerance = .00001)
  expect_equal(tail(x1$rt,1), 36, tolerance = .00001)
  expect_equal(attr(x1, "data_format"), "long")

  expect_equal(sum(x),
    -6851316.46)
  expect_equal(range(x),
    c(-43246.99, 3956.53))
  expect_equal(which.max(x),
    3417L)
  expect_equal(unname(head(x[, 1], 5)),
    c(0.39, 0.59, 0.68, 0.66, 0.52))
  expect_equal(unname(tail(x[, 1], 5)),
    c(135.59, 133.79, 131.99, 130.2, 128.43))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(0.39, 1.34, 3.46, 10.8, 105.96, 20.55, 11.38, -884.79, -106.12, -50.54,
    -20.29, -24.91, -0.75, 7.1, 128.43))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(0.001125, 0.00472524752475248, 0.00832549504950495))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(35.9927995049505, 35.9963997524752, 36))
  expect_equal(attr(x, "intensity_multiplier"),
    0.01)
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
  expect_equal(head(get_times(x),1), 0.000326049995422363, tolerance = .00001)
  expect_equal(tail(get_times(x),1), 18.2346604166667, tolerance = .00001)

  # check metadata
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "393006_A1_diol_Al")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "software"), "Mustang ChemStation")
  expect_equal(attr(x, "method"), "NGS Default Edit.M")
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "data_format"), "wide")

  # test scale argument
  x1 <- read_chroms(path, progress_bar = FALSE, scale=FALSE)[[1]]
  expect_equal(x, x1*attr(x1,"intensity_multiplier"), ignore_attr = TRUE)

  expect_equal(sum(x),
    15517120.0723939)
  expect_equal(range(x),
    c(9.10929965277778, 11367.90334375))
  expect_equal(which.max(x),
    16456L)
  expect_equal(unname(head(x[, 1], 5)),
    c(9.13388628472222, 9.13298298611111, 9.13488923611111, 9.13205260416667,
    9.13278159722222))
  expect_equal(unname(tail(x[, 1], 5)),
    c(18.7625970486111, 18.7272180555556, 18.7075598958333, 18.6918010416667,
    18.6870208333333))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(9.13388628472222, 9.12766527777778, 9.15994513888889, 9.14195121527778,
    3536.96170833333, 9.76124809027778, 9.29714079861111, 9.41890989583334,
    10.0017868055556, 359.667555034722, 139.188453993056, 10.2155680555556,
    349.624839236111, 13.4060611111111, 18.6870208333333))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(0.000326049995422363, 0.000659383347645664, 0.000992716699868966))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(18.2339937499622, 18.2343270833144, 18.2346604166667))
  expect_equal(sum(x1),
    119171482155.985)
  expect_equal(unname(head(x1[, 1], 5)),
    c(70148.2466666667, 70141.3093333333, 70155.9493333334, 70134.164,
    70139.7626666667))
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
  expect_equal(head(get_times(x), 1), 0.00083331667582194, tolerance = .00001)
  expect_equal(tail(get_times(x), 1), 19, tolerance = .00001)
  expect_equal(attr(x, "data_format"), "wide")

  # check metadata
  expect_equal(attr(x, "parser"), "chromconverter")
  expect_equal(attr(x, "sample_name"), "NI cat")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "software"), "Asterix ChemStation")
  expect_equal(attr(x, "method"), "Sine14.M")
  expect_equal(attr(x, "time_unit"), "Minutes")

  expect_equal(sum(x),
    19.8259068687756)
  expect_equal(range(x),
    c(0.000862646102905273, 0.000896255175272624))
  expect_equal(which.max(x),
    21781L)
  expect_equal(unname(head(x[, 1], 5)),
    c(0.000863722960154216, 0.000863691171010335, 0.00086359977722168,
    0.000863456726074219, 0.000863293806711833))
  expect_equal(unname(tail(x[, 1], 5)),
    c(0.000896100203196208, 0.000896108150482178, 0.000896060466766357,
    0.000895984967549642, 0.000895949204762777))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(0.000863722960154216, 0.000863738854726156, 0.000863226254781087,
    0.000863413016001383, 0.0008636474609375, 0.000863635540008545,
    0.000864279270172119, 0.000865034262339274, 0.000865697860717773,
    0.000867696603139242, 0.000871018568674723, 0.000873978932698568,
    0.000875476996103922, 0.000891304016113281, 0.000895949204762777))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(0.00083331667582194, 0.0016666500098859, 0.00249998334394986))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(18.9983333333319, 18.9991666666659, 19))
  expect_equal(attr(x, "intensity_multiplier"),
    0.000130208333333333)
})

test_that("read_chroms can read 'Agilent MassHunter' dad files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")

  path <- system.file("masshunter.d/AcqData/DAD1.sp",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "masshunter_dad", parser = "entab",
                   progress_bar = FALSE)[[1]]
  # the 'aston' parsers are deprecated; the warning fires once per session
  x1 <- suppressWarnings(
    read_chroms(path, format_in = "masshunter_dad", parser = "aston",
                progress_bar = FALSE)[[1]])
  expect_equal(dim(x), c(240, 276))
  expect_equal(class(x)[1], "matrix")
  expect_equal(x, x1, ignore_attr = TRUE)
  expect_equal(attr(x, "parser"), "entab")
  expect_equal(attr(x1, "parser"), "aston")
  expect_equal(attr(x, "data_format"), "wide")
  expect_equal(attr(x1, "data_format"), "wide")

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
  expect_equal(attr(x, "data_format"), "long")
  expect_equal(attr(x1, "data_format"), "long")
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
  expect_equal(head(get_times(x$FID1A), 1), -.00181875, tolerance = .00001)
  expect_equal(tail(get_times(x$FID1A), 1), 19.7048479166667, tolerance = .00001)
  expect_equal(head(get_times(x$V181), 1), -.00126875, tolerance = .00001)
  expect_equal(tail(get_times(x$V181), 1), 19.7053979166667, tolerance = .00001)

  # check metadata
  expect_equal(attr(x[[1]], "sample_name"), "blanc421")
  expect_equal(attr(x[[1]], "file_version"), "181")
  expect_equal(attr(x[[1]], "detector_y_unit"), "pA")
  expect_equal(attr(x[[1]], "method"), "DET3300.M")
  expect_equal(attr(x[[1]], "run_datetime"),
               as.POSIXct("2022-8-23 12:16:25", tz = "UTC"))
  expect_equal(attr(x[[1]], "time_unit"), "Minutes")
  expect_equal(attr(x[[1]], "data_format"), "wide")

  expect_equal(attr(x[[2]], "sample_name"), "140+H")
  expect_equal(attr(x[[2]], "file_version"), "181")
  expect_equal(attr(x[[2]], "detector_y_unit"), "pA")
  expect_equal(attr(x[[2]], "method"), "DET3300.M")
  expect_equal(attr(x[[2]], "run_datetime"),
               as.POSIXct("2022-8-23 12:48:20", tz = "UTC"))
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
  expect_equal(head(x1$FID1A$rt, 1), -.00181875, tolerance = .00001)
  expect_equal(tail(x1$FID1A$rt, 1), 19.7048479166667, tolerance = .00001)

  expect_equal(extract_metadata(x)[,c(1:8)], extract_metadata(x1)[,c(1:8)])
  expect_equal(attr(x1[[1]], "data_format"), "long")

  expect_warning(read_chroms(path, format_in = "agilent_d", what = "dad",
                             progress_bar = FALSE))
  expect_error(read_agilent_d(path, what = "dad"))


  expect_equal(sum(x$FID1A),
    92783.4559895832)
  expect_equal(range(x$FID1A),
    c(2.13880208333333, 263.427213541667))
  expect_equal(which.max(x$FID1A),
    51L)
  expect_equal(unname(head(x$FID1A[, 1], 5)),
    c(2.14322916666667, 2.144140625, 2.13971354166667, 2.13880208333333,
    2.1390625))
  expect_equal(unname(tail(x$FID1A[, 1], 5)),
    c(20.1201822916667, 20.1235677083333, 20.1274739583333, 20.1287760416667,
    20.1287760416667))
  expect_equal(unname(x$FID1A[round(seq(1, nrow(x$FID1A), length.out = 15)), 1]),
    c(2.14322916666667, 4.44778645833333, 8.048828125, 13.4111979166667,
    19.693359375, 7.922265625, 7.55260416666667, 18.4865885416667,
    20.0388020833333, 20.247265625, 20.2434895833333, 20.2096354166667,
    20.1368489583333, 20.1915364583333, 20.1287760416667))
  expect_equal(head(as.numeric(rownames(x$FID1A)), 3),
    c(-0.00181875, 0.00151401960369807, 0.00484678920739613))
  expect_equal(tail(as.numeric(rownames(x$FID1A)), 3),
    c(19.6981823774593, 19.701515147063, 19.7048479166667))
})

test_that("read_chroms can read 'Agilent' .dx files with OL179", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("agilent.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "openlab_dx", what = c("chroms", "instrument"),
                   progress_bar = FALSE)[[1]]
  expect_equal(class(x$chroms)[1], "matrix")
  expect_equal(colnames(x$chroms),"intensity")
  expect_equal(dim(x$chroms), c(10000, 1))
  expect_equal(attr(x$chroms, "parser"), "chromconverter")
  expect_equal(attr(x$chroms, "data_format"), "wide")
  expect_equal(head(get_times(x$chroms),1), 0.001125, tolerance = .00001)
  expect_equal(tail(get_times(x$chroms),1), 36, tolerance = .00001)
  expect_equal(basename(attr(x$chroms, "source_file")), "agilent.dx")

  # auxiliary instrumental data
  expect_equal(dim(x$instrument$`PMP1C,Solvent Ratio A`), c(43253, 1))

  expect_true(all(
    sapply(x$instrument, function(x) round(tail(get_times(x),1))) == 36)
  )

  expect_true(all(
    sapply(x$instrument, function(x) round(head(get_times(x),1))) == 0)
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
  expect_equal(basename(attr(x$instrument[[1]], "source_file")), "agilent.dx")

  x1 <- read_chroms(path, format_in="agilent_dx", what = c("chroms","instrument"),
                    progress_bar = FALSE, data_format = "long",
                    format_out = "data.frame")[[1]]
  expect_s3_class(x1$chroms[1], "data.frame")
  expect_equal(dim(x1$chroms), c(10000, 2))
  expect_equal(dim(x1$instrument[["PMP1C,Solvent Ratio A"]]), c(43253, 2))
  expect_equal(attr(x1$chroms,"data_format"),"long")

  # A failed read is one condition, not a warning plus a message. The list of
  # affected files used to be a `message`, which `suppressWarnings` could not
  # silence and a caller handling `warning` never saw.
  conditions <- list(warnings = character(), messages = character())
  withCallingHandlers(
    read_chroms(path, format_in = "agilent_dx", what = "dad",
                progress_bar = FALSE),
    warning = function(w){
      conditions$warnings <<- c(conditions$warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m){
      conditions$messages <<- c(conditions$messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_length(conditions$warnings, 1)
  expect_length(conditions$messages, 0)
  expect_match(conditions$warnings, "could not be interpreted")
  expect_match(conditions$warnings, "agilent")
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

test_that("read_chroms can read 'Agilent' .dx files with OL130", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("MeOH1.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "agilent_dx",
                   what = c("chroms", "dad", "instrument"),
                   progress_bar = FALSE)[[1]]
  expect_true(inherits(x$chroms, "list"))
  expect_true(inherits(x$chroms[[1]], "matrix"))
  expect_true(inherits(x$dad, "matrix"))
  expect_true(inherits(x$instrument, "list"))
  expect_true(inherits(x$instrument[[1]], "matrix"))

  expect_equal(vapply(x$chroms, nrow, numeric(1), USE.NAMES = FALSE), rep(4050,5))
  expect_equal(dim(x$dad), c(4050,156))

  expect_equal(colnames(x$chroms[[1]]),"intensity")
  expect_equal(dim(x$chroms[[1]]), c(4050, 1))
  expect_equal(attr(x$chroms[[1]], "parser"), "chromconverter")
  expect_equal(attr(x$chroms[[1]], "data_format"), "wide")
  expect_equal(head(get_times(x$chroms),1), 0.00125, tolerance = .00001)
  expect_equal(head(get_times(x$dad),1), 0.00125, tolerance = .00001)

  expect_equal(tail(get_times(x$chroms),1), 27, tolerance = .00001)
  expect_equal(tail(get_times(x$dad),1), 26.9946, tolerance = .00001)

  # auxiliary instrumental data
  expect_equal(dim(x$instrument$`PMP1C,Solvent Ratio A`), c(32440.0, 1))

  expect_true(all(
    sapply(x$instrument, function(x) round(tail(get_times(x),1))) == 27)
  )

  expect_true(all(
    sapply(x$instrument, function(x) round(head(get_times(x),1))) == 0)
  )

  expect_true(all(head(x$instrument$`PMP1C,Solvent Ratio A`) == 92))
  expect_true(all(tail(x$instrument$`PMP1C,Solvent Ratio A`) == 10))
  expect_true(all(x$instrument$`PMP1D,Solvent Ratio C` == 0))

  expect_true(all(round(x$instrument$`THM1B,Right Temperature`) == 20))

  expect_equal(head(names(x$instrument), 5), c("WPS1A,Temperature",
                                              "THM1B,Right Temperature",
                                              "THM1A,Left Temperature",
                                              "DAD1V,UV Lamp Anode Voltage",
                                              "DAD1U,Optical Unit Temperature"))

  expect_equal(sapply(x$instrument, function(xx) attr(xx, "detector_y_unit")),
               c("\u00b0C", "\u00b0C", "\u00b0C", "V", "\u00b0C", "\u00b0C",
               "", "%", "%", "%", "%", "mL/min", "bar", "counts"),
               ignore_attr = TRUE)

  expect_equal(sapply(x$instrument, function(xx) attr(xx, "intensity_multiplier")),
               c(1e-3, 1e-3, 1e-3, 1e-6, 1e-2, 1e-2, 1e-5, 1e-3, 1e-3, 1e-3,
                 1e-3, 1e-6, 5e-3, 1e0),
               ignore_attr = TRUE)

  expect_true(all(
    sapply(x$instrument, function(xx){
      attr(xx, "run_datetime")
    }) == 1749578656))
  expect_equal(attributes(x$dad)$sample_position, "D1B-B3")

  # check that source file is passed through
  expect_equal(basename(attr(x$chroms[[1]], "source_file")), "MeOH1.dx")
  expect_equal(basename(attr(x$dad, "source_file")), "MeOH1.dx")
  expect_equal(basename(attr(x$instrument[[1]], "source_file")), "MeOH1.dx")
})

test_that("read_chroms can read 'Agilent' .sirslt directories", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab.sirslt", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path))

  x <- read_chroms(path, format_in = "agilent_rslt", progress_bar = FALSE)
  expect_s3_class(x, "chrom_list")
  expect_equal(length(x), 1)

  # injections are named after the source .dx file by default
  expect_equal(names(x), "Norbert II-2026-05-26 16-19-41-05-00")

  s <- x[[1]]
  expect_equal(names(s), c("chroms", "dad"))
  expect_equal(names(s$chroms), c("DAD1D,Sig=360,4  Ref=off",
                                  "DAD1C,Sig=260,4  Ref=off",
                                  "DAD1B,Sig=200,4  Ref=off",
                                  "DAD1A,Sig=229,4  Ref=off"))
  expect_equal(vapply(s$chroms, nrow, numeric(1), USE.NAMES = FALSE),
               rep(1500, 4))
  expect_equal(dim(s$dad), c(1500, 106))
  expect_equal(as.numeric(head(colnames(s$dad), 1)), 190)
  expect_equal(as.numeric(tail(colnames(s$dad), 1)), 400)

  expect_equal(class(s$chroms[[1]])[1], "matrix")
  expect_equal(colnames(s$chroms[[1]]), "intensity")
  expect_equal(attr(s$chroms[[1]], "parser"), "chromconverter")
  expect_equal(attr(s$chroms[[1]], "data_format"), "wide")
  expect_equal(attr(s$chroms[[1]], "source_file_format"), "chemstation_179_8b")
  expect_equal(attr(s$dad, "source_file_format"), "chemstation_131_OL")

  expect_equal(head(get_times(s$chroms[[1]]), 1), 0.0054167, tolerance = .00001)
  expect_equal(tail(get_times(s$chroms[[1]]), 1), 10, tolerance = .00001)
  expect_equal(head(get_times(s$dad), 1), 0.0054167, tolerance = .00001)
  expect_equal(tail(get_times(s$dad), 1), 9.99875, tolerance = .00001)

  expect_equal(head(s$chroms[[1]][, 1], 3),
               c(-0.6070137, -0.7395744, -0.8668900),
               tolerance = .0001, ignore_attr = TRUE)
  expect_equal(s$dad[1, 1:3], c(-1056, -2985, -4263), ignore_attr = TRUE)

  # the .dx file inside the .sirslt directory is the source of the raw data
  expect_equal(basename(attr(s$chroms[[1]], "source_file")),
               "Norbert II-2026-05-26 16-19-41-05-00.dx")
  expect_equal(basename(attr(s$dad, "source_file")),
               "Norbert II-2026-05-26 16-19-41-05-00.dx")
})

test_that("read_agilent_rslt attaches metadata from the .acaml file", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab.sirslt", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path))

  s <- read_agilent_rslt(path)[[1]]

  # acaml fields are attached to the list holding the injection's traces,
  # not to the individual traces
  expect_equal(attr(s, "instrument"), "Norbert II")
  expect_equal(attr(s, "software_name"), "OpenLabCDS")
  expect_equal(attr(s, "software_version"), "2.8 - Build  2.8.0-REL_2.8.0.1330")
  expect_equal(attr(s, "method"), "flow_rate_simple_test")
  expect_equal(attr(s, "batch"), "SingleSample")
  expect_equal(attr(s, "sample_type"), "Sample")
  expect_equal(attr(s, "sample_injection_volume"), 0)
  expect_equal(attr(s, "sample_amount"), 0)

  # `InjectionAcqDateTime` is converted to a POSIXct timestamp
  expect_s3_class(attr(s, "run_datetime"), "POSIXct")
  expect_equal(attr(s, "run_datetime"),
               as.POSIXct("2026-05-26 21:20:23", tz = "UTC"))

  # empty acaml fields (`SampleName`, `VialNumber`) are not attached
  expect_null(attr(s, "sample_name"))
  expect_null(attr(s, "sample_position"))

  # the full acaml row is retained
  meta <- attr(s, "acaml_metadata")
  expect_s3_class(meta, "data.frame")
  expect_equal(nrow(meta), 1)
  expect_equal(meta$RawDataFileName, "Norbert II-2026-05-26 16-19-41-05-00.dx")
  expect_equal(meta$InjectionId, "832b1278-5b39-4f20-b37b-3b4a6a1d449a")
  expect_equal(meta$SourceFile, "Norbert II-2026-05-26 16-19-41-05-00.acaml")

  # sample-level fields take precedence over the values read from the .dx file
  x <- read_chroms(path, format_in = "agilent_rslt", progress_bar = FALSE)
  mtd <- extract_metadata(x)
  expect_true(all(mtd$instrument == "Norbert II"))
  expect_true(all(mtd$method == "flow_rate_simple_test"))
  expect_true(all(mtd$batch == "SingleSample"))

  # `read_metadata = FALSE` skips the acaml file altogether
  s0 <- read_agilent_rslt(path, read_metadata = FALSE)[[1]]
  expect_null(attr(s0, "acaml_metadata"))
  expect_null(attr(s0, "instrument"))
  expect_equal(setdiff(names(attributes(s0$chroms[[1]])),
                       c("dim", "dimnames")), character(0))

  # `metadata_format = "raw"` reaches the underlying .dx parser
  sr <- read_agilent_rslt(path, metadata_format = "raw")[[1]]
  expect_true("metadata" %in% names(attributes(sr$chroms[[1]])))
  expect_equal(attr(sr, "instrument"), "Norbert II")
})

test_that("read_agilent_rslt respects its arguments", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab.sirslt", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path))

  # `rslt` and `sirslt` are aliases for `agilent_rslt`
  x <- read_chroms(path, format_in = "agilent_rslt", progress_bar = FALSE)
  expect_equal(x, read_chroms(path, format_in = "rslt", progress_bar = FALSE))
  expect_equal(x, read_chroms(path, format_in = "sirslt", progress_bar = FALSE))

  # .sirslt directories are also found by scanning a parent directory
  xp <- read_chroms(dirname(path), format_in = "sirslt", progress_bar = FALSE)
  expect_equal(names(xp), "Norbert II-2026-05-26 16-19-41-05-00")

  # a single element is collapsed unless `collapse = FALSE`
  xc <- read_agilent_rslt(path, what = "chroms")[[1]]
  expect_equal(names(xc), c("DAD1D,Sig=360,4  Ref=off",
                            "DAD1C,Sig=260,4  Ref=off",
                            "DAD1B,Sig=200,4  Ref=off",
                            "DAD1A,Sig=229,4  Ref=off"))
  expect_equal(names(read_agilent_rslt(path, what = "chroms",
                                       collapse = FALSE)[[1]]), "chroms")

  xd <- read_agilent_rslt(path, what = "dad")[[1]]
  expect_true(inherits(xd, "matrix"))
  expect_equal(dim(xd), c(1500, 106))

  # `data_format` and `format_out` are passed through
  x1 <- read_chroms(path, format_in = "rslt", data_format = "long",
                    format_out = "data.frame", progress_bar = FALSE)[[1]]
  expect_s3_class(x1$chroms[[1]], "data.frame")
  expect_equal(dim(x1$chroms[[1]]), c(1500, 2))
  expect_equal(colnames(x1$chroms[[1]]), c("rt", "intensity"))
  expect_equal(dim(x1$dad), c(159000, 3))
  expect_equal(colnames(x1$dad), c("rt", "lambda", "intensity"))
  expect_equal(attr(x1$chroms[[1]], "data_format"), "long")
  expect_equal(x1$chroms[[1]]$intensity,
               as.numeric(read_agilent_rslt(path)[[1]]$chroms[[1]][, 1]))

  x2 <- read_chroms(path, format_in = "rslt", format_out = "data.table",
                    progress_bar = FALSE)[[1]]
  expect_s3_class(x2$chroms[[1]], "data.table")

  # `path_out` unzips the .dx file instead of using a temporary directory
  tmp <- fs::path(tempdir(), "rslt_out")
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  expect_no_error(read_agilent_rslt(path, path_out = tmp))
  expect_equal(sort(basename(list.files(tmp, recursive = TRUE))),
               sort(c("82ca0ef5-42cd-4148-a513-b3c161895ad7.CH",
                      "e3d7b495-7c9b-4502-9adf-61cf4c0df936.CH",
                      "fa8d419e-53bd-4db9-bdf4-cd790cb61d06.CH",
                      "fcc9759c-dca2-40b9-86fd-2818920958e8.UV",
                      "fe2467a0-7b70-4fff-b616-1379efb3326c.CH")))
})

test_that("read_agilent_rslt handles missing files gracefully", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab.sirslt", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path))

  # a directory without .dx files is an error
  empty <- fs::path(tempdir(), "empty.rslt")
  fs::dir_create(empty)
  on.exit(unlink(empty, recursive = TRUE), add = TRUE)
  expect_error(read_agilent_rslt(empty), "No .dx files found")

  # a .dx file with no corresponding acaml row is returned with a warning
  mismatch <- fs::path(tempdir(), "mismatch.rslt")
  fs::dir_create(mismatch)
  on.exit(unlink(mismatch, recursive = TRUE), add = TRUE)
  file.copy(list.files(path, pattern = "\\.dx$", full.names = TRUE),
            fs::path(mismatch, "other.dx"))
  file.copy(list.files(path, pattern = "\\.acaml$", full.names = TRUE),
            fs::path(mismatch, "meta.acaml"))

  expect_warning(xm <- read_agilent_rslt(mismatch),
                 "No acaml metadata found for other.dx")
  expect_equal(names(xm), "other")
  expect_null(attr(xm[[1]], "acaml_metadata"))
  expect_equal(names(xm[[1]]), c("chroms", "dad"))

  # this file's acaml `SampleName` is blank, which is not a usable name, so
  # `sample_names = "sample_name"` warns and falls back on the file name
  expect_warning(xs <- read_agilent_rslt(path, sample_names = "sample_name"),
                 "could not be determined")
  expect_equal(names(xs), "Norbert II-2026-05-26 16-19-41-05-00")

  # with no metadata read at all there is nothing to name the sample after
  expect_warning(xn <- read_agilent_rslt(path, sample_names = "sample_name",
                                         read_metadata = FALSE),
                 "could not be determined")
  expect_equal(names(xn), "Norbert II-2026-05-26 16-19-41-05-00")

  # a `SampleName` that is actually filled in is used
  named <- fs::path(tempdir(), "named.rslt")
  fs::dir_create(named)
  on.exit(unlink(named, recursive = TRUE), add = TRUE)
  file.copy(list.files(path, pattern = "\\.dx$", full.names = TRUE), named)
  acaml <- list.files(path, pattern = "\\.acaml$", full.names = TRUE)
  writeLines(sub('SampleName=""', 'SampleName="RP_Frt_37C"',
                 readLines(acaml, warn = FALSE), fixed = TRUE),
             fs::path(named, basename(acaml)))

  expect_silent(xy <- read_agilent_rslt(named, sample_names = "sample_name"))
  expect_equal(names(xy), "RP_Frt_37C")
  expect_equal(attr(xy[[1]], "sample_name"), "RP_Frt_37C")
})

test_that("read_chroms can read 'Agilent ACAML' files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("minimal.acaml",
                      package = "chromConverterExtraTests")

  skip_if_not(file.exists(path))

  x <- read_acaml(path, progress_bar = FALSE)
  expect_s3_class(x, "data.frame")
  expect_equal(x$SampleName, "RP_Frt_37C_0.5x")
  expect_equal(x$VialNumber, "D2B-G9")
  x1 <- read_acaml(path, progress_bar = FALSE, format_out = "data.table")
  expect_s3_class(x1, "data.table")
  x2 <- read_acaml(path, progress_bar = FALSE, format_out = "tibble")
  expect_s3_class(x2, "tbl")
})

test_that("read_agilent_amx works correctly, part 1", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("column_storage_ACN100.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  method1 <- read_agilent_amx(path)

  expect_equal(names(method1$metadata),
               c("method_name", "version", "status", "created", "created_by",
                 "modified", "modified_by")
  )
  expect_equal(method1$dad$peakwidth_nm, 4)
  expect_equal(nrow(method1$pump$gradient), 0)
  expect_equal(as.numeric(method1$metadata$created), 1767977263.0)
})

test_that("read_agilent_amx works correctly, part 2", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  method2 <- read_agilent_amx(path)

  expect_equal(names(method2$metadata),
               c("method_name", "version", "status", "created", "created_by",
                 "modified", "modified_by")
  )
  expect_equal(method2$dad$peakwidth_nm, 4)
  expect_equal(c(method2$dad$spectra_from_nm, method2$dad$spectra_to_nm),
               c(190,400))
  expect_shape(method2$pump$gradient, dim = c(9,3))
  expect_equal(method2$column$post_time_min, 6)
  expect_equal(method2$column$temp_controls$temperature_C, c(40, 40))
  expect_equal(method2$autosampler$injection_volume_uL, 5)
  expect_equal(as.numeric(method2$metadata$created), 1770419005)

  method_dt <- read_agilent_amx(path, format_out = "data.table",
                                gradient_format = "long")
  expect_s3_class(method_dt$dad$signals, "data.table")
  expect_s3_class(method_dt$pump$gradient, "data.table")
  expect_shape(method_dt$pump$gradient, dim = c(18, 3))

  method_tibble <- read_agilent_amx(path, format_out = "tibble")
  expect_s3_class(method_tibble$dad$signals, "tbl")
  expect_s3_class(method_tibble$pump$gradient, "tbl")
  expect_shape(method_tibble$pump$gradient, dim = c(9, 3))
})


test_that("read_agilent_amx works correctly, part 3", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("flow_rate_example.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))
  method3 <- read_agilent_amx(path)
  expect_equal(names(method3$metadata),
               c("method_name", "version", "status", "created", "created_by",
                 "modified", "modified_by")
  )
  expect_equal(method3$dad$peakwidth_nm, 4)
  expect_equal(c(method3$dad$spectra_from_nm, method3$dad$spectra_to_nm),
               c(190,400))
  expect_shape(method3$pump$gradient, dim = c(10,4))
  expect_equal(method3$column$post_time_min, 2.5)
  expect_equal(method3$column$temp_controls$temperature_C, c(25, 25))
  expect_equal(method3$autosampler$injection_volume_uL, 1)
  expect_equal(as.numeric(method3$metadata$created), 1779656802.0)
  expect_equal(method3$pump$gradient$flow_mL_min,
               c(0.4,0.5,0.5,0.5,0.55,0.55,0.6,0.6,0.7,0.7))
})

test_that("read_chemstation_uv decodes OpenLab 131 intensities exactly", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab_131.uv", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chemstation_uv(path, read_metadata = FALSE, scale = FALSE)

  expect_equal(dim(x),
    c(9000L, 106L))
  expect_equal(sum(x),
    213757509330016192)
  expect_equal(range(x),
    c(-6765378797.568, 60898499398467.6))
  expect_equal(unname(x[1, 1:5]),
    c(2378875011.072, 945966546.944, -165758894.08, -784502620.16,
    -1149574840.32))
  expect_equal(unname(x[nrow(x), (ncol(x)-4):ncol(x)]),
    c(33698179186.688, 33803540103.168, 33695897485.312, 33659524481.024,
    33599529156.608))
  expect_equal(unname(colSums(x)[c(1, 53, 106)]),
    c(29565274241976344, 1137291182582267, 232994783751045))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 25)), 53]),
    c(-178375360.512, 12957513678.848, 28819901644.8, 19468684099.584,
    62671760130.048, 41008347742.208, 140940962430.976, 278711836344.32,
    567294447058.944, 264927273025.536, 69555787399.168, 67749216780.288,
    55510975905.792, 58605097189.376, 96633811369.984, 57375528583.168,
    50830266859.52, 46150094684.16, 47302219661.312, 47940156522.496,
    46986539565.056, 45314723545.088, 46402692448.256, 45594567507.968,
    45169365745.664))
})

test_that("`precision` and `bin_width` control the m/z grid from 'rainbow'", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_dependencies("rainbow")

  path <- system.file("chemstation_MSD.MS",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  rb <- function(...) call_rainbow(path, format_in = "chemstation_ms", ...)

  # `precision = N` is the same grid as `bin_width = 10^-N`
  nominal <- rb(precision = 0)
  expect_equal(dim(nominal), c(2534L, 841L))
  expect_identical(rb(bin_width = 1), nominal)
  expect_identical(rb(bin_width = 0.1), rb(precision = 1))

  # `bin_width` reaches grids that `precision` cannot express
  half <- rb(bin_width = 0.5)
  expect_equal(dim(half), c(2534L, 1520L))
  expect_gt(ncol(half), ncol(nominal))
  expect_true(any(grepl("\\.5$", colnames(half))))

  # binning re-distributes intensity without losing any of it
  expect_equal(sum(half), sum(nominal))

  # labels are written exactly, rather than rounded onto the coarser grid
  quarter <- rb(bin_width = 0.25)
  expect_equal(dim(quarter), c(2534L, 2548L))
  expect_true("102.75" %in% colnames(quarter))
  expect_false(anyDuplicated(colnames(quarter)) > 0)
  expect_false(anyDuplicated(colnames(half)) > 0)

  # `bin_width` reaches the parser through `read_chroms`
  x <- read_chroms(path, parser = "rainbow", bin_width = 0.5,
                   progress_bar = FALSE)[[1]]
  expect_equal(dim(x), dim(half))
  expect_equal(colnames(x), colnames(half))
})
