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
  expect_equal(attr(x, "detector_model"), "G1315B")
  expect_equal(attr(x, "detector"), "DAD")
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

  expect_equal(attr(x, "detector_model"), "G1315B")
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
  expect_equal(attr(x, "detector_model"), "HP G1530A")
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
  fields <-c("sample_name", "detector_model", "detector_y_unit",
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

test_that("read_chroms can read 'Agilent ChemStation' 179 files from 'Asterix'", {
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

  expect_equal(sum(x), 65975.3162760415)
  expect_equal(range(x), c(2.6671875, 3.76848958333333))
  expect_equal(which.max(x), 21781L)
  expect_equal(unname(head(x[, 1], 5)),
    c(2.70247395833333, 2.70143229166667, 2.6984375, 2.69375, 2.68841145833333))
  expect_equal(unname(tail(x[, 1], 5)),
    c(3.76341145833333, 3.763671875, 3.762109375, 3.75963541666667,
    3.75846354166667))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(2.70247395833333, 2.70299479166667, 2.68619791666667, 2.69231770833333,
    2.7, 2.699609375, 2.720703125, 2.74544270833333, 2.7671875,
    2.83268229166667, 2.94153645833333, 3.03854166666667, 3.08763020833333,
    3.60625, 3.75846354166667))
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
  expect_equal(dim(x), c(240, 276))
  expect_equal(class(x)[1], "matrix")
  expect_equal(attr(x, "parser"), "entab")
  expect_equal(attr(x, "data_format"), "wide")

  x <- read_chroms(path, format_in = "masshunter_dad", parser = "entab",
                   data_format = "long", format_out = "data.frame",
                   progress_bar = FALSE)[[1]]
  expect_equal(dim(x), c(66240, 3))
  expect_equal(colnames(x), c("rt", "lambda", "intensity"))
  expect_s3_class(x, "data.frame")
  expect_equal(attr(x, "parser"), "entab")
  expect_equal(attr(x, "data_format"), "long")
})

# split from the test above so that the 'entab' parser, which is the supported
# one, still runs where 'aston' cannot be provisioned
test_that("the 'aston' parser reads 'Agilent MassHunter' dad files the same way", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_not_installed("entab")
  skip_if_missing_aston()

  path <- system.file("masshunter.d/AcqData/DAD1.sp",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chroms(path, format_in = "masshunter_dad", parser = "entab",
                   progress_bar = FALSE)[[1]]
  # the 'aston' parsers are deprecated; the warning fires once per session
  x1 <- suppressWarnings(
    read_chroms(path, format_in = "masshunter_dad", parser = "aston",
                progress_bar = FALSE)[[1]])
  expect_equal(x, x1, ignore_attr = TRUE)
  expect_equal(attr(x1, "parser"), "aston")
  expect_equal(attr(x1, "data_format"), "wide")

  x1 <- read_chroms(path, format_in = "masshunter_dad", parser = "aston",
                    data_format = "long", format_out = "data.frame",
                    progress_bar = FALSE)[[1]]
  expect_equal(attr(x1, "parser"), "aston")
  expect_equal(colnames(x1), c("rt", "lambda", "intensity"))
  expect_equal(attr(x1, "data_format"), "long")
})

test_that("read_chroms can read 'Agilent ChemStation' version 181 files", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_181.D",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  xs <- read_chroms(path, format_in = "agilent_d", what = "chroms",
                    progress_bar = FALSE)
  x <- xs[[1]]

  expect_equal(class(x), c("matrix", "array"))
  expect_equal(dim(x), c(5914, 1))
  expect_equal(head(get_times(x), 1), -.00126875, tolerance = .00001)
  expect_equal(tail(get_times(x), 1), 19.7053979166667, tolerance = .00001)

  # check metadata
  expect_equal(attr(x, "sample_name"), "140+H")
  expect_equal(attr(x, "file_version"), "181")
  expect_equal(attr(x, "detector_y_unit"), "pA")
  expect_equal(attr(x, "method"), "DET3300.M")
  expect_equal(attr(x, "run_datetime"),
               as.POSIXct("2022-8-23 12:48:20", tz = "UTC"))
  expect_equal(attr(x, "time_unit"), "Minutes")
  expect_equal(attr(x, "data_format"), "wide")

  # the trace and the run's `Result.xml` describe the same injection
  pt <- read_agilent_d(path, what = "peak_table")
  expect_equal(pt$rt[which.max(pt$height)], get_times(x)[which.max(x[, 1])],
               tolerance = 0.005)

  # long format
  x1s <- read_chroms(path, format_in = "agilent_d", what = "chroms",
                     data_format = "long", format_out = "data.table",
                     progress_bar = FALSE)
  x1 <- x1s[[1]]
  expect_s3_class(x1, "data.table")
  expect_equal(x[, 1], x1[[2]], ignore_attr = TRUE)
  expect_equal(as.numeric(rownames(x)), x1[[1]])
  expect_equal(colnames(x1), c("rt", "intensity"))

  expect_equal(extract_metadata(xs)[, c(1:8)], extract_metadata(x1s)[, c(1:8)])
  expect_equal(attr(x1, "data_format"), "long")

  expect_warning(read_chroms(path, format_in = "agilent_d", what = "dad",
                             progress_bar = FALSE))
  expect_error(read_agilent_d(path, what = "dad"))


  expect_equal(sum(x), 5867156.68567706)
  expect_equal(range(x), c(2.08841145833333, 59055.89453125))
  expect_equal(unname(which.max(x[, 1])), 121L)
  expect_equal(unname(head(x[, 1], 5)),
    c(2.10104166666667, 2.10247395833333, 2.10598958333333, 2.11015625,
    2.11080729166667))
  expect_equal(unname(tail(x[, 1], 5)),
    c(20.49375, 20.5, 20.4990885416667, 20.5024739583333, 20.5024739583333))
  expect_equal(unname(x[round(seq(1, nrow(x), length.out = 15)), 1]),
    c(2.10104166666667, 2542.62018229167, 1516.16822916667, 402.08515625,
    82.1053385416667, 24.098828125, 15.4834635416667, 22.480078125,
    21.8817708333333, 21.4076822916667, 21.121484375, 20.8856770833333,
    20.7341145833333, 20.635546875, 20.5024739583333))
  expect_equal(head(as.numeric(rownames(x)), 3),
    c(-0.00126875, 0.00206401960369807, 0.00539678920739613))
  expect_equal(tail(as.numeric(rownames(x)), 3),
    c(19.6987323774593, 19.702065147063, 19.7053979166667))
})


test_that("read_agilent_d reads every signal of a multi-channel `Result.xml`", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_three_channels.D",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_agilent_d(path, what = c("chroms", "peak_table"))
  expect_equal(names(x$chroms), c("FID1A", "TCD2B", "TCD3C"))
  expect_equal(names(x$peak_table), c("FID1 A, Front Signal",
                                      "TCD2 B, Back Signal",
                                      "TCD3 C, Aux Signal"))
  expect_equal(unname(sapply(x$peak_table, nrow)), c(75L, 12L, 8L))
  expect_equal(unname(sapply(x$peak_table, attr, "detector")),
               c("FID", "TCD", "TCD"))
  expect_equal(unique(sapply(x$peak_table, attr, "sample_name")), "U187")

  # the version 179 traces agree with the heights 'ChemStation' integrated
  res <- read_chemstation_xml(file.path(path, "Result.xml"),
                              peaktable_format = "original")
  for (i in seq_along(res)){
    top <- which.max(res[[i]]$Height)
    trace <- x$chroms[[i]]
    near <- abs(get_times(trace) - res[[i]]$RetTime[top]) < 0.01
    expect_equal(max(trace[near, 1]),
                 res[[i]]$Height[top] + res[[i]]$Baseline[top],
                 tolerance = 0.01)
  }
})

test_that("read_agilent_d reads the peak table in a `Result.xml` file", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_181.D",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_agilent_d(path, what = "peak_table")
  expect_s3_class(x, "data.frame")
  expect_equal(names(x), c("rt", "start", "end", "area", "height", "FWHM"))
  expect_equal(nrow(x), 36)
  expect_equal(x$rt[1], 0.167537)
  expect_equal(x$start[1], 0.128731)
  expect_equal(x$end[1], 0.194599)
  expect_equal(x$FWHM[1], 0.036698)
  expect_true(all(x$start <= x$rt & x$rt <= x$end))
  expect_equal(attr(x, "sample_name"), "140+H")
  expect_equal(attr(x, "method"), "DET3300.M")
  expect_equal(attr(x, "detector"), "FID")
  expect_equal(attr(x, "run_datetime"),
               as.POSIXct("2022-08-23 12:48:20", tz = "UTC"))

  y <- read_peaklist(path, format_in = "chemstation",
                     peaktable_format = "original", progress_bar = FALSE)
  y <- y[[1]][[1]]
  expect_true(all(c("TimeStart", "TimeEnd", "Symmetry", "PeakType") %in%
                    names(y)))
  expect_equal(y$PeakType[1], "BV")
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

  expect_true(all(abs(
    sapply(x$instrument, function(xx){
      attr(xx, "run_datetime")
    }) - 1636713543.9322988) < 1e-3))
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

  # DAD intensities are in mAU, like the single-wavelength signals
  sig254 <- x$chroms[["DAD1C,Sig=254,4  Ref=off"]][, 1]
  dad254 <- rowMeans(x$dad[, c("252", "254", "256")])
  peak <- sig254 > 0.2 * max(sig254)
  expect_equal(stats::median(dad254[peak] / sig254[peak]), 1, tolerance = 0.01)

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

  expect_true(all(abs(
    sapply(x$instrument, function(xx){
      attr(xx, "run_datetime")
    }) - 1749596656.8943567) < 1e-3))
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
  expect_equal(attr(s$chroms[[1]], "source_file_format"), "chemstation_179")
  expect_equal(attr(s$dad, "source_file_format"), "chemstation_131_OL")

  expect_equal(head(get_times(s$chroms[[1]]), 1), 0.0054167, tolerance = .00001)
  expect_equal(tail(get_times(s$chroms[[1]]), 1), 10, tolerance = .00001)
  expect_equal(head(get_times(s$dad), 1), 0.0054167, tolerance = .00001)
  expect_equal(tail(get_times(s$dad), 1), 9.99875, tolerance = .00001)

  expect_equal(head(s$chroms[[1]][, 1], 3),
               c(-0.6070137, -0.7395744, -0.8668900),
               tolerance = .0001, ignore_attr = TRUE)
  expect_equal(s$dad[1, 1:3], c(-1056, -2985, -4263) * 1000 * 2^-21,
               ignore_attr = TRUE)

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

  expect_identical(class(read_agilent_rslt(path)), c("chrom_list", "list"))
  s <- read_agilent_rslt(path)[[1]]

  # acaml fields are attached to the list holding the injection's traces,
  # not to the individual traces
  expect_equal(attr(s, "instrument"), "Norbert II")
  expect_equal(attr(s, "software"), "OpenLabCDS")
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
  expect_setequal(names(xp), c("Norbert II-2026-05-26 16-19-41-05-00",
                               "20220718 MOR47_1"))

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
  expect_equal(method1$dad$peakwidth_code, 4)
  expect_equal(method1$dad$slitwidth_nm, 4)
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
  expect_equal(method2$dad$peakwidth_code, 4)
  expect_equal(method2$dad$slitwidth_nm, 4)
  expect_equal(c(method2$dad$spectra_from_nm, method2$dad$spectra_to_nm),
               c(190,400))
  expect_shape(method2$pump$gradient, dim = c(9,3))
  expect_equal(method2$column$post_time_min, 6)
  expect_equal(method2$column$temp_controls$temperature_C, c(40, 40))
  expect_equal(method2$autosampler$injection_volume_uL, 5)
  expect_equal(as.numeric(method2$metadata$created), 1770419005)
  expect_equal(method2$pump$solvents$channel, c("A", "B"))
  expect_equal(method2$pump$solvents$solvent,
               c("Water V.03", "Acetonitrile V.03"))
  expect_equal(method2$dad$signals$id, c("A", "B", "C", "D"))
  expect_true(all(is.na(method2$dad$signals$reference_nm)))

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
  expect_equal(method3$dad$peakwidth_code, 4)
  expect_equal(method3$dad$slitwidth_nm, 4)
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

test_that("'rainbow' metadata is mapped onto chromConverter's fields", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_dependencies("rainbow")

  path <- system.file("chemstation_181.D",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- suppressWarnings(call_rainbow(path, format_in = "agilent_d"))$FID
  expect_equal(attr(x, "sample_name"), "140+H")
  expect_equal(attr(x, "operator"), "IFP1\\R0521_gen")
  expect_equal(attr(x, "method"), "DET3300.M")
  # the chassis module, not the `instrument` 'rainbow' reports, which for this
  # header layout is the 'ChemStation' edition
  expect_equal(attr(x, "instrument"), "6890")
  expect_equal(attr(x, "instrument_modules")[[1]],
               list(name = "6890", model = "GC"))
  expect_equal(attr(x, "sample_dilution"), 1L)
  expect_equal(attr(x, "run_datetime"),
               as.POSIXct("2022-8-23 12:48:20", tz = "UTC"))

  path_dx <- system.file("MeOH1.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_dx))

  y <- call_rainbow(path_dx, format_in = "agilent_d")$UV$DAD1C.CH
  expect_equal(attr(y, "sample_name"), "MeOH1")
  expect_equal(attr(y, "operator"), "SYSTEM (SYSTEM)")
  expect_equal(attr(y, "method"), "coumarins1.amx")
  expect_equal(attr(y, "detector_model"), "DAD")
  expect_equal(attr(y, "signal_descriptor"), "DAD1C,Sig=254,4  Ref=off")
  expect_equal(attr(y, "wavelength"), 254)
  expect_equal(attr(y, "bandwidth"), 4)
  expect_equal(attr(y, "sample_injection_volume"), 5)
  expect_equal(attr(y, "run_datetime"),
               as.POSIXct("2025-06-10 23:04:16.8943567", tz = "UTC"))
})

test_that("a short 'rainbow' time axis is rebuilt rather than dropped", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_dependencies("rainbow")

  path <- system.file("chemstation_181.D",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # 'rainbow' returns 5913 times for the 5914 points it decodes from this
  # container, since it takes the count from the file size
  w <- capture_warnings(x <- call_rainbow(path, format_in = "agilent_d"))
  expect_match(w, "5913 retention times for 5914 rows", all = TRUE)
  x <- x$FID
  y <- read_chemstation_ch(file.path(path, "FID1A.ch"))
  expect_equal(nrow(x), nrow(y))
  expect_equal(get_times(x), get_times(y))

  expect_equal(rb_times(1:5, 5), 1:5)
  expect_null(rb_times(numeric(), 5))
  expect_warning(expect_equal(rb_times(c(0, 4), 5), seq(0, 4, length.out = 5)))
})

rezip_without <- function(path, pattern, dir){
  src <- file.path(dir, "src")
  unzip(path, exdir = src)
  keep <- list.files(src, recursive = TRUE, all.files = TRUE)
  keep <- keep[!grepl(pattern, keep)]
  out <- file.path(dir, basename(path))
  withr::with_dir(src, utils::zip(out, keep, flags = "-q"))
  out
}

test_that("read_agilent_amx drops a module the archive has no driver for", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  # as for an instrument whose detector is not a DAD
  no_dad <- rezip_without(path, "DadDriver", tmp)

  expect_warning(x <- read_agilent_amx(no_dad), "No driver files found for .dad.")
  expect_false("dad" %in% names(x))
  expect_true(all(c("metadata", "pump", "column", "autosampler") %in% names(x)))
  expect_error(read_agilent_amx(no_dad, what = "dad"),
               "No driver files found for .dad.")

  x <- read_agilent_amx(path, what = "pump", format_out = "data.table")
  expect_s3_class(x$pump$solvents, "data.table")
})

unzip_amx <- function(path){
  dir <- tempfile()
  unzip(path, exdir = dir)
  files <- sort(list.files(dir, recursive = TRUE, all.files = TRUE))
  stats::setNames(unname(tools::md5sum(file.path(dir, files))), files)
}

amx_checksums_valid <- function(path){
  dir <- tempfile()
  unzip(path, exdir = dir)
  chk <- list.files(dir, "\\.chk$", recursive = TRUE, full.names = TRUE)
  all(vapply(chk, function(f){
    identical(readBin(f, "raw", 20),
              digest::digest(file = sub("\\.chk$", "", f), algo = "sha1",
                             raw = TRUE))
  }, logical(1)))
}

test_that("write_agilent_amx leaves an unedited method unchanged", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  paths <- system.file(c("Glucosinolates-XDB5.amx",
                         "column_storage_ACN100.amx",
                         "flow_rate_example.amx"),
                       package = "chromConverterExtraTests")
  skip_if_not(length(paths) == 3)

  for (path in paths){
    out <- write_agilent_amx(path, tempfile(fileext = ".amx"))
    expect_equal(unzip(out, list = TRUE)$Name, unzip(path, list = TRUE)$Name)
    expect_equal(unzip_amx(out), unzip_amx(path))
  }
})

zip_headers <- function(path){
  x <- readBin(path, "raw", file.size(path))
  u16 <- function(i){
    readBin(x[i + 0:1], "integer", size = 2, signed = FALSE, endian = "little")
  }
  u32 <- function(i) readBin(x[i + 0:3], "integer", size = 4, endian = "little")
  cd <- grepRaw(as.raw(c(0x50, 0x4b, 0x01, 0x02)), x, all = TRUE)
  local <- vapply(cd, function(p) u32(p + 42) + 1, numeric(1))
  data.frame(made_by = vapply(cd, function(p) u16(p + 4), numeric(1)),
             needed = vapply(cd, function(p) u16(p + 6), numeric(1)),
             utf8 = vapply(cd, function(p) bitwAnd(u16(p + 8), 0x800) > 0,
                           logical(1)),
             attributes = vapply(cd, function(p) u32(p + 38), numeric(1)),
             local_needed = vapply(local, function(p) u16(p + 4), numeric(1)),
             local_utf8 = vapply(local, function(p){
               bitwAnd(u16(p + 6), 0x800) > 0
             }, logical(1)))
}

test_that("write_agilent_amx writes zip headers OpenLab can read", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(nzchar(path))

  out <- write_agilent_amx(path, tempfile(fileext = ".amx"), flow = 0.5)
  h <- zip_headers(out)
  expect_true(all(h$made_by == 45))
  expect_true(all(h$needed %in% c(10, 20)))
  expect_equal(h$local_needed, h$needed)
  expect_false(any(h$utf8 | h$local_utf8))
  expect_true(all(h$attributes == 0))
  expect_true(all(zip_headers(path)$needed %in% c(10, 20)))
})

test_that("write_agilent_amx writes a gradient and pump settings", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  gradient <- data.frame(time_min = c(9.25, 0, 2.5),
                         pct_A = c(5, 65, 65),
                         pct_B = c(95, 35, 35))
  out <- write_agilent_amx(path, tempfile(fileext = ".amx"),
                           gradient = gradient, flow = 1.5,
                           stop_time = 11.5, post_time = 3)
  pump <- read_agilent_amx(out, what = "pump")$pump

  expect_equal(pump$gradient,
               gradient[order(gradient$time_min), ], ignore_attr = TRUE)
  expect_equal(pump$solvents$percentage, c(65, 35))
  expect_equal(pump$flow_mL_min, 1.5)
  expect_equal(pump$stop_time_min, 11.5)
  expect_equal(pump$post_time_min, 3)
  expect_true(amx_checksums_valid(out))

  dir <- tempfile()
  unzip(out, "MethodProperties/MethodProperties", exdir = dir)
  props <- xml2::read_xml(file.path(dir, "MethodProperties/MethodProperties"))
  expect_equal(xml2::xml_text(xml2::xml_find_first(
    props, "//*[local-name()='MethodRuntimeInSeconds']")), "690")

  before <- unzip_amx(path)
  after <- unzip_amx(out)
  edited <- grepl("PumpDriver|MethodProperties", names(before))
  expect_equal(after[!edited], before[!edited])

  expect_warning(write_agilent_amx(path, tempfile(), gradient = gradient,
                                   stop_time = 5),
                 "runs past the stop time")

  out <- write_agilent_amx(path, tempfile(fileext = ".amx"), post_time = 0)
  dir <- tempfile()
  unzip(out, exdir = dir)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  xml <- xml2::read_xml(grep("PumpDriver(?!.*\\.chk$)", files, perl = TRUE,
                             value = TRUE))
  expect_equal(xml2::xml_text(xml2::xml_find_first(
    xml, "/PumpMethod/PostTime/PostTimeMode")), "PostTime_Off")
  expect_length(xml2::xml_find_all(xml, "/PumpMethod/PostTime/PostTimeValue"), 0)
  expect_true(is.na(read_agilent_amx(out, what = "pump")$pump$post_time_min))
  expect_true(amx_checksums_valid(out))
})

test_that("write_agilent_amx adds a gradient to an isocratic method", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("column_storage_ACN100.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  solvents <- read_agilent_amx(path, what = "pump")$pump$solvents$channel
  expect_equal(solvents, c("A", "B"))
  gradient <- data.frame(time_min = rep(c(0, 1, 6), each = 2),
                         channel = c("A", "B"),
                         percent = c(90, 10, 90, 10, 5, 95))
  out <- write_agilent_amx(path, tempfile(fileext = ".amx"),
                           gradient = gradient, post_time = 2)
  pump <- read_agilent_amx(out, what = "pump", gradient_format = "long")$pump

  expect_equal(pump$gradient, gradient, ignore_attr = TRUE)
  expect_equal(pump$post_time_min, 2)
  expect_true(amx_checksums_valid(out))
})

test_that("write_agilent_amx keeps the flow steps of the template", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("flow_rate_example.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  template <- read_agilent_amx(path, what = "pump", gradient_format = "long")
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = template$pump$gradient),
               "cannot change the flow")

  gradient <- data.frame(time_min = c(0, 5), pct_A = c(90, 10),
                         pct_B = c(10, 90))
  out <- write_agilent_amx(path, tempfile(fileext = ".amx"),
                           gradient = gradient)
  x <- read_agilent_amx(out, what = "pump", gradient_format = "long")
  flow <- function(g) g[g$channel == "flow", ]
  expect_equal(flow(x$pump$gradient), flow(template$pump$gradient),
               ignore_attr = TRUE)

  out <- write_agilent_amx(path, tempfile(fileext = ".amx"), flow = 0.8)
  x <- read_agilent_amx(out, what = "pump", gradient_format = "long")$pump
  expect_equal(x$flow_mL_min, 0.8)
  expect_false("flow" %in% x$gradient$channel)
  expect_equal(x$gradient[x$gradient$channel != "flow", ],
               template$pump$gradient[template$pump$gradient$channel != "flow", ],
               ignore_attr = TRUE)
  expect_true(amx_checksums_valid(out))
})

test_that("write_agilent_amx sets the column temperature", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  out <- write_agilent_amx(path, tempfile(fileext = ".amx"),
                           temperature = 32.5)
  temps <- read_agilent_amx(out, what = "comp")$column$temp_controls
  expect_equal(temps$temperature_C, c(32.5, 32.5))
  expect_true(amx_checksums_valid(out))

  before <- unzip_amx(path)
  after <- unzip_amx(out)
  edited <- grepl("ColumnCompDriver", names(before))
  expect_equal(sum(edited), 2)
  expect_equal(after[!edited], before[!edited])
  expect_false(any(after[edited] == before[edited]))

  uncontrolled <- system.file("column_storage_ACN100.amx",
                              package = "chromConverterExtraTests")
  skip_if_not(file.exists(uncontrolled))
  expect_error(write_agilent_amx(uncontrolled, tempfile(), temperature = 30),
               "does not control the temperature")
  expect_error(write_agilent_amx(path, tempfile(), temperature = NA_real_),
               "non-negative")
})

test_that("write_agilent_amx rejects an invalid gradient", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  gradient <- data.frame(time_min = c(0, 5), pct_A = c(90, 10),
                         pct_B = c(10, 90))
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = transform(gradient, pct_B = 20)),
               "add up to 100")
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = gradient[c("time_min", "pct_B")]),
               "'pct_A', 'pct_B'")
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = cbind(gradient, pct_C = 0)),
               "'pct_A', 'pct_B'")
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = gradient[c(1, 1), ]),
               "distinct")
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = gradient[0, ]),
               "no rows")
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = transform(gradient, time_min = c(0, Inf))),
               "distinct, non-negative")
  expect_error(write_agilent_amx(path, tempfile(),
                                 gradient = transform(gradient, time_min = c(1, 5))),
               "row at time 0")
  long <- data.frame(time_min = c(0, 0, 5), channel = c("A", "B", "A"),
                     percent = c(90, 10, 100))
  expect_error(write_agilent_amx(path, tempfile(), gradient = long),
               "every channel at every time")
  expect_error(write_agilent_amx(path, tempfile(), gradient = long[c(1, 1:3), ]),
               "more than one percentage")
  expect_error(write_agilent_amx(path, tempfile(), stop_time = -1),
               "non-negative")
  expect_error(write_agilent_amx(path, tempfile(), flow = Inf),
               "non-negative")
  expect_error(write_agilent_amx(path, tempfile(), post_time = 0.005),
               "at least 0.01")

  out <- tempfile(fileext = ".amx")
  write_agilent_amx(path, out)
  expect_error(write_agilent_amx(path, out), "force = TRUE")
  expect_no_error(write_agilent_amx(path, out, gradient = gradient,
                                    force = TRUE))
})

test_that("write_agilent_amx accepts a read-only template", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  template <- tempfile(fileext = ".amx")
  file.copy(path, template)
  Sys.chmod(template, "444")
  out <- write_agilent_amx(template, tempfile(fileext = ".amx"), flow = 1.2)
  expect_equal(read_agilent_amx(out, what = "pump")$pump$flow_mL_min, 1.2)
})

test_that("write_agilent_amx errors on a template without MethodProperties", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("Glucosinolates-XDB5.amx",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  no_props <- rezip_without(path, "^MethodProperties/", tmp)
  expect_error(write_agilent_amx(no_props, tempfile()),
               "no `MethodProperties/MethodProperties` part")
})

test_that("read_agilent_dx errors when asked for instrument data it lacks", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("agilent.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  no_it <- rezip_without(path, "[.]IT$", tmp)

  expect_error(read_agilent_dx(no_it, what = "instrument"),
               "Instrument data could not be found.")
})

test_that("read_agilent_dx reads the run time and injection volume from `injection.acmd`", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("MeOH1.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  # the file records 2025-06-10T18:04:16.8943567-05:00
  x <- read_agilent_dx(path, what = c("chroms", "dad", "instrument"))
  for (trace in list(x$chroms[[1]], x$dad, x$instrument[[1]])){
    expect_equal(attr(trace, "run_datetime"),
                 as.POSIXct("2025-06-10 23:04:16.8943567", tz = "UTC"))
    expect_equal(attr(trace, "sample_injection_volume"), 5)
  }
  # the channel each trace was recorded on, from its signal descriptor
  expect_equal(unname(sapply(x$chroms, attr, "channel_id")),
               sub(",.*", "", names(x$chroms)))
  expect_equal(attr(x$instrument[["PMP1A,Pressure"]], "channel_id"), "PMP1A")
  expect_equal(attr(x$instrument[["THM1A,Left Temperature"]], "channel_id"),
               "THM1A")

  # without it, the local time in the trace headers is all there is
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  no_acmd <- rezip_without(path, "[.]acmd$", tmp)
  y <- read_agilent_dx(no_acmd, what = "dad")
  expect_equal(attr(y, "run_datetime"),
               as.POSIXct("2025-06-10 18:04:16", tz = "UTC"))
  expect_null(attr(y, "sample_injection_volume"))
})

test_that("call_rainbow works without `format_in`", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_dependencies("rainbow")

  path <- system.file("chemstation_181.D",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- suppressWarnings(call_rainbow(path))
  expect_true("FID" %in% names(x))
})

test_that("read_chemstation_reg reads a ChemStation A.00.01 LCDIAG.REG", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("chemstation_A0001_LCDIAG.REG",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_chemstation_reg(path)
  expect_setequal(unique(x$traces$trace),
                  c("PMP1, Pressure", "PMP1, Flow", paste0("PMP1, Solvent ",
                                                           LETTERS[1:4])))
  p <- x$traces[x$traces$trace == "PMP1, Pressure", ]
  expect_equal(unique(p$unit), "bar")
  expect_equal(nrow(p), 2400)
  expect_equal(range(p$time), c(0, 11.995))
  # RUN.LOG: "Pressure = 53.1 bar" at the end of the run
  expect_lt(abs(p$value[nrow(p)] - 53.1), 0.5)
  expect_true(all(x$traces$value[x$traces$trace == "PMP1, Flow"] == 0.5))

  pmp <- x$conditions[x$conditions$object == "PMP1, Start/Stop Conditions", ]
  expect_lt(abs(as.numeric(pmp$value[pmp$key == "StartPressure"]) - 47.1), 0.05)
  expect_lt(abs(as.numeric(pmp$value[pmp$key == "StopPressure"]) - 53.1), 0.05)
  expect_equal(pmp$value[pmp$key == "DateTime"], "18-Nov-10, 15:48:06")
})

test_that("read_peaklist reads an 'OpenLab' .rx file with no integrated peaks", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab.sirslt", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path))

  x <- read_peaklist(path, format_in = "openlab", progress_bar = FALSE)
  expect_s3_class(x, "peak_list")
  expect_equal(names(x), "Norbert II-2026-05-26 16-19-41-05-00")
  expect_length(x[[1]], 0)
  expect_equal(attr(x[[1]], "method"), "flow_rate_simple_test")
})

test_that("read_peaklist reads peak tables from an 'OpenLab' sequence", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab_rx", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path))

  x <- read_peaklist(path, format_in = "openlab", progress_bar = FALSE)
  expect_s3_class(x, "peak_list")
  expect_equal(names(x), c("D2F-A2_1", "D2F-A3_407"))
  expect_setequal(names(x[[2]]), c("200", "229", "260", "360"))

  tab <- x[["D2F-A3_407"]][["229"]]
  expect_equal(names(tab), c("sample", "lambda", "rt", "start", "end", "area",
                             "height", "FWHM"))
  expect_equal(tab$lambda[1], 229)
  expect_false(is.unsorted(tab$rt))
  expect_true(all(tab$start <= tab$rt & tab$rt <= tab$end))
  # areas and heights as shown in 'OpenLab' Data Analysis
  expect_equal(round(tab$area[1:3], 3), c(101.124, 43.300, 31.893))
  expect_equal(round(tab$height[1:3], 3), c(25.861, 13.791, 12.026))

  expect_equal(attr(tab, "signal_descriptor"), "DAD1A,Sig=229,4  Ref=off")
  expect_equal(attr(tab, "detector"), "DAD")
  expect_equal(attr(tab, "sample_name"), "407")
  expect_equal(attr(tab, "sample_position"), "D2F-A3")
  expect_equal(attr(tab, "batch"), "Demog_S0_P1")
  # each injection gets its own row of the sequence's .acaml
  expect_equal(attr(x[["D2F-A2_1"]][[1]], "sample_name"), "1")
  expect_equal(attr(tab, "run_datetime"),
               as.POSIXct("2026-07-22 15:27:52", tz = "UTC"))

  y <- read_peaklist(path, format_in = "openlab", progress_bar = FALSE,
                     peaktable_format = "original")
  y <- y[["D2F-A3_407"]][["229"]]
  expect_true(all(c("RetentionTime", "BeginTime", "EndTime", "BaselineCode",
                    "WidthBase", "CompoundName") %in% names(y)))
  expect_equal(y$CompoundName[1:3], c("Sinigrin", "GS_1ME", "GS_6MSOH"))
})

test_that("read_agilent_dx reads the mass spectra of an 'OpenLab' GC-MS injection", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("openlab_gcms.sirslt", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path) && nzchar(path))

  dx <- list.files(path, "\\.dx$", full.names = TRUE)
  x <- read_agilent_dx(dx, what = c("MS1", "TIC", "BPC"))
  expect_equal(nrow(x$TIC), 6940)
  expect_equal(nrow(x$MS1), 129230)
  expect_equal(unlist(x$MS1[1]), c(rt = 4.097434, mz = 36, intensity = 875),
               tolerance = 1e-6)
  rt <- as.numeric(rownames(x$TIC))
  expect_equal(unique(x$MS1$rt), rt)
  expect_equal(rowsum(x$MS1$intensity, x$MS1$rt, reorder = FALSE)[, 1],
               x$TIC[, 1], ignore_attr = TRUE)
  expect_equal(as.vector(tapply(x$MS1$intensity, x$MS1$rt, max)), x$BPC$intensity)
  expect_equal(attr(x$TIC, "detector_model"), "5977")
  expect_equal(attr(x$TIC, "n_scans"), 6940)
  expect_equal(attr(x$TIC, "time_range"), range(rt))
  expect_equal(attr(x$TIC, "sample_injection_volume"), 1)

  x <- read_chroms(path, format_in = "agilent_rslt", what = c("chroms", "TIC"),
                   progress_bar = FALSE)
  expect_equal(names(x[[1]]), c("chroms", "TIC"))
  expect_equal(attr(x[[1]]$chroms, "signal_descriptor"), "FID1A")
  expect_equal(attr(x[[1]]$TIC, "sample_name"), "20220718 MOR47_1")
})

test_that("read_agilent_d reads 'MassHunter' GC-MS SIM spectra that match data.ms", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("masshunter_gcms_sim.D", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path) && nzchar(path))

  x <- read_agilent_d(path, what = c("MS1", "TIC"))
  ref <- read_chemstation_ms(file.path(path, "data.ms"), what = c("MS1", "TIC"),
                             format_out = "data.frame")
  expect_equal(as.numeric(rownames(x$TIC)), ref$TIC$rt)
  expect_equal(x$TIC[, 1], ref$TIC$intensity, ignore_attr = TRUE)
  ms1 <- x$MS1[order(x$MS1$rt, x$MS1$mz), ]
  ref <- ref$MS1[order(ref$MS1$rt, ref$MS1$mz), ]
  expect_equal(nrow(ms1), 7446)
  expect_equal(ms1$rt, ref$rt)
  # 'data.ms' rounds m/z to 0.05 and compresses intensities above 2^14
  expect_true(all(abs(ms1$mz - ref$mz) <= 0.051))
  expect_true(all(abs(ms1$intensity - ref$intensity) <=
                    pmax(0.5, 0.005 * ms1$intensity)))
  expect_equal(sort(unique(round(ms1$mz))), c(43, 60, 73, 74, 87, 101, 115, 129))
  expect_equal(attr(x$TIC, "n_scans"), 1474)
  expect_equal(attr(x$TIC, "sample_name"), "AKI2")

  x <- read_chroms(path, format_in = "agilent_d", progress_bar = FALSE)
  expect_equal(names(x[[1]]), c("MS1", "TIC", "BPC"))
  expect_equal(nrow(x[[1]]$TIC), 1474)
})

test_that("read_agilent_d leaves out unreadable mass spectra only by default", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  ms <- system.file("masshunter_gcms_sim.D", "AcqData", package = "chromConverterExtraTests")
  fid <- system.file("chemstation_179_mustang.ch", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(ms) && nzchar(ms) && file.exists(fid))

  path <- file.path(withr::local_tempdir(), "fid_ms.D")
  dir.create(file.path(path, "AcqData"), recursive = TRUE)
  file.copy(list.files(ms, full.names = TRUE), file.path(path, "AcqData"))
  file.copy(fid, file.path(path, "FID1A.ch"))
  con <- file(file.path(path, "AcqData", "MSScan.bin"), "r+b")
  seek(con, 88, rw = "write")
  writeBin(1000L, con, size = 4)
  close(con)

  expect_warning(x <- read_agilent_d(path, collapse = FALSE), "Left out")
  expect_equal(names(x), "chroms")
  expect_error(read_agilent_d(path, what = c("chroms", "MS1")),
               "does not match the record layout")
  expect_error(read_agilent_d(path, what = "MS1"), "parser = \"rainbow\"",
               fixed = TRUE)
})

test_that("read_agilent_d reads integer intensities from 'MassHunter' SIM scans", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("masshunter_qqq_sim.d", package = "chromConverterExtraTests")
  mzml <- system.file("masshunter_qqq_sim.mzML", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path) && nzchar(path) && file.exists(mzml))

  x <- read_agilent_d(path, what = c("MS1", "TIC"), format_out = "data.frame",
                      data_format = "long")
  ref <- RaMS::grabMSdata(mzml, grab_what = "chroms", verbosity = 0)$chroms
  tic <- ref[ref$chrom_type == "TIC", ]
  sim <- ref[grepl("^SIM", ref$chrom_type), ]
  expect_equal(x$TIC$rt, tic$rt, tolerance = 1e-6)
  expect_equal(x$TIC$intensity, tic$int)
  expect_equal(x$MS1$rt, sim$rt, tolerance = 1e-6)
  expect_equal(x$MS1$intensity, sim$int)
  expect_true(all(abs(x$MS1$mz - 609.3) < 0.01))
})

test_that("read_agilent_d reads 'MassHunter' product ion profile spectra", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  src <- system.file("masshunter.d", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(src) && nzchar(src))

  expect_error(read_agilent_d(src, what = "MS1"), "no MS1 scans")
  x <- read_agilent_d(src, what = c("MS1", "MS2", "TIC", "BPC"),
                      format_out = "data.frame", data_format = "long")
  expect_named(x, c("MS2", "TIC", "BPC"))
  expect_equal(nrow(x$MS2), 49 * 5121)
  expect_equal(unique(x$MS2$precursor_mz), 611.3, tolerance = 1e-6)
  expect_equal(range(x$MS2$mz), c(100, 612))
  expect_equal(rowsum(x$MS2$intensity, x$MS2$rt, reorder = FALSE)[, 1],
               x$TIC$intensity, tolerance = 1e-5, ignore_attr = TRUE)
  top <- vapply(split(seq_len(nrow(x$MS2)), x$MS2$rt), function(i){
    i[which.max(x$MS2$intensity[i])]
  }, numeric(1))
  expect_equal(x$MS2$intensity[top], x$BPC$intensity, tolerance = 1e-6)
  expect_equal(x$MS2$mz[top], x$BPC$mz, tolerance = 1e-6)
})

test_that("read_agilent_d reads 'MassHunter' MS2 as 'ProteoWizard' does", {
  skip_if_not_installed("RaMS")
  archive <- extra_test_file("pwiz_agilent_test_data.tar.bz2")
  name <- "Reader_Agilent_Test.data/Thyrxox 5 TS Diff Scan B"
  dir <- withr::local_tempdir()
  utils::untar(archive, files = paste0(name, c(".d", ".mzML")), exdir = dir)
  path <- file.path(dir, name)

  # MS1 profile scans, product ion profile scans, and MRM scans with integer
  # intensities in spectrum format 1
  x <- read_agilent_d(paste0(path, ".d"), what = c("MS1", "MS2"),
                      format_out = "data.table", data_format = "long")
  ref <- RaMS::grabMSdata(paste0(path, ".mzML"), grab_what = c("MS2", "chroms"),
                          verbosity = 0)
  info <- attr(x$MS2, "scan_info")
  expect_equal(sum(info$scan_type == "MRM"), 571)
  expect_equal(sum(info$scan_type == "product ion scan"), 151)
  f <- write_mzml(x, path_out = dir, what = c("MS1", "MS2"),
                  show_progress = FALSE)
  back <- read_mzml(f, what = c("MS2", "chroms"), data_format = "long")
  expect_equal(back$MS2$voltage[!duplicated(back$MS2$rt)],
               info$collision_energy[info$scan_type == "product ion scan"])
  x$MS2$scan <- match(x$MS2$rt, unique(x$MS2$rt))
  scan_of <- function(rt) match(round(rt, 5), round(unique(x$MS2$rt), 5))

  pi <- ref$MS2
  pi$scan <- scan_of(pi$rt)
  ours <- x$MS2[pi, on = c("scan", mz = "fragmz"), roll = "nearest"]
  expect_equal(ours$intensity, ours$int)
  expect_equal(ours$precursor_mz, ours$premz, tolerance = 1e-6)
  info <- attr(x$MS2, "scan_info")
  expect_equal(info$collision_energy[ours$scan], ours$voltage)
  expect_equal(attr(x$MS2, "polarity"), "negative")

  srm <- ref$chroms[grepl("SRM", ref$chroms$chrom_type), ]
  expect_equal(nrow(srm), 4568)
  srm$scan <- scan_of(srm$rt)
  ours <- x$MS2[srm, on = c("scan", mz = "product_mz"), roll = "nearest"]
  expect_false(anyNA(srm$scan))
  expect_equal(ours$intensity, ours$int)
  expect_equal(ours$precursor_mz, ours$target_mz, tolerance = 1e-6)

  # 'msconvert' writes each of the 8 transitions twice
  written <- data.table::as.data.table(back$chroms)
  written <- written[grepl("SRM", written$chrom_type), ]
  expect_equal(length(unique(written$chrom_type)), 8)
  written$scan <- scan_of(written$rt)
  joined <- srm[written, on = c("scan", "product_mz"), roll = "nearest"]
  expect_equal(nrow(joined), sum(info$scan_type == "MRM") * 4)
  expect_equal(joined$intensity, joined$int)
  expect_equal(joined$i.target_mz, joined$target_mz, tolerance = 1e-6)
})

test_that("call_rainbow reads 'MassHunter' centroids with `centroid = TRUE`", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_dependencies("rainbow")

  path <- system.file("masshunter_gcms_sim.D", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(path) && nzchar(path))

  x <- call_rainbow(path, format_in = "masshunter", centroid = TRUE,
                    format_out = "data.frame", data_format = "long")
  expect_true("MSPeak.bin" %in% names(x$MS))
  rb <- x$MS$MSPeak.bin
  native <- read_agilent_d(path, what = "MS1", format_out = "data.frame")
  native <- native[native$intensity != 0, ]
  expect_equal(nrow(rb), nrow(native))
  expect_true(all(abs(sort(rb$intensity) - sort(native$intensity)) < 1))
})

test_that("read_chroms identifies 'MassHunter' and 'OpenLab' directories", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  ms <- system.file("masshunter_gcms_sim.D", package = "chromConverterExtraTests")
  rslt <- system.file("openlab_gcms.sirslt", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(ms) && nzchar(ms) && dir.exists(rslt) && nzchar(rslt))

  expect_equal(get_filetype(rslt), "agilent_rslt")
  expect_equal(names(read_chroms(ms, progress_bar = FALSE)[[1]]),
               c("MS1", "TIC", "BPC"))
  expect_equal(names(read_chroms(rslt, progress_bar = FALSE)),
               "20220718 MOR47_1")
})

test_that("read_agilent_d keeps the TIC when the spectra cannot be read", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  ms <- system.file("masshunter_gcms_sim.D", "AcqData", package = "chromConverterExtraTests")
  skip_if_not(dir.exists(ms) && nzchar(ms))

  path <- file.path(withr::local_tempdir(), "no_peaks.D")
  dir.create(file.path(path, "AcqData"), recursive = TRUE)
  file.copy(list.files(ms, full.names = TRUE), file.path(path, "AcqData"))
  file.rename(file.path(path, "AcqData", "MSPeak.bin"),
              file.path(path, "AcqData", "MSProfile.bin"))

  expect_warning(x <- read_agilent_d(path), "Left out .MS1.: .MSPeak.bin. could not be found")
  expect_equal(names(x), c("TIC", "BPC"))
  expect_equal(nrow(x$TIC), 1474)
  expect_error(read_agilent_d(path, what = "MS1"), "^'MSPeak.bin' could not be found.$")
})

test_that("read_agilent_dx explains where the mass spectra are read from", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("agilent.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path) && nzchar(path))

  path_out <- withr::local_tempdir()
  expect_error(read_agilent_dx(path, what = c("MS1", "TIC"), path_out = path_out),
               "agilent.MSScan.bin")
  expect_length(list.files(path_out, recursive = TRUE), 0)
  expect_error(read_agilent_dx(path, what = "MS1"), "agilent.MSPeak.bin")
})

test_that("Agilent UV files report their time range in every output format", {
  path <- extra_test_file("chemstation_31.uv")
  expected <- attr(read_chemstation_uv(path), "time_range")
  expect_equal(expected, c(0.000333, 31.99), tolerance = 1e-3)
  for (format_out in c("data.frame", "data.table")){
    for (data_format in c("wide", "long")){
      x <- read_chemstation_uv(path, format_out = format_out,
                               data_format = data_format)
      expect_equal(attr(x, "time_range"), expected,
                   label = paste(format_out, data_format))
    }
  }
})

test_that("read_chroms reads 'Agilent' MS files with its default formats", {
  path <- extra_test_file("chemstation_MSD.MS")
  x <- read_chroms(path, parser = "chromconverter", progress_bar = FALSE)[[1]]
  expect_named(x, c("MS1", "BPC", "TIC"))
  expect_s3_class(x$MS1, "data.table")
  expect_equal(colnames(x$MS1), c("rt", "mz", "intensity"))
  expect_equal(nrow(x$MS1), 95471)
  expect_true(is.matrix(x$TIC))
  expect_equal(attr(x$TIC, "data_format"), "wide")
})
