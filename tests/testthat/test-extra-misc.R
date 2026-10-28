# these tests rely on files included in the chromConverterExtraTests package,
# which is available on GitHub (https://github.com/ethanbass/chromConverterExtraTests).

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

  # `format_out` is forwarded to `read_mzml`, which accepts all three classes.
  # The copy has dots in its name, which `read_thermoraw` must also handle.
  dir <- withr::local_tempdir()
  dotted <- file.path(dir, "CirA.rep1.raw")
  file.copy(path, dotted)
  y <- read_chroms(dotted, progress_bar = FALSE, path_out = dir,
                   format_out = "data.table")[[1]]
  expect_equal(names(y), c("MS1", "MS2", "DAD", "BPC", "TIC", "chroms", "metadata"))
  expect_s3_class(y$TIC, "data.table")
  expect_equal(attr(y$TIC, "format_out"), "data.table")
})

test_that("read_chroms can read profile spectra from 'Thermo' RAW files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_thermorawfileparser()

  path <- system.file("small.RAW", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  dir <- withr::local_tempdir()
  x <- read_chroms(path, parser = "thermoraw", progress_bar = FALSE,
                   path_out = dir)[[1]]
  y <- read_chroms(path, parser = "thermoraw", progress_bar = FALSE,
                   path_out = dir, centroid = FALSE)[[1]]
  expect_equal(nrow(x$MS1), 22627)
  expect_equal(nrow(y$MS1), 279869)
  expect_equal(nrow(x$MS2), 25344)
  expect_equal(nrow(y$MS2), 25344)
  expect_error(read_thermoraw(path, centroid = NA), "`centroid` must be")
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
                   verbose = FALSE, export_format = "cdf",
                   path_out = tmp)[[1]]
  expect_equal(dim(x$MS1), c(469732, 3))

  skip_if(is_legacy_openchrom(configure_openchrom()),
          "OpenChrom versions before 1.4 cannot export mzML.")
  x <- read_chroms(path, format_in = "msd", progress_bar = FALSE,
                   verbose = FALSE, path_out = tmp)[[1]]
  expect_type(x, "list")
  expect_equal(dim(x$MS1), c(469732,4))
})

test_that("read_mzml returns chroms long when a trace repeats retention times", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("DCM1.mzML", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  expect_warning(x <- read_mzml(path, what = "chroms"),
                 "repeated retention times")
  expect_s3_class(x$chroms, "data.table")
  expect_equal(attr(x$chroms, "data_format"), "long")
})

test_that("read_cdf function can read peak tables", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("chromConverterExtraTests")

  path <- system.file("VARIAN1.CDF", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))

  x <- read_cdf(path, what = "peak_table")
  expect_s3_class(x, "data.frame")
  expect_equal(names(x), c("rt", "start", "end", "area", "height", "FWHM"))
  expect_equal(nrow(x), 8)
  y <- read_cdf(path, what = "peak_table", peaktable_format = "original")
  expect_equal(dim(y), c(8, 6))
  expect_true("peak_name" %in% names(y))
  expect_equal(x$rt, y$peak_retention_time)
  expect_equal(x$area, y$peak_area)
  expect_true(all(is.na(x$height)))
  expect_true(all(y$peak_height == -1))
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

  x1 <- read_chroms(path, what = c("TIC"), data_format="long",
                    progress_bar = FALSE)[[1]]
  expect_equal(ncol(x1), 2)
  expect_equal(colnames(x1), c("rt", "intensity"))

  x2 <- read_chroms(path, what=c("MS1"), ms_format = "list",
                    progress_bar = FALSE)[[1]]
  expect_type(x2, "list")
  expect_equal(length(x2), length(unique(x$MS1$rt)))
})

test_that("get_filetype works as expected", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")

  expect_equal(get_filetype(system.file("chemstation_MSD.MS",
                                        package = "chromConverterExtraTests")),
               "chemstation_ms"
  )
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
  expect_equal(get_filetype(system.file("agilent.dx",
                                        package="chromConverterExtraTests")),
               "agilent_dx")
})

test_that("read_metadata reads slow formats without their data", {
  files <- c(chromeleon_uv = "chromeleon_3D.txt", waters_arw = "waters_pda.arw",
             varian_sms = "DCM1.SMS", chemstation_uv = "chemstation_31.uv",
             chemstation_uv = "openlab_131.uv", mzml = "STRD15.mzML",
             cdf = "HP_MS.CDF", cdf = "VARIAN1.CDF",
             agilent_dx = "MeOH1.dx", agilent_dx = "agilent.dx")
  for (i in seq_along(files)){
    path <- extra_test_file(files[[i]])
    fast <- read_metadata(path, format_in = names(files)[i],
                          progress_bar = FALSE)
    full <- suppressWarnings(extract_metadata(
      read_chroms(path, format_in = names(files)[i], parser = "chromconverter",
                  progress_bar = FALSE)))
    expect_true(all(names(fast) %in% names(full)), label = files[[i]])
    expect_equal(fast, full[names(fast)], label = files[[i]])
  }
})

test_that("read_metadata reads Thermo RAW files without their data", {
  skip_if_missing_thermorawfileparser()
  path <- extra_test_file("CirA.raw")
  tmp <- withr::local_tempdir()
  fast <- read_metadata(path, format_in = "thermoraw", progress_bar = FALSE)
  full <- extract_metadata(read_chroms(path, format_in = "thermoraw",
                                       path_out = tmp, progress_bar = FALSE))
  expect_true(all(names(fast) %in% names(full)))
  expect_equal(fast, full[names(fast)])
})

test_that("read_metadata reads an OpenLab .dx archive without injection.acmd", {
  skip_if_not_installed("zip")
  path <- extra_test_file("agilent.dx")
  dir <- withr::local_tempdir()
  utils::unzip(path, exdir = file.path(dir, "contents"))
  files <- setdiff(list.files(file.path(dir, "contents"), recursive = TRUE),
                   "injection.acmd")
  copy <- file.path(dir, "agilent.dx")
  zip::zip(copy, files, root = file.path(dir, "contents"))
  fast <- read_metadata(copy, format_in = "agilent_dx", progress_bar = FALSE)
  full <- extract_metadata(read_chroms(copy, format_in = "agilent_dx",
                                       progress_bar = FALSE))
  expect_equal(fast, full)
  expect_equal(fast$sample_name, "STD_1_1mM-1MKHCO3")
})

test_that("read_chromeleon finds the data after a long header mentioning data", {
  path <- extra_test_file("chromeleon_comma.txt")
  x <- readLines(path, warn = FALSE)
  filler <- c("Comment\tsee Raw Data: none",
              sprintf("Field %d\tvalue", seq_len(250)))
  long <- withr::local_tempfile(fileext = ".txt")
  writeLines(c(x[1:3], filler, x[-(1:3)]), long)
  expect_equal(read_chromeleon(long), read_chromeleon(path),
               ignore_attr = TRUE)
  fast <- read_metadata(long, format_in = "chromeleon_uv",
                        progress_bar = FALSE)
  full <- extract_metadata(read_chroms(long, format_in = "chromeleon_uv",
                                       progress_bar = FALSE))
  expect_equal(fast, full[names(fast)])
  expect_equal(fast$sample_name,
               read_metadata(path, format_in = "chromeleon_uv",
                             progress_bar = FALSE)$sample_name)
})
