test_that("python modules are available", {
  skip_on_cran()
  expect_true(reticulate::py_module_available("olefile"))
  expect_true(reticulate::py_module_available("pandas"))
  expect_true(reticulate::py_module_available("rainbow"))
  expect_true(reticulate::py_module_available("scipy"))
})

test_that("get_times works as expected", {
  skip_on_cran()
  path_csv <- test_path("testdata/dad1.csv")

  # wide format
  x <- read_chroms(path_csv, format_in = "chemstation_csv", progress_bar = FALSE)
  expect_equal(head(get_times(x),1), 0.002, tolerance = .00001)
  expect_equal(tail(get_times(x),1), 12.95533, tolerance = .00001)
  expect_equal(head(get_times(x[[1]]),1), 0.002, tolerance = .00001)
  expect_equal(tail(get_times(x[[1]]),1), 12.95533, tolerance = .00001)

  attr(x[[1]],"data_format") <- NULL
  expect_equal(head(get_times(x),1), 0.002, tolerance = .00001)
  expect_equal(tail(get_times(x),1), 12.95533, tolerance = .00001)
  expect_equal(head(get_times(x[[1]]),1), 0.002, tolerance = .00001)
  expect_equal(tail(get_times(x[[1]]),1), 12.95533, tolerance = .00001)

  # long format
  x1 <- read_chroms(path_csv, format_in = "chemstation_csv",
                    data_format = "long", progress_bar = FALSE)
  expect_equal(head(get_times(x1),1), 0.002, tolerance = .00001)
  expect_equal(tail(get_times(x1),1), 12.95533, tolerance = .00001)
  expect_equal(head(get_times(x1[[1]]),1), 0.002, tolerance = .00001)
  expect_equal(tail(get_times(x1[[1]]),1), 12.95533, tolerance = .00001)
})

test_that("check_parser works as expected", {
  expect_equal(check_parser(format_in = "msd", parser = NULL, find = TRUE), "openchrom")
  expect_equal(check_parser(format_in = "wsd", parser = NULL, find = TRUE), "openchrom")
  expect_equal(check_parser(format_in = "csd", parser = NULL, find = TRUE), "openchrom")
  expect_equal(check_parser(format_in = "chemstation_csv", parser=NULL, find = TRUE), "chromconverter")
  expect_equal(check_parser(format_in = "shimadzu_fid", parser=NULL, find = TRUE), "chromconverter")
  expect_equal(check_parser(format_in = "shimadzu_dad", parser=NULL, find = TRUE), "chromconverter")
  expect_equal(check_parser(format_in = "chromeleon_uv", parser=NULL, find = TRUE), "chromconverter")
  expect_equal(check_parser(format_in = "waters_arw", parser=NULL, find = TRUE), "chromconverter")
  expect_equal(check_parser(format_in = "mzml", parser=NULL, find = TRUE), "chromconverter")
  # expect_equal(check_parser(format_in = "chemstation_fid", parser = NULL, find = TRUE), "chromconverter")
  # expect_equal(check_parser(format_in = "chemstation_ch", parser = NULL, find = TRUE), "chromconverter")
  expect_equal(check_parser(format_in = "chemstation_130", parser=NULL, find = TRUE), "chromconverter")
  expect_equal(check_parser(format_in = "thermoraw", parser=NULL, find = TRUE), "thermoraw")
  expect_error(check_parser(format_in = "csd", parser="rainbow", find = FALSE))
})

test_that("check for pkg returns error for fake package", {
  expect_error(check_for_pkg("made_up_package"))
})

test_that("get_filetype returns error for unknown filetype", {
  expect_error(get_filetype("testdata/dad1.csv"))
})

path_msd <- system.file("chemstation_MSD.MS",
            package = "chromConverterExtraTests")
