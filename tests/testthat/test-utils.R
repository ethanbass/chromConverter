test_that("python modules are available", {
  skip_on_cran()
  expect_true(reticulate::py_module_available("olefile"))
  expect_true(reticulate::py_module_available("pandas"))
  expect_true(reticulate::py_module_available("rainbow"))
  expect_true(reticulate::py_module_available("scipy"))
})

test_that("check_data_format works as expected", {
  expect_equal(check_data_format("wide", format_out = "matrix"), "wide")
  expect_equal(check_data_format("long", format_out = "matrix"), "long")
  expect_equal(check_data_format(c("wide","long"), format_out = "matrix"), "wide")

  expect_equal(check_data_format("wide", format_out = "data.frame"), "wide")
  expect_equal(check_data_format("long", format_out = "data.frame"), "long")
  expect_equal(check_data_format(c("wide","long"), format_out = "data.frame"), "wide")

  expect_equal(check_data_format("wide", format_out = "data.table"), "long")
  expect_equal(check_data_format("long", format_out = "data.table"), "long")
  expect_equal(check_data_format(c("wide","long"), format_out = "data.table"), "long")

  expect_error(check_data_format(data_format = "matrix", format_out = "matrix"))
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


test_that("ms_bit_shift expands the 'ChemStation' MS intensity encoding", {
  vals <- c(0L, 1L, 100L, 16383L, 16384L, 16385L, 32768L, 32769L, 49152L,
            49157L, 65535L)

  expect_equal(vapply(vals, ms_bit_shift, numeric(1)),
    c(0, 1, 100, 16383, 0, 8, 0, 64, 0, 2560, 8388096))
  expect_equal(ms_bit_shift(vals),
    c(0, 1, 100, 16383, 0, 8, 0, 64, 0, 2560, 8388096))
  expect_equal(ms_bit_shift(integer(0)), numeric(0))
})

test_that("`precision` maps onto rainbow's m/z grid arguments", {
  # a power-of-ten grid, as `precision` has always produced
  expect_equal(rb_precision_args(1),
               list(bin_width = 0.1, display_precision = 1L))
  expect_equal(rb_precision_args(0),
               list(bin_width = 1, display_precision = 0L))
  expect_equal(rb_precision_args(3),
               list(bin_width = 0.001, display_precision = 3L))
})

test_that("`bin_width` overrides `precision` in `call_rainbow`", {
  expect_equal(rb_precision_args(1, bin_width = 0.5),
               list(bin_width = 0.5, display_precision = 1L))
  # enough decimals that no two bins can share a label
  expect_equal(rb_precision_args(1, bin_width = 0.25),
               list(bin_width = 0.25, display_precision = 2L))
  expect_error(rb_precision_args(1, bin_width = -1), "single positive number")
  expect_error(rb_precision_args(1, bin_width = c(0.1, 0.2)),
               "single positive number")
  expect_error(rb_precision_args(1, bin_width = "0.5"),
               "single positive number")
})
