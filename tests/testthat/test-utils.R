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

test_that("check_parser distinguishes 'ChemStation' file versions", {
  # `entab` reads 'ChemStation' versions 30 and 31 but not 130 or 179;
  # `rainbow` reads 130 and 179 but not 30 or 81. Auto-detection must respect
  # that rather than assuming every version of a `.ch` file is interchangeable.
  expect_equal(check_parser("chemstation_130", find = TRUE), "chromconverter")
  expect_equal(check_parser("chemstation_181", find = TRUE), "chromconverter")
  if (requireNamespace("entab", quietly = TRUE)){
    expect_equal(check_parser("chemstation_30", find = TRUE), "entab")
  }
  expect_error(check_parser("chemstation_181", parser = "entab"),
               "Mismatched arguments")
  expect_error(check_parser("chemstation_30", parser = "rainbow"),
               "Mismatched arguments")
  expect_silent(check_parser("chemstation_130", parser = "rainbow"))
})

test_that("check for pkg returns error for fake package", {
  expect_error(check_for_pkg("made_up_package"))
})

test_that("get_filetype returns error for unknown filetype", {
  expect_error(get_filetype("testdata/dad1.csv"), "File type not recognized")
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

test_that("check_metadata_format resolves the reader's tag", {
  expect_equal(check_metadata_format("chromconverter", "chemstation"),
               "chemstation")
  expect_equal(check_metadata_format("raw", "chemstation"), "raw")
  # readers pass the full default vector when the user supplies nothing
  expect_equal(check_metadata_format(c("chromconverter", "raw"), "waters_arw"),
               "waters_arw")
  expect_equal(check_metadata_format("ChromConverter", "asm"), "asm")
  expect_error(check_metadata_format("nonsense", "asm"))
})

test_that("source_sha1 hashes files and returns NA for anything else", {
  f <- test_path("testdata/dad1.uv")
  expect_match(source_sha1(f), "^[0-9a-f]{40}$")
  # no `names`: the value used to be built with `ifelse(fs::is_file(x), ...)`,
  # and `fs::is_file` returns a *named* logical, so the hash carried the
  # absolute path of the file as a name attribute
  expect_null(names(source_sha1(f)))

  # directories are a legitimate `source_file` for several formats -- a Waters
  # `.raw`, an 'Agilent' `.D` -- and `digest` errors on them
  expect_equal(source_sha1(test_path("testdata/RUTIN2.D")), NA)
  expect_equal(source_sha1(NA), NA)
  expect_equal(source_sha1(character(0)), NA)
  expect_equal(source_sha1("no/such/file"), NA)
})

test_that("sample_name_or_file falls back to the file name", {
  f <- "/tmp/sequence/blue_run.uv"
  expect_equal(sample_name_or_file(list(`Sample Name` = "blue"),
                                   "Sample Name", f), "blue")
  expect_equal(sample_name_or_file(list(), "Sample Name", f), "blue_run")
  expect_equal(sample_name_or_file(list(`Sample Name` = NULL),
                                   "Sample Name", f), "blue_run")
  expect_equal(sample_name_or_file(list(`Sample Name` = character(0)),
                                   "Sample Name", f), "blue_run")
  # a name is returned unchanged, not reshaped like the `ifelse` it replaced
  expect_equal(sample_name_or_file(list(n = c("a", "b")), "n", f), c("a", "b"))
})

test_that("the metadata vocabulary is the single source for `what`", {
  fields <- chrom_metadata_fields()
  expect_type(fields, "character")
  expect_false(anyDuplicated(fields) > 0)
  # `extract_metadata` must not keep its own copy of the list
  expect_equal(eval(formals(extract_metadata)$what), fields)
  # the drift this replaced: the reader asked for a name one format set, and
  # not the name seventeen of them set
  expect_true("sample_injection_volume" %in% fields)
  expect_false("injection_volume" %in% fields)
  expect_true("software" %in% fields)
  expect_false("software_name" %in% fields)
})

test_that("superseded metadata field names are still accepted", {
  expect_equal(resolve_metadata_fields("injection_volume"),
               "sample_injection_volume")
  expect_equal(resolve_metadata_fields("software_name"), "software")
  expect_equal(resolve_metadata_fields("run_date"), "run_datetime")
  expect_equal(resolve_metadata_fields(c("time_start", "time_end")), "time_range")
  # unknown and current names pass through untouched
  expect_equal(resolve_metadata_fields(c("sample_name", "nonsense")),
               c("sample_name", "nonsense"))
})

test_that("bookkeeping_attrs is only the structural set", {
  expect_equal(bookkeeping_attrs(),
               c("names", "class", "dim", "dimnames", "row.names"))
  # `transfer_metadata` must keep the acaml table -- a reshaped chromatogram is
  # the same chromatogram -- while `list_metadata_attrs` must not copy it onto
  # every trace beneath a list
  x <- structure(1:4, acaml_metadata = data.frame(a = 1), instrument = "LC")
  expect_equal(attr(transfer_metadata(1:4, x), "acaml_metadata"),
               data.frame(a = 1))
  expect_named(list_metadata_attrs(x), "instrument")
})

test_that("'Shimadzu' ASCII date-times are read in any locale's format", {
  p <- function(x) format(parse_shimadzu_ascii_datetime(x), "%Y-%m-%d %H:%M:%S")

  # 12-hour month-first, as written by a US locale
  expect_equal(p("4/26/2021 11:01:11 PM"), "2021-04-26 23:01:11")
  expect_equal(p("1/2/2021 3:04:05 AM"), "2021-01-02 03:04:05")
  # 24-hour day-first, with either separator
  expect_equal(p("02/08/2023 17:08:21"), "2023-08-02 17:08:21")
  expect_equal(p("29-03-2022 10:12:19"), "2022-03-29 10:12:19")
  expect_equal(p("19-01-2021 14:55:46"), "2021-01-19 14:55:46")
  # a leading component over 12 can only be a day
  expect_equal(p("13/01/2021 08:00:00"), "2021-01-13 08:00:00")
  # ISO order is also accepted
  expect_equal(p("2022-03-29 10:12:19"), "2022-03-29 10:12:19")

  expect_true(is.na(parse_shimadzu_ascii_datetime("")))
  expect_true(is.na(parse_shimadzu_ascii_datetime(NA)))
  expect_true(is.na(parse_shimadzu_ascii_datetime("not a date")))
  expect_length(parse_shimadzu_ascii_datetime(character(0)), 1)
  expect_s3_class(parse_shimadzu_ascii_datetime("29-03-2022 10:12:19"), "POSIXct")
  expect_equal(attr(parse_shimadzu_ascii_datetime("29-03-2022 10:12:19"), "tzone"), "UTC")
})
