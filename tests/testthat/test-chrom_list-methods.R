read_meoh <- function(){
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("MeOH1.dx", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))
  read_chroms(path, format_in = "agilent_dx",
              what = c("dad", "chroms", "instrument"), progress_bar = FALSE)
}

test_that("flatten_chrom_list enumerates doubly nested chromatograms", {
  # `MeOH1.dx` nests to mixed depths within a single sample: `dad` is itself a
  # chromatogram, while `chroms` (5) and `instrument` (14) are lists of them.
  x <- read_meoh()
  expect_length(x, 1)

  flat <- flatten_chrom_list(x)
  expect_length(flat, 20)
  expect_equal(names(flat)[1], "MeOH1.dad")
  expect_equal(names(flat)[2], "MeOH1.chroms.DAD1E,Sig=344,4  Ref=off")
  # every leaf is a chromatogram, not a list
  expect_true(all(vapply(flat, inherits, logical(1),
                         c("matrix", "data.frame", "data.table"))))
  expect_equal(dim(flat[["MeOH1.dad"]]), c(4050, 156))
})

test_that("flatten_chrom_list handles lists no parser currently produces", {
  # these shapes don't arise from any fixture, so they are built by hand
  mk <- function() matrix(1:4, nrow = 2)
  expect_equal(names(flatten_chrom_list(list(s1 = list(mk(), mk())))),
               c("s1.1", "s1.2"))
  # `read_chemstation_report` stores `chrom_list = NA`; a non-list is a leaf
  expect_equal(names(flatten_chrom_list(list(a = NA, b = mk()))), c("a", "b"))
  expect_length(flatten_chrom_list(list()), 0)
})

test_that("extract_metadata reads doubly nested chromatograms", {
  x <- read_meoh()
  # previously the one-level flattening left `chroms` and `instrument` as
  # lists, which carry no attributes, so all 19 of those rows were lost
  meta <- suppressWarnings(extract_metadata(x, what = c("sample_name",
                                                        "detector")))
  expect_equal(nrow(meta), 20)
  expect_equal(meta$name[1], "MeOH1.dad")
  expect_true(all(meta$sample_name == "MeOH1"))

  # and the `detector` filter reaches nested chromatograms too
  expect_equal(nrow(suppressWarnings(
    extract_metadata(x, what = "detector", detector = "DAD"))), 1)
})

test_that("print.chrom_list groups doubly nested chromatograms", {
  x <- read_meoh()
  local_reproducible_output()
  out <- capture.output(print(x))

  # the count reflects the chromatograms, not `length(x)`
  expect_equal(out[1], "A chrom_list with 1 sample (20 chromatograms)")
  # leaves sit at different depths -- `dad` directly under the sample, the
  # other two a level below it -- but they are all the same sample, so they
  # belong in one block, headed by the sample and with the rest of each path
  # in the `name` column
  expect_true("MeOH1" %in% out)
  expect_false(any(c("MeOH1.chroms", "MeOH1.instrument") %in% out))
  expect_equal(sum(out == "MeOH1"), 1)
  expect_true(any(grepl(" dad ", out, fixed = TRUE)))
  expect_true(any(grepl("chroms.DAD1E,Sig=344,4", out, fixed = TRUE)))
  expect_true(any(grepl("instrument.WPS1A,Temperature", out, fixed = TRUE)))
  # the default `n` is 10, so the remainder is reported rather than dropped
  expect_match(paste(out, collapse = "\n"), "... with 10 more chromatograms",
               fixed = TRUE)

  # `n` counts chromatograms across all groups
  out <- capture.output(print(x, n = 3))
  expect_match(paste(out, collapse = "\n"), "... with 17 more chromatograms",
               fixed = TRUE)
  expect_false(any(grepl("WPS1A,Temperature", out, fixed = TRUE)))
})

test_that("print.chrom_list shows every trace of a multi-detector sample", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  skip_if_missing_dependencies("olefile")
  path <- system.file("multichannel_chrom.lcd",
                      package = "chromConverterExtraTests")
  skip_if_not(file.exists(path))
  local_reproducible_output()

  x <- read_chroms(path, progress_bar = FALSE)
  expect_length(x, 1)
  expect_equal(nrow(suppressWarnings(extract_metadata(x))), 3)

  out <- capture.output(print(x))
  expect_equal(out[1], "A chrom_list with 1 sample (3 chromatograms)")
  expect_true("multichannel_chrom" %in% out)
  # all three traces are listed, by their leaf names
  for (nm in names(x[[1]])){
    expect_true(any(grepl(nm, out, fixed = TRUE)), label = nm)
  }
  expect_true(any(grepl("Detector B", out, fixed = TRUE)))
  # nothing was silently truncated
  expect_false(any(grepl("more chromatogram", out)))
})

test_that("print.chrom_list is unchanged for a flat list", {
  local_reproducible_output()
  x <- c(read_chroms(test_path("testdata/chemstation_130.ch"),
                     find_files = FALSE, progress_bar = FALSE),
         read_chroms(test_path("testdata/dad1.uv"),
                     format_in = "chemstation_uv", parser = "chromconverter",
                     find_files = FALSE, progress_bar = FALSE))

  out <- capture.output(print(x))
  expect_equal(out[1], "A chrom_list with 2 chromatograms")
  # no group headers, and data rows are flush left rather than indented under
  # one (grouped mode prints them as "  1 ..."). Note that `print.data.frame`
  # indents continuation headers when it wraps columns, so only the numbered
  # rows are a reliable signal here.
  expect_false(any(out %in% names(x)))
  expect_true(any(grepl("^1 ", out)))
  expect_false(any(grepl("^ +1 ", out)))
  expect_true(any(grepl("chemstation_130", out, fixed = TRUE)))
  expect_true(any(grepl("dad1", out, fixed = TRUE)))

  expect_equal(capture.output(print(structure(list(), class = "chrom_list"))),
               "A chrom_list with 0 chromatograms")
})

test_that("print.chrom_list does not error when no metadata is present", {
  # no fixture produces a chromatogram without metadata, so build one
  x <- structure(list(a = matrix(1:4, nrow = 2), b = matrix(1:4, nrow = 2)),
                 class = "chrom_list")
  expect_no_error(out <- capture.output(print(x)))
  expect_equal(out[1], "A chrom_list with 2 chromatograms")
  expect_match(paste(out, collapse = "\n"), "no metadata found")
})

test_that("extract_metadata falls back on sample-level attributes", {
  # `read_agilent_rslt` attaches the acaml fields to the list holding the
  # traces rather than to the traces, so reading only the leaves lost them
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  sample <- structure(list(uv = mk(detector = "UV"),
                           ms = mk(detector = "MS", method = "from_trace")),
                      sample_name = "s1", method = "from_sample")
  x <- structure(list(s1 = sample), class = "chrom_list")

  meta <- suppressWarnings(extract_metadata(x, c("sample_name", "detector",
                                                 "method")))
  expect_equal(meta$sample_name, c("s1", "s1"))
  # a per-trace attribute is still per-trace
  expect_equal(meta$detector, c("UV", "MS"))
  # the traces do not disagree about `method` (only one records it), so the
  # sample's value is taken as the better description of both
  expect_equal(meta$method, c("from_sample", "from_sample"))

  # the detector filter sees the inherited attributes too
  expect_equal(nrow(suppressWarnings(
    extract_metadata(x, what = "detector", detector = "MS"))), 1)
})

test_that("extract_metadata expands nested metadata fields", {
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  # `polarity` is a field of chromConverter's own, so `ms_params` cannot be
  # reported under the names of its elements alone
  x <- structure(list(
    a = mk(sample_name = "a", time_range = c(0, 10),
           ms_params = list(polarity = "+",
                            segment_start_time = c(1, 2, 3))),
    b = mk(sample_name = "b",
           ms_params = list(polarity = "-",
                            segment_start_time = c(4, 5, 6)))),
    class = "chrom_list")

  # a nested field is left out unless asked for
  meta <- extract_metadata(x, what = "sample_name")
  expect_named(meta, c("name", "sample_name"))

  # `TRUE` finds it without naming it; an atomic field is not a nested one
  meta <- extract_metadata(x, what = c("sample_name", "time_range"),
                           expand = TRUE)
  expect_named(meta, c("name", "sample_name", "time_range1", "time_range2",
                       "ms_params.polarity",
                       "segment_start_time1", "segment_start_time2",
                       "segment_start_time3"))
  expect_equal(meta$ms_params.polarity, c("+", "-"))
  expect_equal(meta$segment_start_time1, c("1", "4"))

  # naming it gives the same columns, alongside the default fields
  named <- extract_metadata(x, what = c("sample_name", "time_range"),
                            expand = "ms_params")
  expect_equal(named, meta)
  # and so does reaching it through `what`, so the two ways of asking agree
  expect_equal(extract_metadata(x, what = c("sample_name", "time_range",
                                            "ms_params")), meta)

  # `collapse` applies to each element of a nested field, not to the whole
  meta <- extract_metadata(x, what = "sample_name", expand = "ms_params",
                           collapse = TRUE)
  expect_named(meta, c("name", "sample_name", "ms_params.polarity",
                       "segment_start_time"))
  expect_equal(meta$segment_start_time, c("1, 2, 3", "4, 5, 6"))

  expect_warning(extract_metadata(x, what = "sample_name", expand = "nope"),
                 "not found")
  expect_error(extract_metadata(x, expand = 1), "must be TRUE")
})

test_that("a nested field need not be carried by every chromatogram", {
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  x <- structure(list(uv = mk(sample_name = "uv"),
                      ms = mk(sample_name = "ms",
                              ms_params = list(polarity = "+")),
                      # an empty nested field is as good as none
                      fid = mk(sample_name = "fid", ms_params = list())),
                 class = "chrom_list")

  meta <- extract_metadata(x, what = "sample_name", expand = TRUE)
  expect_named(meta, c("name", "sample_name", "ms_params.polarity"))
  expect_equal(meta$ms_params.polarity, c(NA, "+", NA))
})

test_that("a sample-level attribute is not inherited over a differing trace", {
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  # the traces disagree about the range, which the sample's value would hide:
  # agreement is judged on the whole value, not on its first element
  sample <- structure(list(a = mk(time_range = c(0, 10)),
                           b = mk(time_range = c(0, 20))),
                      time_range = c(0, 99))
  meta <- suppressWarnings(extract_metadata(
    structure(list(s1 = sample), class = "chrom_list"), "time_range"))
  expect_equal(meta$time_range2, c("10", "20"))

  # nor is a nested field inherited over a trace that has one of its own,
  # however far into it the traces differ
  sample <- structure(list(a = mk(ms_params = list(polarity = "+",
                                                   event = "a")),
                           b = mk(ms_params = list(polarity = "+",
                                                   event = "b"))),
                      ms_params = list(polarity = "+", event = "sample"))
  meta <- suppressWarnings(extract_metadata(
    structure(list(s1 = sample), class = "chrom_list"), "ms_params"))
  expect_equal(meta$event, c("a", "b"))

  # a trace keeps its own nested field even where the traces agree, unlike a
  # scalar, where the sample's value is taken as the better description
  sample <- structure(list(a = mk(ms_params = list(event = "leaf")),
                           b = mk(ms_params = list(event = "leaf"))),
                      ms_params = list(event = "sample"))
  meta <- suppressWarnings(extract_metadata(
    structure(list(s1 = sample), class = "chrom_list"), "ms_params"))
  expect_equal(meta$event, c("leaf", "leaf"))
})

test_that("extract_metadata reports nothing found the same way", {
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  expect_equal(suppressWarnings(
    extract_metadata(structure(list(a = mk()), class = "chrom_list"),
                     "sample_name")), NA)
  # and for a single chromatogram, which used to give an empty data.frame
  expect_equal(suppressWarnings(extract_metadata(mk(), "sample_name")), NA)
})

test_that("an expanded element is prefixed only where its name is taken", {
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  # an acaml record is named unlike anything else, so it needs no prefix
  acaml <- list(SampleName = "s1", VialNumber = 3, SourceFile = "s1.acaml")
  x <- structure(list(a = mk(sample_name = "a", acaml_metadata = acaml,
                             ms_params = list(polarity = "+",
                                              ion_time = 25))),
                 class = "chrom_list")

  # `polarity` is a metadata field in its own right, so that element alone
  # carries its field; `ion_time` beside it does not
  expect_named(extract_metadata(x, what = "sample_name", expand = TRUE),
               c("name", "sample_name", "SampleName", "VialNumber",
                 "SourceFile", "ms_params.polarity", "ion_time"))

  # asking for another field cannot rename these columns: the comparison is
  # against the whole vocabulary, not against `what`
  expect_equal(names(suppressWarnings(
    extract_metadata(x, what = c("sample_name", "polarity"),
                     expand = "acaml_metadata"))),
    c("name", "sample_name", "SampleName", "VialNumber", "SourceFile"))

  # nor can expanding one field rename the columns of another: `foo` is taken
  # by `acaml_metadata` whether or not it was asked for
  y <- structure(list(a = mk(ms_params = list(foo = 1),
                             acaml_metadata = list(foo = 2)))
                 , class = "chrom_list")
  expect_named(suppressWarnings(
    extract_metadata(y, what = "detector", expand = TRUE)),
    c("name", "ms_params.foo", "acaml_metadata.foo"))
  expect_named(suppressWarnings(
    extract_metadata(y, what = "detector", expand = "ms_params")),
    c("name", "ms_params.foo"))
})

test_that("extract_metadata expands a nested field attached to the sample", {
  # `read_agilent_rslt` attaches the acaml record to the list holding the
  # traces, and one row describes every trace in it
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  acaml <- data.frame(SampleName = "s1", VialNumber = 3,
                      InjectionAcqDateTime = as.POSIXct("2020-01-01 10:00:00",
                                                        tz = "UTC"))
  sample <- structure(list(uv = mk(detector = "UV"), ms = mk(detector = "MS")),
                      sample_name = "s1", acaml_metadata = acaml)
  x <- structure(list(s1 = sample), class = "chrom_list")

  meta <- extract_metadata(x, what = c("sample_name", "detector"),
                           expand = TRUE)
  expect_equal(meta$SampleName, c("s1", "s1"))
  expect_equal(meta$VialNumber, c("3", "3"))
  # `unlist` would otherwise leave a bare number in the column
  expect_equal(meta$InjectionAcqDateTime, rep("2020-01-01 10:00:00", 2))
})

test_that("a blank sample-level value cannot displace a trace's own", {
  # `usable_attr` counts `NA` and an empty string as no value, so a field the
  # parser located but read nothing out of must not overwrite a real one
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  sample <- structure(list(a = mk(sample_name = "REAL", detector = "UV")),
                      sample_name = "", detector = NA)
  x <- structure(list(s1 = sample), class = "chrom_list")

  meta <- suppressWarnings(extract_metadata(x, c("sample_name", "detector")))
  expect_equal(meta$sample_name, "REAL")
  expect_equal(meta$detector, "UV")
})

test_that("a stale sample-level copy cannot flatten a per-trace field", {
  # a multichannel sample's traces each have their own `detector`; a copy of
  # one of them on the enclosing list must not relabel the others
  mk <- function(...) structure(matrix(1:4, nrow = 2), ...)
  sample <- structure(list(a = mk(detector = "UV"), b = mk(detector = "MS")),
                      detector = "UV")
  x <- structure(list(s1 = sample), class = "chrom_list")

  meta <- suppressWarnings(extract_metadata(x, "detector"))
  expect_equal(meta$detector, c("UV", "MS"))
})

test_that("print.chrom_list omits a field that is empty everywhere", {
  local_reproducible_output()
  mk <- function(d) structure(matrix(1:4, nrow = 2), sample_name = "",
                              method = "m", detector = d)
  x <- structure(list(a = mk("UV"), b = mk("MS")), class = "chrom_list")

  out <- capture.output(print(x))
  # an unnamed injection would otherwise print as "sample_name: " with
  # nothing after it
  expect_false(any(grepl("sample_name", out, fixed = TRUE)))
  expect_true(any(grepl("method: m", out, fixed = TRUE)))
})

test_that("print.chrom_list keeps the padding around header separators", {
  local_reproducible_output()
  mk <- function(d) structure(matrix(1:4, nrow = 2), sample_name = "s",
                              method = "m", detector = d)
  x <- structure(list(a = mk("UV"), b = mk("MS")), class = "chrom_list")

  out <- capture.output(print(x))
  # `strwrap` used to collapse the double spaces to single ones
  expect_true(any(grepl("sample_name: s  |  method: m", out, fixed = TRUE)))
})

test_that("print.chrom_list shortens a long metadata value from the middle", {
  local_reproducible_output()
  long <- paste0("C:\\CDSProjects\\", strrep("directory", 9), "\\method.amx")
  mk <- function(d) structure(matrix(1:4, nrow = 2), method = long,
                              detector = d)
  x <- structure(list(a = mk("UV"), b = mk("MS")), class = "chrom_list")

  out <- capture.output(print(x))
  expect_false(any(nchar(out) > getOption("width")))
  # both ends survive, so the value stays identifiable
  expect_true(any(grepl("method: C:\\CDSProjects\\", out, fixed = TRUE)))
  expect_true(any(grepl("...", out, fixed = TRUE)))
  expect_true(any(grepl("\\method.amx", out, fixed = TRUE)))
})

test_that("print.chrom_list heads each block with the sample's own fields", {
  local_reproducible_output()
  mk <- function(name, time, d) structure(matrix(1:4, nrow = 2),
                                          sample_name = name, method = "m",
                                          detector = d,
                                          run_datetime = as.POSIXct(time,
                                                                    tz = "UTC"))
  x <- structure(list(f1 = list(MS1 = mk("DCM1", "2020-01-01", "MS"),
                                TIC = mk("DCM1", "2020-01-01", "MS")),
                      f2 = list(MS1 = mk("STRD15", "2021-01-01", "MS"),
                                TIC = mk("STRD15", "2021-01-01", "MS"))),
                 class = "chrom_list")

  out <- capture.output(print(x))
  # constant across the whole list, so it stays in the top header
  expect_true(any(grepl("method: m", out, fixed = TRUE)))
  # constant within each sample but not across them: shown once per block
  # rather than repeated on every row of it
  expect_true("f1  |  sample_name: DCM1  |  run_datetime: 2020-01-01" %in% out)
  expect_true("f2  |  sample_name: STRD15  |  run_datetime: 2021-01-01" %in% out)
  expect_equal(sum(grepl("DCM1", out, fixed = TRUE)), 1)
  # only the leaf names are left to tell the traces apart
  expect_false(any(grepl("sample_name", grep("^ ", out, value = TRUE),
                         fixed = TRUE)))
  expect_true(any(grepl("^ +1 +MS1$", out)))
})

test_that("print.chrom_list keeps a field that varies within a sample", {
  local_reproducible_output()
  mk <- function(name, d) structure(matrix(1:4, nrow = 2), sample_name = name,
                                    detector = d)
  x <- structure(list(f1 = list(UV = mk("a", "UV"), MS = mk("a", "MS")),
                      f2 = list(UV = mk("b", "UV"), MS = mk("b", "MS"))),
                 class = "chrom_list")

  out <- capture.output(print(x))
  expect_true("f1  |  sample_name: a" %in% out)
  # `detector` describes the trace, not the sample, so it stays in the table
  expect_true(any(grepl("detector", out, fixed = TRUE)))
  expect_true(any(grepl("^ +1 +UV +UV$", out)))
})

test_that("print.chrom_list block header drops empty and redundant fields", {
  local_reproducible_output()
  mk <- function(name, time) structure(matrix(1:4, nrow = 2),
                                       sample_name = name, detector = "MS",
                                       run_datetime = as.POSIXct(time,
                                                                 tz = "UTC"))
  x <- structure(list(DCM1 = list(MS1 = mk("DCM1", "2020-01-01"),
                                  TIC = mk("DCM1", "2020-01-01")),
                      f2 = list(MS1 = mk("", "2021-01-01"),
                                TIC = mk("", "2021-01-01"))),
                 class = "chrom_list")

  out <- capture.output(print(x))
  # the sample is already named `DCM1` by the label, as it is whenever
  # `read_chroms` was called with `sample_names = "sample_name"`
  expect_true("DCM1  |  run_datetime: 2020-01-01" %in% out)
  # and an unnamed injection must not print as "sample_name: "
  expect_true("f2  |  run_datetime: 2021-01-01" %in% out)
})

test_that("print.chrom_list leaves a mix of flat and nested samples alone", {
  local_reproducible_output()
  mk <- function(name) structure(matrix(1:4, nrow = 2), sample_name = name,
                                 detector = "MS")
  # the flat entries share one empty group label, so they are not one sample
  # and their fields cannot be hoisted into a header
  x <- structure(list(a = mk("a"),
                      b = list(MS1 = mk("b"), TIC = mk("b"))),
                 class = "chrom_list")

  out <- capture.output(print(x))
  expect_true("b" %in% out)
  expect_true(any(grepl("sample_name", out, fixed = TRUE)))
  expect_equal(sum(grepl(" a ", out)), 1)
})

test_that("print.chrom_list survives a metadata value that is not UTF-8", {
  local_reproducible_output()
  # A 'Shimadzu' `.lcd` written on a Chinese-locale instrument stores its
  # `method` path in the machine's codepage, so the directory names arrive as
  # GBK bytes. `trimws` and `nchar` both error on such a string, which used to
  # take down the whole summary. The bytes are spelled out here so that this
  # file stays ASCII.
  gbk <- rawToChar(as.raw(c(0x43, 0x3a, 0x5c, 0xca, 0xfd, 0xbe, 0xdd, 0x5c,
                            0x6d, 0x2e, 0x6c, 0x63, 0x6d)))
  expect_false(validUTF8(gbk))
  mk <- function(d) structure(matrix(1:4, nrow = 2), sample_name = "s",
                              method = gbk, detector = d)
  x <- structure(list(a = mk("UV"), b = mk("MS")), class = "chrom_list")

  expect_no_error(out <- capture.output(print(x)))
  expect_equal(out[1], "A chrom_list with 2 chromatograms")
  # the undecodable bytes are replaced, but the ASCII part of the path -- all
  # that identifies it -- is still legible
  expect_true(any(grepl("method: C:\\", out, fixed = TRUE)))
  expect_true(any(grepl("m.lcm", out, fixed = TRUE)))
})

test_that("summary.chrom_list tabulates doubly nested chromatograms", {
  x <- read_meoh()
  s <- summary(x)

  # one row per chromatogram, however deeply nested, with the sample and the
  # path below it in separate columns
  expect_equal(nrow(s), 20)
  expect_equal(s$sample, rep("MeOH1", 20))
  expect_equal(s$trace[1:2], c("dad", "chroms.DAD1E,Sig=344,4  Ref=off"))
  expect_equal(s$n_rows[1], 4050)
  expect_equal(s$n_cols[1], 156)
  # nothing is collapsed into a header, so a field that is constant across the
  # list is still a column
  expect_true(all(c("sample_name", "detector") %in% names(s)))
  expect_true(all(s$sample_name == "MeOH1"))
  # and nothing is truncated to the first `n` rows, as `print` does
  expect_equal(s$trace[20], "instrument.AFC1C,Delay Sensor")

  expect_s3_class(summary(x, format_out = "data.table"), "data.table")
  expect_s3_class(summary(x, format_out = "tibble"), "tbl_df")
  expect_equal(names(summary(x, cols = "detector")),
               c("sample", "trace", "n_rows", "n_cols", "detector"))
})

test_that("summary.chrom_list handles lists with nothing to report", {
  mk <- function() matrix(1:4, nrow = 2)

  # a flat list has nothing to put in `trace`, and no metadata to report
  s <- summary(structure(list(a = mk(), b = mk()), class = "chrom_list"))
  expect_equal(names(s), c("sample", "n_rows", "n_cols"))
  expect_equal(s$sample, c("a", "b"))
  expect_equal(s$n_rows, c(2L, 2L))

  s <- summary(structure(list(), class = "chrom_list"))
  expect_equal(dim(s), c(0L, 3L))
  expect_s3_class(s, "data.frame")
})

test_that("summary.chrom_list and print.chrom_list report the same fields", {
  # the two default to the same columns, so a field added for one cannot go
  # missing from the other
  expect_equal(formals(print.chrom_list)$cols, quote(chrom_summary_cols()))
  expect_equal(formals(summary.chrom_list)$cols, quote(chrom_summary_cols()))
  expect_true(all(c("sample_name", "detector", "scan_type", "precursor_mz",
                    "product_mz", "mz_range") %in% chrom_summary_cols()))
  # an optical trace is described too, not just a mass spectrum
  expect_true(all(c("wavelength", "detector_range") %in% chrom_summary_cols()))
})

test_that("summary.chrom_list omits the fields a detector does not record", {
  skip_on_cran()
  skip_if_missing_dependencies()
  skip_if_not_installed("chromConverterExtraTests")
  path <- system.file("shimadzu_tlm_dda.lcd",
          package = "chromConverterExtraTests")
  path_ms <- system.file("shimadzu_tlm_mrm_multi.lcd",
             package = "chromConverterExtraTests")
  path_pda <- system.file("Anthocyanin.lcd",
              package = "chromConverterExtraTests")
  skip_if_not(all(file.exists(path, path_ms, path_pda)))

  # this file holds both a PDA stream and a triple-quad MS stream, so the
  # optical and the mass spectrometry fields are both in play
  x <- read_chroms(path, format_in = "shimadzu_lcd",
                   what = c("pda", "tic"), progress_bar = FALSE)
  s <- summary(x)
  # the streams are named as the other readers name them, whatever case they
  # were asked for in
  expect_equal(s$trace, c("PDA", paste("TIC.Event", 1:4)))
  expect_equal(s$detector, c("PDA", rep("MS", 4)))
  expect_equal(s$wavelength, c("190, 800", rep(NA_character_, 4)))
  expect_equal(s$mz_range[1], NA_character_)
  expect_equal(s$mz_range[2], "209, 1001")

  # with only one kind of detector in the list, the other's fields are gone
  ms <- read_chroms(path_ms, format_in = "shimadzu_lcd", what = "tic",
                    progress_bar = FALSE)
  expect_false(any(c("wavelength", "detector_range") %in% names(summary(ms))))
  pda <- read_chroms(path_pda, format_in = "shimadzu_lcd",
                     what = "pda", progress_bar = FALSE)
  expect_false(any(c("scan_type", "precursor_mz", "product_mz", "mz_range")
                   %in% names(summary(pda))))
})
