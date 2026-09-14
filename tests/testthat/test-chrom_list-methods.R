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
