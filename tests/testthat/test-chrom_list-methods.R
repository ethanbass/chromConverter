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
  # leaves at different depths are grouped separately: `dad` sits directly
  # under the sample, the other two groups a level below it
  expect_true("MeOH1" %in% out)
  expect_true("MeOH1.chroms" %in% out)
  expect_true("MeOH1.instrument" %in% out)
  expect_true(any(grepl("dad", out, fixed = TRUE)))
  expect_true(any(grepl("DAD1E,Sig=344,4", out, fixed = TRUE)))
  expect_true(any(grepl("WPS1A,Temperature", out, fixed = TRUE)))
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
