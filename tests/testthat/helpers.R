
# helper function to test equality
elementwise.all.equal <- Vectorize(function(x, y, ...) {isTRUE(all.equal(x, y, ...))})

# helper to set (or, with `NA`, unset) an environment variable, returning its
# previous value so a test can restore it with `on.exit`.
set_env_var <- function(name, value){
  old <- Sys.getenv(name, unset = NA)
  if (is.na(value)){
    Sys.unsetenv(name)
  } else {
    do.call(Sys.setenv, stats::setNames(list(value), name))
  }
  invisible(old)
}

# helper function to skip tests if we don't have the right python dependencies.
skip_if_missing_dependencies <- function(reqs = c("numpy", "olefile")) {
  have_reqs <- sapply(reqs, reticulate::py_module_available)
  if (mean(have_reqs) < 1)
    skip(paste("required packages", reqs[!have_reqs],
               "not available for testing"))
}

# The 'aston' module is not installed alongside the other Python dependencies;
# `call_aston` declares it with `py_require` and lets reticulate provision it on
# first use. That works in an ordinary session but not inside the sandbox
# `devtools::check()` runs tests in, where the parser then fails and the reader
# returns nothing. Ask for it here so the test skips rather than erroring.
skip_if_missing_aston <- function() {
  ok <- tryCatch({
    reticulate::py_require(get_parser_reqs("aston"))
    reticulate::py_module_available("aston.tracefile")
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) skip("the 'aston' Python module could not be provisioned.")
}

skip_if_missing_thermorawfileparser <- function() {
  if (.Platform$OS.type != "windows"){
    path <- readLines(system.file("shell/thermofileparser.sh", package = "chromConverter"))[2]
    path <- strsplit(path," ")[[1]][2]
  } else {
    path <- readLines(system.file("shell/path_parser.txt", package = "chromConverter"))
    path <- gsub("\\\\", "/", path)
  }
  have_reqs <- fs::file_exists(path)
  if (mean(have_reqs) < 1)
    skip("ThermoRawFileParser could not be found.")
}

skip_if_missing_openchrom <- function() {
  path_openchrom <- readLines(system.file('shell/path_to_openchrom_commandline.txt',
                        package='chromConverter'))
  if (file.exists(path_openchrom)){
    have_openchrom_cli <- switch(configure_openchrom(cli="status"), true = TRUE,
           false = FALSE)
  } else{
    have_openchrom_cli <- FALSE
  }
  if (!have_openchrom_cli){
    skip("OpenChrom could not be found.")
  }
}

#' Ground truth for a 'Shimadzu' fixture, taken from a 'ProteoWizard'
#' conversion of the same file
#'
#' These slices cover only the first handful of spectra -- enough to pin m/z,
#' MS level and polarity without shipping a 50 MB mzML.
#'
#' m/z carry four decimal places. That is the precision Shimadzu's library
#' hands them over at, not a rounding introduced by the export -- the csv is
#' written with seven. It sets the floor of any m/z comparison against these
#' values: 0.5 ppm at the bottom of a typical mass range, 0.05 ppm at the top.
#'
#' Profile intensities in these slices are the vendor's ringing-suppressed
#' values, not the raw stored ones: they run ~7% high on total ion current and
#' ~20% low at the peak apex. Check profile intensities against the file's own
#' `TIC Data` stream instead. m/z, MS level and polarity are reliable.
sz_ground_truth <- function(name){
  path <- system.file(paste0(name, "_gt.csv.gz"),
                      package = "chromConverterExtraTests")
  if (!file.exists(path)){
    skip(paste0(name, "_gt.csv.gz could not be found."))
  }
  gt <- utils::read.csv(path)
  # a scan 'ProteoWizard' exported with no peaks is one row with `mz` and
  # `intensity` empty; the parsers drop empty scans, so there is nothing for
  # those rows to be compared against
  gt[!is.na(gt$mz), ]
}

#' Read every mass spectrum in a 'Shimadzu' `.lcd` file as one table
#'
#' `read_shimadzu_lcd` returns a table per MS level. A test that checks a whole
#' run -- against a vendor conversion, or against the file's own TIC -- wants
#' the levels stitched back together in scan order, with the per-scan summaries
#' of both as its `scan_info`. Only the columns the levels share are kept, so
#' nothing is padded with `NA` that the file does not record.
sz_read_ms <- function(path, ...){
  sz_stitch_ms(read_shimadzu_lcd(path, what = "MS", format_out = "data.frame",
                                 ...))
}

#' Stitch levels that have already been read
#'
#' The same as `sz_read_ms`, for a test that needs the levels separately as
#' well and should not pay for a second decode to get them.
sz_stitch_ms <- function(ms){
  if (is.data.frame(ms)){
    return(ms)
  }
  cols <- Reduce(intersect, lapply(ms, names))
  out <- do.call(rbind, lapply(ms, function(x) x[, cols]))
  info <- do.call(rbind, lapply(ms, attr, "scan_info"))
  out <- out[order(out$scan), ]
  info <- info[order(info$scan), ]
  row.names(out) <- row.names(info) <- NULL
  drop <- c("names", "row.names", "class", "ms_level")
  # polarity describes a level, so it only describes the run if they agree
  if (length(unique(lapply(ms, attr, "polarity"))) > 1){
    drop <- c(drop, "polarity")
  }
  keep <- setdiff(names(attributes(ms[[1]])), drop)
  for (a in keep) attr(out, a) <- attr(ms[[1]], a)
  attr(out, "scan_info") <- info
  out
}
