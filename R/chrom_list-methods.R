#' Print a chrom_list object
#'
#' Prints a summary of a `chrom_list` without displaying the underlying
#' chromatographic data. Attributes that are constant across all chromatograms
#' are collapsed into a single header line, while varying attributes are shown
#' as a table truncated to the first `n` rows. When a sample holds more than
#' one chromatogram, its traces are printed as a block headed by the sample,
#' and any attribute they all share is shown in that block's header rather
#' than repeated down it.
#'
#' @param x A `chrom_list` object.
#' @param n Integer. Maximum number of chromatograms to show in the table.
#' Defaults to `10`.
#' @eval cols_doc()
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [extract_metadata]
#'
#' @export
print.chrom_list <- function(x, n = 10, cols = chrom_summary_cols(), ...) {
  n <- max(0L, as.integer(n))
  # A element may itself be a list of chromatograms (e.g. one per detector),
  # so the number of chromatograms is not `length(x)`. Both this method and
  # `extract_metadata` count them with `chrom_list_leaves`, so that the header
  # can never disagree with the table beneath it.
  leaves <- chrom_list_leaves(x)
  n_traces <- length(leaves)
  n_samples <- length(x)
  if (n_traces == n_samples) {
    cat(sprintf("A chrom_list with %d chromatogram%s\n", n_traces,
                if (n_traces == 1) "" else "s"))
  } else {
    cat(sprintf("A chrom_list with %d sample%s (%d chromatogram%s)\n",
                n_samples, if (n_samples == 1) "" else "s",
                n_traces, if (n_traces == 1) "" else "s"))
  }

  if (n_traces == 0) return(invisible(x))

  meta <- suppressWarnings(extract_metadata(x, cols, collapse = TRUE))
  if (!inherits(meta, "data.frame")) {
    # `extract_metadata` returns `NA` when none of `cols` could be found. Say
    # so, but still list the chromatograms: their names are the only thing left
    # to report, and dropping them would also drop the "... with N more" notice.
    cat(sprintf("  no metadata found for: %s\n",
                paste(cols, collapse = ", ")))
    meta <- data.frame(name = vapply(leaves, function(l)
      paste(l$path, collapse = "."), character(1)))
  }

  meta <- to_valid_utf8_df(meta)

  # Group by the sample --- the outermost name --- so that every trace belonging
  # to it is printed together under it. Grouping by the whole path above the
  # leaf instead would split one sample across several blocks whenever its
  # traces sit at different depths, as they do for an 'Agilent' `.dx`, where
  # `dad` is a chromatogram in its own right while `chroms` is a list of them.
  groups <- vapply(leaves, function(l)
    if (length(l$path) > 1) l$path[1] else "", character(1))
  grouped <- n_traces > 1 && any(nzchar(groups))

  is_constant <- vapply(meta, function(col) length(unique(col)) == 1, logical(1))
  # In grouped mode the leaf names are the point of the table, so `name` is
  # always shown, even when a group holds a single trace.
  if (grouped) is_constant[names(meta) == "name"] <- FALSE
  # A field that is constant only because it is empty everywhere --- an
  # unnamed injection, say --- says nothing, and printing `sample_name: ` with
  # nothing after it reads as a bug. Drop it from the header rather than
  # moving it to the table, where it would be just as empty.
  is_blank <- is_constant & vapply(meta, function(col) is_blank_value(col[[1]]),
                                   logical(1))
  constant_cols <- meta[1, is_constant & !is_blank, drop = FALSE]
  varying_meta  <- meta[, !is_constant, drop = FALSE]

  if (ncol(constant_cols) > 0) {
    cat(format_chrom_header(constant_cols), sep = "\n")
  }

  n_show <- min(n, n_traces)
  if (ncol(varying_meta) > 0) {
    if (grouped) {
      # the sample is named by the block header, so the rest of the path is
      # what distinguishes one trace from another within it
      varying_meta$name <- vapply(leaves, function(l)
        if (length(l$path) > 1) paste(l$path[-1], collapse = ".") else l$path,
        character(1))
      # A field that is the same for every trace in a sample describes the
      # sample rather than the trace, so it belongs in the block header instead
      # of being repeated down every row of the block --- `sample_name` in a
      # list read with `sample_names = "sample_name"` merely restates the label
      # above it. Constancy is judged over the whole sample rather than over
      # the rows `n` leaves room for, so that truncating the table cannot
      # change what the header claims. A list mixing flat and nested entries
      # groups all the flat ones together under an empty label, and they are
      # not one sample, so it is left alone.
      headers <- NULL
      if (all(nzchar(groups))){
        hoist <- names(varying_meta) != "name" &
          vapply(varying_meta, constant_within, logical(1), groups)
        if (any(hoist)){
          first <- !duplicated(groups)
          headers <- varying_meta[first, hoist, drop = FALSE]
          rownames(headers) <- groups[first]
          varying_meta <- varying_meta[, !hoist, drop = FALSE]
        }
      }
      print_grouped_meta(truncate_meta(varying_meta[seq_len(n_show), ,
                                                    drop = FALSE]),
                         groups[seq_len(n_show)], headers = headers)
    } else {
      print(truncate_meta(varying_meta[seq_len(n_show), , drop = FALSE]),
            row.names = TRUE)
    }
  }

  if (n_traces > n_show) {
    cat(sprintf("... with %d more chromatogram%s\n", n_traces - n_show,
                if (n_traces - n_show == 1) "" else "s"))
  }

  invisible(x)
}

#' Default metadata fields for summarizing a `chrom_list`
#'
#' The fields [print.chrom_list] and [summary.chrom_list] report by default.
#' After the fields every format records come the ones that
#' say what the detector measured --- `wavelength` or `detector_range` for an
#' optical detector, the rest for a mass spectrometer.
#'
#' @return A character vector of metadata field names.
#' @noRd
chrom_summary_cols <- function(){
  c("sample_name", "run_datetime", "method", "detector", "wavelength",
    "detector_range", "scan_type", "polarity", "precursor_mz", "product_mz",
    "mz_range")
}

#' Document the `cols` argument of the `chrom_list` methods
#'
#' `chrom_summary_cols` is the only place the default fields are written down,
#' so the documentation is generated from it rather than kept in step by hand.
#' Inserted by the `@eval` tag on `print.chrom_list` and `summary.chrom_list`,
#' which is why this returns roxygen lines rather than a formatted string.
#'
#' @param extra Further lines to append, for a method that has more to say
#' about the argument than the other does.
#' @noRd
cols_doc <- function(extra = character()){
  c("@param cols Character vector of attribute names to report. Defaults to:",
    paste0(paste0("`", chrom_summary_cols(), "`", collapse = ", "), "."),
    extra)
}

#' Summarize a chrom_list object
#'
#' Returns what [print.chrom_list] displays as a table, with one row per
#' chromatogram: the sample it belongs to, its size, and the metadata fields
#' in `cols`. Unlike `print`, nothing is collapsed into a header, abbreviated
#' or truncated to the first few rows, so the result can be filtered and
#' joined against.
#'
#' @param object A `chrom_list` object.
#' @eval cols_doc("A field that no chromatogram carries is omitted rather
#'   than filled with `NA`.")
#' @param format_out Format of object. Either `data.frame`, `data.table` or
#' `tibble`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `data.frame`, `data.table` or `tibble` (according to the value of
#' `format_out`) with one row per chromatogram. The first columns describe
#' where the chromatogram sits and how large it is --- `sample`, `trace` (only
#' when a sample holds more than one), `n_rows` and `n_cols` --- followed by one
#' column per metadata field found. A field no chromatogram records, or that
#' every one of them leaves empty, is dropped rather than filled with `NA`. A
#' field holding more than one value, such as the `product_mz` of an MRM event
#' monitoring several transitions, is collapsed to a comma-separated string so
#' that it occupies one column.
#'
#' @seealso [extract_metadata], [print.chrom_list]
#'
#' @export
summary.chrom_list <- function(object, cols = chrom_summary_cols(),
                               format_out = c("data.frame", "data.table",
                                              "tibble"), ...){
  format_out <- match.arg(format_out, c("data.frame", "data.table", "tibble"))
  # `extract_metadata` flattens with `chrom_list_leaves` too, so its rows line
  # up with `leaves` one for one, as `print.chrom_list` also relies on
  leaves <- chrom_list_leaves(object)
  out <- data.frame(
    sample = vapply(leaves, function(l) l$path[1], character(1)),
    trace = vapply(leaves, function(l) if (length(l$path) > 1)
      paste(l$path[-1], collapse = ".") else NA_character_, character(1)),
    n_rows = vapply(leaves, function(l) as.integer(NROW(l$chrom)), integer(1)),
    n_cols = vapply(leaves, function(l) as.integer(NCOL(l$chrom)), integer(1)),
    row.names = NULL, stringsAsFactors = FALSE)
  # a flat list has nothing to put in `trace`, and a column of `NA` describes
  # the shape of the list rather than the data
  if (all(is.na(out$trace))) out$trace <- NULL

  if (length(leaves) > 0){
    meta <- suppressWarnings(extract_metadata(object, cols, collapse = TRUE))
    if (inherits(meta, "data.frame")){
      meta <- meta[, setdiff(names(meta), "name"), drop = FALSE]
      # a field that is empty for every chromatogram says only that the format
      # does not record it, which is what `print.chrom_list` judges too
      keep <- !vapply(meta, function(col)
        all(vapply(col, is_blank_value, logical(1))), logical(1))
      out <- cbind(out, meta[, keep, drop = FALSE])
    }
  }
  out <- to_valid_utf8_df(out)
  if (format_out == "data.table"){
    data.table::setDT(out)
  } else if (format_out == "tibble"){
    out <- tibble::as_tibble(out)
  }
  out
}

#' Lay out the constant metadata fields as a header block
#'
#' `strwrap` would do the wrapping, but it normalizes runs of whitespace, which
#' collapses the padding around the separators, and it breaks a line wherever a
#' value happens to contain a space -- a Windows path in `method`, say -- rather
#' than treating each `field: value` pair as a unit. So the fields are
#' truncated to keep them legible and packed onto lines here.
#'
#' @param cols A one-row data frame of the constant fields.
#' @param prefix Optional label to place before the fields, used by
#' `print_grouped_meta` to head a block with the name of its sample.
#' @return A character vector of lines, continuations indented by two spaces.
#' @noRd
format_chrom_header <- function(cols, width = getOption("width"),
                                max_field = 60L, prefix = NULL){
  # `format` rather than `as.character`, so that a value shown in the header
  # renders as `print.data.frame` would render it in the table below
  # (`as.character` on a POSIXct keeps sub-second digits, `format` does not).
  vals <- truncate_middle(trimws(unlist(format(cols), use.names = FALSE)),
                          max_field)
  # a block header leads with the name of the sample, which is a label rather
  # than a `field: value` pair, but wraps along with them
  fields <- c(prefix, paste(names(cols), vals, sep = ": "))
  sep <- "  |  "
  lines <- character()
  current <- fields[1]
  for (field in fields[-1]){
    if (nchar(current) + nchar(sep) + nchar(field) <= width){
      current <- paste0(current, sep, field)
    } else {
      lines <- c(lines, current)
      current <- paste0("  ", field)
    }
  }
  c(lines, current)
}

#' Truncate the character columns of the metadata table
#'
#' `print.data.frame` does not truncate, so one long `source_file` or `method`
#' is enough to push every other column off the edge of the terminal.
#' @noRd
truncate_meta <- function(meta, max_field = 40L){
  for (col in names(meta)){
    if (is.character(meta[[col]])){
      meta[[col]] <- truncate_middle(meta[[col]], max_field)
    }
  }
  meta
}

#' Shorten a string from the middle
#'
#' Both ends of these values carry information --- a path names its directory
#' and its file --- so an elision in the middle keeps more than a trailing one.
#' @noRd
truncate_middle <- function(x, n){
  long <- which(!is.na(x) & nchar(x) > n)
  if (length(long) > 0){
    keep <- n - 3L
    head_n <- ceiling(keep / 2)
    x[long] <- paste0(substr(x[long], 1, head_n), "...",
                      substring(x[long], nchar(x[long]) - (keep - head_n) + 1))
  }
  x
}

#' Does a value amount to nothing worth printing?
#'
#' An unnamed injection leaves an empty `sample_name` behind, and printing
#' `sample_name: ` with nothing after it reads as a bug.
#' @noRd
is_blank_value <- function(v){
  is.na(v) || !nzchar(trimws(format(v)))
}

#' Is a column the same for every row within each group?
#' @noRd
constant_within <- function(col, groups){
  all(vapply(split(col, groups), function(v) length(unique(v)) == 1,
             logical(1)))
}

#' Print metadata in blocks, one per sample
#'
#' Delegates to `print.data.frame` and indents its output, so that column
#' alignment and width truncation are handled by R rather than by hand. Row
#' numbers run across the whole table, so they stay consistent with the
#' "... with N more chromatograms" notice.
#'
#' @param headers Optional data frame of the fields that describe each sample
#' as a whole, one row per group and named for it, shown alongside the sample's
#' name at the head of its block.
#' @noRd
print_grouped_meta <- function(meta, groups, headers = NULL){
  for (group in unique(groups)){
    # a list mixing flat and nested entries gives the flat ones an empty group;
    # printing it would emit a bare blank line
    if (nzchar(group)){
      fields <- if (is.null(headers)) NULL else
        block_header_fields(headers[group, , drop = FALSE], group)
      if (!is.null(fields) && ncol(fields) > 0){
        cat(format_chrom_header(fields, prefix = group), sep = "\n")
      } else cat(group, "\n", sep = "")
    }
    block <- meta[groups == group, , drop = FALSE]
    cat(paste0("  ", utils::capture.output(print(block, row.names = TRUE))),
        sep = "\n")
  }
  invisible(NULL)
}

#' The fields worth showing at the head of a sample's block
#'
#' Drops those with no value to show, along with any that merely repeat the
#' name of the sample: `sample_name` does exactly that in a list read with
#' `sample_names = "sample_name"`.
#' @noRd
block_header_fields <- function(cols, group){
  keep <- vapply(cols, function(col){
    !is_blank_value(col[[1]]) && !identical(trimws(format(col[[1]])), group)
  }, logical(1))
  cols[, keep, drop = FALSE]
}

#' Subset a `chrom_list` object
#'
#' Extracts a subset of a `chrom_list` while preserving its class, so
#' the result remains a `chrom_list` rather than a plain `list`.
#'
#' @param x A `chrom_list` object.
#' @param i Indices specifying elements to extract.
#' @param ... Additional arguments passed to the default `[` method.
#'
#' @return A `chrom_list` containing the selected elements.
#'
#' @export
#' @keywords internal
`[.chrom_list` <- function(x, i, ...) {
  out <- NextMethod()
  class(out) <- class(x)
  out
}

#' Combine `chrom_list` objects
#'
#' Combines multiple `chrom_list` objects (or a mix of `chrom_list` and
#' plain lists/matrices) into a single `chrom_list`, preserving the class.
#'
#' @param ... One or more `chrom_list` objects (or objects coercible via
#'   `c()`) to combine.
#'
#' @return A `chrom_list` containing all elements.
#'
#' @export
#' @keywords internal
c.chrom_list <- function(...) {
  out <- NextMethod()
  class(out) <- "chrom_list"
  out
}
