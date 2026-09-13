#' Print a chrom_list object
#'
#' Prints a summary of a `chrom_list` without displaying the underlying
#' chromatographic data. Attributes that are constant across all chromatograms
#' are collapsed into a single header line, while varying attributes are shown
#' as a table truncated to the first `n` rows.
#'
#' @param x A `chrom_list` object.
#' @param n Integer. Maximum number of chromatograms to show in the table.
#' Defaults to `10`.
#' @param cols Character vector of attribute names to extract and display.
#'   Defaults to `c("sample_name", "run_datetime", "method", "detector")`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [extract_metadata]
#'
#' @export
print.chrom_list <- function(x, n = 10,
                             cols = c("sample_name", "run_datetime",
                                      "method", "detector"), ...) {
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

  meta <- suppressWarnings(extract_metadata(x, cols))
  if (!inherits(meta, "data.frame")) {
    # `extract_metadata` returns `NA` when none of `cols` could be found. Say
    # so, but still list the chromatograms: their names are the only thing left
    # to report, and dropping them would also drop the "... with N more" notice.
    cat(sprintf("  no metadata found for: %s\n",
                paste(cols, collapse = ", ")))
    meta <- data.frame(name = vapply(leaves, function(l)
      paste(l$path, collapse = "."), character(1)))
  }

  # Group by everything above the leaf, so that the traces belonging to one
  # sample are printed together under its name.
  groups <- vapply(leaves, function(l)
    paste(utils::head(l$path, -1), collapse = "."), character(1))
  grouped <- n_traces > 1 && any(nzchar(groups))

  is_constant <- sapply(meta, function(col) length(unique(col)) == 1)
  # In grouped mode the leaf names are the point of the table, so `name` is
  # always shown, even when a group holds a single trace.
  if (grouped) is_constant[names(meta) == "name"] <- FALSE
  constant_cols <- meta[1, is_constant, drop = FALSE]
  varying_meta  <- meta[, !is_constant, drop = FALSE]

  if (any(is_constant)) {
    # `format` rather than `as.character`, so that a value shown in the header
    # renders exactly as `print.data.frame` would render it in the table below
    # (`as.character` on a POSIXct keeps sub-second digits, `format` does not).
    header <- paste(names(constant_cols), unlist(format(constant_cols)),
                    sep = ": ", collapse = "  |  ")
    cat(paste0(paste(strwrap(header, width = getOption("width"), exdent = 2),
                     collapse = "\n"), "\n"))
  }

  n_show <- min(n, n_traces)
  if (ncol(varying_meta) > 0) {
    if (grouped) {
      varying_meta$name <- vapply(leaves, function(l) utils::tail(l$path, 1),
                                  character(1))
      print_grouped_meta(varying_meta[seq_len(n_show), , drop = FALSE],
                         groups[seq_len(n_show)])
    } else {
      print(varying_meta[seq_len(n_show), , drop = FALSE], row.names = TRUE)
    }
  }

  if (n_traces > n_show) {
    cat(sprintf("... with %d more chromatogram%s\n", n_traces - n_show,
                if (n_traces - n_show == 1) "" else "s"))
  }

  invisible(x)
}

#' Print metadata in blocks, one per sample
#'
#' Delegates to `print.data.frame` and indents its output, so that column
#' alignment and width truncation are handled by R rather than by hand. Row
#' numbers run across the whole table, so they stay consistent with the
#' "... with N more chromatograms" notice.
#' @noRd
print_grouped_meta <- function(meta, groups){
  for (group in unique(groups)){
    # a list mixing flat and nested entries gives the flat ones an empty group;
    # printing it would emit a bare blank line
    if (nzchar(group)) cat(group, "\n", sep = "")
    block <- meta[groups == group, , drop = FALSE]
    cat(paste0("  ", utils::capture.output(print(block, row.names = TRUE))),
        sep = "\n")
  }
  invisible(NULL)
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
