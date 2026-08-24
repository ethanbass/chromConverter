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
print.chrom_list <- function(x, n = 5,
                             cols = c("sample_name", "run_datetime",
                                      "method", "detector"), ...) {
  len <- length(x)
  cat(sprintf("A chrom_list with %d chromatogram%s\n", len,
              if (len == 1) "" else "s"))

  if (len == 0) return(invisible(x))

  meta <- suppressWarnings(extract_metadata(x, cols))

  is_constant <- sapply(meta, function(col) length(unique(col)) == 1)
  constant_cols <- meta[1, is_constant, drop = FALSE]
  varying_meta  <- meta[, !is_constant, drop = FALSE]

  if (any(is_constant)) {
    cat(paste(names(constant_cols),
              sapply(constant_cols, as.character),
              sep = ": ", collapse = "  |  "), "\n")
  }

  # Print first n rows of varying columns
  n_show <- min(n, len)
  if (ncol(varying_meta) > 0) {
    print(varying_meta[seq_len(n_show), , drop = FALSE], row.names = TRUE)
  }

  if (len > n_show) {
    cat(sprintf("... with %d more chromatogram%s\n", len - n_show,
                if (len - n_show == 1) "" else "s"))
  }

  invisible(x)
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
