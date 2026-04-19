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
  cat(sprintf("A chrom_list with %d chromatogram%s\n", len, if (len == 1) "" else "s"))

  if (len == 0) return(invisible(x))

  meta <- extract_metadata(x, cols)

  is_constant <- sapply(meta, function(col) length(unique(col)) == 1)
  constant_cols <- meta[1, is_constant, drop = FALSE]
  varying_meta  <- meta[, !is_constant, drop = FALSE]

  if (any(is_constant)) {
    cat(paste(names(constant_cols), unlist(constant_cols), sep = ": ", collapse = "  |  "), "\n")
  }

  # Print first n rows of varying columns
  n_show <- min(n, len)
  if (ncol(varying_meta) > 0) {
    print(varying_meta[seq_len(n_show), , drop = FALSE], row.names = TRUE)
  }

  if (len > n_show) {
    cat(sprintf("... with %d more chromatogram%s\n", len - n_show, if (len - n_show == 1) "" else "s"))
  }

  invisible(x)
}
