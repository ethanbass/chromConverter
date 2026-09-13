#' Reshape chromatogram
#' @noRd
reshape_chrom <- function(x, data_format, ...){
  fn <- switch(data_format,
               long = reshape_chrom_long,
               wide = reshape_chrom_wide)

  fn(x, ...)
}

#' Reshape chromatogram (long)
#' Reshapes a single chromatogram from wide to long format
#'
#' The multi-column branch assembles the three columns directly instead of
#' pivoting. The rows are laid out in row-major order (all wavelengths for the
#' first time point, then the second, ...), which is the order
#' [tidyr::pivot_longer] produced and which callers rely on: `write_spectra`
#' walks the result one retention time at a time, and the wide/long round-trips
#' in the test suite compare `unique(long$rt)` against the wide rownames.
#'
#' `as.numeric(t(x))` flattens in exactly that order, so no sort is needed.
#' Avoiding the pivot also avoids the `apply(data, 2, as.numeric)` that followed
#' it: `apply` coerces its argument with `as.matrix`, and because `rt` entered
#' as a character vector (from the rownames) the whole table was routed through
#' a character matrix, formatting every intensity with `getOption("digits")`
#' and so rounding it to 7 significant figures.
#'
#' @name reshape_chrom
#' @param x A chromatographic matrix in wide format.
#' @param lambdas Wavelength(s) to include.
#' @param names_to Name of the column to hold the wide column names.
#' @return A chromatographic matrix in long format.
#' @author Ethan Bass
#' @noRd
reshape_chrom_long <- function(x, lambdas = NULL, format_out = NULL,
                               names_to = "lambda", sparse = FALSE){
  if (!is.null(attr(x, "data_format")) && attr(x, "data_format") == "long"){
    warning("The data already appear to be in long format!", immediate. = TRUE)
  }
  if (is.null(format_out)){
    format_out <- class(x)[1]
  }

  format_out <- check_format_out(format_out)

  if (ncol(x) == 1){
    xx <- as.data.frame(x)
    data <- data.frame(rt = as.numeric(rownames(xx)), intensity = xx[,1],
               row.names = NULL)
  } else {
    xx <- if (is.matrix(x)) x else as.matrix(x)
    if (!is.null(lambdas)){
      xx <- xx[, lambdas, drop = FALSE]
    }
    rn <- rownames(xx)
    if (is.null(rn)) rn <- seq_len(nrow(xx))
    cn <- colnames(xx)
    if (is.null(cn)) cn <- paste0("V", seq_len(ncol(xx)))

    # non-numeric column names still become `NA` with a coercion warning, as
    # they did when the whole table was coerced at once
    data <- cbind(rt = rep(as.numeric(rn), each = ncol(xx)),
                  lambda = rep(as.numeric(cn), times = nrow(xx)),
                  intensity = as.numeric(t(xx)))
    colnames(data)[2] <- names_to
    if (sparse){
      data <- data[data[, "intensity"] != 0, , drop = FALSE]
    }
  }
  data <- convert_chrom_format(data, format_out = format_out,
                               data_format = "long")
  data <- transfer_metadata(data, x)
  attr(data, "data_format") <- "long"
  data
}

#' Reshape chromatogram (wide)
#' Reshapes a single chromatogram from long to wide format
#' @noRd
reshape_chrom_wide <- function(x, lambdas = NULL, lambda_var = "lambda",
                               time_var = "rt", value_var = "intensity", drop = NULL){
  if (!is.null(attr(x, "data_format")) && attr(x, "data_format") == "wide"){
    warning("The data already appear to be in wide format!", immediate. = TRUE)
  }
  if (is.null(drop)){
    drop <- colnames(x)[which(sapply(x, is.character))]
  }
  if (!is.null(lambdas)){
    x <- x[which(x[[lambda_var]] %in% lambdas),]
  }
  x <- as.data.frame(x)
  data <- data.frame(tidyr::pivot_wider(x, id_cols = !!time_var,
                                     names_from = !!lambda_var,
                                     values_from = !!value_var),
                  row.names = time_var)
  colnames(data) <- gsub("X", "", colnames(data))
  data <- transfer_metadata(data, x)
  attr(data, "data_format") <- "wide"
  data
}
