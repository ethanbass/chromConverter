#' Reshapes list of chromatograms from wide to long format
#' @name reshape_chroms
#' @param x A list of chromatographic matrices in wide format.
#' @param idx Indices of chromatograms to convert
#' @param sample_var String with name of new column containing sample IDs.
#' @param lambdas Wavelength(s) to include.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param combine Whether to combine chromatograms into a single \code{data.frame}
#' (applicable only if \code{data_format} is TRUE).
#' @param ... Additional arguments to \code{reshape_chrom}.
#' @return A list of chromatographic matrices in long format.
#' @author Ethan Bass
#' @noRd
reshape_chroms <- function(x, idx, sample_var = "sample", lambdas = NULL,
                           data_format, combine = TRUE, ...){
  if (missing(data_format)){
    data_format <- switch(attr(x[[1]], "data_format"),
           long = "wide", wide = "long")
  }
  if (missing(idx)){
    idx <- seq_along(x)
  }
  dat <- lapply(idx, function(i){

    xx <- reshape_chrom(x[[i]], lambdas = lambdas, data_format = data_format,
                        ...)
    if (data_format == "long"){
      xx[, sample_var] <- names(x)[[i]]
    }
    xx
  })
  if (combine & data_format == "long"){
    dat <- do.call(rbind,dat)
  } else {
    names(dat) <- names(x)
  }
  dat
}

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
#' @name reshape_chrom
#' @importFrom stats reshape
#' @param x A chromatographic matrix in wide format.
#' @param lambdas Wavelength(s) to include.
#' @param names_to Argument to \code{\link[tidyr]{pivot_longer}}
#' @return A chromatographic matrix in long format.
#' @author Ethan Bass
#' @noRd
reshape_chrom_long <- function(x, lambdas = NULL, format_out = NULL,
                               names_to = "lambda"){
  if (!is.null(attr(x, "data_format")) && attr(x, "data_format") == "long"){
    warning("The data already appear to be in long format!", immediate. = TRUE)
  }
  if (is.null(format_out)){
    format_out <- class(x)[1]
  }

  format_out <- check_format_out(format_out)
  xx <- as.data.frame(x)

  if (ncol(x) == 1){
    data <- data.frame(rt = as.numeric(rownames(xx)), intensity = xx[,1],
               row.names = NULL)
  } else {
    if (!is.null(lambdas)){
      xx <- xx[, lambdas, drop = FALSE]
    }
    data <- data.frame(tidyr::pivot_longer(data.frame(rt = rownames(xx), xx,
                                                      check.names = FALSE),
                                cols = -c("rt"), names_to = names_to,
                                values_to = "intensity"))
    data <- apply(data, 2, as.numeric)
  }
  if (format_out == "matrix"){
    data <- as.matrix(data)
  }
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
