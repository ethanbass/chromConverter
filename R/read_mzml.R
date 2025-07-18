#' Read mzML files
#'
#' Extracts data from \code{mzML} files using parsers from either RaMS or mzR.
#' The RaMS parser (default) will only return data in tidy (long) format. The
#' mzR parser will return data in wide format. Currently the mzR-based parser
#' is configured to return only DAD data.
#'
#' @name read_mzml
#' @importFrom RaMS grabMSdata
#' @param path Path to \code{.mzml} file.
#' @param format_out Class of output. Only applies if \code{mzR} is selected.
#' Either \code{matrix}, \code{data.frame}, or \code{data.table}. \code{RaMS}
#' will return a list of data.tables regardless of what is selected here.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param parser What parser to use. Either \code{RaMS} or \code{mzR}.
#' @param what What types of data to return (argument to \code{\link[RaMS]{grabMSdata}}.
#' Options include \code{MS1}, \code{MS2}, \code{BPC}, \code{TIC}, \code{DAD},
#' \code{chroms}, \code{metadata}, or \code{everything}).
#' @param verbose Argument to \code{\link[RaMS]{grabMSdata}} controlling \code{verbosity}.
#' @param ... Additional arguments to \code{\link[RaMS]{grabMSdata}}.
#' @return If \code{RaMS} is selected, the function will return a list of "tidy"
#' \code{data.table} objects. If \code{mzR} is selected, the function will return a
#' chromatogram in \code{matrix} or \code{data.frame} format according to the
#' value of \code{format_out}.
#' @author Ethan Bass
#' @export read_mzml

read_mzml <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                      data_format = c("wide", "long"),
                      parser = c("RaMS", "mzR"),
                      what = c("MS1", "MS2", "BPC", "TIC", "DAD",
                             "chroms", "metadata", "everything"),
                      verbose = FALSE,
                      ...){
  parser <- match.arg(parser, c("RaMS", "mzR"))
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  what <- match.arg(what, c("MS1","MS2", "BPC", "TIC", "DAD",
                            "chroms", "metadata", "everything"),
                              several.ok = TRUE)
  if (all(c("MS1","MS2", "BPC", "TIC", "DAD",
            "chroms", "metadata", "everything") %in% what)){
    what <- grep("everything", what, invert = TRUE, value = TRUE)
  }
  if (parser == "RaMS"){
    data <- RaMS::grabMSdata(path, grab_what = what, verbosity = verbose, ...)
    data <- lapply(data, function(x){
      int_idx <- which(colnames(x) == "int")
      if (length(int_idx) > 0)
        colnames(x)[int_idx] <- "intensity"
      x
    })
    if (data_format == "wide"){
      data <- purrr::imap(data, function(x, h){
        if (h %in% c("TIC", "BPC") && nrow(x) > 0){
          format_2d_chromatogram(x$rt, x$intensity, data_format = "wide",
                                 format_out = format_out)
        } else if (h == "DAD" && nrow(x) > 0){
          reshape_chrom_wide(x)
        } else{
          x
        }
      })
    }
  } else if (parser == "mzR"){
    if (!requireNamespace("mzR", quietly = TRUE)) {
      stop(
        "The `mzR` package is not installed. Please install it from Bioconductor:
        BiocManager::install('mzR')",
        call. = FALSE)
    }
    x <- mzR::openMSfile(path)
    info <- mzR::header(x)
    UV_scans <- which(info$msLevel == 0)
    rts <- info[UV_scans, "retentionTime"]
    lambdas <- seq(info$scanWindowLowerLimit[UV_scans[1]],
                   info$scanWindowUpperLimit[UV_scans[1]])
    pks <- mzR::peaks(x)
    data <- t(sapply(UV_scans, function(j) pks[[j]][,2]))
    rownames(data) <- rts
    colnames(data) <- lambdas
    if (data_format == "long"){
      data <- reshape_chrom(data)
    }
    data <- convert_chrom_format(data, format_out = format_out)
  }
  data
}
