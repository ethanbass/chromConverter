#' Read mzML files
#'
#' Extracts data from `mzML` files using parsers from either RaMS or mzR.
#'
#' The RaMS parser (default) returns a list with one element per stream in
#' `what`. Mass spectra are always long. With `data_format = "wide"` (the
#' default), `TIC` and `BPC` are returned as 2D chromatograms of class
#' `format_out`, and `DAD` as a wide chromatogram of class `format_out`; with
#' `"long"`, every stream is a long `data.table`. The mzR parser returns only the DAD data, as a
#' single chromatogram.
#'
#' @name read_mzml
#' @importFrom RaMS grabMSdata
#' @inheritParams shared_params
#' @param path Path to `.mzml` file.
#' @param format_out Class of output. Either `matrix`, `data.frame`, or
#' `data.table`. With RaMS, applies only to the `TIC` and `BPC` in wide
#' format.
#' @param parser What parser to use. Either `RaMS` or `mzR`.
#' @param what What types of data to return (argument to [RaMS::grabMSdata]).
#' Options include `MS1`, `MS2`, `BPC`, `TIC`, `DAD`, `chroms`, `metadata`, or
#' `everything`. Defaults to all of them.
#' @param verbose Argument to `grabMSdata` controlling verbosity.
#' @param ... Additional arguments to `grabMSdata`.
#' @return With RaMS, a named list of the streams in `what`. With mzR, a DAD
#' chromatogram in the format specified by `format_out` and `data_format`.
#' @examples \dontrun{
#' read_mzml("path/to/file.mzML")
#' }
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
          convert_chrom_format(reshape_chrom_wide(x), format_out = format_out)
        } else{
          x
        }
      })
    }
    # dispatch through `attach_metadata` like every other format, so the
    # file properties RaMS recovered end up as attributes where
    # `extract_metadata` and `print.chrom_list` can see them
    meta <- rams_meta_to_list(data[["metadata"]])
    data <- purrr::imap(data, function(x, h){
      x <- attach_metadata(x, meta = meta, format_in = "mzml",
                           format_out = format_out,
                           data_format = ifelse(grepl("MS", h), "long",
                                                data_format),
                           parser = "RaMS", source_file = path,
                           source_file_format = "mzML", scale = NULL)
      if (h == "metadata")
        class(x) <- c("chromconverter_metadata", class(x))
      x
      })
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

#' Flatten the file metadata returned by RaMS into a named list
#'
#' `RaMS::grabMSdata` returns file-level properties as a one-row table rather
#' than as the named list that `attach_metadata` expects, so this normalizes
#' the shape before dispatch. Doing it here keeps the knowledge of RaMS's
#' output in this file and lets the `"mzml"` branch read `meta$field` like
#' every other format.
#'
#' Returns an empty list when no metadata was requested -- `what` need not
#' include `"metadata"` -- in which case the branch fills every field with the
#' `NA`s it already declares.
#' @param meta The `metadata` table returned by `RaMS::grabMSdata`, or `NULL`.
#' @return A named list of the non-missing fields, or an empty list.
#' @noRd
rams_meta_to_list <- function(meta){
  if (is.null(meta) || NROW(meta) == 0) return(list())
  out <- lapply(as.list(meta), function(col){
    val <- col[[1]]
    if (length(val) == 0) NA else val
  })
  out[!vapply(out, function(v) all(is.na(v)), logical(1))]
}
