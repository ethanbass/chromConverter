#' Read mzML files
#'
#' Extracts data from `mzML` files using parsers from either RaMS or mzR.
#'
#' Both parsers return a list with one element per stream in
#' `what`. Mass spectra and EICs are always long. With `data_format = "wide"` (the
#' default), `TIC` and `BPC` are returned as 2D chromatograms of class
#' `format_out`, and `DAD` as a wide chromatogram of class `format_out`; with
#' `"long"`, every stream is a long `data.table`.
#'
#' The `chroms` stream holds the chromatograms stored in the file, such as the
#' SIM and SRM traces of a triple quadrupole run. In wide format it is a list
#' of 2D chromatograms named by their ID in the file (or `Chromatogram 1`,
#' `Chromatogram 2` and so on where it has none), each with the `precursor_mz`
#' and `product_mz` it monitors as attributes. If any trace repeats a retention
#' time, as in some malformed files, the whole stream is returned as the long
#' `data.table` instead, with a warning.
#'
#' The mzR parser reads files that RaMS cannot, such as those compressed with
#' MS-Numpress. It returns only `MS1`, `MS2`, `BPC`, `TIC` and `DAD`, and no
#' file metadata. Its `TIC` and `BPC` are the per-scan values recorded in the
#' MS1 spectra.
#'
#' @name read_mzml
#' @importFrom RaMS grabMSdata
#' @inheritParams shared_params
#' @param path Path to `.mzml` file.
#' @param format_out Class of output. Either `matrix`, `data.frame`, or
#' `data.table`. Applies only to the `TIC`, `BPC`, `DAD` and `chroms` in wide
#' format.
#' @param parser What parser to use. Either `RaMS` or `mzR`.
#' @param what What types of data to return (argument to [RaMS::grabMSdata]).
#' Options include `MS1`, `MS2`, `BPC`, `TIC`, `DAD`, `chroms`, `metadata`, or
#' `everything`. Defaults to all of them. `EIC` and `EIC_MS2` return
#' extracted ion chromatograms from the MS1 or MS2 data for the masses given by
#' the `mz` and `ppm` arguments of [RaMS::grabMSdata], passed through `...`.
#' @param verbose Argument to `grabMSdata` controlling verbosity. Ignored by
#' mzR.
#' @param ... Additional arguments to `grabMSdata`. Ignored by mzR.
#' @return A named list of the streams in `what`.
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
  what_given <- !missing(what)
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  what <- match.arg(what, c("MS1","MS2", "BPC", "TIC", "DAD",
                            "chroms", "metadata", "everything",
                            "EIC", "EIC_MS2"),
                              several.ok = TRUE)
  if (all(c("MS1","MS2", "BPC", "TIC", "DAD",
            "chroms", "metadata", "everything") %in% what)){
    what <- grep("everything", what, invert = TRUE, value = TRUE)
  }
  if (parser == "RaMS"){
    data <- RaMS::grabMSdata(path, grab_what = what, verbosity = verbose, ...)
  } else{
    data <- read_mzml_mzr(path, what = what, explicit = what_given)
  }
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
  add_mzml_meta <- function(x, meta, data_format){
    attach_metadata(x, meta = meta, format_in = "mzml",
                    format_out = format_out, data_format = data_format,
                    parser = parser, source_file = path,
                    source_file_format = "mzML", scale = NULL)
  }
  data <- purrr::imap(data, function(x, h){
    if (h == "chroms" && data_format == "wide" && nrow(x) > 0){
      traces <- split(x, by = "chrom_index", sorted = FALSE)
      if (any(vapply(traces, function(tr) anyDuplicated(tr$rt) > 0, logical(1)))){
        warning(sprintf("Chromatograms in '%s' have repeated retention times, so `chroms` is returned in long format.",
                        basename(path)), call. = FALSE)
        return(add_mzml_meta(x, meta, "long"))
      }
      chroms <- lapply(traces, function(tr){
        mz <- list(precursor_mz = tr$target_mz[1], product_mz = tr$product_mz[1])
        add_mzml_meta(format_2d_chromatogram(tr$rt, tr$intensity, data_format = "wide",
                                             format_out = format_out),
                      c(meta, mz[!is.na(mz)]), "wide")
      })
      ids <- vapply(traces, function(tr) tr$chrom_type[1], character(1))
      names(chroms) <- ifelse(is.na(ids), paste("Chromatogram", seq_along(ids)),
                              ids)
      return(chroms)
    }
    x <- add_mzml_meta(x, meta, ifelse(grepl("MS|EIC", h), "long", data_format))
    if (h == "metadata")
      class(x) <- c("chromconverter_metadata", class(x))
    x
    })
  data
}

#' Read mzML streams with mzR
#'
#' Builds the same long tables, with the same columns, that `RaMS::grabMSdata`
#' returns, so both parsers share the reshaping and metadata steps.
#' @param path Path to the file.
#' @param what Streams to return.
#' @param explicit Whether the caller chose `what`. If not, streams mzR cannot
#' read are dropped instead of raising an error.
#' @return A named list of `data.table`s.
#' @noRd
read_mzml_mzr <- function(path, what, explicit = TRUE){
  if (!requireNamespace("mzR", quietly = TRUE)) {
    stop(
      "The `mzR` package is not installed. Please install it from Bioconductor:
      BiocManager::install('mzR')",
      call. = FALSE)
  }
  supported <- c("MS1", "MS2", "BPC", "TIC", "DAD")
  unsupported <- if (explicit) setdiff(what, c(supported, "everything"))
  if (length(unsupported) > 0){
    stop("The mzR parser cannot read ", paste(sQuote(unsupported, q = FALSE),
                                              collapse = ", "),
         ". Use `parser = \"RaMS\"` instead.", call. = FALSE)
  }
  what <- if (!explicit || "everything" %in% what) supported else unique(what)
  x <- mzR::openMSfile(path)
  on.exit(mzR::close(x))
  info <- mzR::header(x)
  rt <- info$retentionTime / 60
  file <- basename(path)
  spectra <- function(level, ...){
    idx <- which(info$msLevel == level)
    pks <- if (length(idx) == 1) list(mzR::peaks(x, idx)) else
      if (length(idx) > 1) mzR::peaks(x, idx) else list()
    n <- vapply(pks, nrow, integer(1))
    m <- do.call(rbind, c(list(matrix(numeric(0), 0, 2)), pks))
    extra <- lapply(list(...), function(col) rep(col[idx], n))
    data.table::data.table(rt = rep(rt[idx], n), as.data.frame(extra),
                           x = m[, 1], intensity = m[, 2],
                           filename = rep(file, nrow(m)))
  }
  ms1 <- which(info$msLevel == 1)
  summary_chrom <- function(values){
    data.table::data.table(rt = rt[ms1], intensity = values[ms1],
                           filename = rep(file, length(ms1)))
  }
  lapply(stats::setNames(nm = what), function(h){
    switch(h,
           MS1 = data.table::setnames(spectra(1), "x", "mz"),
           MS2 = data.table::setcolorder(
             data.table::setnames(spectra(2, premz = info$precursorMZ,
                                          voltage = replace(
                                            info$collisionEnergy,
                                            info$collisionEnergy %in% 0, NA)),
                                  "x", "fragmz"),
             c("rt", "premz", "fragmz", "intensity", "voltage", "filename")),
           TIC = summary_chrom(info$totIonCurrent),
           BPC = summary_chrom(info$basePeakIntensity),
           DAD = data.table::setnames(spectra(0), "x", "lambda"))
  })
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
#'
#' A mixed-polarity file gets one row per polarity, so each column keeps its
#' unique values. RaMS writes `"None found"` for a missing value; it becomes
#' `NA`.
#' @param meta The `metadata` table returned by `RaMS::grabMSdata`, or `NULL`.
#' @return A named list of the non-missing fields, or an empty list.
#' @noRd
rams_meta_to_list <- function(meta){
  if (is.null(meta) || NROW(meta) == 0) return(list())
  out <- lapply(as.list(meta), function(col){
    val <- if (is.list(col)) col[[1]] else unique(col)
    if (is.character(val)) val[val %in% "None found"] <- NA
    if (length(val) == 0) NA else val
  })
  out[!vapply(out, function(v) all(is.na(v)), logical(1))]
}
