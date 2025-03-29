#' Read 'Chromeleon' ASCII files
#'
#' Reads 'Thermo Fisher Chromeleon™ CDS' ASCII (\code{.txt}) files.
#'
#' @importFrom utils tail read.csv
#' @param path Path to 'Chromeleon' ASCII file.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter} or
#' \code{raw}.
#' @param decimal_mark Which character is used as the decimal separator in the
#' file. By default, decimal mark will be detected automatically, but it can
#' also be manually set as \code{"."} or \code{","}.
#' @return A chromatogram in the format specified by \code{format_out}.
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_chromeleon <- function(path, format_out = c("matrix", "data.frame",
                                                 "data.table"),
                            data_format = c("wide", "long"),
                            read_metadata = TRUE,
                            metadata_format = c("chromconverter", "raw"),
                            decimal_mark = NULL){
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chromeleon",
                           raw = raw)
  if (!is.null(decimal_mark)){
    decimal_mark <- match.arg(decimal_mark, c(".", ","))
  }
  xx <- readLines(path)
  xx <- remove_unicode_chars(xx)
  start <- tail(grep("Data:", xx), 1)
  x <- read.csv(path, skip = start, sep = "\t", row.names = NULL,
                check.names = FALSE)
  x <- x[, -2, drop = FALSE]
  x <- x[, colSums(is.na(x)) < nrow(x)]
  meta <- try(read_chromeleon_metadata(xx))
  if (is.null(decimal_mark) && grepl(",", meta$`Dilution Factor`)){
    decimal_mark <- ","
    x <- apply(x, 2, function(x) gsub("\\.", "", x))
    x <- apply(x, 2, function(x) gsub(",", ".", x))
  } else {
    decimal_mark <- "."
    x <- apply(x, 2, function(x) gsub(",", "", x))
  }
  x <- apply(x, 2, as.numeric)
  if (ncol(x) == 2){
    colnames(x) <- c("rt", "intensity")
  }
  if (data_format == "wide"){
    rownames(x) <- x[, 1]
    x <- x[, -1, drop = FALSE]
  }
  if (data_format == "long" && ncol(x) > 2){
    rownames(x) <- x[, 1]
    x <- x[, -1, drop = FALSE]
    x <- reshape_chrom(x, data_format = "long")
  }
  x <- convert_chrom_format(x, format_out = format_out)
  if (read_metadata){
    if (decimal_mark == ","){
      meta <- lapply(meta, function(x) gsub(",", ".", x))
    }
    if (!inherits(meta, "try-error")){
      x <- attach_metadata(x, meta, format_in = metadata_format,
                           format_out = format_out, data_format = data_format,
                           parser = "chromConverter", source_file = path)
    }
  }
  x
}

#' @name read_chromeleon_metadata
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_chromeleon_metadata <- function(x){
  start <- tail(grep("Data:", x), 1)
  meta <- strsplit(x[seq_len(start - 1)], split = '\t')
  meta <- meta[which(sapply(meta,length) == 2)]
  meta <- do.call(rbind, meta)
  rownames(meta) <- meta[, 1]
  meta <- as.list(meta[, -1])
  meta
}
