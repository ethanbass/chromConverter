#' Chromeleon ASCII reader
#'
#' Reads 'Thermo Fisher Chromeleon™ CDS' files into R.
#'
#' @importFrom utils tail read.csv
#' @param path Path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter} or
#' \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}.
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_chromeleon <- function(path, format_out = c("matrix", "data.frame"),
                            data_format = c("wide", "long"),
                            read_metadata = TRUE,
                            metadata_format = c("chromconverter", "raw")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chromeleon",
                           raw = raw)
  xx <- readLines(path)
  xx <- remove_unicode_chars(xx)
  start <- tail(grep("Data:", xx), 1)
  x <- read.csv(path, skip = start, sep = "\t", row.names = NULL)
  x <- x[,-2, drop = FALSE]
  x <- x[,colSums(is.na(x)) < nrow(x)]
  if (any(grepl(",",as.data.frame(x)[-1, 2]))){
    decimal_separator <- ","
    x <- apply(x, 2, function(x) gsub("\\.", "", x))
    x <- apply(x, 2, function(x) gsub(",", ".", x))
  } else {
    decimal_separator <- "."
  }
  x <- apply(x, 2, as.numeric)
  colnames(x) <- c("RT", "Intensity")
  if (data_format == "wide"){
    rownames(x) <- x[,1]
    x <- x[, 2, drop = FALSE]
  }
  if (format_out == "data.frame"){
    x <- as.data.frame(x)
  }
  if (read_metadata){
    meta <- try(read_chromeleon_metadata(xx))
    if (decimal_separator == ","){
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
