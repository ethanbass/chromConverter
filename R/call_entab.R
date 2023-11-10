#' @name call_entab
#' @title Entab parsers
#' @param file path to file
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param format_in Format of input.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @export

call_entab <- function(file, data_format = c("wide", "long"),
                       format_in = NULL,
                       format_out = c("matrix", "data.frame"),
                       read_metadata = TRUE,
                       metadata_format = c("chromconverter", "raw")){
  if (!requireNamespace("entab", quietly = TRUE)){
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
         call. = FALSE)
  }
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))

  metadata_format <- match.arg(tolower(metadata_format), c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = format_in, raw = "raw")
  r <- entab::Reader(file)
  x <- entab::as.data.frame(r)
  signal.idx <- grep("signal", colnames(x))
  if (length(signal.idx) == 1){
    colnames(x)[signal.idx] <- "wavelength"
  }
  if (data_format == "wide"){
      x <- reshape_chrom_wide(x, time_var = "time", lambda_var = "wavelength",
                              value_var = "intensity")
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (read_metadata){
    meta <- r$metadata()
    meta$run_date <- as.POSIXct(eval(meta$run_date))
    meta <- rename_list(meta, c("detector" = "instrument", "method" = "method",
                        "operator" = "operator", "date" = "run_date",
                        "sample_name" = "sample"))

    if (grepl("chemstation", format_in)){
      metadata_from_file <- try(read_chemstation_metadata(file), silent = TRUE)
    } else if (format_in == "masshunter_dad"){
      metadata_from_file <- try(read_masshunter_metadata(file), silent = TRUE)
    }
    if (exists("metadata_from_file") && !inherits(metadata_from_file, "try-error")){
      meta <- c(meta, metadata_from_file)
    }
    x <- attach_metadata(x, meta, format_in = format_in, format_out = format_out,
                         data_format = data_format, parser = "entab",
                         source_file = file)
  }
  x
}
