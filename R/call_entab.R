#' Call Entab
#'
#' Converts chromatography date files using [entab](https://github.com/bovee/entab) parsers.
#'
#' @param path Path to file.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param format_in Format of input.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @family external parsers
#' @export

call_entab <- function(path, data_format = c("wide", "long"),
                       format_out = c("matrix", "data.frame", "data.table"),
                       format_in = "", read_metadata = TRUE,
                       metadata_format = c("chromconverter", "raw")){
  if (!requireNamespace("entab", quietly = TRUE)){
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
         call. = FALSE)
  }
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))

  metadata_format <- match.arg(tolower(metadata_format), c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = format_in, raw = "raw")
  r <- entab::Reader(path)
  file_format <- r$parser()
  x <- entab::as.data.frame(r)
  if (grepl("dad$|uv$", file_format)){
    signal.idx <- grep("signal", colnames(x))
    if (length(signal.idx) == 1){
      colnames(x)[signal.idx] <- "wavelength"
    }
    colnames(x) <- c("rt", "lambda", "intensity")
    if (data_format == "wide"){
      x <- reshape_chrom_wide(x, time_var = "rt", lambda_var = "lambda",
                              value_var = "intensity")
      }
  } else if (grepl("fid$", file_format)){
    if (data_format == "wide"){
      x <- data.frame(row.names = x$time, intensity = x$intensity)
    }
  } else if (grepl("ms$", file_format)){
    colnames(x)[c(1,3)] <- c("rt", "intensity")
  }
  x <- convert_chrom_format(x, format_out = format_out)
  if (read_metadata){
    meta <- r$metadata()
    meta$run_date <- as.POSIXct(eval(meta$run_date))
    meta$detector <- toupper(strsplit(file_format,"_")[[1]][2])
    meta <- rename_list(meta, c("detector_model" = "instrument", "method" = "method",
                        "operator" = "operator", "date" = "run_date",
                        "sample_name" = "sample",
                        "detector_y_units" = "y_units",
                        "intensity_multiplier" = "mult_correction",
                        "intensity_offset" = "offset_correction"))

    if (grepl("chemstation", format_in)){
      metadata_from_file <- try(read_chemstation_metadata(path), silent = TRUE)
    } else if (format_in == "masshunter_dad"){
      metadata_from_file <- try(read_masshunter_metadata(path), silent = TRUE)
    }
    if (exists("metadata_from_file") && !inherits(metadata_from_file, "try-error")){
      meta <- c(meta, metadata_from_file)
    }
    x <- attach_metadata(x, meta, format_in = format_in, format_out = format_out,
                         data_format = data_format, parser = "entab",
                         source_file = path, source_file_format = file_format)
  }
  x
}
