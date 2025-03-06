#' Converter for 'Agilent MassHunter' UV files
#'
#' Converts a single chromatogram from MassHunter \code{.sp} format to R
#' \code{data.frame} using the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name sp_converter
#' @param path Path to file
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to read metadata and attach it to the
#' chromatogram.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @family external parsers
#' @export sp_converter

sp_converter <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                         data_format = c("wide", "long"),
                         read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw")){
  check_aston_configuration()
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "masshunter_dad", raw = "raw")

  x <- trace_file$agilent_uv$AgilentDAD(path)
  x <- pd$DataFrame(x$data$values, columns = x$data$columns,
                    index = x$data$index)
  if (data_format == "long"){
    x <- reshape_chrom(x, data_format = "long")
  }
  x <- convert_chrom_format(x, format_out = format_out)
  if (read_metadata){
    meta <- read_masshunter_metadata(path)
    x <- attach_metadata(x, meta, format_in = metadata_format,
                         format_out = format_out, data_format = "wide",
                         parser = "aston", source_file = path)
  }
  x
}

#' Converter for 'Agilent ChemStation' UV files
#'
#' Converts a single chromatogram from ChemStation \code{.uv} format to R
#' \code{data.frame}.
#'
#' Uses the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name uv_converter
#' @param path Path to file
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param correction Logical. Whether to apply empirical correction. Defaults is
#' TRUE.
#' @param read_metadata Logical. Whether to read metadata and attach it to the
#' chromatogram.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @family external parsers
#' @export uv_converter

uv_converter <- function(path, format_out = c("matrix","data.frame","data.table"),
                         data_format = c("wide","long"),
                         correction = TRUE, read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw")){
  check_aston_configuration()
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide","long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "chemstation_uv", raw = "raw")
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  x <- trace_file$TraceFile(path)
  x <- pd$DataFrame(x$data$values, columns=x$data$columns,
                    index=x$data$index)
  if (data_format == "long"){
    x <- reshape_chrom(x, data_format = "long")
  }
  x <- convert_chrom_format(x, format_out = format_out)
  if (correction){
    # multiply by empirical correction value
    correction_value <- 0.9536743164062551070259132757200859487056732177734375
    x <- apply(x, 2, function(xx)xx*correction_value)
  }
  if (read_metadata){
    meta <- read_chemstation_metadata(path)
    x <- attach_metadata(x, meta, format_in = metadata_format,
                         format_out = format_out, data_format = "wide",
                         parser = "Aston", source_file = path)
  }
  x
}

#' Aston TraceFile Converter
#'
#' Uses Aston parser to figure out file-type and convert to R \code{data.frame}.
#' @name trace_converter
#' @title generic converter for other types of files
#' @param path Path to file
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @noRd
trace_converter <- function(path, format_out = c("matrix", "data.frame"),
                            data_format = c("wide", "long")){
  check_aston_configuration()
  format_out <- check_format_out(format_out)
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  data_format <- match.arg(data_format, c("wide", "long"))
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  x <- trace_file$TraceFile(path)
  x <- pd$DataFrame(x$data$values, columns = x$data$columns,
                    index = x$data$index)
  if (data_format == "long"){
    x <- reshape_chrom(x, data_format = "long")
  }
  x <- convert_chrom_format(x, format_out = format_out)
  x
}

#' Check aston configuration
#' @noRd
check_aston_configuration <- function(){
  assign_trace_file()
  if (length(trace_file) == 0){
    ans <- readline("Aston not found. Configure Aston? (y/n)?")
    if (ans %in% c('y', "Y", "YES", "yes", "Yes")){
      configure_python_environment(parser = "aston")
    }
  }
}

#' @noRd
assign_trace_file <- function(){
  pos <- 1
  envir = as.environment(pos)
  assign("trace_file", reticulate::import("aston.tracefile"), envir = envir)
  assign("pd", reticulate::import("pandas"), envir = envir)
  assign("csv", reticulate::import("csv"), envir = envir)
}
