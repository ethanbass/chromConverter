#' Converter for Agilent MassHunter UV files
#'
#' Converts a single chromatogram from MassHunter \code{.sp} format to R \code{data.frame}.
#'
#' Uses the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name sp_converter
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to read metadata and attach it to the
#' chromatogram.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @export sp_converter

sp_converter <- function(file, format_out = c("matrix", "data.frame"),
                         data_format = c("wide","long"),
                         read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw")){
  check_aston_configuration()
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "masshunter_dad", raw = "raw")

  x <- trace_file$agilent_uv$AgilentDAD(file)
  x <- pd$DataFrame(x$data$values, columns = x$data$columns,
                    index = x$data$index)
  if (data_format == "long"){
    x <- reshape_chrom(x, data_format = "long")
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (read_metadata){
    meta <- read_masshunter_metadata(file)
    x <- attach_metadata(x, meta, format_in = metadata_format,
                         format_out = format_out, data_format = "wide",
                         parser = "aston", source_file = file)
  }
  x
}

#' Converter for Agilent ChemStation UV files
#'
#' Converts a single chromatogram from ChemStation \code{.uv} format to R \code{data.frame}.
#'
#' Uses the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name uv_converter
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param correction Logical. Whether to apply empirical correction. Defaults is
#' TRUE.
#' @param read_metadata Logical. Whether to read metadata and attach it to the
#' chromatogram.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @export uv_converter
uv_converter <- function(file, format_out = c("matrix","data.frame"),
                         data_format = c("wide","long"),
                         correction = TRUE, read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw")){
  check_aston_configuration()
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "chemstation_uv", raw = "raw")
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  x <- trace_file$TraceFile(file)
  x <- pd$DataFrame(x$data$values, columns=x$data$columns,
                    index=x$data$index)
  if (data_format == "long"){
    x <- reshape_chrom(x, data_format = "long")
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (correction){
    # multiply by empirical correction value
    x <- apply(x,2,function(xx)xx*0.9536743164062551070259132757200859487056732177734375)
  }
  # correct column order
  # x <- lapply(x, function(xx) xx[,order(as.numeric(colnames(xx)))])
  if (read_metadata){
    meta <- read_chemstation_metadata(file)
    x <- attach_metadata(x, meta, format_in = metadata_format,
                         format_out = format_out, data_format = "wide",
                         parser = "Aston", source_file = file)
  }
  x
}

#' Aston TraceFile Converter
#'
#' Uses Aston parser to figure out file-type and convert to R \code{data.frame}.
#' @name trace_converter
#' @title generic converter for other types of files
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @noRd
trace_converter <- function(file, format_out = c("matrix", "data.frame"),
                            data_format = c("wide", "long")){
  check_aston_configuration()
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  x <- trace_file$TraceFile(file)
  x <- pd$DataFrame(x$data$values, columns = x$data$columns,
                    index = x$data$index)
  if (data_format == "long"){
    x <- reshape_chrom(x, data_format = "long")
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  x
}

#' Configure Aston
#'
#' Configures reticulate to use Aston file parsers.
#' @name configure_aston
#' @param return_boolean Logical. Whether to return a Boolean value indicating
#' if the chromConverter environment is correctly configured.
#' @return If \code{return_boolean} is \code{TRUE}, returns a Boolean value
#' indicating whether the chromConverter environment is configured correctly.
#' Otherwise, there is no return value.
#' @author Ethan Bass
#' @import reticulate
#' @export
configure_aston <- function(return_boolean=FALSE){
  install <- FALSE
  # path <- miniconda_path()
  if (!dir.exists(miniconda_path())){
    install <- readline("It is recommended to install miniconda in your R library to use Aston parsers. Install miniconda now? (y/n)")
    if (install %in% c('y', "Y", "YES", "yes", "Yes")){
      install_miniconda()
    }
  } # else{
  # envs <- conda_list()
  # use_miniconda(envs[grep("r-reticulate", envs$name)[1],2])
  # }
  env <- reticulate::configure_environment("chromConverter")
  if (!env){
    reqs <- c("pandas","scipy","numpy","aston")
    reqs_available <- sapply(reqs, reticulate::py_module_available)
    if (!all(reqs_available)){
      conda_install(envname = "chromConverter", reqs[which(!reqs_available)], pip = TRUE)
    }
  }
  assign_trace_file()
  if (return_boolean){
    return(env)
  }
}

#' @noRd
check_aston_configuration <- function(){
  assign_trace_file()
  if (length(trace_file) == 0){
    ans <- readline("Aston not found. Configure Aston? (y/n)?")
    if (ans %in% c('y', "Y", "YES", "yes", "Yes")){
      configure_aston()
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
