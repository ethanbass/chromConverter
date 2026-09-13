#' Converter for 'Agilent MassHunter' UV files
#'
#' Converts a single chromatogram from MassHunter `.sp` format to R
#' `data.frame` using the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @section Deprecation:
#' The 'aston' parser is deprecated and will be removed in a future release.
#' 'Aston' has been unmaintained since 2020. This is the only remaining 'aston'
#' binding, and [read_chroms] selects it automatically only as a last resort,
#' when no other parser can read the file. Please use the 'entab' parser (by
#' the same author as 'Aston') instead, e.g.
#' `read_chroms(path, format_in = "masshunter_dad", parser = "entab")`.
#'
#' @name sp_converter
#' @inheritParams shared_params
#' @param path Path to file.
#' @inherit shared_params return
#' @import reticulate
#' @family external parsers
#' @keywords internal
#' @export sp_converter

sp_converter <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                         data_format = c("wide", "long"),
                         read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw")){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "masshunter_dad", raw = "raw")
  trace_file <- check_aston_configuration()
  x <- trace_file$agilent_uv$AgilentDAD(path)
  x <- setNames(data.frame(x$data$values,
                           row.names = x$data$index), x$data$columns)
  if (data_format == "long"){
    x <- reshape_chrom(x, data_format = "long", format_out = format_out)
  }
  x <- convert_chrom_format(x, format_out = format_out,
                            data_format = data_format)
  if (read_metadata){
    meta <- read_masshunter_metadata(path)
    x <- attach_metadata(x, meta, format_in = metadata_format,
                         format_out = format_out, data_format = data_format,
                         parser = "aston", source_file = path)
  }
  x
}

#' Converter for 'Agilent ChemStation' UV files
#'
#' Defunct. Use [read_chemstation_uv] instead.
#'
#' This function wrapped 'Aston''s generic `TraceFile` reader, which imports
#' `scipy.io.netcdf.NetCDFFile`. That symbol was removed in scipy v1.14, so
#' `TraceFile` cannot be used without pinning `scipy < 1.14` for the whole
#' Python session. Since `.uv` files are read by chromConverter's internal
#' parser, [read_chemstation_uv], this wrapper was retired rather than
#' constrain scipy for everyone.
#'
#' @name uv_converter
#' @inheritParams shared_params
#' @param path Path to file
#' @param correction Logical. Whether to apply empirical correction. Defaults is
#' TRUE.
#' @return There is no return value. Calling this function is an error.
#' @family external parsers
#' @keywords internal
#' @export uv_converter

uv_converter <- function(path, format_out = c("matrix","data.frame","data.table"),
                         data_format = c("wide","long"),
                         correction = TRUE, read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw")){
  .Defunct("read_chemstation_uv", package = "chromConverter",
           msg = paste0("'uv_converter' is defunct.\n",
                        "Use 'read_chemstation_uv' instead, or the 'entab' ",
                        "parser (`read_chroms(parser = \"entab\")`).\n",
                        "It relied on 'Aston''s `TraceFile` reader, which ",
                        "requires scipy < 1.14."))
}

#' Check 'Aston' configuration
#'
#' Declares the 'aston' Python requirements, initializes Python (if
#' necessary) and returns the `aston.tracefile` module.
#'
#' The requirements are declared here rather than in `.onLoad` following the
#' "Declaring Optional Dependencies" guidance in
#' `vignette("package", package = "reticulate")`, so that 'Aston' and 'pandas'
#' are only provisioned for users who actually call an 'aston' parser.
#' @noRd
check_aston_configuration <- function(){
  warn_aston_deprecated()
  reticulate::py_require(get_parser_reqs("aston"))
  check_py_module("aston")
  py_import("aston.tracefile")
}

#' Warn (once per session) that the 'aston' parsers are deprecated
#' @noRd
warn_aston_deprecated <- function(){
  if (isTRUE(pkg_state$aston_deprecation_warned)) return(invisible(NULL))
  pkg_state$aston_deprecation_warned <- TRUE
  .Deprecated(msg = paste0(
    "The 'aston' parser is deprecated and will be removed in a future ",
    "release of chromConverter.\n'Aston' has been unmaintained since 2020. ",
    "It is now used only to read 'Agilent MassHunter' `.sp` files, and only ",
    "when no other parser is available.\nPlease use the 'entab' parser (by ",
    "the same author as 'Aston') instead."))
  invisible(NULL)
}
