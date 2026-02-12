#' Read files from 'Agilent ChemStation' .D directories
#'
#' Reads files from 'Agilent' \code{.D} directories.
#'
#' Currently this function is limited to reading \code{.uv}, \code{.ch} and
#' \code{peak_table} elements.
#'
#' @param path Path to 'Agilent' \code{.D} directory.
#' @param what Whether to extract chromatograms (\code{chroms}), DAD data
#' (\code{dad}) and/or peak tables \code{peak_table}. Accepts multiple arguments.
#' \code{ms_spectra}. Accepts multiple arguments.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element.
#' @author Ethan Bass
#' @return A list of chromatograms in the format specified by
#' \code{data_format} and #' \code{format_out}. If \code{data_format} is
#' \code{wide}, the chromatograms will be returned with retention times as rows
#' and columns containing signal intensity for each signal. If \code{long}
#' format is requested, retention times will be in the first column. The
#' \code{format_out} argument determines whether the chromatogram
#' is returned as a \code{matrix}, \code{data.frame} or \code{data.table}.
#' Metadata can be attached to the chromatogram as \code{\link{attributes}} if
#' \code{read_metadata} is \code{TRUE}.
#' @examplesIf interactive()
#' read_agilent_d("tests/testthat/testdata/RUTIN2.D")
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_agilent_d <- function(path, what = c("dad", "chroms", "peak_table"),
                           format_out = c("matrix", "data.frame", "data.table"),
                           data_format = c("wide", "long"),
                           read_metadata = TRUE,
                           metadata_format = c("chromconverter", "raw"),
                           collapse = TRUE){
  what <- match.arg(tolower(what), c("dad", "chroms", "peak_table"), several.ok = TRUE)
  exts <- c(chroms = "\\.ch$", dad = "\\.uv$", peak_table = "Report.TXT")
  exts <- exts[what]
  files <- lapply(exts, function(ext){
    list.files(path, pattern = ext,
                          ignore.case = TRUE, full.names = TRUE)
  })
  files_found <- vapply(files, length, FUN.VALUE = numeric(1)) > 0
  if (!any(files_found)){
    missing <- names(files)[!files_found]
    stop("No files found for any requested type(s): ",
         paste(missing, collapse = ", "),
         "\nSearched in: ", path)
  }
  what <- what[vapply(files, length, FUN.VALUE = numeric(1)) > 0]
  if (any(what == "chroms")){
    if (length(files$chroms) > 0){
    chroms <- lapply(files$chroms, read_chemstation_ch, format_out = format_out,
                                           data_format = data_format,
                                           read_metadata = read_metadata,
                                           metadata_format = metadata_format)
    names(chroms) <- gsub("\\.ch$", "", basename(files$chroms))
    chroms <- collapse_list(chroms)
    } else {
        stop("Trace data could not be found.")
    }
  }
  if (any(what == "dad")){
    if  (length(files$dad) > 0){
    dad <- lapply(files$dad, read_chemstation_uv, format_out = format_out,
                     data_format = data_format,
                     read_metadata = read_metadata,
                     metadata_format = metadata_format)
    names(dad) <- gsub("\\.uv$", "", basename(files$dad))
    dad <- collapse_list(dad)
    } else {
        stop("DAD data could not be found.")
    }
  }
  if (any(what == "peak_table")){
    if (length(files$peak_table) > 0){
    peak_table <- read_chemstation_report(files$peak_table,
                                          data_format = "chromatographR")
    } else{
      stop("Peak table data could not be found.")
    }
  }
  dat <- mget(what)
  if (collapse) dat <- collapse_list(dat)
  dat
}
