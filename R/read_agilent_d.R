#' Read files from 'Agilent ChemStation' .D directories
#'
#' Reads the `.ch`, `.uv` and `Report.TXT` files in an 'Agilent' `.D`
#' directory. Other files in the directory are ignored.
#'
#' @inheritParams shared_params
#' @param path Path to 'Agilent' `.D` directory.
#' @param what Whether to extract chromatograms (`chroms`), DAD data (`dad`)
#' and/or peak tables (`peak_table`). Accepts multiple arguments, and defaults
#' to all three. Types the
#' directory does not contain are left out, and it is an error if it contains
#' none of them.
#' @return A list with one element per type found, each a chromatogram or a
#' list of them named by file, in the format specified by `format_out` and
#' `data_format`. If `data_format` is `wide`, the chromatograms will be
#' returned with retention times as rows and columns containing signal intensity
#' for each signal. If `long` format is requested, retention times will be
#' in the first column. The `format_out` argument determines whether the
#' chromatogram is returned as a `matrix`, `data.frame` or `data.table`.
#' Metadata are attached as [attributes] if `read_metadata` is `TRUE`. With `collapse = TRUE`, a list of one element is replaced by that
#' element.
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
                                          peaktable_format = "chromatographr")
    } else{
      stop("Peak table data could not be found.")
    }
  }
  dat <- mget(what)
  if (collapse) dat <- collapse_list(dat)
  dat
}
