#' Read 'Agilent' DX files
#'
#' Reads 'Agilent' `.dx` files.
#'
#' This function unzips 'Agilent'  `.dx` into a temporary directory using
#' [unzip] and calls the appropriate parser on the unzipped file.
#'
#' @importFrom utils unzip
#' @inheritParams shared_params
#' @param path Path to Agilent `.dx` file.
#' @param what Whether to extract chromatograms (`chroms`), DAD data
#' (`dad`) and/or auxiliary instrumental data (`instrument`) (e.g.,
#' temperature, pressure, solvent composition, etc.). Accepts multiple arguments.
#' @param path_out A directory to export unzipped files. If a path is not
#' specified, the files will be written to a temp directory on the disk. The
#' function will overwrite existing folders in the specified directory
#' that share the basename of the file specified by `path`.
#' @author Ethan Bass
#' @return A chromatogram in the format specified by `format_out` (retention
#' time x wavelength).
#' @examples \dontrun{
#' read_agilent_dx(path)
#' }
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_agilent_dx <- function (path,  what = c("chroms", "dad"), path_out = NULL,
                             format_out = c("matrix", "data.frame", "data.table"),
                             data_format = c("wide", "long"), read_metadata = TRUE,
                             metadata_format = c("chromconverter", "raw"),
                             collapse = TRUE) {
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  what <- match.arg(what, c("chroms", "dad", "instrument"), several.ok = TRUE)
  files <- unzip(path, list = TRUE)
  exts <- c(chroms = "\\.ch$", dad = "\\.uv$", instrument = "\\.it$")
  files <- lapply(exts[what], function(ext){
    grep(ext, files$Name, ignore.case = TRUE, value = TRUE)
  })
  if (length(files) > 1){
    what <- what[vapply(files, length, FUN.VALUE = numeric(1)) > 0]
  }
  if (is.null(path_out)) {
    tmp <- tempdir()
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  }
  path_out <- fs::path(tmp, basename(path))
  fs::dir_create(path_out, force = TRUE)
  unzip(path, files = unlist(files), exdir = path_out)
  files.path <- lapply(files, function(fl){
    fs::path(path_out, fl)
  })
  if (any(what == "chroms")) {
    if (length(files.path$chroms) > 0){
      chroms <- lapply(files.path$chroms, read_chemstation_ch, format_out = format_out,
                       data_format = data_format, read_metadata = read_metadata,
                       metadata_format = metadata_format, source_file = path)
      names(chroms) <- sapply(chroms, function(x) attr(x, "detector_range"))
      chroms <- collapse_list(chroms)
    } else{
      stop("Trace data could not be found.")
    }
  }
  if (any(what == "dad")) {
    if (length(files.path$dad) > 0){
      dad <- read_chemstation_uv(files.path$dad, format_out = format_out,
                                 data_format = data_format,
                                 read_metadata = read_metadata,
                                 metadata_format = metadata_format,
                                 source_file = path)
    } else{
      stop("DAD data could not be found.")
    }
  }
  if (any(what == "instrument")){
    if (length(files.path$instrument) > 0){
      instrument <- lapply(files.path$instrument, read_chemstation_it,
                           format_out = format_out, data_format = data_format,
                           read_metadata = read_metadata,
                           metadata_format = metadata_format, source_file = path)
      names(instrument) <- sapply(instrument, function(x) attr(x, "detector_range"))
      instrument <- collapse_list(instrument)
    } else{
      "Instrument data could not be found."
    }
  }
  dat <- mget(what)
  if (collapse){
    dat <- collapse_list(dat)
  }
  dat
}
