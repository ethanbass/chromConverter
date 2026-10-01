#' Read files from 'Agilent ChemStation' .D directories
#'
#' Reads the `.ch`, `.uv`, `Report.TXT` and `LCDIAG.REG` files in an 'Agilent'
#' `.D` directory. Other files in the directory are ignored.
#'
#' Instrument traces are named from `LCDIAG.REG`, so the names differ between
#' 'ChemStation' revisions and/or instruments (e.g. `"PMP1, Pressure"` and
#' `"PMP1, PMP1A, Pressure"`). Parts of the file that cannot be read are skipped
#' with a warning.
#'
#' @inheritParams shared_params
#' @param path Path to 'Agilent' `.D` directory.
#' @param what Whether to extract chromatograms (`chroms`), DAD data (`dad`),
#' peak tables (`peak_table`) and/or instrument traces (`instrument`), such as
#' pump pressure, flow, solvent composition and temperature, read from
#' `LCDIAG.REG`. Accepts multiple arguments, and
#' defaults to `dad`, `chroms` and `peak_table`. Types the
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
  what <- match.arg(tolower(what), c("dad", "chroms", "peak_table", "instrument"),
                    several.ok = TRUE)
  exts <- c(chroms = "\\.ch$", dad = "\\.uv$", peak_table = "Report.TXT",
            instrument = "^LCDIAG\\.REG$")
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
  if (any(what == "instrument")){
    instrument <- read_lcdiag_traces(files$instrument[1], format_out = format_out,
                                     data_format = data_format,
                                     read_metadata = read_metadata,
                                     metadata_format = metadata_format)
    instrument <- collapse_list(instrument)
  }
  dat <- mget(what)
  if (collapse) dat <- collapse_list(dat)
  dat
}

#' Read the instrument traces in `LCDIAG.REG` as chromatograms
#' @return A list of 2D chromatograms, one per trace, named by trace.
#' @noRd
read_lcdiag_traces <- function(path, format_out = c("matrix", "data.frame",
                                                    "data.table"),
                               data_format = c("wide", "long"),
                               read_metadata = TRUE,
                               metadata_format = c("chromconverter", "raw")){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "chemstation")
  reg <- read_chemstation_reg(path)
  datetime <- reg$conditions$value[reg$conditions$key == "DateTime"][1]
  traces <- split(reg$traces, factor(reg$traces$trace,
                                     levels = unique(reg$traces$trace)))
  lapply(traces, function(tr){
    x <- format_2d_chromatogram(rt = tr$time, int = tr$value,
                                data_format = data_format,
                                format_out = format_out)
    if (read_metadata){
      meta <- list(signal_desc = tr$trace[1], units = tr$unit[1],
                   date = datetime, time_range = range(tr$time))
      x <- attach_metadata(x, meta, format_in = metadata_format,
                           format_out = format_out, data_format = data_format,
                           parser = "chromconverter", source_file = path,
                           source_file_format = "chemstation_reg")
    }
    x
  })
}
