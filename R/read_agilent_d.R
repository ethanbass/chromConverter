#' Read files from 'Agilent ChemStation' .D directories
#'
#' Reads the `.ch`, `.uv`, `Report.TXT` (or `Result.xml`) and `LCDIAG.REG`
#' files in an 'Agilent' `.D` directory. Other files in the directory are
#' ignored.
#'
#' Instrument traces are named from `LCDIAG.REG`, so the names differ between
#' 'ChemStation' revisions and/or instruments (e.g. `"PMP1, Pressure"` and
#' `"PMP1, PMP1A, Pressure"`). Parts of the file that cannot be read are skipped
#' with a warning.
#'
#' Mass spectra are read from the `AcqData` folder of a 'MassHunter' `.d`
#' directory, which holds the `MSScan.bin`, `MSPeak.bin` and `MSScan.xsd` files
#' described in [read_agilent_dx]. Only centroided spectra at MS level 1 can
#' be read, and only from unit-resolution instruments such as single
#' quadrupole GC-MS and LC-MS. The `TIC` and `BPC` come from the record of
#' every scan, so they are also returned for runs whose spectra cannot be read,
#' such as triple quadrupole MRM. For profile (`MSProfile.bin`) or
#' high-resolution spectra, try `parser = "rainbow"` with `centroid` or `hrms`.
#'
#' @inheritParams shared_params
#' @param path Path to 'Agilent' `.D` directory.
#' @param what Whether to extract chromatograms (`chroms`), DAD data (`dad`),
#' peak tables (`peak_table`) and/or instrument traces (`instrument`), such as
#' pump pressure, flow, solvent composition and temperature, read from
#' `LCDIAG.REG`, and/or mass spectra (`MS1`) with the total ion chromatogram
#' (`TIC`) and base peak chromatogram (`BPC`). Accepts multiple arguments, and
#' defaults to all of these but `instrument`. Types the directory does not
#' contain are left out, and it is an error if it contains none of them. With
#' the default, mass spectrometry data that cannot be read are also left out,
#' with a warning, if the directory holds other data. The `TIC` and `BPC` are
#' read from the scan records, so they are returned even when the spectra
#' cannot be.
#' @return A list with one element per type found, each a chromatogram or a
#' list of them named by file, in the format specified by `format_out` and
#' `data_format`. If `data_format` is `wide`, the chromatograms will be
#' returned with retention times as rows and columns containing signal intensity
#' for each signal. If `long` format is requested, retention times will be
#' in the first column. The `format_out` argument determines whether the
#' chromatogram is returned as a `matrix`, `data.frame` or `data.table`.
#' Peak tables are returned in the `chromatographr` layout described in
#' [read_peaklist], named by wavelength (e.g. `"254"`), and keep the report's
#' name for the signal as `signal_descriptor`. They are read from `Result.xml`
#' where the directory has one, since unlike `Report.TXT` it records where each
#' peak starts and ends. Mass spectra and the `BPC` are always in long format,
#' with `rt`, `mz` and `intensity` columns, and are returned as a `data.table`
#' when `format_out` is `matrix`.
#' Metadata are attached as [attributes] if `read_metadata` is `TRUE`. With `collapse = TRUE`, a list of one element is replaced by that
#' element.
#' @examples
#' path <- system.file("extdata", "benzoxazinoid_standards", "BENZOS_250PPM.D",
#'                     package = "chromConverter")
#' run <- read_agilent_d(path)
#' names(run$chroms)
#' pump <- read_agilent_d(path, what = "instrument")
#' names(pump)
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_agilent_d <- function(path, what = c("dad", "chroms", "peak_table",
                                          "MS1", "TIC", "BPC"),
                           format_out = c("matrix", "data.frame", "data.table"),
                           data_format = c("wide", "long"),
                           read_metadata = TRUE,
                           metadata_format = c("chromconverter", "raw"),
                           collapse = TRUE){
  default_what <- missing(what)
  ms_types <- c("MS1", "TIC", "BPC")
  what <- ifelse(toupper(what) %in% ms_types, toupper(what), tolower(what))
  what <- match.arg(what, c("dad", "chroms", "peak_table", "instrument",
                            ms_types), several.ok = TRUE)
  exts <- c(chroms = "\\.ch$", dad = "\\.uv$",
            peak_table = "^(Report\\.TXT|Result\\.xml)$",
            instrument = "^LCDIAG\\.REG$")
  files <- lapply(exts[setdiff(what, ms_types)], function(ext){
    list.files(path, pattern = ext,
                          ignore.case = TRUE, full.names = TRUE)
  })
  ms_files <- fs::path(path, "AcqData", c("MSScan.bin", "MSPeak.bin", "MSScan.xsd"))
  has_scans <- all(file.exists(ms_files[c(1, 3)]))
  has_spectra <- has_scans && any(file.exists(
    c(ms_files[2], fs::path(path, "AcqData", "MSProfile.bin"))))
  files[intersect(what, c("TIC", "BPC"))] <- list(
    if (has_scans) ms_files else character())
  files[intersect(what, "MS1")] <- list(if (has_spectra) ms_files else character())
  files <- files[what]
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
    xml <- grepl("\\.xml$", files$peak_table, ignore.case = TRUE)
    peak_table <- if (any(xml)){
      read_chemstation_xml(files$peak_table[xml][1])
    } else read_chemstation_report(files$peak_table[1])
    peak_table <- transfer_metadata(name_peak_tables(peak_table), peak_table)
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
  ms <- list()
  ms_what <- intersect(what, ms_types)
  failed <- character()
  for (types in Filter(length, list(setdiff(ms_what, "MS1"),
                                    intersect(ms_what, "MS1")))){
    ms <- c(ms, tryCatch(
      read_agilent_ms(ms_files[1], ms_files[2], ms_files[3], what = types,
                      format_out = format_out, data_format = data_format,
                      read_metadata = read_metadata,
                      metadata_format = metadata_format, source_file = path),
      error = function(e){
        msg <- conditionMessage(e)
        if (inherits(e, "agilent_ms_format_error")){
          msg <- paste(msg, "Profile and high-resolution spectra may be",
                       "readable with `parser = \"rainbow\"`, setting `centroid`",
                       "or `hrms` to `TRUE`.")
        }
        if (!default_what) stop(msg, call. = FALSE)
        failed <<- c(failed, msg)
        NULL
      }))
  }
  what <- setdiff(what, setdiff(ms_what, names(ms)))
  if (length(what) == 0){
    stop(failed[1], call. = FALSE)
  }
  if (length(failed) > 0){
    warning("Left out ", paste(sQuote(setdiff(ms_what, names(ms))), collapse = ", "),
            ": ", paste(unique(failed), collapse = " "), call. = FALSE)
  }
  dat <- c(mget(setdiff(what, ms_types)), ms)[what]
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
