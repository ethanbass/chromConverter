#' Read Chromatograms
#'
#' Reads chromatograms from specified folders or vector of paths using file
#' parsers from [Aston](https://github.com/bovee/aston),
#' [Entab](https://github.com/bovee/entab),
#' [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser),
#' [OpenChrom](https://lablicate.com/platform/openchrom),
#' [rainbow](https://rainbow-api.readthedocs.io/), or internal parsers.
#'
#' Provides a general interface to chromConverter parsers. Currently recognizes
#' 'Agilent ChemStation' (\code{.uv}, \code{.ch}), 'MassHunter' (\code{.dad})
#' files, 'Thermo RAW' (\code{.raw}), 'Waters ARW' (\code{.arw}), 'Waters RAW'
#' (\code{.raw}), 'Chromeleon ASCII' (\code{.txt}), 'Shimadzu ASCII'
#' (\code{.txt}). Also, wraps Openchrom parsers, which include many additional
#' formats. To use 'Entab', 'ThermoRawFileParser', or 'Openchrom' parsers,
#' they must be manually installed. Please see the instructions in the
#' [README](https://ethanbass.github.io/chromConverter/) for further details.
#'
#' @name read_chroms
#' @param paths paths to files or folders containing files
#' @param find_files Logical. Set to \code{TRUE} (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to\code{FALSE}.
#' @param format_in Format of files to be imported/converted. Current options
#' include: \code{chemstation_uv}, \code{chemstation}, \code{chemstation_ch},
#' \code{chemstation_csv}, \code{masshunter}, \code{masshunter_dad},
#' \code{shimadzu_fid}, \code{shimadzu_dad}, \code{chromeleon_uv},
#' \code{agilent_d}, \code{thermoraw}, \code{mzml}, \code{cdf}, \code{mdf},
#' \code{waters_arw}, \code{waters_raw}, \code{msd}, \code{csd}, \code{wsd},
#' or \code{other}.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format_in}.
#' @param parser What parser to use. Current option are \code{chromconverter},
#' \code{aston}, \code{entab}, \code{thermoraw}, \code{openchrom}, or
#' \code{rainbow}.
#' @param format_out R object format (i.e. data.frame or matrix).
#' @param data_format Whether to output data in wide or long format. Either
#' \code{wide} or \code{long}.
#' @param export Logical. If TRUE, the program will export files in the format
#' specified by \code{export_format} in the directory specified by \code{path_out}.
#' @param path_out Path for exporting files. If path not specified, files will
#' export to current working directory.
#' @param export_format Export format. Currently the options include \code{.csv},
#' \code{chemstation_csv} (utf-16 encoding), and \code{cdf}, unless you are
#' using OpenChrom parsers, where there are two additional options: \code{mzml},
#' and \code{animl}.
#' @param read_metadata Logical, whether to attach metadata (if it's available).
#' Defaults to TRUE.
#' @param progress_bar Logical. Whether to show progress bar. Defaults to
#' \code{TRUE} if \code{\link[pbapply]{pbapply}} is installed.
#' @param sample_names An optional character vector of sample names. Otherwise
#' sample names default to the basename of the specified files.
#' @param dat Existing list of chromatograms to append results.
#' (Defaults to NULL).
#' @return A list of chromatograms in \code{matrix} or \code{data.frame} format,
#' according to the value of \code{format_out}.
#' @section Side effects: If \code{export} is TRUE, chromatograms will be
#' exported in the format specified by \code{export_format} in the folder
#' specified by \code{path_out}. Currently, the only option for export is
#' \code{csv} unless the \code{parser} is \code{openchrom}.
#' @import reticulate
#' @importFrom utils write.csv file_test
#' @importFrom purrr partial
#' @examplesIf interactive()
#' path <- "tests/testthat/testdata/dad1.uv"
#' chr <- read_chroms(path, find_files = FALSE, format_in = "chemstation_uv")
#' @author Ethan Bass
#' @export read_chroms

read_chroms <- function(paths, find_files,
                        format_in=c("agilent_d", "chemstation", "chemstation_uv",
                                    "chemstation_csv", "chemstation_ch",
                                    "chemstation_fid", "masshunter_dad",
                                    "shimadzu_fid", "shimadzu_dad", "chromeleon_uv",
                                    "thermoraw", "mzml", "waters_arw", "waters_raw",
                                    "msd", "csd", "wsd", "mdf", "other"),
                        pattern = NULL,
                        parser = c("", "chromconverter", "aston", "entab",
                                   "thermoraw", "openchrom", "rainbow"),
                        format_out = c("matrix", "data.frame"),
                        data_format = c("wide","long"),
                        export = FALSE, path_out = NULL,
                        export_format = c("csv", "chemstation_csv", "cdf", "mzml", "animl"),
                        read_metadata = TRUE, progress_bar, sample_names = NULL,
                        dat = NULL){
  data_format <- match.arg(data_format, c("wide","long"))
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  parser <- match.arg(parser, c("", "chromconverter", "aston","entab",
                                          "thermoraw", "openchrom", "rainbow"))
  if (missing(progress_bar)){
    progress_bar <- check_for_pkg("pbapply", return_boolean = TRUE)
  }
  if (missing(find_files)){
    if (length(format_in) == 1){
      if (!(format_in %in% c("agilent_d", "waters_raw"))){
        ft <- all(file_test("-f", paths))
      } else {
        ext <- switch(format_in,
                          agilent_d = "\\.d",
                          waters_raw = "\\.raw")
        ft <- all(grepl(ext, paths, ignore.case = TRUE))
      }
      find_files <- !ft
    } else{
      find_files <- FALSE
    }
  }
  if (length(format_in) > 1){
    if (!find_files){
      format_in <- get_filetype(paths[1])
    } else{
        stop("Please specify the file format of your chromatograms by setting the `format_in` argument.")
    }
  }
  format_in <- match.arg(format_in, c("agilent_d", "chemstation", "chemstation_uv",
                                      "chemstation_ch", "chemstation_fid",
                                      "chemstation_csv", "masshunter_dad",
                                      "shimadzu_fid", "shimadzu_dad", "chromeleon_uv",
                                      "thermoraw", "mzml", "waters_arw",
                                      "waters_raw", "msd", "csd", "wsd", "mdf",
                                      "cdf", "other"))
  if (parser == ""){
    parser <- check_parser(format_in, find = TRUE)
  }
  export_format <- match.arg(export_format, choices =
                               c("csv", "chemstation_csv", "cdf", "mzml", "animl"))
  check_parser(format_in, parser)
  if (parser != "openchrom" && !(export_format %in% c("csv", "chemstation_csv", "cdf")))
    stop("The selected export format is currently only supported by `openchrom` parsers.")
  # if (export_format == "cdf" && format_in != "mdf" && parser != "openchrom")
    # stop("Currently CDF exports are only available for MDF files.")
  if (parser == "entab" & !requireNamespace("entab", quietly = TRUE)) {
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
      call. = FALSE)
  }
  exists <- dir.exists(paths) | file.exists(paths)
  if (all(!exists)){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  if (export | format_in == "thermoraw" | parser == "openchrom"){
    if (is.null(path_out)){
      path_out <- set_temp_directory()
    }
    if (!dir.exists(path_out)){
      stop(paste0("The export directory '", path_out, "' could not be found."))
    }
  }
  if (is.null(dat)){
    dat <- list()}

  # choose converter
  entab_parser <- partial(call_entab, format_in = format_in,
                          format_out = format_out,
                          data_format = data_format,
                          read_metadata = read_metadata)

  rainbow_parser <- partial(call_rainbow, format_in = format_in,
          format_out = format_out, data_format = data_format,
          read_metadata = read_metadata)

  if (format_in == "agilent_d"){
    pattern <- ifelse(is.null(pattern), ".D", pattern)
    converter <- rainbow_parser
  } else if (format_in == "masshunter_dad"){
    pattern <- ifelse(is.null(pattern), ".sp", pattern)
    converter <- switch(parser,
                        "aston" = partial(sp_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata),
                        "entab" = entab_parser)
  } else if (format_in == "chemstation_uv"){
    pattern <- ifelse(is.null(pattern), ".uv", pattern)
    converter <- switch(parser,
                        "chromconverter" = partial(read_chemstation_uv,
                                                   format_out = format_out,
                                                   data_format = data_format,
                                                   read_metadata = read_metadata),
                        "aston" = partial(uv_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata),
                        "entab" = entab_parser,
                        "rainbow" = rainbow_parser)
  } else if (format_in == "chemstation"){
    pattern <- ifelse(is.null(pattern), "*", pattern)
    converter <- rainbow_parser
    } else if (format_in == "chromeleon_uv"){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    converter <- partial(read_chromeleon, format_out = format_out,
                         data_format = data_format, read_metadata = read_metadata)
  } else if (format_in == "shimadzu_fid"){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    converter <- partial(read_shimadzu, format_in = "fid",
                         format_out = format_out, data_format = data_format,
                         read_metadata = read_metadata)
  } else if (format_in == "shimadzu_dad"){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    converter <- partial(read_shimadzu, format_in = "dad",
                         format_out = format_out, data_format = data_format,
                         read_metadata = read_metadata)
    } else if (format_in == "thermoraw"){
    pattern <- ifelse(is.null(pattern), ".raw", pattern)
    converter <- switch(parser,
                    "thermoraw" = partial(read_thermoraw, path_out = path_out,
                                          format_out = format_out,
                                          read_metadata = read_metadata),
                     "entab" = entab_parser)
  } else if (format_in == "mzml"){
    pattern <- ifelse(is.null(pattern), ".mzML", pattern)
    converter <- partial(read_mzml, format_out = format_out)
  } else if (format_in == "waters_arw"){
    pattern <- ifelse(is.null(pattern), ".arw", pattern)
    converter <- partial(read_waters_arw, format_out = format_out)
  } else if (format_in == "waters_raw"){
      pattern <- ifelse(is.null(pattern), ".raw", pattern)
      converter <- rainbow_parser
  } else if (format_in == "chemstation_csv"){
    pattern <- ifelse(is.null(pattern), ".csv|.CSV", pattern)
    converter <- partial(read_chemstation_csv, format_out = format_out)
  } else if (format_in %in% c("chemstation_fid", "chemstation_ch")){
    pattern <- ifelse(is.null(pattern), ".ch", pattern)
    converter <- switch(parser,
                        "chromconverter" = partial(read_chemstation_ch,
                                                   format_out = format_out,
                                                   data_format = data_format,
                                                   read_metadata = read_metadata),
                        "rainbow" = rainbow_parser,
                        "entab" = entab_parser)
  } else if (format_in %in% c("msd", "csd", "wsd")){
    if (is.null(pattern) & find_files){
      stop("Please supply `pattern` (e.g. a suffix) or set `find_files = FALSE`")
    }
    return_paths <- ifelse(export_format == "csv", FALSE, TRUE)
    converter <- partial(call_openchrom, path_out = path_out,
                         format_in = format_in, export_format = export_format,
                         return_paths = return_paths)
  } else if (format_in == "mdf"){
    pattern <- ifelse(is.null(pattern), ".mdf|.MDF", pattern)
    converter <- partial(read_mdf, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata)
  } else if (format_in == "cdf"){
      pattern <- ifelse(is.null(pattern), ".cdf|.CDF", pattern)
      converter <- partial(read_cdf, format_out = format_out,
                           data_format = data_format,
                           read_metadata = read_metadata)
  } else {
    converter <- switch(parser,
                        "aston" = partial(trace_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata),
                        "entab" = entab_parser
    )
  }

  if (find_files){
    files <- find_files(paths, pattern)
  } else {
    files <- paths
    if (!is.null(pattern)){
    match <- grep(pattern, files, ignore.case = TRUE)
    if (length(match) == 0){
      warning("The provided files do not match the expected file extension.
      Please confirm that the specified format ('format_in') is correct.",
              immediate. = TRUE)
    } else if (length(match) < length(files)){
      warning(paste("Some of the files do not have the expected file extension:",
                    files[match]), immediate. = TRUE)
    }
    }
  }
  if (all(grepl("\\.[Dd]$|\\.[Dd]?[/\\\\]",files))){
    file_names <- strsplit(files, "/")
    file_names <- gsub("\\.[Dd]", "",
                       sapply(file_names, function(n){
                         ifelse(any(grepl("\\.[Dd]", n)), grep("\\.[Dd]", n, value = TRUE), tail(n,1))
                       }))
  } else {
    file_names <- sapply(strsplit(basename(files),"\\."), function(x) x[1])
  }
  if (parser != "openchrom"){
    laplee <- choose_apply_fnc(progress_bar)
    data <- laplee(X = files, function(file){
      df <- try(converter(file), silent = TRUE)
    })
    errors <- which(sapply(data, function(x) inherits(x,"try-error")))
    if (length(errors) > 0){
      warning(data[errors], immediate. = TRUE)
      message(paste0("The following chromatograms could not be interpreted: ",
                    paste(errors, collapse = ", ")))
      data <- data[-errors]
      file_names <- file_names[-errors]
    }
  } else{
    data <- converter(files)
  }
  if (!is.null(sample_names)){
    names(data) <- sample_names
  } else{
    names(data) <- file_names
  }
  if (export & !(parser %in% c("thermoraw", "openchrom"))){
    writer <- switch(export_format, csv = export_csvs,
                     chemstation_csv = purrr::partial(export_csvs, fileEncoding = "utf16"),
                     cdf = export_cdfs)

    writer(data, path_out)
  }
  dat <- append(dat, data)
  dat
}
