#' Read Chromatograms
#'
#' Reads chromatograms from specified folders or vector of paths using file parsers
#' from [Aston](https://github.com/bovee/aston), [Entab](https://github.com/bovee/entab),
#' [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser),
#' [OpenChrom](https://lablicate.com/platform/openchrom), and
#' [rainbow](https://rainbow-api.readthedocs.io/).
#'
#' Provides a general interface to chromConverter parsers. Currently recognizes
#' 'Agilent ChemStation' (.uv), 'MassHunter' (.dad) files, 'Thermo RAW',
#' 'Waters ARW' (.arw), 'Waters RAW' (.raw), 'Chromeleon ASCII' (.txt),
#' 'Shimadzu ASCII' (.txt). Also, wraps Openchrom parsers, which include many
#' additional formats. To use 'Entab', 'ThermoRawFileParser', or 'Openchrom'
#' parsers, they must be manually installed. Please see the instructions in the
#' [README](https://ethanbass.github.io/chromConverter/) for further details.
#'
#' @name read_chroms
#' @param paths paths to files or folders containing files
#' @param find_files Logical. Set to \code{TRUE} (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to\code{FALSE}.
#' @param format_in Format of files to be imported/converted. The current options
#' are: \code{chemstation_uv}, \code{chemstation}, \code{chemstation_csv},
#' \code{masshunter}, \code{masshunter_dad}, \code{shimadzu_fid}, \code{shimadzu_dad},
#' \code{chromeleon_uv}, \code{agilent_d}, \code{thermoraw}, \code{mzml},
#' \code{waters_arw}, \code{waters_raw}, \code{msd}, \code{csd}, \code{wsd},
#' or \code{other}.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format_in}.
#' @param parser What parser to use. Current option are \code{chromconverter},
#' \code{aston}, \code{entab}, \code{thermoraw}, \code{openchrom}, or \code{rainbow}.
#' @param format_out R object format (i.e. data.frame or matrix).
#' @param data_format Whether to output data in wide or long format. Either
#' \code{wide} or \code{long}.
#' @param export Logical. If TRUE, will export files as csvs.
#' @param path_out Path for exporting files. If path not specified, files will
#' export to current working directory.
#' @param export_format Export format. Currently the only option is \code{.csv},
#' unless you are using OpenChrom parsers, where you could have \code{csv},
#' \code{cdf}, \code{mzml}, or \code{animl}.
#' @param read_metadata Logical, whether to attach metadata (if it's available).
#' Defaults to TRUE.
#' @param dat Existing list of chromatograms to append results.
#' (Defaults to NULL).
#' @return A list of chromatograms in \code{matrix} or \code{data.frame} format,
#' according to the value of \code{format_out}.
#' @section Side effects: If \code{export} is TRUE, chromatograms will be
#' exported in the format specified by \code{export_format} in the folder specified
#' by \code{path_out}. Currently, the only option for export is \code{csv} unless
#' the \code{parser} is \code{openchrom}.
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
                                    "chemstation_csv", "masshunter_dad",
                                    "shimadzu_fid", "shimadzu_dad", "chromeleon_uv",
                                   "thermoraw", "mzml", "waters_arw", "waters_raw",
                                   "msd", "csd", "wsd", "other"),
                        pattern = NULL,
                        parser = c("", "chromconverter", "aston", "entab",
                                   "thermoraw", "openchrom", "rainbow"),
                        format_out = c("matrix", "data.frame"),
                        data_format = c("wide","long"),
                        export = FALSE, path_out = NULL,
                        export_format = c("csv", "cdf", "mzml", "animl"),
                        read_metadata = TRUE, dat = NULL){
  format_in <- match.arg(format_in, c("agilent_d", "chemstation", "chemstation_uv",
                                      "chemstation_csv", "masshunter_dad",
                                      "shimadzu_fid", "shimadzu_dad", "chromeleon_uv",
                                      "thermoraw", "mzml", "waters_arw",
                                      "waters_raw", "msd", "csd", "wsd", "other"))
  data_format <- match.arg(data_format, c("wide","long"))
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  parser <- match.arg(parser, c("", "chromconverter", "aston","entab",
                                          "thermoraw", "openchrom", "rainbow"))
  if (missing(find_files)){
    if (!(format_in %in% c("agilent_d", "waters_raw"))){
      ft <- all(file_test("-f", paths))
    } else{
      ext <- switch(format_in,
                        agilent_d = "\\.d",
                        waters_raw = "\\.raw")
      ft <- all(grepl(ext, paths, ignore.case = TRUE))
    }
    find_files <- !ft
  }
  if (parser == ""){
    parser <- check_parser(format_in, find = TRUE)
  }
  export_format <- match.arg(export_format, c("csv", "cdf", "mzml", "animl"))
  check_parser(format_in, parser)
  if (parser != "openchrom" & export_format != "csv")
    stop("Only `csv` format is currently supported for exporting files unless the parser is `openchrom`.")
  if (parser == "entab" & !requireNamespace("entab", quietly = TRUE)) {
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
      call. = FALSE)
  }
  exists <- dir.exists(paths) | file.exists(paths)
  if (all(!exists)){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  if (!is.null(path_out)){
    path_out <- check_path(path_out)
  }
  if (export | format_in == "thermoraw" | parser == "openchrom"){
    if (is.null(path_out)){
      path_out <- set_temp_directory()
    }
    if (!dir.exists(path_out)){
      stop(paste0("The export directory '", path_out, "' does not exist."))
    }
  }
  if (is.null(dat)){
    dat<-list()}

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
                        "aston" = partial(uv_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata),
                        "entab" = entab_parser,
                        "rainbow" = rainbow_parser)
  } else if (format_in == "chromeleon_uv"){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    converter <- partial(read_chromeleon, read_metadata = read_metadata, format_out = format_out)
  } else if (format_in == "shimadzu_fid"){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    converter <- partial(read_shimadzu, format_in = "fid",
                         read_metadata = read_metadata, format_out = format_out)
  } else if (format_in == "shimadzu_dad"){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    converter <- partial(read_shimadzu, format_in = "dad",
                         read_metadata = read_metadata, format_out = format_out)
    } else if (format_in == "thermoraw"){
    pattern <- ifelse(is.null(pattern), ".raw", pattern)
    converter <- switch(parser,
                    "thermoraw" = partial(read_thermoraw, path_out = path_out,
                                            read_metadata = read_metadata,
                                            format_out = format_out),
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
  } else if (format_in %in% c("msd", "csd", "wsd")){
    if (is.null(pattern) & find_files){
      stop("Please supply `pattern` (e.g. a suffix) or set `find_files = FALSE`")
    }
    return_paths <- ifelse(export_format == "csv", FALSE, TRUE)
    converter <- partial(call_openchrom, path_out = path_out,
                         format_in = format_in, export_format = export_format,
                         return_paths = return_paths)
  } else{
    converter <- switch(parser,
                        "aston" = partial(trace_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata),
                        "entab" = partial(call_entab,
                                          read_metadata = read_metadata,
                                          format_out = format_out)
    )
  }
  writer <- switch(export_format, "csv" = export_csvs)

  if (find_files){
    files <- find_files(paths, pattern)
  } else{
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

  if (format_in %in% c("chemstation_uv", "masshunter_dad")){
    file_names <- strsplit(files, "/")
    file_names <- gsub("\\.[Dd]", "",
                       sapply(file_names, function(n) n[grep("\\.[Dd]", n)]))
  } else {file_names <- sapply(strsplit(basename(files),"\\."), function(x) x[1])}
  if (parser != "openchrom"){
    data <- lapply(X=files, function(file){
      df <- try(converter(file), silent = TRUE)
    })
    errors <- which(sapply(data, function(x) inherits(x,"try-error")))
    if (length(errors) > 0){
      warning(data[errors], immediate. = TRUE)
      message(paste("The following chromatograms could not be interpreted:", errors))
      data <- data[-errors]
      file_names <- file_names[-errors]
    }
  } else{
    data <-converter(files)
  }
  names(data) <- file_names
  if (export & !(parser %in% c("thermoraw", "openchrom"))){
    writer(data, path_out)
  }
  dat <- append(dat, data)
  dat
}
