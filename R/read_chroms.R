#' Read Chromatograms
#'
#' Reads chromatograms from specified folders or vector of paths using either an
#' internal parser or bindings to an external library, such as
#' [Aston](https://github.com/bovee/aston),
#' [Entab](https://github.com/bovee/entab),
#' [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser),
#' [OpenChrom](https://lablicate.com/platform/openchrom),
#' [rainbow](https://rainbow-api.readthedocs.io/).
#'
#' Provides a unified interface to all chromConverter parsers. Currently recognizes
#' 'Agilent ChemStation' (\code{.uv}, \code{.ch}, \code{.dx}), 'Agilent
#' MassHunter' (\code{.dad}), 'Thermo RAW' (\code{.raw}), 'Waters ARW' (\code{.arw}),
#' 'Waters RAW' (\code{.raw}), 'Chromeleon ASCII' (\code{.txt}), 'Shimadzu ASCII'
#' (\code{.txt}), 'Shimadzu GCD', and 'Shimadzu LCD' files (preliminary support).
#' Also, wraps 'OpenChrom' parsers, which include many additional formats. To use 'Entab',
#' 'ThermoRawFileParser', or 'OpenChrom' parsers, they must be manually installed.
#' Please see the instructions in the
#' [README](https://ethanbass.github.io/chromConverter/) for further details.
#'
#' If paths to individual files are provided, \code{read_chroms} will try to
#' infer the file format and select an appropriate parser. However, when
#' providing paths to directories, the file format must be specified using the
#' \code{format_in} argument.
#'
#' @name read_chroms
#' @param paths Paths to data files or directories containing the files.
#' @param find_files Logical. Set to \code{TRUE} (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to\code{FALSE}.
#' @param format_in Format of files to be imported/converted. Current options
#' include: \code{agilent_d}, \code{agilent_dx}, \code{chemstation},
#' \code{chemstation_uv}, \code{chemstation_ch}, \code{chemstation_csv},
#' \code{masshunter}, \code{masshunter_dad}, \code{chromeleon_uv},
#' \code{shimadzu_ascii}, \code{shimadzu_fid}, \code{shimadzu_dad},
#' \code{thermoraw}, \code{waters_arw}, \code{waters_raw}, \code{mzml},
#' \code{mzxml}, \code{cdf}, \code{mdf}, \code{msd}, \code{csd}, \code{wsd},
#' or \code{other}.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format_in}.
#' @param parser What parser to use (optional). Current option are
#' \code{chromconverter}, \code{aston}, \code{entab}, \code{thermoraw},
#' \code{openchrom}, or \code{rainbow}.
#' @param format_out Class of output (i.e. data.frame or matrix).
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
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param progress_bar Logical. Whether to show progress bar. Defaults to
#' \code{TRUE} if \code{\link[pbapply]{pbapply}} is installed.
#' @param cl Argument to \code{\link[pbapply]{pbapply}} specifying the number
#' of clusters to use or a cluster object created by
#' \code{\link[parallel]{makeCluster}}. Defaults to 1.
#' @param verbose Logical. Whether to print output from external parsers to the
#' R console.
#' @param sample_names An optional character vector of sample names. Otherwise
#' sample names default to the \code{\link{basename}} of the specified files.
#' @param dat Existing list of chromatograms to append results.
#' (Defaults to NULL).
#' @param ... Additional arguments to parser.
#' @return A list of chromatograms in \code{matrix} or \code{data.frame} format,
#' according to the value of \code{format_out}. Chromatograms may be returned
#' in either \code{wide} or \code{long} format according to the value of
#' \code{data_format}.
#' @section Side effects: If \code{export} is TRUE, chromatograms will be
#' exported in the format specified by \code{export_format} in the folder
#' specified by \code{path_out}. Currently, the most versatile option for
#' exporting files is \code{csv}. However, unidimensional chromatograms can
#' also be exported in ANDI Chromatography (netCDF) format by selecting
#' \code{cdf}. If an \code{openchrom} parser is selected, ANIML and mzML are
#' available as additional options.
#' @import reticulate
#' @importFrom utils write.csv file_test
#' @importFrom purrr partial
#' @examplesIf interactive()
#' path <- "tests/testthat/testdata/dad1.uv"
#' chr <- read_chroms(path, find_files = FALSE, format_in = "chemstation_uv")
#' @author Ethan Bass
#' @export read_chroms

read_chroms <- function(paths, find_files,
                        format_in = c("agilent_d", "agilent_dx", "chemstation",
                                    "chemstation_fid", "chemstation_ch",
                                    "chemstation_csv", "chemstation_uv",
                                    "masshunter_dad", "chromeleon_uv",
                                    "mzml", "mzxml", "mdf",
                                    "shimadzu_ascii", "shimadzu_dad",
                                    "shimadzu_fid", "shimadzu_gcd",
                                    "shimadzu_lcd", "thermoraw",
                                    "varian_sms",
                                    "waters_arw", "waters_raw",
                                    "msd", "csd", "wsd", "other"),
                        pattern = NULL,
                        parser = c("", "chromconverter", "aston", "entab",
                                   "thermoraw", "openchrom", "rainbow"),
                        format_out = c("matrix", "data.frame"),
                        data_format = c("wide","long"),
                        export = FALSE, path_out = NULL,
                        export_format = c("csv", "chemstation_csv", "cdf",
                                          "mzml", "animl"),
                        read_metadata = TRUE,
                        metadata_format = c("chromconverter", "raw"),
                        progress_bar, cl = 1,
                        verbose = getOption("verbose"),
                        sample_names = NULL, dat = NULL, ...){
  data_format <- match.arg(data_format, c("wide","long"))
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  parser <- match.arg(tolower(parser), c("", "chromconverter", "aston","entab",
                                          "thermoraw", "openchrom", "rainbow"))
  metadata_format <- match.arg(tolower(metadata_format), c("chromconverter", "raw"))
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
  format_in <- match.arg(tolower(format_in), c("agilent_d", "agilent_dx", "chemstation",
                                      "chemstation_uv", "chemstation_ch",
                                      "chemstation_30", "chemstation_31",
                                      "chemstation_130", "chemstation_131",
                                      "openlab_131", "chemstation_179",
                                      "chemstation_81", "chemstation_181",
                                      "chemstation_fid", "chemstation_csv",
                                      "masshunter_dad", "shimadzu_ascii",
                                      "shimadzu_dad", "shimadzu_fid",
                                      "shimadzu_gcd", "shimadzu_lcd",
                                      "varian_sms",
                                      "chromeleon_uv", "thermoraw", "mzml", "mzxml",
                                      "waters_arw", "waters_raw", "msd", "csd",
                                      "wsd", "mdf", "cdf", "other"))
  if (parser == ""){
    parser <- check_parser(format_in, find = TRUE)
  }
  if (length(export_format) > 1 && parser == "openchrom"){
    export_format = "mzml"
  } else{
    export_format <- match.arg(export_format, choices =
                                 c("csv", "chemstation_csv",
                                   "cdf", "mzml", "animl"))
  }
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
  if (export){
    if (is.null(path_out)){
      path_out <- set_temp_directory()
    }
    if (!dir.exists(path_out)){
      stop(paste0("The export directory '", path_out, "' could not be found."))
    }
  }
  if (is.null(dat)){
    dat <- list()
  }

  entab_parser <- partial(call_entab, format_in = format_in,
                          format_out = format_out,
                          data_format = data_format,
                          read_metadata = read_metadata)

  rainbow_parser <- partial(call_rainbow, format_in = format_in,
                            format_out = format_out, data_format = data_format,
                            read_metadata = read_metadata, ...)

  if (format_in == "agilent_d"){
    converter <- rainbow_parser
  } else if (format_in == "agilent_dx"){
    converter <- partial(read_agilent_dx, path_out = path_out,
                         format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata)
  } else if (format_in == "masshunter_dad"){
    converter <- switch(parser,
                        "aston" = partial(sp_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata,
                                          metadata_format = metadata_format),
                        "entab" = entab_parser)
  } else if (format_in == "chemstation_uv" | grepl("31", format_in)){
    converter <- switch(parser,
                        "chromconverter" = partial(read_chemstation_uv,
                                                   format_out = format_out,
                                                   data_format = data_format,
                                                   read_metadata = read_metadata,
                                                   metadata_format = metadata_format),
                        "aston" = partial(uv_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata,
                                          metadata_format = metadata_format),
                        "entab" = entab_parser,
                        "rainbow" = rainbow_parser)
  } else if (format_in == "chromeleon_uv"){
    converter <- partial(read_chromeleon, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata,
                         metadata_format = metadata_format)
  } else if (format_in == "shimadzu_fid"){
    converter <- partial(read_shimadzu, include = "fid",
                         format_out = format_out, data_format = data_format,
                         read_metadata = read_metadata,
                         metadata_format = metadata_format, ...)
  } else if (format_in == "shimadzu_dad"){
    converter <- partial(read_shimadzu, include = "dad",
                         format_out = format_out, data_format = data_format,
                         read_metadata = read_metadata,
                         metadata_format = metadata_format, ...)
  }  else if (format_in == "shimadzu_ascii"){
    converter <- partial(read_shimadzu, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata,
                         metadata_format = metadata_format, ...)
  } else if (format_in == "shimadzu_gcd"){
    converter <- partial(read_shimadzu_gcd, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata,
                         metadata_format = metadata_format, ...)
  } else if (format_in == "shimadzu_lcd"){
    converter <- partial(read_shimadzu_lcd, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata, ...)
  } else if (format_in == "thermoraw"){
    converter <- switch(parser,
                        "thermoraw" = partial(read_thermoraw, path_out = path_out,
                                              format_out = format_out,
                                              read_metadata = read_metadata,
                                              metadata_format = metadata_format,
                                              verbose = verbose),
                        "entab" = entab_parser)
  } else if (format_in %in% c("mzml","mzxml")){
    converter <- partial(read_mzml, format_out = format_out, ...)
  } else if (format_in == "varian_sms"){
    converter <- partial(read_varian_sms, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata, ...)
  } else if (format_in == "waters_arw"){
    converter <- partial(read_waters_arw, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata)
  } else if (format_in == "waters_raw"){
    converter <- switch(parser, "rainbow" = rainbow_parser,
                        "chromconverter" = partial(read_waters_raw,
                                                   format_out = format_out,
                                                   data_format = data_format,
                                                   read_metadata = read_metadata,
                                                   metadata_format = metadata_format))
  } else if (format_in == "chemstation_csv"){
    converter <- partial(read_chemstation_csv, format_out = format_out)
  } else if (grepl("chemstation", format_in)){
    converter <- switch(parser,
                        "chromconverter" = partial(read_chemstation_ch,
                                                   format_out = format_out,
                                                   data_format = data_format,
                                                   read_metadata = read_metadata,
                                                   metadata_format = metadata_format,
                                                   ...),
                        "rainbow" = rainbow_parser,
                        "entab" = entab_parser)
  } else if (format_in %in% c("msd", "csd", "wsd")){
    if (is.null(pattern) & find_files){
      stop("Please supply `pattern` (e.g. a suffix) or set `find_files = FALSE`")
    }
    # return paths if animl is selected
    return_paths <- ifelse(export_format == "animl", TRUE, FALSE)
    converter <- partial(call_openchrom, path_out = path_out,
                         format_in = format_in, export_format = export_format,
                         return_paths = return_paths, verbose = verbose)
  } else if (format_in == "mdf"){
    converter <- partial(read_mdf, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata)
  } else if (format_in == "cdf"){
    converter <- partial(read_cdf, format_out = format_out,
                         data_format = data_format,
                         read_metadata = read_metadata, ...)
  } else {
    converter <- switch(parser,
                        "aston" = partial(trace_converter, format_out = format_out,
                                          data_format = data_format,
                                          read_metadata = read_metadata,
                                          metadata_format = metadata_format),
                        "entab" = entab_parser
    )
  }
  pattern <- ifelse(is.null(pattern), format_to_extension(format_in), pattern)
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
                         ifelse(any(grepl("\\.[Dd]", n)),
                                yes = grep("\\.[Dd]", n, value = TRUE),
                                no = tail(n,1))
                       }))
  } else {
    file_names <- sapply(strsplit(basename(files),"\\."), function(x) x[1])
  }
  if (parser != "openchrom"){
    laplee <- choose_apply_fnc(progress_bar, cl = cl)
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
    writer <- switch(export_format,
                     csv = export_csvs,
                     chemstation_csv = purrr::partial(export_csvs,
                                                      fileEncoding = "utf16"),
                     cdf = export_cdfs)
    writer(data, path_out)
  }
  dat <- append(dat, data)
  dat
}
