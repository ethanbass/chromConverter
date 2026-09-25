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
#' Provides a unified interface to all chromConverter parsers. The formats it
#' recognizes are listed under the `format_in` argument. It also wraps the
#' 'OpenChrom' parsers, which cover many additional formats but require
#' 'OpenChrom' 1.4 or earlier (see [call_openchrom]). The 'Entab',
#' 'ThermoRawFileParser' and 'OpenChrom' parsers must be installed separately;
#' see the instructions in the
#' [README](https://ethanbass.github.io/chromConverter/).
#'
#' If paths to individual files are provided, `read_chroms` infers the file
#' format from the first file and selects a parser for it. When providing
#' paths to directories, the file format must be specified using the
#' `format_in` argument.
#'
#' @name read_chroms
#' @param paths Paths to data files or directories containing the files.
#' @eval format_in_doc()
#' @param parser What parser to use (optional). Current options are
#' `chromconverter`, `aston`, `entab`, `thermoraw`, `openchrom`, `rainbow`.
#' @param find_files Logical. Whether to treat `paths` as directories to
#' search for data files. Inferred from `paths` if not supplied: anything that
#' is not a file is searched as a directory, except for the formats that are
#' themselves directories (e.g. 'Agilent' `.d`), which are recognized by their
#' extension.
#' @param pattern Regular expression that file names must match (e.g. a file
#' extension). Defaults to `NULL`, in which case the extension is deduced from
#' `format_in`.
#' @param format_out Class of output. Either `matrix`, `data.frame`, or
#' `data.table`.
#' @param data_format Whether to output data in wide or long format. Either
#' `wide` (default) or `long`.
#' @param path_out Path for exporting files. If it is not specified, the user
#' is asked whether to export to a `temp` directory in the working directory.
#' A directory that does not exist is created after asking.
#' @param export_format Export format: `csv`, `chemstation_csv` (UTF-16
#' encoding), `cdf`, `mzml`, `arw`, or `animl`, which requires an `openchrom`
#' parser.
#' @param force Logical. Whether to overwrite files when exporting. Defaults to
#' `FALSE`.
#' @param read_metadata Logical, whether to attach metadata (if it's available).
#' Defaults to `TRUE`.
#' @param metadata_format Format to output metadata. Either `chromconverter`
#' or `raw`.
#' @param progress_bar Logical. Whether to show progress bar. Defaults to `TRUE`
#' if `pbapply` is installed.
#' @param cl Argument to [pbapply][pbapply::pbapply] specifying the number
#' of parallel workers to use or a cluster object created by
#' [makeCluster][parallel::makeCluster]. Defaults to `1`.
#' @param verbose Logical. Whether to print status messages, and the output of
#' external parsers, to the R console.
#' @param sample_names Which sample names to use. Options are `basename` to
#' use the filename (default) or `sample_name` to use the sample
#' name encoded in the file metadata. A sample with no `sample_name`, or with
#' conflicting ones, is named for its file with a warning.
#' @param sort_by How to sort the chromatograms. Either `none` (default), which
#' keeps them in the order of `paths`, with files found in a directory in
#' alphabetical order; `acquisition_time`, which sorts by the `run_datetime`
#' attribute, oldest first, placing chromatograms without one last with a
#' warning (requires `read_metadata = TRUE`); or `file_time`, which sorts the
#' files by modification time before reading, oldest first.
#' @param dat Deprecated. Existing list of chromatograms to append results
#' to. Use `c()` on the returned `chrom_list` objects instead. Defaults to `NULL`.
#' @param ... Additional arguments to the parser. Where the parser does not
#' take `...`, arguments it does not accept are dropped with a warning.
#' @return A `chrom_list` of chromatograms in `matrix`, `data.frame`, or
#' `data.table` format, according to the value of `format_out`. Chromatograms
#' may be returned in either `wide` or `long` format according to the value of
#' `data_format`.
#' @section Side effects: If `export_format` is provided, chromatograms are
#' written to the folder given by `path_out` in that format. The options are
#' `csv`, `chemstation_csv`, `cdf`, `mzml` and `arw`, as well as `animl`
#' (AnIML) when an `openchrom` parser is selected. Files are also
#' written to `path_out` whenever the `thermoraw` or `openchrom` parser is
#' used, as these parsers convert the files before reading them: `thermoraw`
#' to mzML, and `openchrom` to `export_format` (`mzml` by default).
#' @import reticulate
#' @importFrom utils write.csv file_test
#' @importFrom purrr partial
#' @examples
#' path <- system.file("extdata/ladder.txt", package = "chromConverter")
#' chroms <- read_chroms(path, format_in = "shimadzu_ascii",
#'                       find_files = FALSE, progress_bar = FALSE)
#' @author Ethan Bass
#' @export read_chroms

read_chroms <- function(paths,
                        format_in = supported_formats(),
                        find_files,
                        pattern = NULL,
                        parser = c("", "chromconverter", "aston", "entab",
                                   "thermoraw", "openchrom", "rainbow"),
                        format_out = c("matrix", "data.frame", "data.table"),
                        data_format = c("wide", "long"),
                        path_out = NULL,
                        export_format = c("", "csv", "chemstation_csv", "cdf",
                                          "mzml", "animl", "arw"),
                        force = FALSE,
                        read_metadata = TRUE,
                        metadata_format = c("chromconverter", "raw"),
                        progress_bar, cl = 1,
                        verbose = getOption("verbose"),
                        sample_names = c("basename", "sample_name"),
                        sort_by = c("none", "acquisition_time", "file_time"),
                        dat = NULL, ...){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  parser <- match.arg(tolower(parser), c("", "chromconverter", "aston","entab",
                                          "thermoraw", "openchrom", "rainbow"))
  metadata_format <- match.arg(tolower(metadata_format),
                               c("chromconverter", "raw"))
  sample_names <- match.arg(sample_names, c("basename", "sample_name"))
  sort_by <- match.arg(sort_by, c("none", "acquisition_time", "file_time"))
  if (!is.null(dat)){
    warning("The `dat` argument is deprecated and will be removed in a future ",
            "version. Use `c()` on the returned `chrom_list` objects instead, ",
            "e.g. `c(old_dat, read_chroms(...))`.", immediate. = TRUE)
  }
  if (missing(progress_bar)){
    progress_bar <- check_for_pkg("pbapply", return_boolean = TRUE)
  }
  search_dirs <- if (missing(find_files)){
    infer_search_dirs(paths, format_in)
  } else find_files
  if (length(format_in) > 1){
    if (!search_dirs){
      format_in <- get_filetype(paths[1])
    } else{
        stop("Files could not be identified. Please specify a file format using
             the `format_in` argument.")
    }
  }
  format_in <- match.arg(tolower(format_in), supported_formats())
  format_in <- canonical_format(format_in)
  if (parser == ""){
    parser <- check_parser(format_in, find = TRUE)
    if (is.na(parser)) stop(sprintf(
      "Parser could not be identified for format %s", format_in))
  } else{
    check_parser(format_in, parser)
  }

  export_format <- match.arg(tolower(export_format),
                             choices = c("", "csv", "chemstation_csv",
                                          "cdf", "mzml", "animl", "arw"))
  if (export_format == "" && parser == "openchrom"){
    export_format <- "mzml"
  }
  if (parser != "openchrom" && export_format == "animl")
    stop("The selected export format is currently only supported by `openchrom`
         parsers.")

  paths_exist <- dir.exists(paths) | file.exists(paths)
  if (all(!paths_exist)){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  export <- export_format != "" || parser == "openchrom" ||
    format_in == "thermoraw"
  if (export){
    if (is.null(path_out)){
      path_out <- set_temp_directory()
    }
    path_out <- fs::path_expand(path_out)
    if (!dir.exists(path_out)){
        ans <- readline("Export directory not found. Create directory (y/n)?")
        if (ans %in% c("y", "Y", "yes", "Yes", "YES")){
          fs::dir_create(path_out)
        } else
          stop(sprintf("The export directory '%s' could not be found.", path_out))
    }
  }
  if (is.null(dat)){
    dat <- list()
  }
  if (format_is_batch(format_in) && is.null(pattern) && search_dirs){
    stop("Please supply `pattern` (e.g. a suffix) or set `find_files = FALSE`")
  }

  opts <- list(format_out = format_out, data_format = data_format,
               read_metadata = read_metadata, metadata_format = metadata_format,
               path_out = path_out, sample_names = sample_names,
               verbose = verbose)
  opts <- switch(parser,
                 entab = c(opts, list(format_in = format_in)),
                 rainbow = c(opts, list(format_in = format_in)),
                 openchrom = c(opts,
                               list(format_in = format_in,
                                    export_format = export_format,
                                    return_paths = export_format == "animl")),
                 opts)
  converter <- build_converter(format_in, parser, opts = opts, dots = list(...))

  if (is.null(pattern)){
    pattern <- format_to_extension(format_in)
  }
  files <- collect_files(paths, pattern, search_dirs = search_dirs,
                         dirs = format_is_dir(format_in))
  if (sort_by == "file_time"){
    files <- files[order(fs::file_info(files)$modification_time)]
  }
  file_names <- extract_filenames(files)
  if (verbose)
    message(sprintf("Reading %d %s files", length(files), sQuote(format_in)))

  if (format_is_batch(format_in)){
    data <- converter(files)
  } else{
    data <- read_files(files, converter, progress_bar = progress_bar, cl = cl,
                       verbose = verbose)
    errors <- which(vapply(data, inherits, logical(1), "try-error"))
    if (length(errors) > 0){
      warning(paste0(unlist(data[errors]), collapse = ""),
              "The following chromatograms could not be interpreted: ",
              paste(sQuote(file_names[errors]), collapse = ", "),
              immediate. = TRUE)
      data <- data[-errors]
      file_names <- file_names[-errors]
    }
  }
  if (format_in == "agilent_rslt"){
    data <- do.call(`c`, data)
  } else if (sample_names == "basename"){
    names(data) <- file_names
  } else if (sample_names == "sample_name"){
    names(data) <- name_by_sample_name(data, file_names)
  }
  if (anyDuplicated(names(data))){
    duplicated_names <- unique(names(data)[duplicated(names(data))])
    warning("The following names are duplicated: ",
            paste(sQuote(duplicated_names), collapse = ", "),
            ". This may interfere with downstream analyses.", immediate. = TRUE)
  }
  if (sort_by == "acquisition_time"){
    if (!read_metadata){
      warning("`sort_by = \"acquisition_time\"` requires `read_metadata = TRUE`; skipping sort.",
              immediate. = TRUE)
    } else {
      data <- sort_chroms_by_time(data)
    }
  }
  if (export & !(parser %in% c("thermoraw", "openchrom"))){
    writer <- get_exporter(export_format, force = force,
                           show_progress = progress_bar, verbose = verbose)
    if (verbose){
      message(sprintf("Writing to %s...", toupper(export_format)))
    }
    writer(data, path_out = path_out)
  }
  dat <- append(dat, data)
  class(dat) <- c("chrom_list", "list")
  dat
}

#' Infer whether the supplied paths are directories to search
#'
#' Called when `find_files` is not supplied. Most formats are single files, so
#' anything that is not a file is taken to be a directory to search. The
#' formats that are themselves directories (e.g. 'Agilent' `.d`) are instead
#' recognized by their extension, which is matched unanchored so that a
#' trailing separator does not defeat it.
#' @noRd
infer_search_dirs <- function(paths, format_in){
  if (length(format_in) != 1) return(FALSE)
  # `format_in` has not been through `match.arg` yet, so it may be unknown, in
  # which case `format_lookup` returns `NULL` and the file test is used.
  entry <- format_lookup(format_in)
  if (isTRUE(entry$dir)){
    pattern <- gsub("\\$", "", entry$ext)
    ft <- all(grepl(pattern, paths, ignore.case = TRUE))
  } else {
    ft <- all(file_test("-f", paths))
  }
  !ft
}

#' Collect the files to read
#'
#' Either searches the supplied directories for files matching `pattern`, or
#' takes the supplied paths as they are, warning if they do not look like the
#' expected format.
#' @noRd
collect_files <- function(paths, pattern, search_dirs, dirs = FALSE){
  if (search_dirs){
    return(find_files(paths, pattern, dirs = dirs))
  }
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
  files
}

#' Apply a converter to each file
#'
#' Errors are collected rather than thrown, so that one unreadable file does
#' not discard a whole batch.
#' @noRd
read_files <- function(files, converter, progress_bar, cl = 1,
                       verbose = FALSE){
  laplee <- choose_apply_fnc(progress_bar, cl = cl)
  laplee(X = files, function(file){
    if (verbose){
      message(sprintf("Reading %s", basename(file)))
    }
    try(converter(file), silent = TRUE)
  })
}

#' Name each sample after its `sample_name` attribute
#'
#' A sample may be a single chromatogram or a (possibly nested) list of them,
#' each trace carrying its own copy of the sample-level metadata, so the name
#' is resolved with `sample_attr_values` rather than read straight off the
#' element. The traces making up a sample should all give the same name; if
#' they disagree there is no basis for preferring one over another, so the name
#' is treated as unknown. Samples with no usable name fall back to the file
#' name, which is what `sample_names = "basename"` would have given.
#' @noRd
name_by_sample_name <- function(data, file_names){
  vals <- lapply(data, function(x){
    unique(as.character(unlist(sample_attr_values(x, "sample_name"))))
  })
  nms <- vapply(vals, function(v) if (length(v) == 1) v else NA_character_,
                character(1))
  conflicting <- which(lengths(vals) > 1)
  if (length(conflicting) > 0){
    warning(sprintf(paste0("Conflicting `sample_name` attributes for %s. ",
                           "Using the file name instead."),
                    paste(sQuote(file_names[conflicting]), collapse = ", ")),
            immediate. = TRUE)
  }
  unnamed <- which(lengths(vals) == 0)
  if (length(unnamed) > 0){
    warning(sprintf(paste0("`sample_name` could not be determined for %s. ",
                           "Using the file name instead."),
                    paste(sQuote(file_names[unnamed]), collapse = ", ")),
            immediate. = TRUE)
  }
  nms[is.na(nms)] <- file_names[is.na(nms)]
  unname(nms)
}

#' Sort a list of chromatograms by acquisition time
#'
#' A sample may be a single chromatogram or a (possibly nested) list of them,
#' so the timestamp is resolved with `get_sample_attr` rather than read
#' straight off the element.
#'
#' Samples with no usable `run_datetime` keep their relative order and are
#' placed last, so that one unreadable file does not discard the ordering for a
#' whole batch. `order` is stable for numeric input.
#' @noRd
sort_chroms_by_time <- function(data){
  vals <- vapply(data, function(x){
    val <- get_sample_attr(x, "run_datetime")
    if (is.null(val)) return(NA_real_)
    suppressWarnings(as.numeric(val)) # non-coercible becomes NA, i.e. missing
  }, numeric(1))
  if (anyNA(vals)){
    labs <- names(data)
    if (is.null(labs)) labs <- seq_along(data)
    warning(sprintf(paste0("`run_datetime` could not be determined for %s. ",
                           "These chromatograms are placed last."),
                    paste(sQuote(labs[is.na(vals)]), collapse = ", ")),
            immediate. = TRUE)
  }
  if (all(is.na(vals))) return(data)
  data[order(vals, na.last = TRUE)]
}
