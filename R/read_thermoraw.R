#' Read ThermoRaw
#'
#' Converts a 'Thermo' `.raw` file to `mzML` with the [ThermoRawFileParser](
#' https://github.com/compomics/ThermoRawFileParser) and reads the result with
#' [read_mzml].
#'
#' The ThermoRawFileParser must be installed separately.
#'
#' @name read_thermoraw
#' @inheritParams shared_params
#' @param path Path to 'Thermo' `.raw` file.
#' @param path_out Path to directory to export `mzML` files. If `path_out` isn't
#' specified, a temporary directory is used and deleted afterwards.
#' @param verbose Logical. Whether to print output from ThermoRawFileParser to
#' the console. Ignored on Windows, where the output is always printed.
#' @return A list of chromatograms, as returned by [read_mzml], in the format
#' specified by `format_out`.
#' @section Side effects: If `path_out` is specified, the `mzML` file (and,
#' if `read_metadata` is `TRUE`, a metadata `.txt` file) is left there.
#' @author Ethan Bass
#' @references
#' Hulstaert Niels, Jim Shofstahl, Timo Sachsenberg, Mathias Walzer,
#' Harald Barsnes, Lennart Martens, and Yasset Perez-Riverol.
#' ThermoRawFileParser: Modular, Scalable, and Cross-Platform RAW File Conversion.
#' *Journal of Proteome Research* **19**, no. 1 (January 3, 2020): 537–42.
#' \doi{10.1021/acs.jproteome.9b00328}.
#' @examples \dontrun{
#' read_thermoraw(path)
#' }
#' @family external parsers
#' @export read_thermoraw

read_thermoraw <- function(path, path_out = NULL,
                           format_out = c("matrix", "data.frame",
                                          "data.table"),
                           read_metadata = TRUE,
                           metadata_format = c("chromconverter", "raw"),
                           verbose = getOption("verbose")){
  format_out <- check_format_out(format_out)
  metadata_format <- check_metadata_format(metadata_format, "thermoraw")
  if(!file.exists(path)){
    stop("File not found. Check path.")
  }
  path <- fs::path_expand(path)
  if (is.null(path_out)){
    path_out <- temp_directory(path)
    on.exit(unlink(path_out, recursive = TRUE), add = TRUE)
  } else{
    path_out <- fs::path_expand(path_out)
  }
  if(!dir.exists(path_out)){
    stop("Export directory not found. Please check `path_out` argument and try again.")
  }
  parser_path <- configure_thermo_parser()
  verbose <- switch(as.character(verbose), "TRUE" = "")
  if (.Platform$OS.type != "windows"){
    io <- shQuote(c(path.expand(parser_path), paste0("-i=", path),
                    paste0("-o=", path_out)))
    system2("mono", args = c(io, "-a"), stdout = verbose)
    if (read_metadata){
      system2("mono", args = c(io, "-m=1"), stdout = verbose)
    }
  } else {
      shell(paste0(parser_path, " -i=", path,
                                " -o=", path_out, " -a"))
    if (read_metadata){
      shell(paste0(parser_path, " -i=", path,
                                " -o=", path_out, " -m=1"))
    }
  }
  base_name <- fs::path_ext_remove(basename(path))
  new_path <- fs::path(path_out, base_name, ext = "mzML")
  x <- read_mzml(new_path, format_out)
  if (read_metadata){
    meta_path <- fs::path(path_out, paste0(base_name, "-metadata"), ext = "txt")
    meta <- strsplit(readLines(meta_path), "=", fixed = TRUE)
    meta <- do.call(rbind, meta)
    rownames(meta) <- meta[,1]
    meta <- as.list(meta[,-1])
    # attach to the individual chromatograms as well as to the container:
    # `read_chroms` sorts on the container's `run_datetime`, while
    # `extract_metadata` and `print` see only the leaves.
    x <- lapply(x, attach_metadata, meta = meta, format_in = metadata_format,
                format_out = format_out, data_format = "long",
                source_file = path,
                source_file_format = "thermoraw")
    x <- attach_metadata(x, meta, format_in = metadata_format,
                         format_out = format_out, data_format = "long",
                         source_file = path,
                source_file_format = "thermoraw")
  }
  x
}

#' Find ThermoRawFileParser
#'
#' @name configure_thermo_parser
#' @param check Logical. If `TRUE`, report whether the parser exists without
#' asking for its path.
#' @return The path to ThermoRawFileParser or, if `check` is `TRUE`, whether
#' it exists.
#' @author Ethan Bass
#' @noRd

configure_thermo_parser <- function(check = FALSE){
  default <- switch(.Platform$OS.type,
                    windows = "ThermoRawFileParser\\ThermoRawFileParser.exe",
                    "~/software/ThermoRawFileParser/ThermoRawFileParser.exe")
  path_parser <- read_config_path("thermorawfileparser_path", default)
  exists <- file.exists(path_parser)
  if (!exists & !check){
    warning("ThermoRawFileParser not found!", immediate. = TRUE)
    path_parser <- readline(prompt = "Please provide path to `ThermoRawFileParser.exe`):")
    if (.Platform$OS.type == "windows"){
      path_parser <- gsub("/", "\\\\", path_parser)
    }
    if (!file.exists(path_parser)){
      stop("ThermoRawFileParser not found at '", path_parser, "'.", call. = FALSE)
    }
    write_config_path("thermorawfileparser_path", path_parser)
  }
  if (check) exists else path_parser
}
