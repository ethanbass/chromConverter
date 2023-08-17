#' Read ThermoRaw files into R using ThermoRawFileParser
#'
#' Converts ThermoRawFiles to mzmL by calling the ThermoRawFileParser from the
#' command-line.
#'
#' To use this function, the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser)
#' must be manually installed.
#'
#' @name read_thermoraw
#' @param path_in Path to file.
#' @param path_out Path to directory to export \code{mzML} files. If
#' \code{path_out} isn't specified, a temp directory will be used.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param read_metadata Whether to read metadata from file.
#' @return A chromatogram in the format specified by \code{format_out}.
#' @section Side effects: Exports chromatograms in \code{mzml format} to the
#' folder specified by \code{path_out}.
#' @author Ethan Bass
#' @examples \dontrun{
#' read_thermoraw(path)
#' }
#' @references
#' Hulstaert Niels, Jim Shofstahl, Timo Sachsenberg, Mathias Walzer,
#' Harald Barsnes, Lennart Martens, and Yasset Perez-Riverol.
#' ThermoRawFileParser: Modular, Scalable, and Cross-Platform RAW File Conversion.
#' \emph{Journal of Proteome Research} \bold{19}, no. 1 (January 3, 2020): 537–42.
#' \doi{10.1021/acs.jproteome.9b00328}.
#' @export read_thermoraw

read_thermoraw <- function(path_in, path_out = NULL, format_out = c("matrix", "data.frame"),
                           read_metadata = TRUE){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  if(!file.exists(path_in)){
    stop("File not found. Check path.")
  }
  base_name <- basename(path_in)
  base_name <- strsplit(base_name, "\\.")[[1]][1]
  if (is.null(path_out)){
    path_out <- tempdir()
  }
  if(!dir.exists(path_out)){
    stop("Export directory not found. Please check `path_out` argument and try again.")
  }
  configure_thermo_parser()
  if (.Platform$OS.type != "windows"){
    system2("sh", args=paste0(system.file('shell/thermofileparser.sh', package='chromConverter'),
                              " -i=", path_in, " -o=", path_out, " -a"))
    if (read_metadata){
      system2("sh", args = paste0(system.file('shell/thermofileparser.sh', package='chromConverter'),
                                  " -i=", path_in, " -o=", path_out, " -m=1"))
    }
  } else {
    parser_path <- readLines(system.file('shell/path_parser.txt', package='chromConverter'))
    shell(paste0(parser_path, " -i=", path_in,
                              " -o=", path_out, " -a"))
    if (read_metadata){
      shell(paste0(parser_path, " -i=", path_in,
                                " -o=", path_out, " -m=1"))
    }
  }
  new_path <- fs::path(path_out, base_name, ext = "mzML")
  x <- read_mzml(new_path, format_out)
  if (read_metadata){
    meta_path <- fs::path(path_out, paste0(base_name, "-metadata"), ext = "txt")
    meta <- strsplit(readLines(meta_path), "=",fixed = TRUE)
    meta <- do.call(rbind,meta)
    rownames(meta) <- meta[,1]
    meta <- as.list(meta[,-1])
    x <- structure(x, instrument = c(meta$`Instrument model`, meta$`Instrument name`, meta$`Instrument serial number`),
                    detector = NA,
                    software = meta$`Software version`,
                    method = NA,
                    batch = NA,
                    operator = NA,
                    run_date = meta$`Creation date`,
                    sample_name = basename(meta$`RAW file path`),
                    sample_id = meta$`Sample id`,
                    vial = meta$`Sample vial`,
                    injection_volume = meta$`Sample injection volume`,
                    sample_dilution = meta$`Sample dilution factor`,
                    time_range = meta$`Time range`,
                    time_interval = meta$`Interval(msec)`,
                    format = "long",
                    parser = "chromConverter",
                    class = format_out)
  }
  x
}

#' Configure shell script to call ThermoRawFileParser
#'
#' @name configure_thermo_parser
#' @param reconfigure Whether to re-write the shell script. (Defaults to FALSE,
#' unless the program finds that reconfiguration is needed).
#' @return No return value.
#' @author Ethan Bass
#' @noRd
configure_thermo_parser <- function(reconfigure = FALSE, check = FALSE){
  if (.Platform$OS.type == "windows"){
    path_parser <- readLines(system.file("shell/path_parser.txt", package = 'chromConverter'))
    exists <- file.exists(path_parser)
    if (!exists & !check){
      warning("ThermoRawFileParser not found!", immediate. = TRUE)
      path_parser <- readline(prompt="Please provide path to `ThermoRawFileParser.exe`):")
      path_parser <- gsub("/", "\\\\", path_parser)
      writeLines(path_parser, con=system.file('shell/path_parser.txt', package='chromConverter'))
    }
  } else {
    shell_script <- readLines(system.file('shell/thermofileparser.sh', package='chromConverter'))
    path_parser <- strsplit(shell_script[2]," ")[[1]]
    path_parser <- path_parser[grep(".exe", path_parser)]
    exists <- file.exists(path_parser)
    if (!exists & !check){
      warning("ThermoRawFileParser not found!", immediate. = TRUE)
      path_parser <- readline(prompt="Please provide path to `ThermoRawFileParser.exe`):")
      shell_script[2] <- paste0("mono ", path_parser, ' "$@"')
      writeLines(shell_script, con = system.file('shell/thermofileparser.sh', package='chromConverter'))
    }
  }
  if (check){
    exists
  }
}
