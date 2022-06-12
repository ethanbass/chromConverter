#' Read ThermoRaw files into R using ThermoRawFileParser
#'
#' Converts ThermoRawFiles to mzmL by calling the ThermoRawFileParser from the
#' command-line.
#'
#' To use this function, the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser)
#' must be manually installed.
#'
#' @name read_thermoraw
#' @param path_in path to file
#' @param path_out directory to export \code{mzML} files.
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
#' “=ThermoRawFileParser: Modular, Scalable, and Cross-Platform RAW File Conversion.”
#' \emph{Journal of Proteome Research} \bold{19}, no. 1 (January 3, 2020): 537–42.
#' \doi{10.1021/acs.jproteome.9b00328}.
#' @export read_thermoraw

read_thermoraw <- function(path_in, path_out, format_out = c("matrix", "data.frame"),
                           read_metadata=TRUE){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  if(!file.exists(path_in)){
    stop("File not found. Check path.")
  }
  base <- basename(path_in)
  if (missing(path_out)){
    path_out <- set_temp_directory()
  }
  if(!file.exists(path_out)){
    stop("'path_out' not found. Make sure directory exists.")
  }
  configure_thermo_parser()
  if (.Platform$OS.type != "windows"){
    system2("sh", args=paste0(system.file('shell/thermofileparser.sh', package='chromConverter'), " -i=", path_in,
                              " -o=", path_out, " -a"))
    new_path <- paste0(path_out, strsplit(base,"\\.")[[1]][1],".mzML")
    if (read_metadata){
      system2("sh", args=paste0(system.file('shell/thermofileparser.sh', package='chromConverter'), " -i=", path_in,
                                " -o=", path_out, " -m=1"))
      meta_path <- paste0(path_out, strsplit(base, "\\.")[[1]][1], "-metadata.txt")
    }
  } else {
    parser_path <- readLines(system.file('shell/path_parser.txt', package='chromConverter'))
    shell(paste0(parser_path, " -i=", path_in,
                              " -o=", path_out, " -a"))
    new_path <- paste(path_out,
                      paste0(strsplit(base,"\\.")[[1]][1],".mzML"),
                      sep="\\")
    if (read_metadata){
      shell(paste0(parser_path, " -i=", path_in,
                                " -o=", path_out, " -m=1"))
      meta_path <- paste(path_out,
                         paste0(strsplit(base, "\\.")[[1]][1], "-metadata.txt"),
                         sep = "\\")
    }
  }
  x <- read_mzml(new_path, format_out)
  if (read_metadata){
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

#' Extract UV data from mzML files
#'
#' Extracts UV data from mzML files
#'
#' @name read_mzml
#' @param path path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @return A chromatograms in \code{matrix} format.
#' @author Ethan Bass
#' @export read_mzml
read_mzml <- function(path, format_out = c("matrix", "data.frame")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  if (!requireNamespace("mzR", quietly = TRUE)) {
    stop(
      "The `mzR` package must be installed from Bioconductor to read `mzML` files:
      BiocManager::install('mzR')",
      call. = FALSE)
  }
  x <- mzR::openMSfile(path)
  info <- mzR::header(x)
  UV_scans <- which(info$msLevel==0)
  rts <- info[UV_scans,"retentionTime"]
  lambdas <- seq(info$scanWindowLowerLimit[UV_scans[1]], info$scanWindowUpperLimit[UV_scans[1]])
  pks <- mzR::peaks(x)
  data <- t(sapply(UV_scans, function(j) pks[[j]][,2]))
  rownames(data) <- rts
  colnames(data) <- lambdas
  if (format_out == "data.frame"){
    as.data.frame(data)
  }
  data
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
      # reconfigure <- TRUE
      writeLines(path_parser, con=system.file('shell/path_parser.txt', package='chromConverter'))
    }
  } else{
    shell_script <- readLines(system.file('shell/thermofileparser.sh', package='chromConverter'))
    path_parser <- strsplit(shell_script[2]," ")[[1]]
    path_parser <- path_parser[grep(".exe",path_parser)]
    exists <- file.exists(path_parser)
    if (!exists & !check){
      warning("ThermoRawFileParser not found!", immediate. = TRUE)
      path_parser <- readline(prompt="Please provide path to `ThermoRawFileParser.exe`):")
      # arg1 <- "mono "
      shell_script[2] <- paste0("mono ", path_parser, ' "$@"')
      writeLines(shell_script, con = system.file('shell/thermofileparser.sh', package='chromConverter'))
    }
  }
  if (check){
    exists
  }
}
