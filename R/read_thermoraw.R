#' Read ThermoRaw files into R using ThermoRawFileParser
#'
#' Converters ThermoRawFiles to mzmL by calling the ThermoRawFileParser from the
#' command-line.
#'
#' To use this function, the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser)
#' must be manually installed.
#'
#' @name read_thermoraw
#' @param path_in path to file
#' @param path_out directory to export \code{mzML} files.
#' @return A chromatograms in \code{matrix} format.
#' @author Ethan Bass
#' @examples \dontrun{
#' read_thermoraw(path)
#' }
#' @references
#' Hulstaert Niels, Jim Shofstahl, Timo Sachsenberg, Mathias Walzer,
#' Harald Barsnes, Lennart Martens, and Yasset Perez-Riverol.
#' “ThermoRawFileParser: Modular, Scalable, and Cross-Platform RAW File Conversion.”
#' \emph{Journal of Proteome Research} \bold{19}, no. 1 (January 3, 2020): 537–42.
#' \doi{10.1021/acs.jproteome.9b00328}.
#' @export read_thermoraw

read_thermoraw <- function(path_in, path_out){
  if (missing(path_out)){
    warning("setting path for exported files to `temp` folder in current working directory. To choose a different directory,
            set the `path_out` argument", immediate. = TRUE)
    path_out  <- getwd()
    if (!dir.exists("temp"))
      dir.create("temp")
    path_out <- paste0(path_out,'/temp/')
  }
  if(!file.exists(path_in)){
    stop("File not found. Check path.")
  }
  if(!file.exists(path_out)){
    stop("'path_out' not found. Make sure directory exists.")
  }
  configure_shell_script()
  # system(paste0("sh ", system.file('shell/thermofileparser.sh', package='chromConverter'), " -i=", path_in,
  #               " -o=", path_out, " -a"))
  system2("sh", args=paste0(system.file('shell/thermofileparser.sh', package='chromConverter'), " -i=", path_in,
                            " -o=", path_out, " -a"))
  base <- basename(path_in)
  path <- paste0(path_out, strsplit(base,"\\.")[[1]][1],".mzML")
  read_mzml(path)
}

#' Extract UV data from mzML files
#'
#' Extracts UV data from mzML files
#'
#' @name read_mzml
#' @param path path to file
#' @return A chromatograms in \code{matrix} format.
#' @author Ethan Bass
#' @export read_mzml
read_mzml <- function(path){
  if (!requireNamespace("mzR", quietly = TRUE)) {
    stop(
      "The `mzR` package must be installed from Bioconductor to read `mzML` files:
      BiocManager::install('mzR')",
      call. = FALSE)
  }
  x<-mzR::openMSfile(path)
  info<-mzR::header(x)
  UV_scans <- which(info$msLevel==0)
  rts <- info[UV_scans,"retentionTime"]
  lambdas <- seq(info$scanWindowLowerLimit[UV_scans[1]], info$scanWindowUpperLimit[UV_scans[1]])
  pks <- mzR::peaks(x)
  data <- t(sapply(UV_scans, function(j) pks[[j]][,2]))
  rownames(data) <- rts
  colnames(data) <- lambdas
  data
}

#' Configure shell script to call ThermoRawFileParser
#'
#' @name configure_shell_script
#' @param reconfigure Whether to re-write the shell script. (Defaults to FALSE,
#' unless the program finds that reconfiguration is needed).
#' @return No return value.
#' @author Ethan Bass
#' @noRd
configure_shell_script <- function(reconfigure = FALSE){
  shell_script <- readLines(system.file('shell/thermofileparser.sh', package='chromConverter'))
  path_parser <- strsplit(shell_script[2]," ")[[1]][2]
  if(!file.exists(path_parser)){
    warning("ThermoRawFileParser not found!", immediate. = TRUE)
    path_parser <- readline(prompt="Please provide path to `ThermoRawFileParser.exe`):")
    reconfigure <- TRUE
  }
  if (.Platform$OS.type == "windows"){
    if (length(grep("mono", shell_script)) != 0){
      arg1 <- ""
      reconfigure <- TRUE
      }
  } else{
    arg1 <- "mono "
  }
    if (reconfigure){
      shell_script[2] <- paste0(arg1, path_parser, ' "$@"')
      writeLines(shell_script, con=system.file('shell/thermofileparser.sh', package='chromConverter'))
    }
  }
