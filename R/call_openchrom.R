#' Parse files with OpenChrom
#'
#' Writes `xml` batch-files and calls OpenChrom file parsers using a
#' system call to the command-line interface. To use this function
#' [OpenChrom](https://lablicate.com/platform/openchrom) must be manually installed.
#'
#' The \code{call_openchrom} works by creating an \code{xml} batchfile and
#' feeding it to the OpenChrom command-line interface. OpenChrom batchfiles
#' consist of \code{InputEntries} (the files you want to convert) and \code{
#' ProcessEntries} (what you want to do to the files). The parsers are organized
#' into broad categories by detector-type and output format. The detector-types
#' are \code{msd} (mass selective detectors), \code{csd} (current selective
#' detectors, such as FID, ECD, NPD), and \code{wsd} (wavelength selective
#' detectors, such as  DAD, and UV/VIS). Thus, when calling the OpenChrom parsers,
#' you must select one of these three options for the input format (\code{format_in}).
#'
#' **Note:** Turning on the OpenChrom command-line will deactivate the graphical
#' user interface (GUI). Thus, if you wish to continue using the OpenChrom GUI,
#' it is recommended to create a separate command-line version of OpenChrom to
#' call from R.

#' @import xml2
#' @import magrittr
#' @param files files to parse
#' @param path_out directory to export converted files.
#' @param format_in Either `msd` for mass spectrometry data, `csd` for flame ionization data, or `wsd` for DAD/UV data.
#' @param export_format Either \code{csv}, \code{cdf}, \code{mzml},  \code{animl}.
#' @param return_paths Logical. If TRUE, the function will return a character vector of paths to the newly created files.
#' @return If \code{return_paths} is TRUE, the function will return a vector of paths to the newly created files.
#' If \code{return_paths} is FALSE and \code{export_format} is \code{csv}, the function will return a list
#' of chromatograms in \code{data.frame} format. Otherwise, it will not return anything.
#' @section Side effects: Chromatograms will be exported in the format specified
#' by \code{export_format} in the folder specified by \code{path_out}.
#' @author Ethan Bass
#' @export call_openchrom

call_openchrom <- function(files, path_out, format_in,
                             export_format=c("csv", "cdf", "mzml", "animl"),
                             return_paths = FALSE){
  if (missing(format_in))
    stop("Format must be specified. The options are `msd` for mass spectrometry, `csd` for flame ionization (FID),
    or `wsd` for DAD/UV data.")
  export_format <- match.arg(export_format, c("csv", "cdf", "mzml", "cdf"))
  if (missing(path_out)){
    path_out <- set_temp_directory()
  }
  if(!file.exists(path_out)){
    stop("'path_out' not found. Make sure directory exists.")
  }
  openchrom_path <- configure_call_openchrom()
  path_template <- system.file("openchrom_template.xml", package = "chromConverter")
  x <- xml2::read_xml(x = path_template)
  # add files to InputEntries
  for (file in files){
  x %>% xml_children %>% .[[3]] %>%
    xml_add_child(.value = "InputEntry")  %>% xml_add_child(xml_cdata(file))
  }
  # add parser to ProcessEntries
  msd_mzml_converter<-'ProcessEntry id="msd.export.org.eclipse.chemclipse.msd.converter.supplier.mzml" name="mzML Chromatogram (*.mzML)" description="Reads mzML Chromatograms" jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  msd_netcdf_converter<-'ProcessEntry id="msd.export.net.openchrom.msd.converter.supplier.cdf" name="ANDI/AIA CDF Chromatogram (*.CDF)" description="Reads an writes ANDI/AIA CDF Chromatograms." jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  msd_animl_converter <- 'ProcessEntry id="msd.export.net.openchrom.msd.converter.supplier.animl.chromatogram" name="AnIML MSD Chromatogram (*.animl)" description="Reads and writes Analytical Information Markup Language Chromatograms" jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  msd_csv_converter <- 'ProcessEntry id="msd.export.org.eclipse.chemclipse.msd.converter.supplier.csv" name="CSV Chromatogram (*.csv)" description="Reads and Writes Chromatograms to CSV." jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  csd_csv_converter <- 'ProcessEntry id="csd.export.org.eclipse.chemclipse.csd.converter.supplier.csv" name="CSV Chromatogram (*.csv)" description="Reads and Writes Chromatograms to CSV." jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  csd_animl_converter <- 'ProcessEntry id="csd.export.net.openchrom.csd.converter.supplier.animl" name="AnIML FID Chromatogram (*.animl)" description="Writes AnIML Chromatograms." jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  wsd_animl_converter <- 'ProcessEntry id="wsd.export.net.openchrom.wsd.converter.supplier.animl.chromatogram" name="AnIML UV-Vis Chromatogram (*.animl)" description="Reads Analytical Information Markup Language Chromatograms" jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  wsd_csv_converter <- 'ProcessEntry id="wsd.export.org.eclipse.chemclipse.csd.converter.supplier.csv" name="CSV Chromatogram (*.csv)" description="Reads and Writes Chromatograms to CSV." jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  if (format_in == "msd"){
  parser <- switch(export_format,
                   "mzml" = msd_mzml_converter,
                   "cdf" = msd_netcdf_converter,
                   "animl" = msd_animl_converter,
                    "csv" = msd_csv_converter)
  } else if (format_in == "csd"){
    parser <- switch(export_format,
                     "csv" = csd_csv_converter,
                     "animl" = csd_animl_converter)
  } else if (format_in == "wsd"){
    parser <- switch(export_format,
                     "csv" = wsd_csv_converter,
                     "animl" = wsd_animl_converter)
  }
  x %>% xml_children %>% .[[4]] %>% xml_add_child(.value=gsub("path_out", path_out, parser))
  path_xml <- paste0(path_out, "batchfile_", strftime(Sys.time(),format = "%Y-%m-%d_%H-%M-%S"), ".xml")
  write_xml(x, file = path_xml)
  openchrom_path <- "/Applications/OpenChrom_CL.app/Contents/MacOS/openchrom"
  system(paste0(openchrom_path, " -nosplash -cli -batchfile ", path_xml))
  new_files <- paste0(path_out, sapply(strsplit(basename(files), "\\."), function(x) x[1]), ".", export_format)
  if (return_paths){
    new_files
  } else{
    if (export_format == "csv"){
      lapply(new_files, read.csv)
    }
  }
}

#' Configure OpenChrom parser
#'
#' @name configure_call_openchrom
#' @param cli Defaults to NULL. If "true", R will rewrite openchrom ini file to enable CLI.
#' If "false", R will disable CLI. If NULL, R will not modify the ini file.
#' @return No return value.
#' @author Ethan Bass
#' @noRd
configure_call_openchrom <- function(cli = c(NULL, "true", "false")){
  cli <- match.arg(cli, c(NULL, "true", "false"))
  path_parser <- readLines(system.file("shell/path_to_openchrom_commandline.txt", package = 'chromConverter'))
  if (!file.exists(path_parser)){
    warning("OpenChrom not found!", immediate. = TRUE)
    path_parser <- readline(prompt="Please provide path to `OpenChrom` command line):")
    writeLines(path_parser, con = system.file('shell/path_to_openchrom_commandline.txt', package='chromConverter'))
  }
  path_ini <- switch(.Platform$OS.type,
                     "unix" = paste0(gsub("MacOS/openchrom", "", path_parser), "Eclipse/openchrom.ini"),
                     "linux" = paste0(path_parser, ".uni"),
                     "windows" = paste0(gsub(".exe", "", path_parser), ".ini"))
  ini <- readLines(path_ini)
  cli_index <- grep("-Denable.cli.support",ini)
  ini_split <- strsplit(ini[cli_index],"=")[[1]]
  cli_tf <- ini_split[2]
  if(cli_tf == "false"){
    message("    The OpenChrom command-line interface is turned off!
    Update `openchrom.ini` to activate the command-line interface (y/n)?
    (Warning: This will deactivate the GUI on your OpenChrom installation!)")
    ans <- readline()
    if (ans %in% c("y","Y", "yes", "Yes", "YES")){
      cli <- "true"
    } else{
      stop("-Denable.cli.support must be enabled to use the OpenChrom parsers from R.")
    }
  }
  if (cli %in% c("true", "false")){
    ini_split[2] <- cli
    ini[cli_index] <- paste(ini_split, collapse="=")
    writeLines(ini, path_ini)
  }
  path_parser[1]
}
