#' Parse files with OpenChrom
#'
#' Writes `xml` batch-files and calls OpenChrom file parsers using a
#' system call to the command-line interface. Unfortunately, the command-line
#' interface is no longer supported in newer versions of OpenChrom (starting with
#' version 1.5.0) and older versions of OpenChrom that do support
#' the command line interface are no longer available from Lablicate. Thus, this
#' function is deprecated since it will only work if you happen to have access
#' to OpenChrom version 1.4.0, which has been scrubbed from the internet.
#'
#' The \code{call_openchrom} function works by creating an \code{xml} batchfile
#' and feeding it to the OpenChrom command-line interface. OpenChrom batchfiles
#' consist of \code{InputEntries} (the files you want to convert) and
#' \code{ProcessEntries} (what you want to do to the files). The parsers are
#' organized into broad categories by detector-type and output format. The
#' detector-types are \code{msd} (mass selective detectors), \code{csd}
#' (current selective detectors, e.g., FID, ECD, NPD), and \code{wsd}
#' (wavelength selective detectors, e.g.,  DAD, and UV/VIS). Thus, when calling
#' the OpenChrom parsers, you must select one of these three options for the
#' input format (\code{format_in}).
#'
#' @note Activating the OpenChrom command-line will deactivate the graphical
#' user interface (GUI). Thus, if you wish to continue using the OpenChrom GUI,
#' it is recommended to create a separate command-line version of OpenChrom to
#' call from R.
#'
#' @import xml2
#' @param files Path to files.
#' @param path_out Directory to export converted files.
#' @param format_in Either `msd` for mass spectrometry data, `csd` for flame
#' ionization data, or `wsd` for DAD/UV data.
#' @param format_out R format. Either \code{matrix}, \code{data.frame} or
#' \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param export_format Either  \code{mzml}, \code{csv}, \code{cdf},  \code{animl}.
#' Defaults to \code{mzml}.
#' @param return_paths Logical. If TRUE, the function will return a character
#' vector of paths to the newly created files.
#' @param verbose Logical. Whether to print output from OpenChrom to the console.
#' @return If \code{return_paths} is \code{FALSE}, the function will return a
#' list of chromatograms (if an appropriate parser is available to import the
#' files into R). The chromatograms will be returned in \code{matrix} or
#' \code{data.frame} format according to the value of \code{format_out}. If
#' \code{return_paths} is \code{TRUE}, the function will return a character
#' vector of paths to the newly created files.
#' @section Side effects: Chromatograms will be exported in the format specified
#' by \code{export_format} in the folder specified by \code{path_out}.
#' @author Ethan Bass
#' @references
#' Wenig, Philip and Odermatt, Juergen. OpenChrom: A Cross-Platform Open Source
#' Software for the Mass Spectrometric Analysis of Chromatographic Data. \emph{
#' BMC Bioinformatics} \bold{11}, no. 1 (July 30, 2010): 405.
#' \doi{10.1186/1471-2105-11-405}.
#' @family external parsers
#' @export

call_openchrom <- function(files, path_out = NULL, format_in,
                           format_out = c("matrix", "data.frame", "data.table"),
                           data_format = c("wide", "long"),
                           export_format = c("mzml", "csv", "cdf", "animl"),
                           return_paths = FALSE,
                           verbose = getOption("verbose")){
  format_out <- check_format_out(format_out)
  if (length(files) == 0){
    stop("Files not found.")
  }
  if (missing(format_in)){
    stop("Format must be specified. The options are `msd` for mass spectrometry,
    `csd` for flame ionization (FID), or `wsd` for DAD/UV data.")}
  export_format <- match.arg(export_format, c("mzml", "csv", "cdf", "animl"))
  if (is.null(path_out)){
    path_out <- tempdir()
  } else{
    path_out <- fs::path_expand(path_out)
  }
  if(!dir.exists(path_out)){
    stop("Export directory not found. Please check `path_out` argument and try again.")
  }
  openchrom_path <- configure_openchrom()
  path_xml <- write_openchrom_batchfile(files = files, path_out = path_out,
                                        format_in = format_in,
                                        export_format = export_format)
  system(paste0(openchrom_path, " -nosplash -cli -batchfile ", path_xml),
         ignore.stdout = !verbose, ignore.stderr = !verbose)
  new_files <- fs::path(path_out,
                        fs::path_ext_remove(fs::path_file(files)),
                        ext = switch(export_format, "animl" = "animl",
                                     "csv" = "csv", "cdf" = "CDF",
                                     "mzml" = "mzML"))
  if (return_paths){
    new_files
  } else{
    file_reader <- switch(export_format,
                          "csv" = read.csv,
                          "cdf" = purrr::partial(read_cdf, format_out = format_out,
                                                 data_format = data_format),
                          "animl" = warning("An animl parser is not currently available in chromConverter"),
                          "mzml" = read_mzml)
      lapply(new_files, function(x){
        xx <- file_reader(x)
        if (export_format == "csv" && format_out == "matrix"){
          xx <- as.matrix(xx)
        }
        xx
      })
  }
}

#' Writes OpenChrom XML batch file
#' This function is called internally by \code{call_openchrom}.
#' @import xml2
#' @param files Paths to files for conversion
#' @param path_out directory to export converted files.
#' @param format_in Either \code{msd} for mass spectrometry data, \code{csd} for
#' flame ionization data, or \code{wsd} for DAD/UV data.
#' @param export_format Either \code{csv}, \code{cdf}, \code{mzml}, or \code{animl}.
#' @return Returns path to newly created xml batch file.
#' @author Ethan Bass
#' @noRd
write_openchrom_batchfile <- function(files, path_out,
                                      format_in = c("msd", "csd", "wsd"),
                                      export_format = c("csv", "cdf", "mzml",
                                                        "animl")){
  path_template <- system.file("openchrom_template.xml",
                               package = "chromConverter")
  x <- xml2::read_xml(x = path_template)

  input_entries <- xml_find_first(x,"//InputEntries")
  for (file in files){
    xml_add_child(input_entries, .value = "InputEntry")  |>
      xml_add_child(xml_cdata(file))
  }


  ### add appropriate parser to ProcessEntries ###
  msd_mzml_converter <- 'ProcessEntry id="msd.export.org.eclipse.chemclipse.msd.converter.supplier.mzml" name="mzML Chromatogram (*.mzML)" description="Reads mzML Chromatograms" jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
  msd_netcdf_converter <- 'ProcessEntry id="msd.export.net.openchrom.msd.converter.supplier.cdf" name="ANDI/AIA CDF Chromatogram (*.CDF)" description="Reads an writes ANDI/AIA CDF Chromatograms." jsonSettings="{&quot;Filename&quot;:&quot;{chromatogram_name}{extension}&quot;,&quot;Export Folder&quot;:&quot;path_out&quot;}" symbolicName="" className="" dataTypes=""'
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
  process_entries <- xml_find_first(x,"//ProcessEntries")
  for (file in files){
    xml_add_child(input_entries, .value = gsub("path_out", path_out, parser))
  }

  path_xml <- fs::path(path_out, paste0("batchfile_",
                                        strftime(Sys.time(),
                                                 format = "%Y-%m-%d_%H-%M-%S")),
                       ext = "xml")
  write_xml(x, file = path_xml)
  path_xml
}

#' Configure 'OpenChrom' parser
#'
#' Configures [OpenChrom](https://lablicate.com/platform/openchrom) to use
#' command-line interface. Requires OpenChrom version prior to 0.5.0.
#'
#' @name configure_openchrom
#' @param cli Defaults to NULL. If "true", R will rewrite openchrom ini file to enable CLI.
#' If "false", R will disable CLI. If NULL, R will not modify the ini file.
#' @param path Path to 'OpenChrom' executable (Optional). The supplied path will
#' overwrite the current path.
#' @importFrom utils read.table write.table
#' @return If \code{cli} is set to \code{"status"}, returns a Boolean value
#' indicating whether 'OpenChrom' is configured correctly. Otherwise, returns
#' the path to OpenChrom command-line application.
#' @author Ethan Bass
#' @seealso [call_openchrom]
#' @export

configure_openchrom <- function(cli = c("null", "true", "false", "status"), path = NULL){
  cli <- match.arg(cli, c("null", "true", "false", "status"))
  if (is.null(path)){
    path_parser <- readLines(system.file("shell/path_to_openchrom_commandline.txt", package = 'chromConverter'))
    if (path_parser == "NULL"){
      path_parser <- switch(.Platform$OS.type,
                            unix = "/Applications/Eclipse.app/Contents/MacOS/openchrom",
                            windows = fs::path(fs::path_home(),
                                               "AppData/Local/Programs/OpenChrom/openchrom.exe"),
                            linux = "/snap/bin/openchrom"
      )
    }
  } else{
    path_parser <- path
  }
  if (grepl("app/?$", path_parser)){
    path_parser <- fs::path(path_parser, "Contents/MacOS/openchrom")
  }
  writeLines(path_parser,
             con = system.file('shell/path_to_openchrom_commandline.txt',
                               package='chromConverter'))

  if (!file.exists(path_parser)){
    warning("OpenChrom not found!", immediate. = TRUE)
    path_parser <- readline(prompt = "Please provide path to `OpenChrom` command line (v0.4)):")
    if (.Platform$OS.type == "windows"){
      path_parser <- gsub("/","\\\\", path_parser)
    }
    writeLines(path_parser,
               con = system.file('shell/path_to_openchrom_commandline.txt',
                                 package = 'chromConverter'))
  }
  path_ini <- switch(.Platform$OS.type,
                     "unix" = fs::path(gsub("MacOS/openchrom", "", path_parser),
                                     "Eclipse/openchrom.ini"),
                     "linux" = fs::path(path_parser, ".uni"),
                     "windows" = paste0(gsub(".exe", "", path_parser), ".ini"))
  ini <- readLines(path_ini)
  cli_index <- grep("-Denable.cli.support", ini)
  if (length(cli_index) == 0) stop("OpenChrom bindings require OpenChrom version < 1.5")
  ini_split <- strsplit(ini[cli_index], "=")[[1]]
  cli_bool <- ini_split[2]

  if (cli == "null"){
    if (cli_bool == "false"){
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
  } else if (cli == "status"){
    return(cli_bool)
  }
  if (cli %in% c("true", "false")){
    ini_split[2] <- cli
    ini[cli_index] <- paste(ini_split, collapse = "=")
    writeLines(ini, path_ini)
  }
  path_parser[1]
}
