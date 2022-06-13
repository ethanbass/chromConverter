#' Attaches metadata to chromatogram
#'
#' @name attach_metadata
#' @param x chromatogram
#' @param meta List object containing metadata.
#' @param format_in Chromatogram format
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param format_data Whether data is in wide or long format.
#' @param parser What parser was used to decode the data
#' @return A chromatogram with attached metadata.
#' @author Ethan Bass
attach_metadata <- function(x, meta, format_in, format_out, format_data, parser = NULL){
  if (format_in == "waters_arw"){
    structure(x, instrument = NA,
              detector = NA,
              software = NA,
              method = meta$`Instrument Method Name`,
              batch = meta$`Sample Set Name`,
              operator = NA,
              run_date = NA,
              sample_name = meta$SampleName,
              sample_id = NA,
              injection_volume = NA,
              time_range = NA,
              time_interval = NA,
              detector_range = meta$Channel,
              format = "long",
              parser = "chromConverter",
              class = format_out)
  } else if (format_in == "shimadzu"){
    structure(x, instrument = meta$`Instrument Name`,
              detector = meta$`Detector Name`,
              software = c(software = meta$`Application Name`, version = meta$Version),
              method = meta$`Method File`,
              batch = meta$`Batch File`,
              operator = meta$`Operator Name`,
              run_date = meta$Acquired,
              sample_name = meta$`Sample Name`,
              sample_id = meta$`Sample ID`,
              injection_volume = meta$`Injection Volume`,
              time_range = c(meta$`Start Time(min)`, meta$`End Time(min)`),
              time_interval = meta$`Interval(msec)`,
              detector_range = c(meta$`Start Wavelength(nm)`, meta$`End Wavelength(nm)`),
              format = format,
              parser = "chromConverter",
              class = format_out)
  } else if (format_in == "chromeleon"){
    structure(x, instrument = NA,
              detector = meta$Detector,
              software = meta$`Generating Data System`,
              method = meta$`Instrument Method`,
              batch = NA,
              operator = meta$`Operator`,
              run_date = c(date=meta$`Injection Date`, time=meta$`Injection Time`),
              sample_name = meta$Injection,
              sample_id = NA,
              injection_volume = meta[[grep("Injection Volume", names(meta))]],
              time_range = c(meta$`Time Min. (min)`, meta$`Time Max. (min)`),
              time_interval = meta$`Average Step (s)`,
              detector_range = NA,
              format = "long",
              parser = "chromConverter",
              class = format_out)
  # } else if (format_in == "entab"){
  #   structure(x, instrument = meta$instrument,
  #             detector = NA,
  #             software = meta$Version,
  #             method = meta$method,
  #             batch = meta$SeqPathAndFile,
  #             operator = meta$operator,
  #             run_date = meta$run_date,
  #             sample_name = meta$sample,
  #             sample_id = NA,
  #             injection_volume = meta$InjVolume,
  #             time_range = NA,
  #             time_interval = NA,
  #             detector_range = NA,
  #             format = format_data,
  #             parser = "entab",
  #             class = format_out)
  } else if (format_in == "chemstation_uv"){
    structure(x, instrument = meta$AcqInstName,
              detector = NA,
              software = meta$Version,
              method = meta$AcqMeth,
              batch = meta$SeqPathAndFile,
              operator = meta$AcqOp,
              run_date = meta$InjDateTime,
              sample_name = meta$SampleName,
              sample_id = NA,
              injection_volume = meta$InjVolume,
              time_range = NA,
              time_interval = NA,
              detector_range = NA,
              format = format_data,
              parser = parser,
              class = format_out)
  } else if (format_in == "masshunter_dad"){
    structure(x, instrument = meta$Instrument,
    detector = NA,
    software = NA,
    method = meta$Method,
    batch = NA,
    operator = meta$OperatorName,
    run_date = meta$AcqTime,
    sample_name = meta$`Sample Name`,
    sample_id = meta$`Sample ID`,
    injection_volume = meta[[grep("Inj Vol", names(meta))]],
    time_range = NA,
    time_interval = NA,
    detector_range = NA,
    format = format_data,
    parser = parser,
    class = format_out)
  }
}

#' @name read_chemstation_metadata
#' @param file file
#' @param what Whether to return \code{metadata} or \code{peaktable}.
#' @importFrom readxl read_xls
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_chemstation_metadata <- function(file, what=c("metadata", "peaktable")){
  what <- match.arg(what, c("metadata", "peaktable"))
  # find xls csv files
  folder <- gsub(basename(file), "", file)
  # check for .D folder
  if (grepl("\\.D/$", folder,ignore.case = T)){
    # find xls/csv
    rep <- list.files(folder, pattern = '.xls|.csv',
                      ignore.case = TRUE, full.names = TRUE)
    if (length(rep) > 0){
      if (what == "metadata"){
        meta <- as.data.frame(readxl::read_xls(rep, sheet=1, skip=1))
        meta2<-as.list(meta$Results)
        names(meta2) <- meta$Title
        meta2
      } else if (what == "peaktable"){
        pktab <- as.data.frame(readxl::read_xls(rep, sheet="Peak"))
        pktab <- pktab[,-c(1:2)]
        pktab
      }
    }
  }
}

#' @name read_masshunter_metadata
#' @param file file
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @import magrittr
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_masshunter_metadata <- function(file){
  # check for .D folder
  folder <- gsub(basename(file), "", file)
  if (grepl("\\.D/|\\.d/$", folder,ignore.case = T)){
    # find xml
    rep <- list.files(folder, pattern = '.xml',
                      ignore.case = TRUE, full.names = TRUE)
    if (length(rep) > 0){
      path_devices <- rep[basename(rep)=="Devices.xml"]
      path_sample <- rep[basename(rep)=="sample_info.xml"]
      if (length(path_sample) == 1){
        meta_sample <- xml2::read_xml(path_sample)
        name <- xml_text(xml_find_all(meta_sample, xpath = "//Name"))
        meta_sample <- as.list(xml_text(xml_find_all(meta_sample, xpath = "//Value")))
        names(meta_sample) <- name
      }
      if (length(path_devices) == 1){
        meta_devices <- xml2::read_xml(path_devices)
        name <- xml_text(xml_find_all(meta_devices, xpath = "//Name"))
        meta_devices <- as.character(xml_text(xml_find_all(meta_devices, xpath = "//ModelNumber")))
        names(meta_devices) <- name
      }
      meta_sample$Instrument <- meta_devices
    }
  }
  meta_sample
}
