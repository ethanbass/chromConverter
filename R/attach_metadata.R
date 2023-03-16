#' Attaches metadata to chromatogram
#'
#' @name attach_metadata
#' @param x chromatogram
#' @param meta List object containing metadata.
#' @param format_in Chromatogram format
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether data is in wide or long format.
#' @param parser What parser was used to decode the data
#' @return A chromatogram with attached metadata.
#' @author Ethan Bass

attach_metadata <- function(x, meta, format_in, format_out, data_format, parser = NULL){
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
              data_format = "long",
              parser = "chromConverter",
              format_out = format_out)
  } else if (format_in == "shimadzu"){
    structure(x,
              instrument = meta$`Instrument Name`,
              detector = meta$`Detector Name`,
              software_name = meta$`Application Name`,
              software_version = meta$Version,
              method = meta$`Method File`,
              batch = meta$`Batch File`,
              operator = meta$`Operator Name`,
              run_date = meta$Acquired,
              sample_name = meta$`Sample Name`,
              sample_id = meta$`Sample ID`,
              injection_volume = meta$`Injection Volume`,
              start_time = meta$`Start Time(min)`,
              end_time = meta$`End Time(min)`,
              time_interval = meta$`Interval(msec)`,
              start_wavelength = meta$`Start Wavelength(nm)`,
              end_wavelength = meta$`End Wavelength(nm)`,
              data_format = data_format,
              parser = "chromConverter",
              format_out = format_out)
  } else if (format_in == "chromeleon"){
    structure(x, instrument = NA,
              detector = meta$Detector,
              software = meta$`Generating Data System`,
              method = meta$`Instrument Method`,
              batch = NA,
              operator = meta$`Operator`,
              run_date = meta$`Injection Date`,
              run_time = meta$`Injection Time`,
              sample_name = meta$Injection,
              sample_id = NA,
              injection_volume = meta$`Injection Volume`,
              start_time = meta$`Time Min. (min)`,
              end_time = meta$`Time Max. (min)`,
              time_interval = meta$`Average Step (s)`,
              detector_range = NA,
              data_format = "long",
              parser = "chromConverter",
              format_out = format_out)
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
  #             format = data_format,
  #             parser = "entab",
  #             format_out = format_out)
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
              data_format = data_format,
              parser = parser,
              format_out = format_out)
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
    injection_volume = meta$`Inj Vol`,
    time_range = NA,
    time_interval = NA,
    detector_range = NA,
    data_format = data_format,
    parser = parser,
    format_out = format_out)
  } else {
    structure(x, instrument = meta$Instrument,
              detector = NA,
              software = NA,
              method = meta$Method,
              batch = NA,
              operator = meta$OperatorName,
              run_date = meta$AcqTime,
              sample_name = meta$`Sample Name`,
              sample_id = meta$`Sample ID`,
              injection_volume = meta$`Inj Vol`,
              time_range = NA,
              time_interval = NA,
              detector_range = NA,
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = ifelse(missing(parser), NA, parser),
              format_out = ifelse(missing(format_out), NA, format_out)
    )
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
  if (grepl("\\.D/$", folder,ignore.case = TRUE)){
    # find xls/csv
    reps <- list.files(folder, pattern = '.xls',
                      ignore.case = TRUE, full.names = TRUE)
    if (length(reps) > 0){
      if (what == "metadata"){
        meta <- as.data.frame(readxl::read_xls(reps[1], sheet=1, skip=1))
        meta2<-as.list(meta$Results)
        names(meta2) <- meta$Title
        meta2
      } else if (what == "peaktable"){
        pktab <- as.data.frame(readxl::read_xls(rep, sheet = "Peak"))
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
  if (grepl("\\.D/|\\.d/$", folder, ignore.case = TRUE)){
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


#' @name read_chromeleon_metadata
#' @param file file
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @import magrittr
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_chromeleon_metadata <- function(x){
  meta_fields <- grep("Information:", x)
  meta <- do.call(rbind, strsplit(x[(meta_fields[1]+1):(meta_fields[length(meta_fields)]-1)],"\t"))
  rownames(meta) <- meta[,1]
  meta <- as.list(meta[,-1])
  meta
}

#' @name read_waters_metadata
#' @param file file
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @import magrittr
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_waters_metadata <- function(file){
  meta <- gsub("\\\"", "", do.call(cbind, strsplit(readLines(file, n = 2),"\t")))
  rownames(meta) <- meta[,1]
  meta <- as.list(meta[,-1])
}

#' Extract metadata
#'
#' Extract metadata as a data.frame from a list of chromatograms.
#'
#' @param chrom_list A list of chromatograms with attached metadata (as returned
#' by \code{read_chroms} with \code{read_metadata = TRUE}).
#' @param what A character vector specifying the metadata elements to extract.
#' @param format_out Format of object. Either \code{data.frame} or \code{tibble}.
#' @return A data.frame or tibble (according to the value of \code{format_out}),
#' with samples as rows and the specified metadata elements as columns.
#' @export
extract_metadata <- function(chrom_list,
                             what = c("instrument", "detector", "software",
                                      "method", "batch", "operator", "run_date",
                                      "sample_name", "sample_id",
                                      "injection_volume", "time_range",
                                      "time_interval", "detector_range",
                                      "data_format", "parser","format_out"),
                             format_out = c("data.frame", "tibble")
                                                  ){
  what <- match.arg(what, several.ok = TRUE)
  format_out <- match.arg(format_out, c("data.frame", "tibble"))
  metadata <- purrr::map_df(chrom_list, function(chrom){
    unlist(sapply(what, function(x){
      attr(chrom, which = x)
    }, simplify = FALSE))
  })
  if (format_out == "tibble"){
    metadata <- tibble::add_column(.data = metadata,
                                   data.frame(name=names(chrom_list)),
                                   .before=TRUE)
  } else if (format_out == "data.frame"){
    metadata <- data.frame(metadata, row.names = names(chrom_list))
  }
  metadata
}
