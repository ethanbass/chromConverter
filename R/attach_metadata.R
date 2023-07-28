#' Attaches metadata to chromatogram
#'
#' @name attach_metadata
#' @param x chromatogram
#' @param meta List object containing metadata.
#' @param format_in Chromatogram format
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether data is in wide or long format.
#' @param parser What parser was used to decode the data.
#' @param source_file The path to the source file.
#' @return A chromatogram with attached metadata.
#' @author Ethan Bass

attach_metadata <- function(x, meta, format_in, format_out, data_format, parser = NULL,
                            source_file){
  switch(format_in,
    "waters_arw" = {
      structure(x, instrument = NA,
                detector = get_metadata_field(meta, "Channel Type"),
                software = get_metadata_field(meta,"Source S/W Info"),
                method = get_metadata_field(meta,"Instrument Method Name"),
                batch = get_metadata_field(meta,"Sample Set Name"),
                operator = NA,
                run_datetime = NA,
                sample_name = get_metadata_field(meta,"SampleName"),
                sample_id = NA,
                sample_injection_volume = NA,
                sample_amount = NA,
                time_range = c(get_metadata_field(meta, "Data Start"),
                               get_metadata_field(meta, "Data End")),
                time_interval = NA,
                time_unit = NA,
                detector_range = ifelse("Channel Description" %in% names(meta),
                                          get_metadata_field(meta, "Channel Description"),
                                          get_metadata_field(meta, "Channel")
                                        ),
                detector_unit = get_metadata_field(meta, "Det. Units"),
                source_file = source_file,
                data_format = "long",
                parser = "chromConverter",
                format_out = format_out)
  }, "shimadzu" = {
    structure(x,
              instrument = meta$`Instrument Name`,
              detector = meta$`Detector Name`,
              software_name = meta$`Application Name`,
              software_version = meta$Version,
              method = meta$`Method File`,
              batch = meta$`Batch File`,
              operator = meta$`Operator Name`,
              run_datetime = as.POSIXct(meta$Acquired, format = "%m/%d/%Y %I:%M:%S %p"),
              sample_name = meta$`Sample Name`,
              sample_id = meta$`Sample ID`,
              sample_injection_volume = meta$`Injection Volume`,
              sample_amount = meta$`Injection Volume`,
              time_range = c(meta$`Start Time(min)`, meta$`End Time(min)`),
              # start_time = meta$`Start Time(min)`,
              # end_time = meta$`End Time(min)`,
              time_interval = meta$`Interval(msec)`,
              time_interval_unit = get_time_unit(
                grep("Interval", names(meta), value = TRUE)[1], format_in = "shimadzu"),
              time_unit = get_time_unit(
                grep("Start Time", names(meta), value=TRUE)[1], format_in = "shimadzu"),
              detector_range = c(meta$`Start Wavelength(nm)`, meta$`End Wavelength(nm)`),
              # detector_end = meta$`End Wavelength(nm)`,
              detector_unit = NA,
              source_file = source_file,
              data_format = data_format,
              parser = "chromConverter",
              format_out = format_out)
  }, "chromeleon" = {
    datetime.idx <- unlist(sapply(c("Date$","Time$"), function(str) grep(str, names(meta))))
    datetime <- unlist(meta[datetime.idx])
    if (length(datetime > 1)){
      datetime <- paste(datetime, collapse=" ")
    }
    datetime <- as.POSIXct(datetime, format = c("%m/%d/%Y %H:%M:%S", "%d.%m.%Y %H:%M:%S",
                                      "%m/%d/%Y %H:%M:%S %p %z"))
    datetime <- datetime[!is.na(datetime)]
    time_interval_unit <- tryCatch({
      get_time_unit(grep("Average Step", names(meta), value = TRUE)[1],
                    format_in = "chromeleon")}, error = function(err) NA)
    time_unit <- tryCatch({
      get_time_unit(grep("Time Min.", names(meta), value = TRUE)[1],
                    format_in="chromeleon")}, error = function(err) NA)
    structure(x, instrument = NA,
              detector = meta$Detector,
              software = meta$`Generating Data System`,
              method = meta$`Instrument Method`,
              batch = NA,
              operator = meta$`Operator`,
              run_datetime = datetime,
              # run_date = meta$`Injection Date`,
              # run_time = meta$`Injection Time`,
              sample_name = meta$Injection,
              sample_id = NA,
              sample_injection_volume = meta$`Injection Volume`,
              sample_amount =  meta$`Injection Volume`,
              time_range = c(meta$`Time Min. (min)`, meta$`Time Max. (min)`),
              # start_time = meta$`Time Min. (min)`,
              # end_time = meta$`Time Max. (min)`,
              time_interval = meta[[grep("Average Step", names(meta))]],
              time_interval_unit = time_interval_unit,
              time_unit = time_unit,
              # uniform_sampling = meta$`Min. Step (s)` == meta$`Max. Step (s)`,
              detector_range = NA,
              detector_unit = meta$`Signal Unit`,
              source_file = source_file,
              format_out = format_out,
              data_format = "long",
              parser = "chromConverter"
              )
  # } else if (format_in == "entab"){
  #   structure(x, instrument = meta$instrument,
  #             detector = NA,
  #             software = meta$Version,
  #             method = meta$method,
  #             batch = meta$SeqPathAndFile,
  #             operator = meta$operator,
  #             run_datetime = meta$run_date,
  #             sample_name = meta$sample,
  #             sample_id = NA,
  #             injection_volume = meta$InjVolume,
  #             time_range = NA,
  #             time_interval = NA,
  #             detector_range = NA,
  #             format = data_format,
  #             parser = "entab",
  #             format_out = format_out)
  }, "chemstation_uv" = {
    structure(x, instrument = meta$AcqInstName,
              detector = NA,
              software = meta$Version,
              method = meta$AcqMeth,
              batch = meta$SeqPathAndFile,
              operator = meta$AcqOp,
              run_datetime = meta$InjDateTime,
              sample_name = meta$SampleName,
              sample_id = NA,
              sample_injection_volume = meta$InjVolume,
              sample_amount = meta$InjVolume,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_unit = NA,
              source_file = source_file,
              data_format = data_format,
              parser = parser,
              format_out = format_out)
  }, "chemstation_peaklist" = {
    structure(x, instrument = meta$AcqInstName,
              detector = NA,
              software = meta$Version,
              method = meta$AcqMeth,
              batch = meta$SeqPathAndFile,
              operator = meta$AcqOp,
              run_datetime = meta$InjDateTime,
              sample_name = meta$SampleName,
              sample_id = NA,
              sample_injection_volume = meta$InjVolume,
              sample_amount = meta$InjVolume,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_unit = NA,
              source_file = source_file,
              data_format = data_format,
              parser = parser,
              format_out = format_out)
  }, "masshunter_dad" = {
      structure(x, instrument = meta$Instrument,
                detector = NA,
                software = NA,
                method = meta$Method,
                batch = NA,
                operator = meta$OperatorName,
                run_datetime = meta$AcqTime,
                sample_name = meta$`Sample Name`,
                sample_id = meta$`Sample ID`,
                sample_injection_volume = meta$`Inj Vol`,
                sample_amount = meta$`Inj Vol`,
                time_range = NA,
                time_interval = NA,
                time_unit = NA,
                detector_range = NA,
                detector_unit = NA,
                source_file = source_file,
                data_format = data_format,
                parser = parser,
                format_out = format_out)
  }, "cdf" = {
    structure(x, instrument = NA,
              detector = get_metadata_field(meta, "detector_name"),
              software = NA,
              method = NA,
              batch = get_metadata_field(meta, "experiment_title"),
              operator = get_metadata_field(meta, "operator_name"),
              run_datetime = as.POSIXct(
                get_metadata_field(meta, "injection_date_time_stamp"),
                                        format = "%Y%m%d%H%M%S%z"),
              sample_name = get_metadata_field(meta, "sample_name"),
              sample_id = get_metadata_field(meta, "sample_id"),
              sample_type = get_metadata_field(meta, "sample_type"),
              sample_injection_volume = get_metadata_field(meta, "sample_injection_volume"),
              sample_amount = get_metadata_field(meta, "sample_amount"),
              time_start = NA,
              time_end = NA,
              time_interval = NA,
              time_unit = get_metadata_field(meta, "retention_unit"),
              detector_range = NA,
              # detector_end = NA,
              detector_unit = get_metadata_field(meta, "detector_unit"),
              source_file = ifelse(missing(source_file), NA, source_file),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = "chromConverter")
  }, "mdf" = {
    structure(x, instrument = meta[meta$Property == "Instrument","Value"],
              detector = "Variable Wavelength Detector",
              software = NA,
              method = NA,
              batch = get_metadata_field(meta, "experiment_title"),
              operator = meta[meta$Property == "Operator", "Value"],
              run_datetime = as.POSIXct(
                meta[meta$Property == "Time", "Value"],
                format = "%d.%m.%Y %H:%M:%S"),
              sample_name = get_metadata_field(meta, "sample_name"),
              sample_id = get_metadata_field(meta, "sample_id"),
              sample_type = "unknown",
              sample_injection_volume = 1,
              sample_amount = 1,
              time_start = meta[meta$Group=="Interval Time" & meta$Property == "From", "Value"],
              time_end = meta[meta$Group=="Interval Time" & meta$Property == "To", "Value"],
              time_interval = meta[meta$Group=="Interval Time" & meta$Property == "Step", "Value"],
              time_unit = meta[meta$Group=="Interval Time" & meta$Property == "Units", "Value"],
              detector_range = meta[meta$Property == "Wave", "Value"],
              # detector_end = meta[meta$Property == "Wave", "Value"],
              detector_unit = meta[meta$Group=="Array photometric" & meta$Property == "Units", "Value"],
              source_file = ifelse(missing(source_file), NA, source_file),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = "chromConverter")
  }, "default" = {
    structure(x, instrument = meta$Instrument,
              detector = NA,
              software = NA,
              method = meta$Method,
              batch = NA,
              operator = meta$OperatorName,
              run_datetime = meta$AcqTime,
              sample_name = meta$`Sample Name`,
              sample_id = meta$`Sample ID`,
              sample_injection_volume = meta$`Inj Vol`,
              sample_amount = meta$`Inj Vol`,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_unit = NA,
              source_file = source_file,
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = ifelse(missing(parser), NA, parser)
              )
  }
 )
}

#' @noRd
get_metadata_field <- function(x, field){
  ifelse(!is.null(x[[field]]), x[[field]], NA)
}

#' @noRd
get_time_unit <- function(string, format_in){
  if (length(string) == 0 || is.na(string)){
    NA
  } else{
    if (format_in %in% c("chromeleon", "shimadzu")){
      pattern <- "\\((.*?)\\)"
      unit <- gsub("\\(|\\)", "", regmatches(string, regexpr(pattern, string))[[1]])
      switch(unit, "min" = "Minutes", "sec" = "Seconds")
    } else NA
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
  ll <- readLines(file, n=2)
  ll <- iconv(ll,from = "ISO-8859-1", to = "UTF-8")
  meta <- gsub("\\\"", "", do.call(cbind, strsplit(ll,"\t")))
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
  if (is.matrix(chrom_list) | is.data.frame(chrom_list)){
    chrom_list <- list(chrom_list)
  }
  what <- match.arg(what, several.ok = TRUE)
  format_out <- match.arg(format_out, c("data.frame", "tibble"))
  metadata <- purrr::map_df(chrom_list, function(chrom){
    unlist(sapply(what, function(x){
      attr(chrom, which = x)
    }, simplify = FALSE))
  })
  if (format_out == "tibble"){
    metadata <- tibble::add_column(.data = metadata,
                                   data.frame(name = names(chrom_list)),
                                   .before=TRUE)
  } else if (format_out == "data.frame"){
    metadata <- data.frame(name = names(chrom_list), metadata, row.names = names(chrom_list))
  }
  metadata
}
