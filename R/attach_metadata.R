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
#' @param scale Whether the data has been scaled.
#' @return A chromatogram with attached metadata.
#' @author Ethan Bass
#' @noRd

attach_metadata <- function(x, meta, format_in, format_out, data_format,
                            parser = NULL, source_file,
                            source_file_format = format_in,
                            scale = NULL){
  if (grepl("chemstation", format_in)){
    format_in <- "chemstation"
  }

  switch(format_in,
    "raw" = {
      structure(x, metadata = meta, data_format = data_format, parser = parser,
                source_file = source_file,
                source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE))
    }, "asm" = {
        structure(x,
                  sample_name = meta$`sample document`$written_name,
                  sample_id = meta$`sample document`$sample_identifier,
                  file_version = meta$file_version,
                  file_type = NA,
                  instrument = meta$`asset management identifier`,
                  detector_id = meta$detector_model_number,
                  detector_range = get_asm_wavelength(meta),
                  detector_y_unit = meta$detector_unit,
                  detector_reference = get_asm_wavelength(meta,
                                                          lab = "reference_wavelength_setting_value"),
                  software = NA,
                  software_version = NA,
                  software_revision = NA,
                  method = NA,
                  batch = NA,
                  operator = meta$operator,
                  run_datetime = as.POSIXct(strptime(meta$`injection document`$injection_time,
                                                     format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")),
                  sample_injection_volume = paste(meta$`injection document`$autosampler_injection_volume_setting_chromatography_value,
                                                  meta$`injection document`$autosampler_injection_volume_setting_chromatography_unit),
                  sample_amount = NA,
                  # time_range = meta$time_range,
                  # time_interval = NA,
                  time_unit = meta$time_unit,
                  intensity_multiplier = NA,
                  source_file = source_file,
                  source_file_format = source_file_format,
                  source_sha1 = digest::digest(source_file, algo = "sha1",
                                               file = TRUE),
                  data_format = data_format,
                  parser = parser,
                  format_out = format_out)
    }, "rainbow" = {
      meta$date <- convert_timestamp(meta$date, datetime_formats =
                          c("%d %b %y %I:%M %p %z", "%d-%b-%Y %H:%M:%S",
                            "%d-%b-%y, %H:%M:%S", "%d %b %y %I:%M %p"))
      structure(x,
                sample_name = ifelse(is.null(meta$notebook),
                                     fs::path_ext_remove(basename(source_file)),
                                     meta$notebook),
                sample_id = NA,
                vial = meta$vialpos,
                file_version = NA,
                file_type =  NA,
                instrument =  NA,
                detector = meta$detector,
                detector_range = NA,
                detector_y_unit = meta$unit,
                polarity = meta$polarity,
                software =  NA,
                software_version =  NA,
                software_revision =  NA,
                method = meta$method,
                batch =  NA,
                operator =  NA,
                run_datetime = meta$date,
                sample_injection_volume =  NA,
                sample_amount =  NA,
                time_range =  NA,
                time_interval = NA,
                time_unit =  NA,
                intensity_multiplier =  NA,
                scaled = NA,
                source_file = source_file,
                source_file_format = source_file_format,
                source_sha1 = ifelse(fs::is_file(source_file),
                                     digest::digest(source_file, algo = "sha1",
                                                    file = TRUE),
                                     NA),
                data_format = data_format,
                parser = parser,
                format_out = format_out)
    }, "varian_sms" = {
      meta$max_ionization_time <- sapply(meta$segment_metadata, function(x){
        x$max_ionization_time
      })
      structure(x,
                sample_name = ifelse(is.null(meta$sample_name),
                                     fs::path_ext_remove(basename(source_file)),
                                             meta$sample_name),
                sample_id = NA,
                sample_id = NA,
                instrument = NA,
                detector = "MS",
                detector_id = NA,
                software_name = get_metadata_field(meta, "software"),
                software_version = get_metadata_field(meta, "version"),
                method = NA,
                batch = NA,
                operator = NA,
                run_datetime = c(meta$t1, meta$t2),
                sample_injection_volume = NA,
                sample_amount = NA,
                time_start = sapply(meta$segment_metadata, function(x){
                  x$start_time}),
                end_time = sapply(meta$segment_metadata, function(x){
                  x$end_time}),
                no_scans = meta$n_scan,
                ms_params = meta[c("ion_time", "emission_current", "max_ric_scan",
                                    "max_ric_val", "max_ionization_time",
                                   "temp_trap", "temp_manifold", "temp_transferline",
                                   "axial_modulation")],
                time_interval = NA,
                time_interval_unit = NA,
                time_unit = "Minutes",
                time_multiplier = 1/60000,
                wavelength = NA,
                detector_y_unit = NA,
                intensity_multiplier = 1,
                mz_multiplier = 1/20,
                scaled = FALSE,
                source_file = source_file,
                source_file_format = source_file_format,
                source_sha1 = digest::digest(source_file, algo = "sha1", file = TRUE),
                data_format = data_format,
                parser = "chromconverter",
                format_out = format_out)
          }, "waters_arw" = {
      structure(x, instrument = NA,
                detector = get_metadata_field(meta, "Channel Type"),
                software = get_metadata_field(meta, "Source S/W Info"),
                method = get_metadata_field(meta, "Instrument Method Name"),
                batch = get_metadata_field(meta, "Sample Set Name"),
                operator = NA,
                run_datetime = NA,
                sample_name = ifelse(is.null(meta$SampleName),
                                     fs::path_ext_remove(basename(source_file)),
                                     meta$SampleName),
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
                detector_y_unit = get_metadata_field(meta, "Det. Units"),
                source_file = source_file,
                source_file_format = source_file_format,
                source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
                data_format = data_format,
                parser = "chromconverter",
                format_out = format_out)
  }, "waters_raw" = {
    structure(x, instrument = get_metadata_field(meta, "Instrument"),
              detector = NA,
              software = NA,
              method = NA,
              batch = NA,
              operator = get_metadata_field(meta, "User_Name"),
              run_datetime = as.POSIXct(paste(meta$Acquired_Date, meta$Acquired_Time,
                                              collapse = " "),
                                        format = "%d-%b-%Y %I:%M:%S",
                                        tz = "UTC"),
              sample_name = ifelse(is.null(meta$`Acquired Name`),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta$`Acquired Name`),
              sample_id = NA,
              sample_injection_volume = NA,
              sample_amount = NA,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_y_unit = get_metadata_field(meta, "Detector_Unit"),
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = NA,
              data_format = data_format,
              parser = "chromconverter",
              format_out = format_out)
  }, "shimadzu_dad" = {
    structure(x,
              instrument = get_metadata_field(meta, "Instrument Name"),
              detector = "DAD",
              detector_id = get_metadata_field(meta, "Detector Name"),
              software_name = get_metadata_field(meta, "Application Name"),
              software_version = get_metadata_field(meta, "Version"),
              method = get_metadata_field(meta, "Method File"),
              batch = get_metadata_field(meta, "Batch File"),
              operator = get_metadata_field(meta, "Operator Name"),
              run_datetime = as.POSIXct(meta$Acquired,
                                        format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"),
              sample_name = ifelse(is.null(meta[["Sample Name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["Sample Name"]]),
              sample_id = get_metadata_field(meta, "Sample ID"),
              sample_injection_volume = get_metadata_field(meta, "Injection Volume"),
              sample_amount = get_metadata_field(meta, "Injection Volume"),
              time_range = c(meta$`Start Time(min)`, meta$`End Time(min)`),
              time_interval = meta$`Interval(msec)`,
              time_interval_unit = get_time_unit(
                grep("Interval", names(meta), value = TRUE)[1],
                                          format_in = "shimadzu"),
              time_unit = get_time_unit(
                grep("Start Time", names(meta), value = TRUE)[1],
                                          format_in = "shimadzu"),
              detector_range = c(meta$`Start Wavelength(nm)`,
                                 meta$`End Wavelength(nm)`),
              detector_y_unit = NA,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              data_format = data_format,
              parser = "chromconverter",
              format_out = format_out)
  }, "shimadzu_chrom" = {
    structure(x,
              instrument = get_metadata_field(meta, "Instrument Name"),
              detector_id = get_metadata_field(meta, "Detector Name"),
              software_name = get_metadata_field(meta, "Application Name"),
              software_version = get_metadata_field(meta, "Version"),
              method = get_metadata_field(meta, "Method File"),
              batch = get_metadata_field(meta, "Batch File"),
              operator = get_metadata_field(meta, "Operator Name"),
              run_datetime = as.POSIXct(meta$Acquired,
                                        format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"),
              sample_name = ifelse(is.null(meta[["Sample Name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["Sample Name"]]),
              sample_id = get_metadata_field(meta, "Sample ID"),
              sample_injection_volume = get_metadata_field(meta, "Injection Volume"),
              sample_amount = get_metadata_field(meta, "Injection Volume"),
              time_range = c(meta$`Start Time(min)`, meta$`End Time(min)`),
              time_interval = meta$`Interval(msec)`,
              time_interval_unit = get_time_unit(
                grep("Interval", names(meta), value = TRUE)[1],
                                          format_in = "shimadzu"),
              time_unit = get_time_unit(
                grep("Start Time", names(meta), value=TRUE)[1],
                                          format_in = "shimadzu"),
              wavelength = get_metadata_field(meta, "Wavelength(nm)"),
              bandwidth = get_metadata_field(meta, "Bandwidth(nm)"),
              detector_y_unit = get_metadata_field(meta, "Intensity Units"),
              intensity_multiplier = as.numeric(get_metadata_field(meta, "Intensity Multiplier")),
              scaled = scale,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              data_format = data_format,
              parser = "chromconverter",
              format_out = format_out)
  },  "shimadzu_lcd" = {
    structure(x,
              instrument = get_metadata_field(meta, "DSN"),
              detector = get_metadata_field(meta, "DETN"),
              detector_id = get_metadata_field(meta, "DSID"),
              # software_name = get_metadata_field(meta, "Application Name"),
              software_version = get_metadata_field(meta, "DataFileProperty.szVersion"),
              method = get_metadata_field(meta, "SampleInfoFile.methodfile"),
              batch = get_metadata_field(meta, "SampleInfoFile.batchfile"),
              operator = get_metadata_field(meta, "SampleInfo.operator_name"),
              run_datetime = as.POSIXct(meta$time_acq, tz = "UTC"),
              sample_name = ifelse(is.null(meta[["SampleInfo.smpl_name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["SampleInfo.smpl_name"]]),
              sample_id = get_metadata_field(meta, "SampleInfo.smpl_id"),
              vial = get_metadata_field(meta, 'SampleInfo.smpl_vial'),
              sample_type = get_metadata_field(meta, "SampleInfo.smpl_type"),
              sample_dilution = get_metadata_field(meta, "SampleInfo.dil_factor"),
              sample_injection_volume = get_metadata_field(meta, "SampleInfo.inj_vol"),
              sample_amount = get_metadata_field(meta, "SampleInfo.smpl_amount"),
              time_range = c(get_metadata_field(meta, "DLT"),
                             get_metadata_field(meta, "AT")),
              time_interval = get_metadata_field(meta, "Rate"),
              time_interval_unit = get_metadata_field(meta, "time.unit"),
              time_unit = get_metadata_field(meta, "time.unit"),
              time_multiplier = get_metadata_field(meta, "time.vf"),
              wavelength = get_sz_wv(meta),
             detector_y_unit = get_metadata_field(meta, "detector.unit"),
             intensity_multiplier = get_metadata_field(meta, "detector.vf"),
             scaled = scale,
             source_file = source_file,
             source_file_format = source_file_format,
             source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
             data_format = data_format,
             parser = "chromconverter",
             format_out = format_out)
    }, "chromeleon" = {
      if (is.null(meta$`Inject Time`)){
        datetime.idx <- unlist(sapply(c("Date$", "Time$"), function(str){
          grep(str, names(meta))
        })
        )
        datetime <- unlist(meta[datetime.idx])
        if (length(datetime > 1)){
          datetime <- paste(datetime, collapse = " ")
        }
        datetime <- as.POSIXct(datetime, format = c("%m/%d/%Y %H:%M:%S",
                                                    "%d.%m.%Y %H:%M:%S",
                                                    "%m/%d/%Y %H:%M:%S %p %z"),
                               tz = "UTC")
        datetime <- datetime[!is.na(datetime)]
      } else {
        datetime <- sub("(\\+\\d{2}):(\\d{2})$", "\\1\\2", meta$`Inject Time`)
        datetime <- as.POSIXct(strptime(datetime, format = "%d/%m/%Y %H:%M:%S %z"),
                               tz="UTC")

      }
    time_interval_unit <- tryCatch({
      get_time_unit(grep("Average Step", names(meta), value = TRUE)[1],
                    format_in = "chromeleon")}, error = function(err) NA)
    time_unit <- tryCatch({
      get_time_unit(grep("Time Min.", names(meta), value = TRUE)[1],
                    format_in = "chromeleon")}, error = function(err) NA)
    if (is.null(meta$Name) && !is.null(meta$Injection)){
      meta$Name <- meta$Injection
    }
    if (is.null(meta$`Signal Unit`)){
      unit <- grep("Signal Min", names(meta), value = TRUE)
      unit <- sub(".*(?:\\((.*)\\)).*|.*", "\\1", unit)
      meta$`Signal Unit` <- unit
    }

    structure(x, instrument = NA,
              detector = meta$Detector,
              software = meta$`Generating Data System`,
              method = meta$`Instrument Method`,
              batch = meta$Sequence,
              operator = meta$`Operator`,
              run_datetime = datetime,
              sample_name = ifelse(is.null(meta$Name),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta$Name),
              sample_id = NA,
              vial = meta$Position,
              sample_injection_volume = meta[[which(grepl("Volume",names(meta)))]],
              sample_amount = NA,
              sample_dilution = meta$`Dilution Factor`,
              sample_type = get_metadata_field(meta, "Type"),
              time_range = c(get_metadata_field(meta, "Time Min. (min)"),
                             get_metadata_field(meta, "Time Max. (min)")),
              time_interval = tryCatch({
                meta[[grep("Average Step", names(meta))]]
                }, error = function(err) NA),
              time_interval_unit = time_interval_unit,
              time_unit = time_unit,
              detector_range = ifelse(meta$`Spectral Field` == "3DFIELD",
                                      c(get_metadata_field(meta, "Scan Min. (nm)"),
                                        get_metadata_field(meta, "Scan Max. (nm)")),
                                      NA),
              detector_y_unit = meta$`Signal Unit`,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = digest::digest(source_file, algo = "sha1",
                                           file = TRUE),
              format_out = format_out,
              data_format = data_format,
              parser = "chromconverter"
              )
  }, "chemstation" = {
    meta$date <- convert_timestamp(meta$date, datetime_formats =
                        c("%d-%b-%y, %H:%M:%S", "%m/%d/%Y %I:%M:%S %p",
                          "%d/%m/%Y %I:%M:%S %p"))
    structure(x, sample_name = iconv(meta$sample_name, sub = ""),
              sample_id = meta$vial,
              file_version = meta$version,
              file_type = meta$file_type,
              instrument = meta$AcqInstName,
              detector = meta$detector,
              detector_id = meta$detector_model,
              detector_range = meta$signal,
              detector_y_unit = meta$units,
              detector_x_unit = meta$detector_x_unit,
              software = meta$software,
              software_version = meta$software_version,
              software_revision = meta$software_revision,
              # software = meta$Version,
              method = meta$method,
              batch = meta$SeqPathAndFile,
              operator = meta$operator,
              run_datetime = meta$date,
              sample_injection_volume = meta$InjVolume,
              sample_amount = meta$InjVolume,
              time_range = meta$time_range,
              time_interval = NA,
              time_unit = "Minutes",
              intensity_multiplier = meta$intensity_multiplier,
              scaled = scale,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = digest::digest(source_file, algo = "sha1",
                                           file = TRUE),
              data_format = data_format,
              parser = parser,
              format_out = format_out)
  }, "chemstation_peaklist" = {
    structure(x, instrument = meta$`Acq. Instrument`,
              detector = NA,
              software = NA,
              method = meta$Method,
              batch = NA,
              operator = meta$`Acq. Operator`,
              run_datetime = NA,
              sample_name = ifelse(is.null(meta[["Sample Name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["Sample Name"]]),
              sample_id = NA,
              sample_injection_volume = meta$`Inj Volume`,
              sample_amount = meta$`Inj Volume`,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_y_unit = NA,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              data_format = data_format,
              parser = parser,
              format_out = format_out)
  }, "masshunter_dad" = {
      structure(x, instrument = meta$Instrument,
                detector = "DAD",
                software = NA,
                method = meta$Method,
                batch = NA,
                operator = meta$OperatorName,
                run_datetime = meta$AcqTime,
                sample_name = ifelse(is.null(meta[["Sample Name"]]),
                                     fs::path_ext_remove(basename(source_file)),
                                     meta[["Sample Name"]]),
                sample_id = meta$`Sample ID`,
                sample_injection_volume = meta$`Inj Vol`,
                sample_amount = meta$`Inj Vol`,
                time_range = NA,
                time_interval = NA,
                time_unit = NA,
                detector_range = NA,
                detector_y_unit = NA,
                source_file = source_file,
                source_file_format = source_file_format,
                source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
                data_format = data_format,
                parser = parser,
                format_out = format_out)
  }, "andi_chrom" = {
    structure(x, instrument = NA,
              detector = get_metadata_field(meta, "detector"),
              detector_id = get_metadata_field(meta, "detector_name"),
              software = NA,
              method = NA,
              batch = get_metadata_field(meta, "experiment_title"),
              operator = get_metadata_field(meta, "operator_name"),
              run_datetime = as.POSIXct(
                get_metadata_field(meta, "injection_date_time_stamp"),
                                        format = "%Y%m%d%H%M%S%z", tz = "UTC"),
              sample_name = ifelse(is.null(meta[["sample_name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["sample_name"]]),
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
              detector_y_unit = get_metadata_field(meta, "detector_y_unit"),
              detector_x_unit = NA,
              source_file = ifelse(missing(source_file), NA, source_file),
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = "chromconverter")
  }, "andi_ms" = {
    structure(x, instrument = NA,
              detector = get_metadata_field(meta, "detector"),
              detector_id = get_metadata_field(meta, "detector_name"),
              software = NA,
              method = NA,
              batch = get_metadata_field(meta, "experiment_title"),
              operator = get_metadata_field(meta, "operator_name"),
              run_datetime = as.POSIXct(
                get_metadata_field(meta, "experiment_date_time_stamp"),
                format = "%Y%m%d%H%M%S%z", tz = "UTC"),
              sample_name = ifelse(is.null(meta[["sample_name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["sample_name"]]),
              sample_id = get_metadata_field(meta, "sample_id"),
              sample_type = get_metadata_field(meta, "sample_type"),
              sample_injection_volume = get_metadata_field(meta, "sample_injection_volume"),
              sample_amount = get_metadata_field(meta, "sample_amount"),
              time_start = NA,
              time_end = NA,
              time_interval = NA,
              time_unit = get_metadata_field(meta, "raw_data_time_units"),
              detector_range = NA,
              detector_y_unit = get_metadata_field(meta, "detector_y_unit"),
              detector_x_unit = get_metadata_field(meta, "raw_data_mass_units"),
              intensity_multiplier = get_metadata_field(meta, "raw_data_intensity_factor"),
              intensity_offset = get_metadata_field(meta, "raw_data_intensity_offset"),
              ms_params = list(n_scans = get_metadata_field(meta, "raw_data_nscans"),
                               ionization_mode = get_metadata_field(meta, "test_ionization_mode"),
                               polarity = get_metadata_field(meta, "test_ionization_polarity"),
                               detector_type = get_metadata_field(meta, "test_detector_type")),
              source_file = ifelse(missing(source_file), NA, source_file),
              source_file_format = get_metadata_field(meta, "source_file_format"),
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = "chromconverter")
  }, "mdf" = {
    structure(x, instrument = meta[meta$Property == "Instrument", "Value"],
              detector = "Variable Wavelength Detector",
              software = NA,
              method = NA,
              batch = get_metadata_field(meta, "experiment_title"),
              operator = meta[meta$Property == "Operator", "Value"],
              run_datetime = as.POSIXct(
                meta[meta$Property == "Time", "Value"],
                format = "%d.%m.%Y %H:%M:%S", tz = "UTC"),
              sample_name = ifelse(is.null(meta[["sample_name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["sample_name"]]),
              sample_id = get_metadata_field(meta, "sample_id"),
              sample_type = "unknown",
              sample_injection_volume = 1,
              sample_amount = 1,
              time_start = meta[meta$Group == "Interval Time" &
                                  meta$Property == "From", "Value"],
              time_end = meta[meta$Group == "Interval Time" &
                                meta$Property == "To", "Value"],
              time_interval = meta[meta$Group == "Interval Time" &
                                     meta$Property == "Step", "Value"],
              time_unit = meta[meta$Group == "Interval Time" &
                                 meta$Property == "Units", "Value"],
              detector_range = meta[meta$Property == "Wave", "Value"],
              # detector_end = meta[meta$Property == "Wave", "Value"],
              detector_y_unit = meta[meta$Group == "Array photometric" &
                                     meta$Property == "Units", "Value"],
              source_file = ifelse(missing(source_file), NA, source_file),
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = "chromconverter")
  }, "thermoraw" = {
    structure(x, instrument = c(meta$`Instrument model`, meta$`Instrument name`,
                                meta$`Instrument serial number`),
              detector = NA,
              software = meta$`Software version`,
              method = NA,
              batch = NA,
              operator = NA,
              run_date = meta$`Creation date`,
              sample_name = fs::path_ext_remove(basename(meta$`RAW file path`)),
              sample_id = meta$`Sample id`,
              vial = meta$`Sample vial`,
              injection_volume = meta$`Sample injection volume`,
              sample_dilution = meta$`Sample dilution factor`,
              time_range = meta$`Time range`,
              time_interval = meta$`Interval(msec)`,
              source_file = ifelse(missing(source_file), NA, source_file),
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = "long",
              parser = "ThermoRawFileParser"
    )
  }, "default" = {
    structure(x, instrument = meta$Instrument,
              detector = NA,
              software = NA,
              method = meta$Method,
              batch = NA,
              operator = meta$OperatorName,
              run_datetime = meta$AcqTime,
              sample_name = ifelse(is.null(meta[["Sample Name"]]),
                                   fs::path_ext_remove(basename(source_file)),
                                   meta[["Sample Name"]]),
              sample_id = meta$`Sample ID`,
              sample_injection_volume = meta$`Inj Vol`,
              sample_amount = meta$`Inj Vol`,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_y_unit = NA,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = digest::digest(source_file, algo="sha1", file=TRUE),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = ifelse(missing(parser), NA, parser)
              )
  }
 )
}

#' @noRd
get_metadata_field <- function(x, field, num = FALSE, null_val = NA){
  ifelse(!is.null(x[[field]]),
         ifelse(num, as.numeric(x[[field]]), x[[field]]),
         null_val)
}

#' @noRd
get_metadata_field2 <- function(x, field, class = c("char","float","int"),
                                null_val = NA){
  class <- match.arg(class, c("char","float","int"))
  conv <- switch(class, char = as.character, float = as.numeric,
         int = as.integer)
  ifelse(!is.null(attr(x,field)) && !is.na(attr(x,field)),
         conv(attr(x, field)), null_val)
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
read_chemstation_metadata <- function(file, what = c("metadata", "peaktable")){
  what <- match.arg(what, c("metadata", "peaktable"))
  # find xls csv files
  folder <- gsub(basename(file), "", file)
  # check for .D folder
  if (grepl("\\.D/$", folder, ignore.case = TRUE)){
    # find xls/csv
    reps <- list.files(folder, pattern = '.xls',
                      ignore.case = TRUE, full.names = TRUE)
    if (length(reps) > 0){
      if (what == "metadata"){
        meta <- as.data.frame(readxl::read_xls(reps[1], sheet = 1, skip = 1))
        meta2<-as.list(meta$Results)
        names(meta2) <- meta$Title
        meta2
      } else if (what == "peaktable"){
        pktab <- as.data.frame(readxl::read_xls(rep, sheet = "Peak"))
        pktab <- pktab[, -c(1:2)]
        pktab
      }
    }
  }
}

#' @name read_masshunter_metadata
#' @param file file
#' @importFrom xml2 read_xml xml_find_all xml_text
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
      path_devices <- rep[basename(rep) == "Devices.xml"]
      path_sample <- rep[basename(rep) == "sample_info.xml"]
      if (length(path_sample) == 1){
        meta_sample <- xml2::read_xml(path_sample)
        name <- xml2::xml_text(xml2::xml_find_all(meta_sample, xpath = "//Name"))
        meta_sample <- as.list(xml2::xml_text(
          xml2::xml_find_all(meta_sample, xpath = "//Value")
        ))
        names(meta_sample) <- name
      }
      if (length(path_devices) == 1){
        meta_devices <- xml2::read_xml(path_devices)
        name <- xml_text(xml_find_all(meta_devices, xpath = "//Name"))
        meta_devices <- as.character(xml_text(
          xml_find_all(meta_devices, xpath = "//ModelNumber")
        ))
        names(meta_devices) <- name
      }
      meta_sample$Instrument <- meta_devices
    }
  }
  meta_sample
}

#' @name read_waters_metadata
#' @param file file
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_waters_metadata <- function(file){
  ll <- readLines(file, n = 2)
  ll <- iconv(ll,from = "ISO-8859-1", to = "UTF-8")
  meta <- gsub("\\\"", "", do.call(cbind, strsplit(ll, "\t")))
  rownames(meta) <- meta[, 1]
  meta <- as.list(meta[, -1])
}


#' Extract metadata
#'
#' Extract metadata as a \code{data.frame} or \code{tibble} from a list of
#' chromatograms.
#'
#' @param chrom_list A list of chromatograms with attached metadata (as returned
#' by \code{read_chroms} with \code{read_metadata = TRUE}).
#' @param what A character vector specifying the metadata elements to extract.
#' @param format_out Format of object. Either \code{data.frame}, \code{tibble},
#' or \code{data.table}.
#' @return A \code{data.frame}, \code{tibble}, or \code{data.table} (according
#' to the value of \code{format_out}), with samples as rows and the specified
#' metadata elements as columns.
#' @export

extract_metadata <- function(chrom_list,
                             what = c("instrument", "detector", "detector_id",
                                      "software", "method", "batch", "operator",
                                      "run_datetime",
                                      "sample_name", "sample_id",
                                      "injection_volume", "time_range",
                                      "time_interval", "time_unit", "detector_range",
                                      "detector_y_unit", "detector_x_unit",
                                      "intensity_multiplier", "scaled", "source_file",
                                      "source_file_format", "source_sha1",
                                      "data_format", "parser", "format_out"),
                             format_out = c("data.frame", "data.table", "tibble")
                                                  ){
  if (inherits(chrom_list, c("matrix", "data.table", "data.frame"))){
    chrom_list <- list(chrom_list)
    use_names <- FALSE
  } else use_names <- TRUE
  format_out <- match.arg(format_out, c("data.frame", "data.table", "tibble"))
  metadata <- purrr::map_df(chrom_list, function(chrom){
    unlist(sapply(what, function(w){
      attr(chrom, which = w)
    }, simplify = FALSE))
  })
  missing <- what[which(!(what %in% colnames(metadata)))]
  if (nrow(metadata) == 0){
    stop("The specified metadata elements were not found")
  }
  if (length(what) < 25 && length(missing) > 0){
    warning(sprintf("The following metadata elements were not found: %s.",
                    paste(sQuote(missing),collapse = ", ")),immediate. = TRUE)
  }
  if (any(colnames(metadata) == "run_datetime")){
    metadata$run_datetime <- as.POSIXct(as.numeric(metadata$run_datetime), tz = "UTC")
  }
  if (use_names){
    metadata <- tibble::add_column(.data = metadata,
                                   data.frame(name = names(chrom_list)),
                                   .before = TRUE)
  }
  if (format_out == "data.frame"){
    metadata <- as.data.frame(metadata, row.names = NULL)
  } else if (format_out == "data.table"){
    data.table::setDT(metadata)
  }
  metadata
}

#' Transfer metadata
#'@noRd
transfer_metadata <- function (new_object, old_object,
                               exclude = c("names", "row.names",
                                           "class", "dim", "dimnames")){
  a <- attributes(old_object)
  a[exclude] <- NULL
  attributes(new_object) <- c(attributes(new_object), a)
  new_object
}

#' Get 'Shimadzu' Wavelength
#' @noRd
get_sz_wv <- function(meta){
  if ("WVB" %in% names(meta)){
    c(get_metadata_field(meta, "WVB"),
      get_metadata_field(meta, "WVE"))
  } else{
    get_metadata_field(meta, "ADN")
  }
}

#' Extract ASM wavelength from metadata list
#' @author Ethan Bass
#' @noRd
get_asm_wavelength <- function(meta, lab = "absorbance_wavelength_setting.value"){
  wv_idx <- grep(lab, names(meta$`device control aggregate document`))
  unique(unlist(meta$`device control aggregate document`[wv_idx]))
}

#' Convert date-time string to POSIXct
#' @author Ethan Bass
#' @noRd
convert_timestamp <- function(string, datetime_formats){
  tryCatch({
    as.POSIXct(string, tz = "UTC", tryFormats = datetime_formats)
  }, error = function(cond){
    warning("Run date-time could not be converted to POSIXct format, returning string instead.")
    string
  })
}
