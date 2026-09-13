#' Attaches metadata to chromatogram
#'
#' @name attach_metadata
#' @param x chromatogram
#' @param meta List object containing metadata.
#' @param format_in Chromatogram format
#' @param format_out R format. Either `matrix` or `data.frame`.
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
                sample_position = meta$vialpos,
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
                run_datetime = get_metadata_field(meta, "date"),
                sample_injection_volume =  NA,
                sample_amount =  NA,
                time_range =  NA,
                time_interval = NA,
                time_unit =  "Minutes",
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
                instrument = get_metadata_field(meta, "instrument"),
                detector = "MS",
                detector_id = NA,
                software_name = get_metadata_field(meta, "software"),
                software_version = get_metadata_field(meta, "version"),
                method = get_metadata_field(meta, "method"),
                batch = NA,
                operator = NA,
                run_datetime = get_metadata_field(meta, "acquisition_start"),
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
              sample_position = NA,
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
              sample_position = NA,
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
              sample_position = get_metadata_field(meta, 'SampleInfo.smpl_vial'),
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
        datetime <- as.POSIXct(strptime(datetime,
                                        format = "%d/%m/%Y %H:%M:%S %z"),
                               tz = "UTC")
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
              sample_position = meta$Position,
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
                          "%d/%m/%Y %I:%M:%S %p", "%d %b %y %I:%M %p %z",
                          "%d %b %y %I:%M %p"))
    structure(x, sample_name = clean_vendor_string(meta$sample_name),
              sample_position = meta$vial,
              file_version = meta$version,
              file_type = meta$file_type,
              instrument = get_metadata_field(meta, "instrument"),
              detector = get_metadata_field(meta, "detector"),
              detector_id = get_metadata_field(meta, "detector_model"),
              detector_range = get_metadata_field(meta, "signal"),
              signal_descriptor = get_metadata_field(meta, "signal_desc"),
              detector_y_unit = meta$units,
              detector_x_unit = meta$detector_x_unit,
              software = meta$software,
              software_version = meta$software_version,
              software_revision = meta$software_revision,
              method = meta$method,
              batch = meta$SeqPathAndFile,
              operator = meta$operator,
              run_datetime = get_metadata_field(meta, "date"),
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
                run_datetime = convert_timestamp(meta$AcqTime,
                                                 datetime_formats = masshunter_datetime_formats),
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
              method = get_metadata_field(meta, "instrument_method_filename"),
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
              detector_range = get_metadata_field(meta, "detector_method_comments"),
              # detector_end = NA,
              detector_y_unit = get_metadata_field(meta, "detector_unit"),
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
              source_sha1 = digest::digest(source_file, algo = "sha1",
                                           file = TRUE),
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
              run_datetime = convert_timestamp(meta$`Creation date`,
                                               datetime_formats =
                                                 c("%m/%d/%Y %H:%M:%S",
                                                   "%d/%m/%Y %H:%M:%S")),
              sample_name = fs::path_ext_remove(basename(meta$`RAW file path`)),
              sample_id = meta$`Sample id`,
              sample_position = meta$`Sample vial`,
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
  }, "chromatotec" = {
    structure(x, sample_name = meta$Description,
              sample_id = meta$SubstanceTableName,
              file_version = meta$version,
              file_type = NA,
              instrument = meta$Serial_no,
              detector = meta$detector,
              detector_id = meta$detector_model,
              detector_range = NA,
              detector_y_unit = NA,
              detector_x_unit = NA,
              software = NA,
              software_version = NA,
              software_revision = NA,
              method = meta$Method,
              batch = NA,
              operator = meta$Operator,
              run_datetime = NA,
              sample_injection_volume = NA,
              sample_amount = NA,
              time_range = c(0, meta$Sampling_duration),
              time_interval = (1/meta$Sampling_rate),
              time_unit = "Seconds",
              intensity_multiplier = NA,
              scaled = scale,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = digest::digest(source_file, algo = "sha1",
                                           file = TRUE),
              data_format = data_format,
              parser = parser,
              format_out = format_out)
  }, "default" = {
    structure(x, instrument = meta$Instrument,
              detector = NA,
              software = NA,
              method = meta$Method,
              batch = NA,
              operator = meta$OperatorName,
              run_datetime = convert_timestamp(meta$AcqTime,
                                               datetime_formats = masshunter_datetime_formats),
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

#' Clean a string decoded from a vendor file
#'
#' Some vendor metadata strings are Latin-1 (the `method` field of a 'Shimadzu'
#' `.qgd` file) and some carry embedded control bytes (the same field of an
#' 'OpenLab' 131 `.uv` file contains `\032`). Left alone they produce strings
#' for which `validUTF8()` is `FALSE`, so `nchar()` and `toupper()` error and
#' `grepl()` warns and fails to match.
#'
#' Only strings that are not already valid UTF-8 are re-encoded: some fields
#' (e.g. the `units` of an 'Agilent' `.dx` instrument channel, `"\u00b0C"`)
#' are UTF-8 as read, and treating those as Latin-1 would mojibake them. The
#' re-encoding has to come first either way, because `gsub` errors on a string
#' that is not yet valid in the current encoding.
#' @noRd
clean_vendor_string <- function(x){
  if (!is.character(x)) return(x)
  broken <- !is.na(x) & !validUTF8(x)
  x[broken] <- iconv(x[broken], from = "ISO-8859-1", to = "UTF-8")
  gsub("[[:cntrl:]]", "", x)
}

#' Get a metadata field
#'
#' Returns `null_val` when the field is absent or empty, and otherwise the
#' value unchanged. Note: this deliberately avoids `ifelse()`, which returns a
#' value shaped and typed like its *test* -- that silently truncated
#' multi-element fields (e.g. a `c(250, 600)` detector range) to their first
#' element and dropped attributes such as the class of a `POSIXct`.
#' @noRd
get_metadata_field <- function(x, field, num = FALSE, null_val = NA){
  val <- x[[field]]
  if (is.null(val) || length(val) == 0){
    return(null_val)
  }
  if (num) as.numeric(val) else val
}

#' @noRd
get_metadata_field2 <- function(x, field, class = c("char","float","int"),
                                null_val = NA){
  class <- match.arg(class, c("char","float","int"))
  conv <- switch(class, char = as.character, float = as.numeric,
         int = as.integer)
  ifelse(!is.null(attr(x, field)) && !is.na(attr(x, field)),
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
#' @param what Whether to return `metadata` or `peaktable`.
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
#' Extract metadata as a `data.frame`, `data.table` or `tibble` from a list of
#' chromatograms.
#'
#' @param chrom_list A list of chromatograms with attached metadata (as returned
#' by `read_chroms` with `read_metadata = TRUE`).
#' @param what A character vector specifying the metadata elements to extract.
#' @param detector A character vector of detectors to include (e.g. `"UV"` or
#' `c("UV", "MS")`), matched case-insensitively against each chromatogram's
#' `detector` attribute. Defaults to `NULL`, in which case all chromatograms
#' are included. Useful for lists containing more than one detector per
#' sample.
#' @param format_out Format of object. Either `data.frame`, `data.table` or
#' `tibble`.
#' @return A `data.frame`, `tibble`, or `data.table` (according to the value of
#' `format_out`), with samples as rows and the specified metadata elements as
#' columns.
#' @export
extract_metadata <- function(chrom_list,
                             what = c("instrument", "detector", "detector_id",
                                      "software", "method", "batch", "operator",
                                      "run_datetime", "sample_position",
                                      "sample_name", "sample_id",
                                      "injection_volume", "time_range",
                                      "time_interval", "time_unit", "detector_range",
                                      "detector_y_unit", "detector_x_unit",
                                      "intensity_multiplier", "scaled", "source_file",
                                      "source_file_format", "source_sha1",
                                      "data_format", "parser", "format_out"),
                             detector = NULL,
                             format_out = c("data.frame", "data.table", "tibble")
){
  if (inherits(chrom_list, c("matrix", "data.table", "data.frame"))){
    chrom_list <- list(chrom_list)
    use_names <- FALSE
  } else use_names <- TRUE
  chrom_list <- flatten_chrom_list(chrom_list)
  if (!is.null(detector)){
    chrom_list <- filter_by_detector(chrom_list, detector)
  }
  format_out <- match.arg(format_out, c("data.frame", "data.table", "tibble"))
  metadata <- purrr::imap_dfr(chrom_list, function(chrom, name){
    c(name = name, unlist(sapply(what, function(w){
      val <- attr(chrom, which = w, exact = TRUE)
      # `run_datetime` is expected to be a single timestamp, but a parser may
      # attach more than one. `unlist` would split a pair into
      # `run_datetime1`/`run_datetime2`, which then miss the POSIXct conversion
      # below and print as raw epoch seconds.
      if (w == "run_datetime" && length(val) > 1) val <- val[1]
      val
    }, simplify = FALSE)))
  })
  missing <- what[which(!(what %in% colnames(metadata)))]
  if (nrow(metadata) == 0){
    stop("The specified metadata elements were not found")
  }
  if (length(what) < 25 && length(missing) > 0){
    warning(sprintf("The following metadata elements were not found: %s.",
                    paste(sQuote(missing),collapse = ", ")),immediate. = TRUE)
  }
  if (use_names && ncol(metadata) == 1) {
    return(NA)
  }
  if (any(colnames(metadata) == "run_datetime")){
    metadata$run_datetime <- as.POSIXct(as.numeric(metadata$run_datetime),
                                        tz = "UTC")
  }
  if (!use_names){
    metadata <- metadata[,-1]
  }
  if (format_out == "data.frame"){
    metadata <- as.data.frame(metadata, row.names = NULL)
  } else if (format_out == "data.table"){
    data.table::setDT(metadata)
  }
  metadata
}

#' Enumerate the individual chromatograms in a (possibly nested) list
#'
#' A `chrom_list` element can itself be a list of chromatograms, e.g. one entry
#' per detector for a multichannel file, or one entry per data file within each
#' detector when `read_chroms` is called with `collapse = FALSE`. This function
#' walks the list to whatever depth is needed and returns one entry per actual
#' chromatogram, together with the path taken to reach it.
#'
#' Both `extract_metadata` and `print.chrom_list` are built on this, so that
#' they cannot disagree about how many chromatograms a list contains.
#'
#' @return A list of `list(path = <character vector>, chrom = <object>)`.
#' @noRd
chrom_list_leaves <- function(x, path = character()){
  # elements a parser returns alongside the traces without being traces
  # themselves (e.g. the `metadata` table from `read_mzml`)
  if (inherits(x, "chromconverter_metadata")) return(list())
  if (!is.list(x) || inherits(x, c("matrix", "data.table", "data.frame"))){
    return(list(list(path = path, chrom = x)))
  }
  if (length(x) == 0) return(list())
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  nms[!nzchar(nms)] <- seq_along(x)[!nzchar(nms)]
  unlist(lapply(seq_along(x), function(i){
    chrom_list_leaves(x[[i]], c(path, nms[i]))
  }), recursive = FALSE)
}

#' Flatten a (possibly nested) list of chromatograms
#'
#' Nested chromatograms are named for the path taken to reach them, so a
#' multichannel sample `blue` with a `UV` channel becomes `blue.UV`.
#' @noRd
flatten_chrom_list <- function(x){
  leaves <- chrom_list_leaves(x)
  stats::setNames(lapply(leaves, `[[`, "chrom"),
                  vapply(leaves, function(l) paste(l$path, collapse = "."),
                         character(1)))
}

#' Resolve a sample-level attribute from a chromatogram or a list of them
#'
#' `read_chroms` needs per-sample values like `sample_name` and `run_datetime`,
#' but a sample may be a single chromatogram or a (possibly nested) list of
#' them, and parsers only ever attach metadata to the individual traces. Look on
#' the element itself first -- `read_agilent_rslt` writes acaml attributes
#' directly onto whatever `read_agilent_dx` returned, which may be a list --
#' then fall back to the first leaf that carries the attribute.
#' @noRd
get_sample_attr <- function(x, which){
  val <- attr(x, which, exact = TRUE)
  if (is.null(val)){
    for (leaf in chrom_list_leaves(x)){
      val <- attr(leaf$chrom, which, exact = TRUE)
      if (!is.null(val)) break
    }
  }
  if (length(val) == 0) return(NULL)
  # a parser may attach more than one value; `extract_metadata` takes the first
  # for `run_datetime` and this must agree with it
  val[[1]]
}

#' Subset a list of chromatograms by detector
#'
#' Matches against the `detector` attribute rather than the name of the list
#' element, since the two do not always agree: the `rainbow` parser happens to
#' name its sub-lists after the detector (`MS`, `UV`, `CAD`), but other parsers
#' name them after the kind of data (e.g. `pda`, `tic`, `chroms`).
#'
#' Chromatograms with no `detector` attribute cannot match and are dropped.
#' Called by `extract_metadata`.
#' @noRd
filter_by_detector <- function(chrom_list, detector){
  detectors <- lapply(chrom_list, attr, "detector", exact = TRUE)
  keep <- vapply(detectors, function(x){
    !is.null(x) && any(tolower(x) %in% tolower(detector))
  }, logical(1))
  if (!any(keep)){
    found <- unique(unlist(detectors))
    stop(sprintf(paste0("No chromatograms were found for the requested ",
                        "detector(s): %s.\n%s"),
                 paste(sQuote(detector), collapse = ", "),
                 if (length(found) == 0)
                   "The chromatograms do not have a 'detector' attribute." else
                   sprintf("The following detector(s) are present: %s.",
                           paste(sQuote(found), collapse = ", "))),
         call. = FALSE)
  }
  chrom_list[keep]
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

#' Date-time formats used by 'MassHunter' (`sample_info.xml`)
#'
#' Note the literal `Z` rather than `%z`, which does not accept the military
#' `Z` designator for UTC.
#' @noRd
masshunter_datetime_formats <- c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%OS",
                                 "%m/%d/%Y %I:%M:%S %p")

#' Convert date-time string to POSIXct
#' @author Ethan Bass
#' @noRd
convert_timestamp <- function(string, datetime_formats){
  if (length(string) == 0 || all(is.na(string))){
    return(.POSIXct(NA_real_, tz = "UTC"))
  }
  tryCatch({
    as.POSIXct(string, tz = "UTC", tryFormats = datetime_formats)
  }, error = function(cond){
    warning("Run date-time could not be converted to POSIXct format, returning string instead.")
    string
  })
}

#' Attach metadata minimal
#'
#' Attaches metadata from chromConverter function call and SHA Hashsum
#' without providing a metadata object.
#'
#' @noRd

attach_metadata_minimal <- function(x, format_out, data_format,
                                    parser = NULL, source_file,
                                    source_file_format = NA,
                                    scale = NULL){
  structure(x, instrument = NA,
            detector = NA,
            software = NA,
            method = NA,
            batch = NA,
            operator = NA,
            run_datetime = NA,
            sample_name = fs::path_ext_remove(basename(source_file)),
            sample_injection_volume = NA,
            sample_amount = NA,
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
}
