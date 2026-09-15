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
  switch(format_in,
    "raw" = {
      structure(x, metadata = meta, data_format = data_format, parser = parser,
                source_file = source_file,
                source_sha1 = source_sha1(source_file))
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
                  source_sha1 = source_sha1(source_file),
                  data_format = data_format,
                  parser = parser,
                  format_out = format_out)
    }, "rainbow" = {
      meta$date <- convert_timestamp(meta$date, datetime_formats =
                          c("%d %b %y %I:%M %p %z", "%d-%b-%Y %H:%M:%S",
                            "%d-%b-%y, %H:%M:%S", "%d %b %y %I:%M %p"))
      structure(x,
                sample_name = sample_name_or_file(meta, "notebook", source_file),
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
                source_sha1 = source_sha1(source_file),
                data_format = data_format,
                parser = parser,
                format_out = format_out)
    }, "varian_sms" = {
      meta$max_ionization_time <- sapply(meta$segment_metadata, function(x){
        x$max_ionization_time
      })
      segment_start <- sapply(meta$segment_metadata, function(x) x$start_time)
      segment_end <- sapply(meta$segment_metadata, function(x) x$end_time)
      structure(x,
                sample_name = sample_name_or_file(meta, "sample_name", source_file),
                instrument = get_metadata_field(meta, "instrument"),
                detector = "MS",
                detector_id = NA,
                software = get_metadata_field(meta, "software"),
                software_version = get_metadata_field(meta, "version"),
                method = get_metadata_field(meta, "method"),
                batch = NA,
                operator = NA,
                run_datetime = get_metadata_field(meta, "acquisition_start"),
                sample_injection_volume = NA,
                sample_amount = NA,
                # an SMS file is acquired in segments, so the per-segment
                # bounds go in `ms_params` and `time_range` reports the span of
                # the whole run, as it does for every other format
                time_range = c(min(segment_start), max(segment_end)),
                n_scans = meta$n_scan,
                ms_params = c(meta[c("ion_time", "emission_current",
                                     "max_ric_scan", "max_ric_val",
                                     "max_ionization_time", "temp_trap",
                                     "temp_manifold", "temp_transferline",
                                     "axial_modulation")],
                              list(segment_start_time = segment_start,
                                   segment_end_time = segment_end)),
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
                source_sha1 = source_sha1(source_file),
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
                sample_name = sample_name_or_file(meta, "SampleName", source_file),
                sample_injection_volume = NA,
                sample_amount = NA,
                time_range = c(get_metadata_field(meta, "Data Start"),
                               get_metadata_field(meta, "Data End")),
                time_interval = NA,
                time_unit = NA,
                detector_range = if ("Channel Description" %in% names(meta))
                                   get_metadata_field(meta, "Channel Description") else
                                   get_metadata_field(meta, "Channel"),
                detector_y_unit = get_metadata_field(meta, "Det. Units"),
                source_file = source_file,
                source_file_format = source_file_format,
                source_sha1 = source_sha1(source_file),
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
              sample_name = sample_name_or_file(meta, "Acquired Name", source_file),
              sample_injection_volume = NA,
              sample_amount = NA,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_y_unit = get_metadata_field(meta, "Detector_Unit"),
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = source_sha1(source_file),
              data_format = data_format,
              parser = "chromconverter",
              format_out = format_out)
  }, "shimadzu_dad" = {
    structure(x,
              instrument = get_metadata_field(meta, "Instrument Name"),
              detector = "DAD",
              detector_id = get_metadata_field(meta, "Detector Name"),
              software = get_metadata_field(meta, "Application Name"),
              software_version = get_metadata_field(meta, "Version"),
              method = get_metadata_field(meta, "Method File"),
              batch = get_metadata_field(meta, "Batch File"),
              operator = get_metadata_field(meta, "Operator Name"),
              run_datetime = as.POSIXct(meta$Acquired,
                                        format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"),
              sample_name = sample_name_or_file(meta, "Sample Name", source_file),
              sample_id = get_metadata_field(meta, "Sample ID"),
              sample_position = NA,
              sample_injection_volume = get_metadata_field(meta, "Injection Volume"),
              sample_amount = NA,
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
              source_sha1 = source_sha1(source_file),
              data_format = data_format,
              parser = "chromconverter",
              format_out = format_out)
  }, "shimadzu_chrom" = {
    structure(x,
              instrument = get_metadata_field(meta, "Instrument Name"),
              detector_id = get_metadata_field(meta, "Detector Name"),
              software = get_metadata_field(meta, "Application Name"),
              software_version = get_metadata_field(meta, "Version"),
              method = get_metadata_field(meta, "Method File"),
              batch = get_metadata_field(meta, "Batch File"),
              operator = get_metadata_field(meta, "Operator Name"),
              run_datetime = as.POSIXct(meta$Acquired,
                                        format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"),
              sample_name = sample_name_or_file(meta, "Sample Name", source_file),
              sample_id = get_metadata_field(meta, "Sample ID"),
              sample_position = NA,
              sample_injection_volume = get_metadata_field(meta, "Injection Volume"),
              sample_amount = NA,
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
              source_sha1 = source_sha1(source_file),
              data_format = data_format,
              parser = "chromconverter",
              format_out = format_out)
  },  "shimadzu_lcd" = {
    structure(x,
              instrument = get_metadata_field(meta, "DSN"),
              detector = get_metadata_field(meta, "DETN"),
              detector_id = get_metadata_field(meta, "DSID"),
              # software = get_metadata_field(meta, "Application Name"),
              software_version = get_metadata_field(meta, "DataFileProperty.szVersion"),
              method = get_metadata_field(meta, "SampleInfoFile.methodfile"),
              batch = get_metadata_field(meta, "SampleInfoFile.batchfile"),
              operator = get_metadata_field(meta, "SampleInfo.operator_name"),
              run_datetime = as.POSIXct(meta$time_acq, tz = "UTC"),
              sample_name = sample_name_or_file(meta, "SampleInfo.smpl_name", source_file),
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
             source_sha1 = source_sha1(source_file),
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
    volume_field <- grep("Volume", names(meta), value = TRUE)[1]
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
              sample_name = sample_name_or_file(meta, "Name", source_file),
              sample_position = meta$Position,
              sample_injection_volume = if (is.na(volume_field)) NA else
                                          get_metadata_field(meta, volume_field),
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
              detector_range = if (identical(meta$`Spectral Field`, "3DFIELD"))
                                 c(get_metadata_field(meta, "Scan Min. (nm)"),
                                   get_metadata_field(meta, "Scan Max. (nm)")) else
                                 NA,
              detector_y_unit = meta$`Signal Unit`,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = source_sha1(source_file),
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
              sample_amount = NA,
              time_range = meta$time_range,
              time_interval = NA,
              time_unit = "Minutes",
              intensity_multiplier = meta$intensity_multiplier,
              scaled = scale,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = source_sha1(source_file),
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
              sample_name = sample_name_or_file(meta, "Sample Name", source_file),
              sample_injection_volume = meta$`Inj Volume`,
              sample_amount = NA,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_y_unit = NA,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = source_sha1(source_file),
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
                sample_name = sample_name_or_file(meta, "Sample Name", source_file),
                sample_id = meta$`Sample ID`,
                sample_injection_volume = meta$`Inj Vol`,
                sample_amount = NA,
                time_range = NA,
                time_interval = NA,
                time_unit = NA,
                detector_range = NA,
                detector_y_unit = NA,
                source_file = source_file,
                source_file_format = source_file_format,
                source_sha1 = source_sha1(source_file),
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
              sample_name = sample_name_or_file(meta, "sample_name", source_file),
              sample_id = get_metadata_field(meta, "sample_id"),
              sample_type = get_metadata_field(meta, "sample_type"),
              sample_injection_volume = get_metadata_field(meta, "sample_injection_volume"),
              sample_amount = get_metadata_field(meta, "sample_amount"),
              time_range = NA,
              time_interval = NA,
              time_unit = get_metadata_field(meta, "retention_unit"),
              detector_range = get_metadata_field(meta, "detector_method_comments"),
              # detector_end = NA,
              detector_y_unit = get_metadata_field(meta, "detector_unit"),
              detector_x_unit = NA,
              source_file = source_file,
              source_sha1 = source_sha1(source_file),
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
              sample_name = sample_name_or_file(meta, "sample_name", source_file),
              sample_id = get_metadata_field(meta, "sample_id"),
              sample_type = get_metadata_field(meta, "sample_type"),
              sample_injection_volume = get_metadata_field(meta, "sample_injection_volume"),
              sample_amount = get_metadata_field(meta, "sample_amount"),
              time_range = NA,
              time_interval = NA,
              time_unit = get_metadata_field(meta, "raw_data_time_units"),
              detector_range = NA,
              detector_y_unit = get_metadata_field(meta, "detector_y_unit"),
              detector_x_unit = get_metadata_field(meta, "raw_data_mass_units"),
              intensity_multiplier = get_metadata_field(meta, "raw_data_intensity_factor"),
              intensity_offset = get_metadata_field(meta, "raw_data_intensity_offset"),
              n_scans = get_metadata_field(meta, "raw_data_nscans"),
              ms_params = list(ionization_mode = get_metadata_field(meta, "test_ionization_mode"),
                               polarity = get_metadata_field(meta, "test_ionization_polarity"),
                               detector_type = get_metadata_field(meta, "test_detector_type")),
              source_file = source_file,
              source_file_format = get_metadata_field(meta, "source_file_format"),
              source_sha1 = source_sha1(source_file),
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
              sample_name = sample_name_or_file(meta, "sample_name", source_file),
              sample_id = get_metadata_field(meta, "sample_id"),
              sample_type = "unknown",
              sample_injection_volume = NA,
              sample_amount = NA,
              time_range = c(meta[meta$Group == "Interval Time" &
                                    meta$Property == "From", "Value"],
                             meta[meta$Group == "Interval Time" &
                                    meta$Property == "To", "Value"]),
              time_interval = meta[meta$Group == "Interval Time" &
                                     meta$Property == "Step", "Value"],
              time_unit = meta[meta$Group == "Interval Time" &
                                 meta$Property == "Units", "Value"],
              detector_range = meta[meta$Property == "Wave", "Value"],
              # detector_end = meta[meta$Property == "Wave", "Value"],
              detector_y_unit = meta[meta$Group == "Array photometric" &
                                     meta$Property == "Units", "Value"],
              source_file = source_file,
              source_sha1 = source_sha1(source_file),
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
              # `get_metadata_field` rather than `meta$x`: this branch stamps
              # over the `"mzml"` one, since `read_thermoraw` reads the mzML it
              # exported, and a `NULL` here would delete the field mzml had set
              # rather than leaving it `NA`
              sample_id = get_metadata_field(meta, "Sample id"),
              sample_position = get_metadata_field(meta, "Sample vial"),
              sample_injection_volume = get_metadata_field(
                meta, "Sample injection volume"),
              sample_dilution = get_metadata_field(meta, "Sample dilution factor"),
              time_range = get_metadata_field(meta, "Time range"),
              time_interval = get_metadata_field(meta, "Interval(msec)"),
              source_file = source_file,
              source_sha1 = source_sha1(source_file),
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
              source_sha1 = source_sha1(source_file),
              data_format = data_format,
              parser = parser,
              format_out = format_out)
  }, "mzml" = {
    # `meta` comes from `rams_meta_to_list`, which flattens the one-row table
    # that `RaMS::grabMSdata` returns. `timestamp` is already a UTC `POSIXct`,
    # so it is not routed through `convert_timestamp`. `time_unit` is fixed
    # rather than read from the file: `RaMS:::grabSpectraRt` divides by 60
    # unless the file says minutes, so the values it returns are always minutes
    # whatever the file declared.
    structure(x, sample_name = fs::path_ext_remove(basename(source_file)),
              sample_id = NA,
              file_version = NA,
              file_type = NA,
              instrument = NA,
              detector = NA,
              detector_id = NA,
              detector_range = c(meta$lambda_lowest, meta$lambda_highest),
              detector_y_unit = NA,
              detector_x_unit = NA,
              software = NA,
              software_version = NA,
              software_revision = NA,
              method = NA,
              batch = NA,
              operator = NA,
              run_datetime = meta$timestamp,
              sample_injection_volume = NA,
              sample_amount = NA,
              time_range = c(meta$rt_start, meta$rt_end),
              time_interval = NA,
              time_unit = "Minutes",
              intensity_multiplier = NA,
              scaled = scale,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = source_sha1(source_file),
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
              sample_name = sample_name_or_file(meta, "Sample Name", source_file),
              sample_id = meta$`Sample ID`,
              sample_injection_volume = meta$`Inj Vol`,
              sample_amount = NA,
              time_range = NA,
              time_interval = NA,
              time_unit = NA,
              detector_range = NA,
              detector_y_unit = NA,
              source_file = source_file,
              source_file_format = source_file_format,
              source_sha1 = source_sha1(source_file),
              format_out = ifelse(missing(format_out), NA, format_out),
              data_format = ifelse(missing(data_format), NA, data_format),
              parser = ifelse(missing(parser), NA, parser)
              )
  },
  {
    warning(sprintf(paste("Metadata for the %s format could not be",
                          "interpreted. Returning the data with its source",
                          "file and parser recorded, but without the",
                          "instrument metadata."),
                    sQuote(format_in)), call. = FALSE)
    attach_metadata_minimal(x, format_out = format_out,
                            data_format = data_format, parser = parser,
                            source_file = source_file,
                            source_file_format = source_file_format)
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

#' Sample name recorded by the file, or the file's own name
#'
#' Most formats record a sample name, but not all files fill it in, so the
#' basename of the source file is the fallback. Written out with `ifelse()` in
#' every branch that needed it, which is the hazard described under
#' [get_metadata_field]: the result is shaped like the *test*, so a name that
#' arrived as anything but a length-1 vector was silently truncated.
#' @noRd
sample_name_or_file <- function(meta, field, source_file){
  val <- meta[[field]]
  if (is.null(val) || length(val) == 0){
    return(fs::path_ext_remove(basename(source_file)))
  }
  val
}

#' SHA-1 of the source file
#'
#' `NA` for anything that is not a file, rather than an error. Several formats
#' are directories -- a Waters `.raw`, an 'Agilent' `.D` -- and `digest` errors
#' on those, which each branch worked around differently: `rainbow` guarded with
#' `fs::is_file`, `waters_raw` hardcoded `NA`, and the rest would have failed had
#' they ever been handed one.
#' @noRd
source_sha1 <- function(path){
  if (length(path) != 1 || is.na(path) || !fs::is_file(path)) return(NA)
  digest::digest(path, algo = "sha1", file = TRUE)
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
        pktab <- as.data.frame(readxl::read_xls(reps[1], sheet = "Peak"))
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
  # Both are left `NULL` when the sidecar they come from is absent, so that a
  # path without a `sample_info.xml` returns `NULL` rather than raising
  # "object 'meta_sample' not found". The masshunter field map reads `meta$x`
  # throughout, which is `NULL` either way.
  meta_sample <- NULL
  meta_devices <- NULL
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
      if (!is.null(meta_sample) && !is.null(meta_devices)){
        meta_sample$Instrument <- meta_devices
      }
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
#' @param what A character vector specifying the metadata elements to
#' extract. Defaults to every field chromConverter attaches; no format
#' records all of them, so the elements a format does not provide are
#' simply absent from the result. Superseded names (`injection_volume`,
#' `software_name`, `time_start`) are accepted and mapped to the names
#' that replaced them.
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
                             what = chrom_metadata_fields(),
                             detector = NULL,
                             format_out = c("data.frame", "data.table", "tibble")
){
  defaulted <- identical(what, chrom_metadata_fields())
  what <- resolve_metadata_fields(what)
  if (inherits(chrom_list, c("matrix", "data.table", "data.frame"))){
    chrom_list <- list(chrom_list)
    use_names <- FALSE
  } else use_names <- TRUE
  chrom_list <- flatten_chrom_list(chrom_list, inherit = TRUE)
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
  # only warn about fields the caller actually asked for: the default asks for
  # everything chromConverter can attach, and no format records all of it
  if (!defaulted && length(missing) > 0){
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
#' Each leaf also carries the attributes of the lists it was reached through
#' (`inherited`), since a parser may attach sample-level metadata to the list
#' holding the traces rather than to the traces themselves --
#' `read_agilent_rslt` writes the acaml fields this way.
#'
#' @return A list of
#' `list(path = <character vector>, chrom = <object>, inherited = <list>)`.
#' @noRd
chrom_list_leaves <- function(x, path = character()){
  # elements a parser returns alongside the traces without being traces
  # themselves (e.g. the `metadata` table from `read_mzml`)
  if (inherits(x, "chromconverter_metadata")) return(list())
  if (!is.list(x) || inherits(x, c("matrix", "data.table", "data.frame"))){
    return(list(list(path = path, chrom = x, inherited = list())))
  }
  if (length(x) == 0) return(list())
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  nms[!nzchar(nms)] <- seq_along(x)[!nzchar(nms)]
  # gather the leaves first, so that each list can be asked whether the traces
  # beneath it agree about a field before it offers its own copy of one
  leaves <- unlist(lapply(seq_along(x), function(i){
    chrom_list_leaves(x[[i]], c(path, nms[i]))
  }), recursive = FALSE)
  for (nm in names(list_metadata_attrs(x))){
    val <- attr(x, nm, exact = TRUE)
    # a field the list records but reads nothing into is no value at all, and
    # must not displace one the traces do have
    if (is.null(usable_attr(val))) next
    # a field the traces disagree about describes the trace rather than the
    # sample -- `detector` in a multichannel file -- so the list's copy of it
    # must not flatten them. Where they agree, the list is the better source:
    # `read_agilent_rslt` has the method's name from the `acaml` file while
    # every trace has the path to the method file from its own `.dx`.
    vals <- unique(Filter(Negate(is.null), lapply(leaves, function(l){
      usable_attr(attr(l$chrom, nm, exact = TRUE))
    })))
    if (length(vals) > 1) next
    for (i in seq_along(leaves)){
      # an inner list is the more specific description of the traces beneath
      # it, so it wins over an outer one
      if (is.null(leaves[[i]]$inherited[[nm]])){
        leaves[[i]]$inherited[[nm]] <- val
      }
    }
  }
  leaves
}

#' Attributes of a list of chromatograms that describe the data
#'
#' Everything except the bookkeeping attributes that say how the list itself is
#' put together.
#' @noRd
list_metadata_attrs <- function(x){
  a <- attributes(x)
  # beyond the structural attributes, `comment` and the acaml table describe the
  # list as a whole; copying either onto every trace beneath it would be
  # meaningless, and the acaml table is a data.frame per injection
  a[!(names(a) %in% c(bookkeeping_attrs(), "comment", "acaml_metadata"))]
}

#' Flatten a (possibly nested) list of chromatograms
#'
#' Nested chromatograms are named for the path taken to reach them, so a
#' multichannel sample `blue` with a `UV` channel becomes `blue.UV`.
#'
#' @param inherit Whether to apply the attributes of the enclosing lists to
#' each chromatogram. `chrom_list_leaves` decides which may be applied: a list
#' describes the sample, so where its traces agree about a field it is the
#' better source (`read_agilent_rslt` has the method's name from the `acaml`
#' file, while every trace has the path to the method file), but where they
#' disagree the field belongs to the trace and is left alone.
#' @noRd
flatten_chrom_list <- function(x, inherit = FALSE){
  leaves <- chrom_list_leaves(x)
  chroms <- lapply(leaves, function(l){
    chrom <- l$chrom
    if (inherit){
      # `chrom_list_leaves` has already dropped the values that must not be
      # applied, so what is left describes the sample better than the trace does
      for (nm in names(l$inherited)) attr(chrom, nm) <- l$inherited[[nm]]
    }
    chrom
  })
  stats::setNames(chroms,
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
#' then fall back to the first leaf that carries a usable value.
#' @noRd
get_sample_attr <- function(x, which){
  vals <- sample_attr_values(x, which, first_only = TRUE)
  if (length(vals) == 0) return(NULL)
  vals[[1]]
}

#' Collect the usable values of a sample-level attribute
#'
#' An attribute on the element itself describes the whole sample, so it is used
#' on its own. Otherwise every leaf is a candidate: with `first_only = TRUE` the
#' search stops at the first usable value (what `get_sample_attr` wants), and
#' otherwise all of them are returned so the caller can check that the traces
#' making up a sample agree.
#'
#' @return A list of length-1 values, empty if the attribute is nowhere to be
#' found.
#' @noRd
sample_attr_values <- function(x, which, first_only = FALSE){
  val <- usable_attr(attr(x, which, exact = TRUE))
  if (!is.null(val)) return(list(val))
  vals <- list()
  for (leaf in chrom_list_leaves(x)){
    val <- usable_attr(attr(leaf$chrom, which, exact = TRUE))
    if (!is.null(val)){
      vals <- c(vals, list(val))
      if (first_only) break
    }
  }
  vals
}

#' Reduce an attribute to a single usable value, or `NULL` if it has none
#' @noRd
usable_attr <- function(val){
  if (length(val) == 0) return(NULL)
  # a parser may attach more than one value; `extract_metadata` takes the first
  # for `run_datetime` and this must agree with it
  val <- val[[1]]
  if (!is.atomic(val) || length(val) != 1 || is.na(val)) return(NULL)
  # a parser that locates the field but reads nothing out of it leaves an empty
  # string behind, which is no more of a name than `NA` is
  if (is.character(val) && !nzchar(trimws(val))) return(NULL)
  val
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
                               exclude = bookkeeping_attrs()){
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
            source_sha1 = source_sha1(source_file),
            data_format = data_format,
            parser = parser,
            format_out = format_out)
}
