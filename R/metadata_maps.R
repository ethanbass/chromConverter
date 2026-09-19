# Metadata field maps ------------------------------------------------------
#
# One function per metadata format, each mapping that format's vendor metadata
# onto chromConverter's vocabulary and returning a named list.
# `finalize_metadata` adds the provenance attributes, so nothing here handles
# `source_sha1`, `parser` or `format_out`, and `chrom_metadata_fields` defines
# the names they may use. `.metadata_maps` in R/metadata_registry.R is what
# reaches them.
#
# They live together rather than beside each reader because they are short and
# uniform: comparing how two formats describe the same field is easiest when
# both are in view.

# --- 'Agilent' ---------------------------------------------------------------

#' Field map for 'ChemStation' and 'OpenLab' binary files
#' @noRd
meta_chemstation <- function(meta, ctx){
  meta$date <- convert_timestamp(meta$date, datetime_formats =
  c("%d-%b-%y, %H:%M:%S", "%m/%d/%Y %I:%M:%S %p",
  "%d/%m/%Y %I:%M:%S %p", "%d %b %y %I:%M %p %z",
  "%d %b %y %I:%M %p"))
  list(sample_name = clean_vendor_string(meta$sample_name),
       sample_position = meta$vial,
       file_version = meta$version,
       file_type = meta$file_type,
       instrument = get_metadata_field(meta, "instrument"),
       detector = get_metadata_field(meta, "detector"),
       detector_model = get_metadata_field(meta, "detector_model"),
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
       intensity_multiplier = meta$intensity_multiplier)
}

#' Field map for 'ChemStation' report (peak list) files
#' @noRd
meta_chemstation_peaklist <- function(meta, ctx){
  list(instrument = meta$`Acq. Instrument`,
       detector = NA,
       software = NA,
       method = meta$Method,
       batch = NA,
       operator = meta$`Acq. Operator`,
       run_datetime = NA,
       sample_name = sample_name_or_file(meta, "Sample Name", ctx$source_file),
       sample_injection_volume = meta$`Inj Volume`,
       sample_amount = NA,
       time_range = NA,
       time_interval = NA,
       time_unit = NA,
       detector_range = NA,
       detector_y_unit = NA)
}

#' Field map for 'MassHunter' `.sp` files
#' @noRd
meta_masshunter_dad <- function(meta, ctx){
  list(instrument = meta$Instrument,
       detector = "DAD",
       software = NA,
       method = meta$Method,
       batch = NA,
       operator = meta$OperatorName,
       run_datetime = convert_timestamp(meta$AcqTime,
                                                 datetime_formats = masshunter_datetime_formats),
       sample_name = sample_name_or_file(meta, "Sample Name", ctx$source_file),
       sample_id = meta$`Sample ID`,
       sample_injection_volume = meta$`Inj Vol`,
       sample_amount = NA,
       time_range = NA,
       time_interval = NA,
       time_unit = NA,
       detector_range = NA,
       detector_y_unit = NA)
}

# --- 'Shimadzu' --------------------------------------------------------------

#' Field map for 'Shimadzu' PDA (3D) ASCII exports
#' @noRd
meta_shimadzu_dad <- function(meta, ctx){
  list(instrument = get_metadata_field(meta, "Instrument Name"),
       detector = "DAD",
       detector_model = get_metadata_field(meta, "Detector Name"),
       software = get_metadata_field(meta, "Application Name"),
       software_version = get_metadata_field(meta, "Version"),
       method = get_metadata_field(meta, "Method File"),
       batch = get_metadata_field(meta, "Batch File"),
       operator = get_metadata_field(meta, "Operator Name"),
       run_datetime = parse_shimadzu_ascii_datetime(meta$Acquired),
       sample_name = sample_name_or_file(meta, "Sample Name", ctx$source_file),
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
       parser = "chromconverter")
}

#' Field map for 'Shimadzu' single-channel ASCII exports
#' @noRd
meta_shimadzu_chrom <- function(meta, ctx){
  list(instrument = get_metadata_field(meta, "Instrument Name"),
       detector_model = get_metadata_field(meta, "Detector Name"),
       software = get_metadata_field(meta, "Application Name"),
       software_version = get_metadata_field(meta, "Version"),
       method = get_metadata_field(meta, "Method File"),
       batch = get_metadata_field(meta, "Batch File"),
       operator = get_metadata_field(meta, "Operator Name"),
       run_datetime = parse_shimadzu_ascii_datetime(meta$Acquired),
       sample_name = sample_name_or_file(meta, "Sample Name", ctx$source_file),
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
       parser = "chromconverter")
}

#' Field map for 'Shimadzu' OLE containers (`.lcd`, `.gcd`, `.qgd`)
#' @noRd
meta_shimadzu_lcd <- function(meta, ctx){
  list(instrument = get_metadata_field(meta, "DSN"),
       detector = get_metadata_field(meta, "DETN"),
       detector_id = get_metadata_field(meta, "DSID"),
       detector_model = sz_detector_model(meta),
       # the channel this trace was read from, which is also what names the
       # peak table that goes with it (`PT-LC.1.1.DET.1.CH#1`)
       channel_id = get_metadata_field(meta, "DSID", null_val = NULL),
       software_version = get_metadata_field(meta, "DataFileProperty.szVersion"),
       file_version = get_metadata_field(meta, "FileProperty.szVersion"),
       method = get_metadata_field(meta, "SampleInfoFile.methodfile"),
       batch = get_metadata_field(meta, "SampleInfoFile.batchfile"),
       operator = get_metadata_field(meta, "SampleInfo.operator_name"),
       run_datetime = as.POSIXct(meta$time_acq, tz = "UTC"),
       sample_name = sample_name_or_file(meta, "SampleInfo.smpl_name", ctx$source_file),
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
       parser = "chromconverter")
}

# --- 'Waters' ----------------------------------------------------------------

#' Field map for 'Waters' ASCII (`.arw`) files
#' @noRd
meta_waters_arw <- function(meta, ctx){
  list(instrument = NA,
       detector = get_metadata_field(meta, "Channel Type"),
       software = get_metadata_field(meta, "Source S/W Info"),
       method = get_metadata_field(meta, "Instrument Method Name"),
       batch = get_metadata_field(meta, "Sample Set Name"),
       operator = NA,
       run_datetime = NA,
       sample_name = sample_name_or_file(meta, "SampleName", ctx$source_file),
       sample_injection_volume = NA,
       sample_amount = NA,
       time_range = c(get_metadata_field(meta, "Data Start"),
                      get_metadata_field(meta, "Data End")),
       time_interval = NA,
       time_unit = NA,
       # an `.arw` names the channel one way or the other depending on how it
       # was exported
       detector_range = if ("Channel Description" %in% names(meta))
         get_metadata_field(meta, "Channel Description") else
         get_metadata_field(meta, "Channel"),
       detector_y_unit = get_metadata_field(meta, "Det. Units"),
       parser = "chromconverter")
}

#' Field map for 'Waters' `.raw` directories
#' @noRd
meta_waters_raw <- function(meta, ctx){
  list(instrument = get_metadata_field(meta, "Instrument"),
       detector = NA,
       software = NA,
       method = NA,
       batch = NA,
       operator = get_metadata_field(meta, "User_Name"),
       run_datetime = as.POSIXct(paste(meta$Acquired_Date, meta$Acquired_Time,
                                       collapse = " "),
                                 format = "%d-%b-%Y %I:%M:%S", tz = "UTC"),
       sample_name = sample_name_or_file(meta, "Acquired Name", ctx$source_file),
       sample_injection_volume = NA,
       sample_amount = NA,
       time_range = NA,
       time_interval = NA,
       time_unit = NA,
       detector_range = NA,
       detector_y_unit = get_metadata_field(meta, "Detector_Unit"),
       parser = "chromconverter")
}

# --- 'Chromeleon', 'Chromatotec', 'Lumex' and 'Varian' -----------------------

#' Field map for 'Chromeleon' ASCII exports
#' @noRd
meta_chromeleon <- function(meta, ctx){
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
  list(instrument = NA,
       detector = meta$Detector,
       software = meta$`Generating Data System`,
       method = meta$`Instrument Method`,
       batch = meta$Sequence,
       operator = meta$`Operator`,
       run_datetime = datetime,
       sample_name = sample_name_or_file(meta, "Name", ctx$source_file),
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
       parser = "chromconverter")
}

#' Field map for 'Chromatotec' `.Chrom` files
#' @noRd
meta_chromatotec <- function(meta, ctx){
  list(sample_name = meta$Description,
       sample_id = meta$SubstanceTableName,
       file_version = meta$version,
       file_type = NA,
       instrument = meta$Serial_no,
       detector = meta$detector,
       detector_model = meta$detector_model,
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
       intensity_multiplier = NA)
}

#' Field map for 'Lumex' MDF files
#' @noRd
meta_mdf <- function(meta, ctx){
  list(instrument = meta[meta$Property == "Instrument", "Value"],
       detector = "Variable Wavelength Detector",
       software = NA,
       method = NA,
       batch = get_metadata_field(meta, "experiment_title"),
       operator = meta[meta$Property == "Operator", "Value"],
       run_datetime = as.POSIXct(
                meta[meta$Property == "Time", "Value"],
                format = "%d.%m.%Y %H:%M:%S", tz = "UTC"),
       sample_name = sample_name_or_file(meta, "sample_name", ctx$source_file),
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
       detector_y_unit = meta[meta$Group == "Array photometric" &
                                     meta$Property == "Units", "Value"],
       parser = "chromconverter")
}

#' Field map for 'Varian' SMS files
#' @noRd
meta_varian_sms <- function(meta, ctx){
  meta$max_ionization_time <- sapply(meta$segment_metadata, function(x){
  x$max_ionization_time
  })
  # a file whose header lists no segments leaves these empty
  segment_start <- unlist(lapply(meta$segment_metadata, function(x) x$start_time))
  segment_end <- unlist(lapply(meta$segment_metadata, function(x) x$end_time))
  list(sample_name = sample_name_or_file(meta, "sample_name", ctx$source_file),
       instrument = get_metadata_field(meta, "instrument"),
       detector = "MS",
       detector_model = NA,
       software = get_metadata_field(meta, "software"),
       software_version = get_metadata_field(meta, "version"),
       method = get_metadata_field(meta, "method"),
       batch = NA,
       operator = NA,
       run_datetime = get_metadata_field(meta, "acquisition_start"),
       sample_injection_volume = NA,
       sample_amount = NA,
       time_range = if (length(segment_start) == 0) NA else
         c(min(segment_start), max(segment_end)),
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
       parser = "chromconverter")
}

# --- open interchange formats ------------------------------------------------

#' Field map for Allotrope Simple Model (ASM) documents
#' @noRd
meta_asm <- function(meta, ctx){
  list(sample_name = meta$`sample document`$written_name,
       sample_id = meta$`sample document`$sample_identifier,
       file_version = meta$file_version,
       file_type = NA,
       instrument = meta$`asset management identifier`,
       detector_model = meta$detector_model_number,
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
       time_unit = meta$time_unit,
       intensity_multiplier = NA)
}

#' Field map for ANDI chromatography netCDF files
#' @noRd
meta_andi_chrom <- function(meta, ctx){
  list(instrument = NA,
       detector = get_metadata_field(meta, "detector"),
       detector_model = get_metadata_field(meta, "detector_name"),
       software = NA,
       method = get_metadata_field(meta, "instrument_method_filename"),
       batch = get_metadata_field(meta, "experiment_title"),
       operator = get_metadata_field(meta, "operator_name"),
       run_datetime = as.POSIXct(
                get_metadata_field(meta, "injection_date_time_stamp"),
                                        format = "%Y%m%d%H%M%S%z", tz = "UTC"),
       sample_name = sample_name_or_file(meta, "sample_name", ctx$source_file),
       sample_id = get_metadata_field(meta, "sample_id"),
       sample_type = get_metadata_field(meta, "sample_type"),
       sample_injection_volume = get_metadata_field(meta, "sample_injection_volume"),
       sample_amount = get_metadata_field(meta, "sample_amount"),
       time_range = NA,
       time_interval = NA,
       time_unit = get_metadata_field(meta, "retention_unit"),
       detector_range = get_metadata_field(meta, "detector_method_comments"),
       detector_y_unit = get_metadata_field(meta, "detector_unit"),
       detector_x_unit = NA,
       parser = "chromconverter")
}

#' Field map for ANDI MS netCDF files
#' @noRd
meta_andi_ms <- function(meta, ctx){
  list(instrument = NA,
       detector = get_metadata_field(meta, "detector"),
       detector_model = get_metadata_field(meta, "detector_name"),
       software = NA,
       method = NA,
       batch = get_metadata_field(meta, "experiment_title"),
       operator = get_metadata_field(meta, "operator_name"),
       run_datetime = as.POSIXct(
                get_metadata_field(meta, "experiment_date_time_stamp"),
                format = "%Y%m%d%H%M%S%z", tz = "UTC"),
       sample_name = sample_name_or_file(meta, "sample_name", ctx$source_file),
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
       parser = "chromconverter")
}

#' Field map for mzML files read by 'RaMS'
#' @noRd
meta_mzml <- function(meta, ctx){
  # `meta` comes from `rams_meta_to_list`, which flattens the one-row table
  # that `RaMS::grabMSdata` returns. `timestamp` is already a UTC `POSIXct`,
  # so it is not routed through `convert_timestamp`. `time_unit` is fixed
  # rather than read from the file: `RaMS:::grabSpectraRt` divides by 60
  # unless the file says minutes, so the values it returns are always minutes
  # whatever the file declared.
  list(sample_name = fs::path_ext_remove(basename(ctx$source_file)),
       sample_id = NA,
       file_version = NA,
       file_type = NA,
       instrument = NA,
       detector = NA,
       detector_model = NA,
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
       intensity_multiplier = NA)
}

# --- external parsers --------------------------------------------------------

#' Field map for the metadata file written by ThermoRawFileParser
#'
#' `read_thermoraw` reads the mzML the parser writes and then annotates it from
#' the `-metadata.txt` file written alongside it, so this map is applied on top
#' of `meta_mzml`. The two overlap but neither is redundant: the mzML holds the
#' instrument details, but 'RaMS' does not surface them, while the text file does
#' not carry the wavelength range or polarity that it does.
#'
#' Twelve fields are set by both maps, so fields are read with `meta$x` rather
#' than `get_metadata_field`: one the text file omits yields `NULL`, which
#' `finalize_metadata` drops, leaving the value `meta_mzml` set. Reading it as
#' `NA` would overwrite that value instead.
#' @noRd
meta_thermoraw <- function(meta, ctx){
  # `meta$x`, not `get_metadata_field`: a field the text file omits yields `NULL`,
  # which `finalize_metadata` drops, leaving whatever [meta_mzml] set. Reading
  # it as `NA` instead would overwrite that value. Fields the text file never
  # carries are omitted entirely, for the same reason.
  raw_path <- meta$`RAW file path`
  list(instrument = c(meta$`Instrument model`, meta$`Instrument name`,
                      meta$`Instrument serial number`),
       software = meta$`Software version`,
       run_datetime = if (!is.null(meta$`Creation date`))
         convert_timestamp(meta$`Creation date`,
                           datetime_formats = c("%m/%d/%Y %H:%M:%S",
                                                "%d/%m/%Y %H:%M:%S")),
       sample_name = if (!is.null(raw_path))
         fs::path_ext_remove(basename(raw_path)),
       sample_id = meta$`Sample id`,
       sample_position = meta$`Sample vial`,
       sample_injection_volume = meta$`Sample injection volume`,
       sample_dilution = meta$`Sample dilution factor`,
       time_range = meta$`Time range`,
       time_interval = meta$`Interval(msec)`,
       parser = "ThermoRawFileParser")
}

#' Field map for files read by the 'rainbow' parser
#' @noRd
meta_rainbow <- function(meta, ctx){
  meta$date <- convert_timestamp(meta$date, datetime_formats =
  c("%d %b %y %I:%M %p %z", "%d-%b-%Y %H:%M:%S",
  "%d-%b-%y, %H:%M:%S", "%d %b %y %I:%M %p"))
  list(sample_name = sample_name_or_file(meta, "notebook", ctx$source_file),
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
       intensity_multiplier =  NA)
}

#' Field map used when a reader names no format of its own
#' @noRd
meta_default <- function(meta, ctx){
  list(instrument = meta$Instrument,
       detector = NA,
       software = NA,
       method = meta$Method,
       batch = NA,
       operator = meta$OperatorName,
       run_datetime = convert_timestamp(meta$AcqTime,
                                               datetime_formats = masshunter_datetime_formats),
       sample_name = sample_name_or_file(meta, "Sample Name", ctx$source_file),
       sample_id = meta$`Sample ID`,
       sample_injection_volume = meta$`Inj Vol`,
       sample_amount = NA,
       time_range = NA,
       time_interval = NA,
       time_unit = NA,
       detector_range = NA,
       detector_y_unit = NA)
}

#' Field map for 'ChemStation' CSV exports
#'
#' A CSV export carries no metadata beyond the file name, so every field is
#' `NA`, so that `extract_metadata` reports an empty column rather than
#' omitting it.
#' @noRd
meta_chemstation_csv <- function(meta, ctx){
  utils::modifyList(empty_metadata(),
                    list(sample_name = fs::path_ext_remove(
                      basename(ctx$source_file))))
}
