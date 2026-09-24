#' Write ANDI MS CDF file from chromatogram
#'
#' Exports mass spectrometry data in ANDI (Analytical Data Interchange) MS
#' format (ASTM E1947-98). A total ion chromatogram is derived from the MS1
#' scans if the supplied object does not already carry one.
#'
#' Retention times are written in seconds, the only time unit the
#' specification suggests, and declared in `raw_data_time_units`. A
#' chromatogram is converted unless its `time_unit` already reports seconds, so
#' one whose unit is missing or unrecognized is taken to be in minutes.
#'
#' @author Ethan Bass
#' @importFrom data.table :=
#' @importFrom data.table setorder
#' @param x A list of chromatograms containing an element whose name includes
#' `MS` (e.g. `MS1`), or a single chromatogram of MS1 scans, in `wide` or
#' `long` format.
#' @param path_out The path to write the file.
#' @param sample_name The name of the file. If a name is not provided, the name
#' will be derived from the `sample_name` attribute.
#' @param force Whether to overwrite existing files at the specified path.
#' Defaults to `FALSE`.
#' @param ms_params A list of settings recorded in the file:
#' `ionization_mode`, `ionization_polarity`, `detector_type`, `scan_function`
#' and `experiment_type`. A setting that is not given is taken from the
#' chromatogram's metadata where it records one: the polarity from its
#' `polarity` attribute, the scan function from `scan_type`, and the
#' ionization mode and detector type from its `ms_params` attribute. Failing
#' that, the defaults are `"Electron Impact"`, `"Positive Polarity"` and
#' `"Electron Multiplier"`, and a scan function is left out of the file.
#' `experiment_type` is not read from metadata and defaults to
#' `"Centroided Mass Spectrum"`.
#' @return Invisibly returns the path to the written CDF file.
#' @section Side effects:
#' Exports mass spectrometry data in ANDI MS format (netCDF) in the directory
#' specified by `path_out`. The file will be named according to the value
#' of `sample_name`. If no `sample_name` is provided, the `sample_name`
#' attribute will be used if it exists.
#' @examples \dontrun{
#' write_andi_ms(chrom, path_out = "path/to/directory")
#' }
#' @family write functions
#' @export
write_andi_ms <- function(x, path_out, sample_name = NULL, force = FALSE,
                          ms_params = list()){
  if (!inherits(x, "list")){
    x <- list(MS1 = x)
  }
    MS_idx <- grep("MS", names(x))
    if (length(MS_idx) == 0){
      stop("MS data could not be found.")
    }
    MS_idx <- if ("MS1" %in% names(x)) "MS1" else names(x)[MS_idx[1]]
    dropped <- setdiff(grep("MS", names(x), value = TRUE), MS_idx)
    if (length(dropped) > 0){
      warning("ANDI MS cannot hold ",
              paste(sQuote(dropped, q = FALSE), collapse = ", "),
              " scans, which were not written. Use `write_mzml` to keep them.",
              call. = FALSE)
    }
    dat <- x[[MS_idx]]
    if (attr(dat, "data_format") == "wide"){
      dat <- reshape_chrom(dat, data_format = "long", names_to = "mz",
                           format_out="data.table", sparse = TRUE)
    }
  if (is.null(sample_name)){
    sample_name <- attr(dat, "sample_name")
    if (is.null(sample_name)){
      stop("Sample name must be provided.")
    }
  }
  file_out <- get_filepath(path_out = path_out, sample_name = sample_name,
                           ext = "cdf", force = force)
  if (is.null(x$TIC)){
    x$TIC <- data.table::as.data.table(dat)[,list(intensity=sum(intensity)),
                                            by = rt]
    x$TIC <- transfer_metadata(x$TIC, x[[MS_idx]])
  }
  x$TIC <- as.data.frame(x$TIC)
  if (ncol(x$TIC) == 1){
    x$TIC <- reshape_chrom_long(x$TIC, format_out = "data.frame")
  }
  # seconds is the only time unit the ANDI MS specification suggests, so the
  # times are converted unless they are already in seconds. An absent or
  # unrecognized unit is taken to be minutes, as elsewhere in the package.
  rt_unit <- attr(dat, "time_unit")
  in_seconds <- length(rt_unit) == 1 && !is.na(rt_unit) &&
    grepl("sec", rt_unit, ignore.case = TRUE)
  if (!in_seconds){
    dat[, "rt"] <- dat[, "rt"]*60
    x$TIC[, "rt"] <- x$TIC[, "rt"]*60
  }
  attr(dat, "time_unit") <- "Seconds"
  if (is.matrix(dat)){
    dat <- as.data.frame(dat)
  }
  dat <- data.table::setDT(dat)
  dat <- dat[order(rt, mz)]

  extra_vals <- nrow(x$TIC) - length(unique(dat[["rt"]]))
  x$TIC$scan_index <- as.integer(c(rep(0, extra_vals),
                                   which(!duplicated(dat[["rt"]])) - 1))
  x$TIC$points <- c(rep(0, extra_vals), table(cumsum(!duplicated(dat[["rt"]]))))
  intensity_format <- ifelse(all(floor(dat$intensity) - dat$intensity == 0),
                             "integer", "float")

  check_for_pkg("ncdf4")

  if (is.null(attr(dat, "data_format"))){
    is_long <- is.null(rownames(x)) || all(rownames(x) == seq_len(nrow(x)))
    attr(x, "data_format") <- ifelse(is_long, "long", "wide")
  }

  # define dimensions
  point_number <- ncdf4::ncdim_def("point_number", "",
                                   vals = seq_len(nrow(dat)),
                                   create_dimvar = FALSE)

  scan_number <- ncdf4::ncdim_def("scan_number", "",
                                  vals = seq_len(nrow(x$TIC)),
                                  create_dimvar = FALSE)

  instrument_number <- ncdf4::ncdim_def("instrument_number", "",
                                  vals = seq_len(1),
                                  create_dimvar = FALSE)

  string32 <- ncdf4::ncdim_def("_32_byte_string", "",
                               vals = seq_len(32),
                               create_dimvar = FALSE)

  nc_scan_time <- ncdf4::ncvar_def("scan_acquisition_time", "",
                                   dim = scan_number, prec = "double")

  nc_tic <- ncdf4::ncvar_def("total_intensity", "", dim = scan_number,
                             prec = "double")

  nc_scan <- ncdf4::ncvar_def("scan_index", "", dim = scan_number,
                              prec = "integer")

  nc_points <- ncdf4::ncvar_def("point_count", "", dim = scan_number,
                                prec = "integer")

  nc_flags <- ncdf4::ncvar_def("flag_count", "", dim = scan_number,
                               prec = "integer")

  nc_time <- ncdf4::ncvar_def("time_values", "", dim = point_number,
                              prec = "float")

  nc_intensity <- ncdf4::ncvar_def("intensity_values", "", dim = point_number,
                                   prec = intensity_format)
  nc_mz <- ncdf4::ncvar_def("mass_values", "", dim = point_number,
                            prec = "float")
  instrument_vars <- c("instrument_name", "instrument_mfr", "instrument_model",
                       "instrument_sw_version", "instrument_os_version")
  range_vars <- c("mass_range_min", "mass_range_max",
                  "time_range_min", "time_range_max")

  range_vars <- lapply(range_vars, function(x){
    ncdf4::ncvar_def(x, "", dim = scan_number, prec = "double")
  })
  instrument_vars <- lapply(instrument_vars, function(x){
    ncdf4::ncvar_def(x, units = "", dim = list(string32, instrument_number),
                     prec = "char")
  })

  nc <- ncdf4::nc_create(file_out, c(list(nc_time, nc_mz, nc_intensity,
                                    nc_scan_time, nc_tic, nc_scan, nc_points,
                                    nc_flags),
                               range_vars, instrument_vars))

  ncdf4::ncvar_put(nc = nc, varid = "scan_acquisition_time",
                   vals = x$TIC[["rt"]])
  ncdf4::ncvar_put(nc = nc, varid = "time_values", vals = dat[["rt"]])
  ncdf4::ncatt_put(nc = nc, varid = "time_values", attname = "units",
                   attval = "Seconds")
  ncdf4::ncvar_put(nc = nc, varid = "intensity_values",
                   vals = dat[["intensity"]])
  intensity_unit <- andi_ms_intensity_unit(attr(x[[MS_idx]], "detector_y_unit"))
  ncdf4::ncatt_put(nc = nc, varid = "intensity_values", attname = "units",
                   attval = intensity_unit)

  ncdf4::ncvar_put(nc = nc, varid = "mass_values", vals = dat[["mz"]])
  ncdf4::ncatt_put(nc = nc, varid = "mass_values", attname = "units",
                   attval = "M/Z")

  ncdf4::ncvar_put(nc = nc, varid = "scan_index", vals = x$TIC[["scan_index"]])
  ncdf4::ncvar_put(nc = nc, varid = "point_count", vals = x$TIC[["points"]])
  ncdf4::ncvar_put(nc = nc, varid = "flag_count",
                   vals = rep(0L, scan_number$len))
  ncdf4::ncvar_put(nc = nc, varid = "total_intensity",
                   vals = as.data.frame(x$TIC)[["intensity"]])
  ncdf4::ncatt_put(nc = nc, varid = "total_intensity", attname = "units",
                   attval = intensity_unit)

  mz_range <- dat[, list(min = min(mz), max = max(mz)), by = rt]
  scan <- c(rep(1L, extra_vals), seq_len(nrow(mz_range)))
  ncdf4::ncvar_put(nc = nc, varid = "mass_range_min",
                   vals = mz_range[["min"]][scan])
  ncdf4::ncvar_put(nc = nc, varid = "mass_range_max",
                   vals = mz_range[["max"]][scan])
  ncdf4::ncvar_put(nc = nc, varid = "time_range_min", vals = x$TIC[["rt"]])
  ncdf4::ncvar_put(nc = nc, varid = "time_range_max", vals = x$TIC[["rt"]])
  instrument_vals <- list(
    instrument_name = andi_ms_setting(attr(dat, "instrument")),
    instrument_model = andi_ms_setting(attr(dat, "detector_model")),
    instrument_sw_version = andi_ms_setting(attr(dat, "software_version")))
  lapply(instrument_vars, function(v){
    ncdf4::ncvar_put(nc = nc, varid = v$name,
                     vals = substr(instrument_vals[[v$name]] %||% "", 1, 31))
  })

  meta <- format_metadata_for_andi_ms(dat, intensity_format = intensity_format,
                                      ms_params = ms_params)
  nc_add_global_attributes(nc = nc, meta = meta, sample_name = sample_name)

  ncdf4::nc_close(nc)
  return(invisible(file_out))
}

#' Format metadata for ANDI MS
#' @author Ethan Bass
#' @noRd
format_metadata_for_andi_ms <- function(x, intensity_format, ms_params){
  obj_params <- attr(x, "ms_params")
  datetime <- format(as.POSIXct(as.POSIXct(attr(x, "run_datetime")), tz = "UTC"),
                     "%Y%m%d%H%M%S%z")[1]
  rt_units <- attr(x, "time_unit")
  rt_units <- ifelse(!is.null(rt_units) && !is.na(rt_units),
                     tolower(rt_units), NA)
  rt_units <- switch(tolower(rt_units),
                     "sec" = "Seconds", "seconds" = "Seconds",
                     "min" = "Minutes", "minutes" = "Minutes",
                     "Seconds")
  rt_units <- ifelse(!is.null(rt_units), rt_units, "")
  meta <- list(dataset_completeness = "C1",
               ms_template_revision = "1.0.1",
               netcdf_revision = paste("netCDF",
                                       stringr::str_extract(ncdf4::nc_version(),
                                        "(?<=library version\\s)\\d+\\.\\d+\\.\\d+")),
               netcdf_file_date_time_stamp = format(Sys.time(),"%Y%m%d%H%M%S%z"),
               administrative_comments = paste("Collected on", attr(x, "instrument")),
               languages = "English only",
               #                converter_name = "chromconverter",
               #                converter_description = "AIA/ANDI netCDF Chromatography",
               source_file_reference = get_metadata_field2(x, "source_file",
                                                           null_val = ""),
               source_file_format = get_metadata_field2(x, "source_file_format",
                                                        null_val = ""),
               # source_file_date_time = ,
               experiment_date_time_stamp = datetime,
               operator_name = get_metadata_field2(x, "operator",
                                                   null_val = ""),
               experiment_type = andi_ms_setting(ms_params$experiment_type,
                                                 "Centroided Mass Spectrum"),
               # test_ms_inlet_temperature,
               # test_electron_energy,
               # test_source_temperature,
               # test_filament_current,
               # test_emission_current = attr(x, "ms_params")$emission_current,
               test_ionization_mode = andi_ms_setting(ms_params$ionization_mode,
                                                      obj_params$ionization_mode,
                                                      "Electron Impact"),
               test_ionization_polarity =
                 andi_ms_setting(ms_params$ionization_polarity,
                                 andi_ms_polarity(attr(x, "polarity")),
                                 andi_ms_polarity(obj_params$polarity),
                                 "Positive Polarity"),
               test_detector_type = andi_ms_setting(ms_params$detector_type,
                                                    obj_params$detector_type,
                                                    "Electron Multiplier"),
               test_scan_function =
                 andi_ms_setting(ms_params$scan_function,
                                 andi_ms_scan_function(attr(x, "scan_type")),
                                 andi_ms_scan_function(obj_params$scan_type)),
               # injection_date_time_stamp = datetime,
               raw_data_nscans = get_metadata_field2(x, "n_scans", class = "int",
                                                     null_val = ""),
               raw_data_starting_scan_no = as.integer(1),
               raw_data_mass_units = "M/Z",
               raw_data_time_units = rt_units,
               raw_data_intensity_units = get_metadata_field2(x, "detector_y_unit",
                                                              null_val = ""),
               raw_data_total_intensity_units = get_metadata_field2(x, "detector_y_unit",
                                                                    null_val = ""),
               sample_id_comments = "",
               detector_name = get_metadata_field2(x, "detector_model", class = "char",
                                                   null_val = ""),
               # experiment_title = "",
               sample_amount = get_metadata_field2(x, "sample_amount", class = "float",
                                                   null_val = ""),
               sample_injection_volume = get_metadata_field2(x, "sample_injection_volume",
                                                             class = "float",
                                                             null_val = ""),
               sample_type = get_metadata_field2(x, "sample_type",
                                                 null_val = ""),
               raw_data_mass_format = "Float",
               raw_data_time_format = "Float",
               raw_data_intensity_format = simple_cap(intensity_format)
  )
  meta[vapply(meta, is.null, logical(1))] <- NULL
  meta
}

#' First of `...` that holds a usable value, or `NULL`
#' @noRd
andi_ms_setting <- function(...){
  for (value in list(...)){
    if (length(value) == 1 && !is.na(value) && nzchar(as.character(value))){
      return(as.character(value))
    }
  }
  NULL
}

#' ANDI MS polarity for a `polarity` attribute
#' @noRd
andi_ms_polarity <- function(polarity){
  polarity <- andi_ms_setting(polarity)
  if (is.null(polarity)) return(NULL)
  if (grepl("^-|neg", polarity, ignore.case = TRUE)){
    "Negative Polarity"
  } else if (grepl("^[+]|pos", polarity, ignore.case = TRUE)){
    "Positive Polarity"
  } else NULL
}

#' ANDI MS intensity unit for a `detector_y_unit` attribute
#' @noRd
andi_ms_intensity_unit <- function(unit){
  unit <- andi_ms_setting(unit)
  if (is.null(unit)) return("Arbitrary Intensity Units")
  if (grepl("per second|^cps$|/s(ec)?$", unit, ignore.case = TRUE)){
    "Counts Per Second"
  } else if (grepl("count", unit, ignore.case = TRUE)){
    "Total Counts"
  } else if (grepl("^v$|^volts?$", unit, ignore.case = TRUE)){
    "Volts"
  } else if (grepl("^a$|^amps?$|^amperes?$|^current$", unit, ignore.case = TRUE)){
    "Current"
  } else if (grepl("^other intensity$", unit, ignore.case = TRUE)){
    "Other Intensity"
  } else "Arbitrary Intensity Units"
}

#' ANDI MS scan function for a `scan_type` attribute
#' @noRd
andi_ms_scan_function <- function(scan_type){
  scan_type <- andi_ms_setting(scan_type)
  if (is.null(scan_type)) return(NULL)
  if (grepl("sim|mrm|srm|sid|selected", scan_type, ignore.case = TRUE)){
    "Selected Ion Detection"
  } else if (grepl("scan", scan_type, ignore.case = TRUE)){
    "Mass Scan"
  } else "Other"
}
