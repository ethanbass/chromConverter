#' Write ANDI MS CDF file
#' @importFrom data.table :=
#' @importFrom data.table setorder
#' @author Ethan Bass
#' @noRd

write_andi_ms <- function(x, path_out, sample_name = NULL, force = FALSE,
                          ms_params = list(ionization_mode = "Electron Impact",
                                        ionization_polarity = "Positive Polarity",
                                        detector_type = "Electron Multiplier")
                          ){
  if (!inherits(x, "list")){
    x1 <- data.table::as.data.table(x)
    x1 <- list(MS1 = x1, TIC = x1[, list(intensity = sum(intensity)), by = rt])
    x <- lapply(x1, function(xx) transfer_metadata(xx, x))
  }
  dat <- x$MS1
  if (is.null(sample_name)){
    sample_name <- attr(x[["MS1"]], "sample_name")
    if (is.null(sample_name)){
      stop("Sample name must be provided.")
    }
  }
  file_out <- get_filepath(path_out = path_out, sample_name = sample_name,
                           ext = "cdf", force = force)

  x$TIC <- as.data.frame(x$TIC)
  if (ncol(x$TIC) == 1){
    x$TIC <- reshape_chrom_long(x$TIC, format_out = "data.frame")
  }
  if (grepl("min", attr(dat, "time_unit"), ignore.case = TRUE)){
    dat[, "rt"] <- dat[, "rt"]*60
    x$TIC[, "rt"] <- x$TIC[, "rt"]*60
    attr(x$MS1, "time_unit") = "Seconds"
  }
  dat <- data.table::as.data.table(dat)
  dat <- dat[order(rt, mz)]


  extra_vals <- nrow(x$TIC) - length(unique(dat[["rt"]]))
  x$TIC$scan_index <- as.integer(c(rep(0, extra_vals), which(!duplicated(dat[["rt"]]))-1))
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
  # define variables
  nc_scan_time <- ncdf4::ncvar_def("scan_acquisition_time", "",
                                   dim = scan_number, prec = "double")
  nc_tic <- ncdf4::ncvar_def("total_intensity", "", dim = scan_number,
                             prec = "double")
  nc_scan <- ncdf4::ncvar_def("scan_index", "", dim = scan_number,
                              prec = "integer")
  nc_points <- ncdf4::ncvar_def("point_count", "", dim = scan_number,
                                prec = "integer")
  # nc_flags <- ncdf4::ncvar_def("flag_count", "", dim = scan_number,
  #                               prec = "integer")

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
  # char_vars <- c("source_data_file_reference", instrument_vars)

  range_vars <- lapply(range_vars, function(x){
    ncdf4::ncvar_def(x, "", dim = scan_number, prec = "double")
  })
  instrument_vars <- lapply(instrument_vars, function(x){
    ncdf4::ncvar_def(x, units = "", dim = list(string32, instrument_number),
                     prec = "char")
  })

  # write netcdf file
  ncdf4::nc_create(file_out, c(list(nc_time, nc_mz, nc_intensity,
                                    nc_scan_time, nc_tic, nc_scan, nc_points),
                               range_vars, instrument_vars))
  # open netcdf file
  nc <- ncdf4::nc_open(file_out, write = TRUE)

  # write data to file
  ncdf4::ncvar_put(nc = nc, varid = "scan_acquisition_time",
                   vals = x$TIC[["rt"]])
  ncdf4::ncvar_put(nc = nc, varid = "time_values", vals = dat[["rt"]])
  ncdf4::ncatt_put(nc = nc, varid="time_values", attname = "units",
                   attval = "Seconds")
  ncdf4::ncvar_put(nc = nc, varid = "intensity_values",
                   vals = dat[["intensity"]])
  ncdf4::ncatt_put(nc = nc, varid = "intensity_values", attname = "units",
                   attval = "Arbitrary Intensity Units")

  # ncdf4::ncatt_put(nc, varid="ordinate_values",
  #                  attname = "uniform_sampling_flag", attval = "Y")
  ncdf4::ncvar_put(nc = nc, varid = "mass_values", vals = dat[["mz"]])
  ncdf4::ncatt_put(nc = nc, varid = "mass_values", attname = "units",
                   attval = "M/Z")

  ncdf4::ncvar_put(nc = nc, varid = "scan_index", vals = x$TIC[["scan_index"]])
  ncdf4::ncvar_put(nc = nc, varid = "point_count", vals = x$TIC[["points"]])
  ncdf4::ncvar_put(nc = nc, varid = "total_intensity",
                   vals = as.data.frame(x$TIC)[["intensity"]])

  ncdf4::ncvar_put(nc = nc, varid = "mass_range_min",
                   vals = rep(0, scan_number$len))
  ncdf4::ncvar_put(nc = nc, varid = "mass_range_max",
                   vals = rep(1000, scan_number$len))
  ncdf4::ncvar_put(nc = nc, varid = "time_range_min",
                   vals = rep(min(dat[["rt"]]), scan_number$len))
  ncdf4::ncvar_put(nc = nc, varid = "time_range_max",
                   vals = rep(min(dat[["rt"]]), scan_number$len))
  lapply(instrument_vars, function(v) ncdf4::ncvar_put(nc = nc, varid = v$name,
                                                       vals = ""))
  # write metadata as global attributes
  meta <- format_metadata_for_andi_ms(x$MS1, intensity_format = intensity_format,
                                      ms_params = ms_params)
  nc_add_global_attributes(nc = nc, meta = meta, sample_name = sample_name)

  # finish writing file
  ncdf4::nc_close(nc)
  return(invisible(file_out))
}

#' Format metadata for ANDI MS
#' @author Ethan Bass
#' @noRd
format_metadata_for_andi_ms <- function(x, intensity_format, ms_params){
  datetime <- format(attr(x, "run_datetime"), "%Y%m%d%H%M%S%z")[1]
  rt_units <- attr(x, "time_unit")
  rt_units <- ifelse(!is.null(rt_units) && !is.na(rt_units),
                     tolower(rt_units), NA)
  rt_units <- switch(tolower(rt_units),
                     "sec" = "Seconds", "seconds" = "Seconds",
                     "min" = "Minutes", "minutes" = "Minutes",
                     "default" = "Minutes")
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
               # experiment_type,
               # test_ms_inlet_temperature,
               # test_electron_energy,
               # test_source_temperature,
               # test_filament_current,
               # test_emission_current = attr(x, "ms_params")$emission_current,
               test_ionization_mode = ms_params$ionization_mode,
               test_ionization_polarity = ms_params$ionization_polarity,
               test_detector_type = ms_params$detector_type,
               # test_resolution_type,
               # test_scan_function,
               # test_scan_direction,
               # test_scan_law,
               # test_source_temperature,
               # injection_date_time_stamp = datetime,
               raw_data_nscans = get_metadata_field2(x, "no_scans", class = "int",
                                                     null_val = ""),
               raw_data_starting_scan_no = as.integer(1),
               raw_data_mass_factor = 1,
               raw_data_time_factor = 1,
               raw_data_intensity_factor = 1,
               raw_data_intensity_offset = 0,
               raw_data_mass_units = "m/z",
               raw_data_time_units = rt_units,
               raw_data_intensity_units = get_metadata_field2(x, "detector_unit",
                                                              null_val = ""),
               raw_data_total_intensity_units = get_metadata_field2(x, "detector_unit",
                                                                    null_val = ""),
               sample_id_comments = "",
               detector_name = get_metadata_field2(x, "detector_id", class = "char",
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
  meta
}
