#' Export chromatograms as csvs
#' @author Ethan Bass
#' @noRd
export_csvs <- function(data, path_out, fileEncoding = "utf8", row.names = TRUE){
  sapply(seq_along(data), function(i){
    write.csv(data[[i]], file = fs::path(path_out, names(data)[i], ext = "csv"),
              fileEncoding = fileEncoding, row.names = row.names)
  })
}

#' Export chromatograms as CDFs
#' @author Ethan Bass
#' @noRd
export_cdfs <- function(data, path_out){
  sapply(seq_along(data), function(i){
    write_cdf(data[[i]], sample_name = names(data)[i],
              path_out = path_out)
  })
}

#' Write CDF file from chromatogram
#' @author Ethan Bass
#' @noRd
write_cdf <- function(x, sample_name, path_out){
  check_for_pkg("ncdf4")
  if (ncol(x) + as.numeric(attr(x, "data_format") == "wide") > 2){
    warning("The supplies chromatogram contains more than two dimensions. Only
            the first two dimensions will be written to the ANDI chrom file.",
            immediate. = TRUE)
  }
  if (attr(x, "data_format") == "wide"){
    x1 <- data.frame(RT = as.numeric(rownames(x)), Intensity = x[,1])
    x <- transfer_metadata(x1, x)
  }
  # if (!missing(column)){
  #   x1 <- x[,column,drop = FALSE]
  #   x <- transfer_metadata(x1,x)
  # }
  filename <- fs::path_ext_remove(fs::path_file(sample_name))
  file_out <- fs::path(path_out, filename, ext = "cdf")
  if (fs::file_exists(file_out)){
    warning("File already exists and will not be overwritten.", immediate. = TRUE)
  }
  # define dimensions
  point_number <- ncdf4::ncdim_def("point_number", "",
                                   vals = seq_along(x[,1]),
                                   create_dimvar = FALSE)

  # define variables
  nc_time <- ncdf4::ncvar_def("raw_data_retention", "", dim = point_number)
  nc_intensity <- ncdf4::ncvar_def("ordinate_values", "", dim = point_number)
  other_vars <- c("actual_delay_time", "actual_run_time_length", "actual_sampling_interval",
                  "detector_maximum_value", "detector_minimum_value")
  other_vars <- lapply(other_vars, function(x) ncdf4::ncvar_def(x, "",list()))

  # write netcdf file
  ncdf4::nc_create(file_out, c(list(nc_time, nc_intensity), other_vars))

  # open netcdf file
  nc <- ncdf4::nc_open(file_out, write = TRUE)

  # write data to file
  ncdf4::ncvar_put(nc = nc, varid = "raw_data_retention", vals = x[,1])
  ncdf4::ncvar_put(nc = nc, varid = "ordinate_values", vals = x[,2])
  ncdf4::ncatt_put(nc, varid="ordinate_values",
                   attname = "uniform_sampling_flag", attval = "Y")
  ncdf4::ncvar_put(nc = nc, varid = "actual_run_time_length", vals = tail(x[,1],1))
  ncdf4::ncvar_put(nc = nc, varid = "actual_delay_time", vals = head(x[,1],1))
  ncdf4::ncvar_put(nc = nc, varid = "actual_sampling_interval", vals = mean(diff(x[,1])))
  ncdf4::ncvar_put(nc = nc, varid = "detector_maximum_value", vals = 1000)
  ncdf4::ncvar_put(nc = nc, varid = "detector_minimum_value", vals = -1000)

  # write metadata as global attributes
  meta <- format_metadata_for_cdf(x)
  nc_add_global_attributes(nc = nc, meta = meta, sample_name = sample_name)
  # finish writing file
  ncdf4::nc_close(nc)
}

#' Add global attributes to CDF file
#' @noRd
nc_add_global_attributes <- function(nc, meta, sample_name){
  sapply(seq_along(meta), function(i){
    ncdf4::ncatt_put(nc = nc, varid = 0,
                     attname = names(meta)[i], attval = meta[[i]],
                     prec = ifelse(names(meta)[i] %in% c("sample_amount", "sample_injection_volume"),
                                   "float","text"))
  })
  if (!is.null(sample_name)){
    ncdf4::ncatt_put(nc = nc, varid = 0,
                     attname = "sample_name", attval = sample_name)
    ncdf4::ncatt_put(nc = nc, varid = 0,
                     attname = "experiment_title", attval = sample_name)
  }
}

#' Format metadata for CDF
#' @author Ethan Bass
#' @noRd
format_metadata_for_cdf <- function(x){
  # datetime_str <- x[which(x$Property=="Time"),"Value"]
  # datetime_standard <- as.POSIXct(datetime_str, format = "%d.%m.%Y %H:%M:%S")
  datetime <- format(attr(x, "run_datetime"), "%Y%m%d%H%M%S%z")
  # rt_units <- x[which(x$Group=="Interval Time" & x$Property == "Units"), "Value"]
  rt_units <- switch(tolower(attr(x, "time_unit")),
                     "sec" = "Seconds", "seconds" = "Seconds",
                     "min" = "Minutes", "minutes" = "Minutes",
                     "default" = "Minutes")
  get_nc_version <- switch(.Platform$OS.type, "windows" = )
  meta <- list(dataset_completeness = "C1",
             aia_template_revision = "1.0",
             protocol_template_revision = "1.0",
             netcdf_revision = stringr::str_extract(ncdf4::nc_version(),
                                                             "(?<=library version\\s)\\d+\\.\\d+\\.\\d+")
             administrative_comments = paste("Collected on", attr(x, "instrument")),
             languages = "English only",
             converter_name = "chromconverter",
             converter_description = "AIA/ANDI netCDF Chromatography",
             converter_input_source = attr(x,"source_file"),
             date_time_stamp = datetime,
             dataset_date_time_stamp = datetime,
             injection_date_time_stamp = datetime,
             detector_units = attr(x,"detector_unit"),
             detector_unit = attr(x,"detector_unit"),
             retention_units = rt_units,
             retention_unit = rt_units,
             sample_id_comments = "",
             detector_name = attr(x,"detector"),
             # experiment_title = "",
             sample_amount = as.numeric(attr(x, "sample_amount")),
             sample_injection_volume = as.numeric(attr(x, "sample_injection_volume")),
             sample_type = attr(x, "sample_type")
  )
  meta$sample_type <- ifelse(!is.null(meta$sample_type), meta$sample_type, "unknown")
  meta$sample_amount <-
    ifelse(length(meta$sample_amount) != 0, meta$sample_amount, 1)
  meta$sample_injection_volume <-
    ifelse(length(meta$sample_injection_volume) != 0, meta$sample_injection_volume, 1)
  meta[sapply(meta, is.null)] <- ""
  meta[sapply(meta, is.na)] <- ""
  meta
}
