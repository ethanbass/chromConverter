#' Write chromatograms
#'
#' Writes chromatograms to disk in the format specified by \code{export_format}:
#' either (\code{mzml}), \code{cdf} or \code{csv}.
#'
#' @param chrom_list A list of chromatograms.
#' @param path_out Path to directory for writing files.
#' @param export_format Format to export files: either \code{mzml}, \code{cdf},
#' or \code{csv}.
#' @param what What to write. Either \code{MS1} or \code{chrom}.
#' @param force Logical. Whether to overwrite existing files. Defaults to \code{TRUE}.
#' @param show_progress Logical. Whether to show progress bar. Defaults to \code{TRUE}.
#' @param verbose Logical. Whether to print verbose output.
#' @param ... Additional arguments to write function.
#' @return No return value. The function is called for its side effects.
#' @section Side effects:
#' Exports a chromatogram in the file format specified by \code{export_format}
#' in the directory specified by \code{path_out}.
#' @author Ethan Bass
#' @family write functions
#' @export

write_chroms <- function(chrom_list, path_out,
                         export_format = c("mzml", "cdf", "csv"),
                         what = "", force = FALSE,
                         show_progress = TRUE,
                         verbose = getOption("verbose"), ...){
  export_format <- match.arg(export_format, c("mzml", "cdf", "csv"))
  path_out <- fs::path_expand(path_out)
  if (!dir.exists(path_out)){
    ans <- readline("Export directory not found. Create directory (y/n)?")
    if (ans %in% c("y", "Y", "yes", "Yes", "YES")){
      fs::dir_create(path_out)
    } else
      stop(paste0("The export directory '", path_out, "' could not be found."))
  }

  writer <- switch(export_format,
                   csv = export_csvs,
                   cdf = purrr::partial(export_cdf, what = what,
                                        show_progress = show_progress),
                   mzml = purrr::partial(export_mzml,
                                         show_progress = show_progress))
  if (verbose){
    message(sprintf("Writing to %s...", toupper(export_format)))
  }
  writer(chrom_list, path_out = path_out, force = force, verbose = verbose, ...)
}

#' Write ANDI chrom CDF file from chromatogram
#'
#' Exports a chromatogram in ANDI (Analytical Data Interchange) chromatography
#' format (ASTM E1947-98). This format can only accommodate unidimensional data.
#' For two-dimensional chromatograms, the column to export can be specified
#' using the \code{lambda} argument. Otherwise, a warning will be generated and
#' the first column of the chromatogram will be exported.
#'
#' @author Ethan Bass
#' @param x A chromatogram in (wide) format.
#' @param path_out The path to write the file.
#' @param sample_name The name of the file. If a name is not provided, the name
#' will be derived from the \code{sample_name} attribute.
#' @param lambda The wavelength to export (for 2-dimensional chromatograms).
#' Must be a string matching one the columns in \code{x} or the index of the
#' column to export.
#' @param force Whether to overwrite existing files at the specified path.
#' Defaults to \code{FALSE}.
#' @return Invisibly returns the path to the written CDF file.
#' @section Side effects:
#' Exports a chromatogram in ANDI chromatography format (netCDF) in the directory
#' specified by \code{path_out}. The file will be named according to the value
#' of \code{sample_name}. If no \code{sample_name} is provided, the
#' \code{sample_name} attribute will be used if it exists.
#' @family write functions
#' @export

write_andi_chrom <- function(x, path_out, sample_name = NULL, lambda = NULL, force = FALSE){
  check_for_pkg("ncdf4")
  if (is.null(sample_name)){
    sample_name <- attr(x, "sample_name")
    if (is.null(sample_name)){
      stop("Sample name must be provided.")
    }
  }
  if (is.null(attr(x,"data_format"))){
    is_long <- is.null(rownames(x)) || all(rownames(x) == seq_len(nrow(x)))
    attr(x, "data_format") <- ifelse(is_long, "long", "wide")
  }
  if (is.null(lambda) && ncol(x) > 1 && attr(x, "data_format") ==  "wide") {
    warning("The supplied chromatogram contains more than two dimensions. Only
            the retention times and the first column of data will be written to
            the ANDI chrom file.",
            immediate. = TRUE)
  }
  lambda <- ifelse(is.null(lambda), 1, lambda)
  if (attr(x, "data_format") == "wide"){
    x1 <- data.frame(RT = as.numeric(rownames(x)), Intensity = x[, lambda])
    x <- transfer_metadata(x1, x)
  }

  file_out <- get_filepath(path_out = path_out, sample_name = sample_name,
                           force = force, ext = "cdf")

  # define dimensions
  point_number <- ncdf4::ncdim_def("point_number", "",
                                   vals = seq_along(x[,1]),
                                   create_dimvar = FALSE)

  # define variables
  nc_time <- ncdf4::ncvar_def("raw_data_retention", "", dim = point_number)
  nc_intensity <- ncdf4::ncvar_def("ordinate_values", "", dim = point_number)
  other_vars <- c("actual_delay_time", "actual_run_time_length",
                  "actual_sampling_interval", "detector_maximum_value",
                  "detector_minimum_value")

  other_vars <- lapply(other_vars, function(x) ncdf4::ncvar_def(x, "", list()))

  # write netcdf file
  ncdf4::nc_create(file_out, c(list(nc_time, nc_intensity), other_vars))

  # open netcdf file
  nc <- ncdf4::nc_open(file_out, write = TRUE)

  # write data to file
  ncdf4::ncvar_put(nc = nc, varid = "raw_data_retention", vals = x[,1])
  ncdf4::ncvar_put(nc = nc, varid = "ordinate_values", vals = x[,2])
  ncdf4::ncatt_put(nc, varid="ordinate_values",
                   attname = "uniform_sampling_flag", attval = "Y")
  ncdf4::ncvar_put(nc = nc, varid = "actual_run_time_length",
                   vals = tail(x[,1], 1))
  ncdf4::ncvar_put(nc = nc, varid = "actual_delay_time", vals = head(x[,1], 1))
  ncdf4::ncvar_put(nc = nc, varid = "actual_sampling_interval",
                   vals = mean(diff(x[,1])))
  ncdf4::ncvar_put(nc = nc, varid = "detector_maximum_value", vals = 1000)
  ncdf4::ncvar_put(nc = nc, varid = "detector_minimum_value", vals = -1000)

  # write metadata as global attributes
  meta <- format_metadata_for_cdf(x)
  nc_add_global_attributes(nc = nc, meta = meta, sample_name = sample_name)

  # finish writing file
  ncdf4::nc_close(nc)
  return(invisible(file_out))
}

#' Get filename
#' @author Ethan Bass
#' @noRd
get_filepath <- function(path_out, sample_name, ext, force = FALSE,
                         replacement = "_"){
  path_out <- fs::path_expand(path_out)
  sample_name <- gsub(" ", replacement, sample_name)
  # filename <- fs::path_ext_remove(fs::path_file(sample_name))
  file_out <- fs::path(path_out, sample_name, ext = ext)
  if (fs::file_exists(file_out)){
    if (!force){
      stop(sprintf("File %s already exists. Set 'force = TRUE' to
                               overwrite the existing file.",
                             sQuote(basename(file_out))))
    } else{
      fs::file_delete(file_out)
    }
  }
  file_out
}

#' Export chromatograms as CSVs
#' @author Ethan Bass
#' @noRd
export_csvs <- function(data, path_out, fileEncoding = "utf8", row.names = TRUE,
                        force = FALSE, verbose = getOption("verbose")){
  sapply(seq_along(data), function(i){
    if (verbose) message(sprintf("Writing %s", paste0(names(data)[i],".csv")))
    if (attr(data[[i]], "data_format") == "wide"){
      data[[i]] <- data.frame(rt = rownames(data[[i]]), data[[i]])
    }
    write.csv(data[[i]], file = fs::path(path_out, names(data)[i], ext = "csv"),
              fileEncoding = fileEncoding, row.names = FALSE)
  })
}

#' Export chromatograms as ANDI netCDF files
#' @author Ethan Bass
#' @noRd
export_cdf <- function(data, path_out, what = "", force = FALSE,
                       show_progress = TRUE, verbose = getOption("verbose")){
  check_for_pkg("ncdf4")
  if (!inherits(data, "list")){
    data <- list(data)
  }
  laplee <- choose_apply_fnc(show_progress)
  if (what == ""){
    if (get_metadata_field2(data[[1]], "detector", null_val = FALSE) == "MS" ||
        inherits(data[[1]], "list")){
      what <- "MS1"
    } else what <- "chrom"
  }
  what <- match.arg(what, c("MS1", "chrom"))

  write_fn <- switch(what, "MS1" = write_andi_ms,
                     write_andi_chrom)
  data <- infer_sample_names(data)
  x <- laplee(seq_along(data), function(i){
    if (verbose) message(sprintf("Writing %s", paste0(names(data)[i], ".cdf")))
    try(write_fn(data[[i]], path_out = path_out, force = force))
  })
}

#' @noRd
infer_sample_names <- function(data){
  dat <- lapply(seq_along(data), function(i){
    if (inherits(data[[i]], "list")){
      smpl_name <- attr(data[[i]][[1]], "sample_name")
      if (is.null(smpl_name) || is.na(smpl_name)){
        attr(data[[i]][[1]], "sample_name") <- basename(names(data)[i])
      }
    } else{
      smpl_name <- attr(data[[i]], "sample_name")
      if (is.null(smpl_name) || is.na(smpl_name)){
        attr(data[[i]], "sample_name") <- basename(names(data)[i])
      }
    }
    data[[i]]
  })
  names(dat) <- names(data)
  dat
}

#' Export chromatograms as mzML files
#' @author Ethan Bass
#' @noRd
export_mzml <- function(data, path_out, force = FALSE,
                        show_progress = TRUE, verbose = getOption("verbose"),
                        ...){
  laplee <- choose_apply_fnc(show_progress)

  data <- infer_sample_names(data)

  laplee(seq_along(data), function(i){
    if (verbose) message(sprintf("Writing %s", paste0(names(data)[i],".mzml")))
    try(write_mzml(data[[i]], path_out = path_out,
                   show_progress = FALSE, force = force, ...))
  })
}

#' Add global attributes to CDF file
#' @noRd
nc_add_global_attributes <- function(nc, meta, sample_name){
  sapply(seq_along(meta), function(i){
    ncdf4::ncatt_put(nc = nc, varid = 0,
                     attname = names(meta)[i], attval = meta[[i]],
                     prec = switch(names(meta)[i],
                                   "sample_amount" = "float",
                                   "sample_injection_volume" = "float",
                                    "test_emission_current" = "float",
                                    "raw_data_nscans" = "int",
                                    "raw_data_starting_scan_no" = "int",
                                    "raw_data_mass_factor" = "float",
                                    "raw_data_time_factor" = "float",
                                    "raw_data_intensity_factor" = "float",
                                    "raw_data_intensity_offset" = "float",
                                    "text"))
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
  datetime <- format(attr(x, "run_datetime"), "%Y%m%d%H%M%S%z")
  rt_units <- attr(x, "time_unit")
  rt_units <- ifelse(!is.null(rt_units), tolower(rt_units), NA)
  rt_units <- switch(tolower(rt_units),
                     "sec" = "Seconds", "seconds" = "Seconds",
                     "min" = "Minutes", "minutes" = "Minutes",
                     "default" = "Minutes")
  # get_nc_version <- switch(.Platform$OS.type, "windows" = )
  meta <- list(dataset_completeness = "C1",
             aia_template_revision = "1.0",
             protocol_template_revision = "1.0",
             netcdf_revision = paste("netCDF",
                                     stringr::str_extract(ncdf4::nc_version(),
                                        "(?<=library version\\s)\\d+\\.\\d+\\.\\d+")),
             administrative_comments = paste("Collected on",
                                             attr(x, "instrument")),
             languages = "English only",
             converter_name = "chromconverter",
             converter_description = "AIA/ANDI netCDF Chromatography",
             converter_input_source = attr(x, "source_file"),
             date_time_stamp = datetime,
             dataset_date_time_stamp = datetime,
             injection_date_time_stamp = datetime,
             detector_units = attr(x, "detector_unit"),
             detector_unit = attr(x, "detector_unit"),
             retention_units = rt_units,
             retention_unit = rt_units,
             sample_id_comments = "",
             detector_name = attr(x, "detector"),
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
