#' Read CDF
#'
#' Reads 'Analytical Data Interchange' (ANDI) netCDF (`.cdf`) files.
#'
#' Retention times are returned in minutes, converted from the unit the file
#' declares. An ANDI chrom file declares it in its `retention_unit` attribute,
#' which also governs the peak table. Seconds is both what the template uses
#' and what all but one of its conformance files declare, so a file that
#' declares no unit is read as seconds, unless chromConverter wrote it, in
#' which case it is read as minutes.
#'
#' Either kind of file warns about a unit it does not recognize and reads it
#' as seconds.
#'
#' @inheritParams shared_params
#' @param path Path to ANDI netCDF file.
#' @param data_format Whether to return data in `wide` or `long` format.
#' For 2D files, "long" format returns the retention time as the first column of
#' the data.frame or matrix while "wide" format returns the retention time as the
#' rownames of the object. This argument applies only to 2D chromatograms, since
#' MS data will always be returned in long format.
#' @param what For `ANDI chrom` files, whether to extract `chroms`
#' and/or `peak_table`. For `ANDI ms` files, whether to extract MS1 scans
#' (`MS1`) or the total ion chromatogram (`TIC`).
#' @param ... Additional arguments to parser. The `ms_format` argument
#' can be used here to specify whether to return mass spectra in `list`
#' format or as a `data.frame`.
#' @return A chromatogram in the format specified by the `format_out` and
#' `data_format` arguments.
#' @examples \dontrun{
#' read_cdf("path/to/file.cdf")
#' }
#' @author Ethan Bass
#' @export

read_cdf <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                     data_format = c("wide", "long"),
                     what = NULL, read_metadata = TRUE,
                     metadata_format = c("chromconverter", "raw"),
                     collapse = TRUE, ...){
  check_for_pkg("ncdf4")
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  if ("ordinate_values" %in% names(nc$var)){
    format <- "chrom"
  } else if (all(c("intensity_values", "mass_values",
                   "scan_index", "scan_acquisition_time") %in% names(nc$var))){
    format <- "ms"
  } else {
    format <- "unknown"
  }
  fn <- switch(format, chrom = read_andi_chrom, ms = read_andi_ms,
               unknown = function(...){
                 stop("The format of the provided cdf file could not be recognized.")
               })
  fn(path = path, nc = nc, data_format = data_format, format_out = format_out,
     what = what, read_metadata = read_metadata,
     metadata_format = metadata_format, collapse = collapse, ...)
}

#' Minutes per unit of an ANDI time unit attribute
#' @param unit The attribute's value, or `NA` where the file has none.
#' @param attname Name of the attribute the value came from.
#' @return `1` for a unit naming minutes, `60` otherwise.
#' @noRd
andi_retention_divisor <- function(unit, attname = "retention_unit"){
  if (length(unit) != 1 || is.na(unit) || !nzchar(unit)) return(60)
  if (grepl("min", unit, ignore.case = TRUE)) return(1)
  if (!grepl("sec", unit, ignore.case = TRUE)){
    warning("Unrecognized `", attname, "` (", unit, "). Reading the ",
            "retention times as seconds.", call. = FALSE)
  }
  60
}

#' Retention times of an ANDI chrom raw data table
#' @noRd
andi_chrom_retention_times <- function(nc, n){
  flag <- ncdf4::ncatt_get(nc, varid = "ordinate_values",
                           attname = "uniform_sampling_flag")
  if (isTRUE(flag$hasatt) && toupper(trimws(flag$value)) == "N"){
    if ("raw_data_retention" %in% names(nc$var)){
      return(ncdf4::ncvar_get(nc, "raw_data_retention"))
    }
    warning("`uniform_sampling_flag` is \"N\", but the file does not contain ",
            "a `raw_data_retention` variable. Assuming uniform sampling.",
            call. = FALSE)
  }
  n_start <- ncdf4::ncvar_get(nc, "actual_delay_time")
  n_interval <- if ("actual_sampling_interval" %in% names(nc$var)){
    ncdf4::ncvar_get(nc, "actual_sampling_interval")
  } else NA_real_
  if (!isTRUE(n_interval > 0)){
    n_interval <- ncdf4::ncvar_get(nc, "actual_run_time_length") / n
  }
  n_start + (seq_len(n) - 1) * n_interval
}

#' ANDI chrom peak table variables expressed in `retention_unit`
#' @noRd
andi_chrom_peak_time_vars <- c("peak_retention_time", "peak_start_time",
                               "peak_end_time", "peak_width",
                               "baseline_start_time", "baseline_stop_time",
                               "migration_time")

#' Read ANDI chrom file
#' @param path Path to file.
#' @param format_out Class of output. Either `matrix`, `data.frame`, or
#' `data.table`.
#' @param data_format Whether to return data in `wide` or `long` format.
#' For 2D files, `"long"` format returns the retention time as the first column
#' of the data.frame or matrix while `"wide"` format returns the retention time
#' as the rownames of the object.
#' @param what Whether to extract `chromatogram` and/or `peak_table`.
#' @param read_metadata Whether to read metadata from file. Defaults to `TRUE`.
#' @param metadata_format Format to output metadata. Either `chromconverter` or
#' `raw`.
#' @return A chromatogram in the format specified by the `format_out` and
#' `data_format` arguments (retention time x wavelength).
#' @author Ethan Bass
#' @noRd
read_andi_chrom <- function(path, format_out = c("matrix", "data.frame",
                                                 "data.table"),
                            data_format = c("wide", "long"),
                            what = "chroms", read_metadata = TRUE,
                            metadata_format = "chromconverter",
                            collapse = TRUE, nc = NULL){
  metadata_format <- check_metadata_format(metadata_format, "andi_chrom")
  what <- if(is.null(what)) "chroms" else what
  if (any(what == "chromatogram")){
    warning("The `chromatogram` argument to `what` is deprecated. Please use `chroms` instead.")
    what[which(what == "chromatogram")] <- "chroms"
  }
  what <- match.arg(what, c("chroms", "peak_table"), several.ok = TRUE)
  if (is.null(nc)){
    nc <- ncdf4::nc_open(path)
    on.exit(ncdf4::nc_close(nc))
  }
  rt_unit <- ncdf4::ncatt_get(nc, varid = 0, attname = "retention_unit")
  rt_unit <- if (isTRUE(rt_unit$hasatt)) rt_unit$value else NA_character_
  converter <- ncdf4::ncatt_get(nc, varid = 0, attname = "converter_name")
  if (is.na(rt_unit) && isTRUE(converter$hasatt) &&
      identical(tolower(converter$value), "chromconverter")){
    rt_unit <- "Minutes"
  }
  rt_divisor <- andi_retention_divisor(rt_unit)
  if (any(what == "chroms")){
    y <- ncdf4::ncvar_get(nc, "ordinate_values")
    x <- andi_chrom_retention_times(nc, n = length(y)) / rt_divisor
    chroms <- format_2d_chromatogram(rt = x, int = y,
                                           data_format = data_format,
                                           format_out = format_out)
  }
  if (any(what == "peak_table")){
    peak_table_vars <- names(which(sapply(nc$var, function(x){
      any(sapply(x$dim, function(d) d$name) == "peak_number")
      })))
    if (length(peak_table_vars) > 0){
      # `lapply` + `as.data.frame`, not `sapply`: with a single peak `sapply`
      # returns a vector and the table comes out transposed
      peak_table <- lapply(peak_table_vars, function(var){
        ncdf4::ncvar_get(nc, varid = var)
      })
      names(peak_table) <- peak_table_vars
      peak_table <- as.data.frame(peak_table)
      time_cols <- intersect(names(peak_table), andi_chrom_peak_time_vars)
      peak_table[time_cols] <- peak_table[time_cols] / rt_divisor
    } else {
      warning("No peak table found in this file.", call. = FALSE)
      what <- setdiff(what, "peak_table")
    }
  }
  data <- mget(what)
  if (collapse) data <- collapse_list(data)
  if (read_metadata){
    meta <- ncdf4::ncatt_get(nc, varid = 0)
    meta$retention_unit <- "Minutes"
    if (inherits(data, "list")){
      data <- lapply(data, function(xx){
        attach_metadata(xx, meta = meta, format_in = metadata_format,
                        format_out = format_out, data_format = data_format,
                        parser = "chromconverter", source_file = path)
      })
    } else{
    data <- attach_metadata(data, meta = meta, format_in = metadata_format,
                            format_out = format_out, data_format = data_format,
                            parser = "chromconverter", source_file = path,
                            source_file_format = "andi_chrom")
    }
  }
  data
}

#' Read ANDI MS file
#' @param path Path to file.
#' @param format_out Class of output. Either `matrix`, `data.frame`,
#' or `data.table`.
#' @param data_format Whether to return the total ion chromatogram in `wide`
#' or `long` format. The "long" format returns the retention time as the
#' first column of the data.frame or matrix while "wide" format returns the
#' retention time as the rownames of the object.
#' @param what Whether to extract MS1 scans `MS1` and/or the total ion 
#' chromatogram (`TIC`).
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either `chromconverter`
#' or `raw`.
#' @param ms_format Whether to return mass spectral data as a (long)
#' `data.frame` or a `list`.
#' @return A chromatogram in the format specified by the `format_out` and
#' `data_format` arguments and MS spectra as either a long-format
#' `data.frame` or a `list` of spectra, according to the value of
#' `ms_format`.
#' @author Ethan Bass
#' @noRd

read_andi_ms <- function(path,
                         format_out = c("matrix", "data.frame", "data.table"),
                         data_format = c("wide", "long"),
                         what = c("MS1", "TIC"),
                         ms_format = c("data.frame", "list"),
                         read_metadata = TRUE,
                         metadata_format = "chromconverter",
                         collapse = TRUE, nc = NULL){
  format_out <- check_format_out(format_out)
  metadata_format <- check_metadata_format(metadata_format, "andi_ms")
  ms_format <- match.arg(ms_format, c("data.frame", "list"))
  what <- if(is.null(what)) c("MS1", "TIC") else what
  what <- match.arg(toupper(what), c("MS1", "TIC"), several.ok = TRUE)
  if (is.null(nc)){
    nc <- ncdf4::nc_open(path)
    on.exit(ncdf4::nc_close(nc))
  }
  if (any(what == "TIC")){
    x <- ncdf4::ncvar_get(nc, "scan_acquisition_time")
    y <- ncdf4::ncvar_get(nc, "total_intensity")

    TIC <- format_2d_chromatogram(rt = x, int = y, data_format = data_format,
                           format_out = format_out)
  }
  if (any(what == "MS1")){
    int <- ncdf4::ncvar_get(nc, "intensity_values")
    mz <- ncdf4::ncvar_get(nc, "mass_values")
    scan_idx <- ncdf4::ncvar_get(nc, "scan_index")
    n_scans <- ncdf4::ncvar_get(nc, "point_count")
    rt_scan <- ncdf4::ncvar_get(nc, "scan_acquisition_time")
    if (ms_format == "data.frame"){
      rts <- rep(rt_scan, n_scans)
      MS1 <- data.frame(rt = rts, mz = mz, intensity = int)
      if (check_format_out_table(format_out) == "data.table"){
        data.table::setDT(MS1)
      }
    } else if (ms_format == "list"){
      zeros <- as.list(rep(NA, length(which(scan_idx == 0)) - 1))
      scans <- Map(function(x, y){
        cbind(mz = x, int = y)
      }, split_at(mz, scan_idx + 1), split_at(int, scan_idx + 1))
      MS1 <- c(zeros, scans)
      names(MS1) <- rt_scan
    }
  }
  data <- mget(what)
  if (read_metadata){
    meta <- ncdf4::ncatt_get(nc, varid = 0)
    meta$detector <- "MS"
    data <- purrr::imap(data, function(x, h){
      attach_metadata(x, meta = meta, format_in = metadata_format,
                      format_out = ifelse(h == "MS1",
                                          check_format_out_table(format_out),
                                          format_out),
                      data_format = ifelse(h == "MS1", "long", data_format),
                      parser = "chromconverter", source_file = path,
                      source_file_format = "andi_ms")
    })
  }
  if (collapse){
    data <- collapse_list(data)
  }
  data
}
