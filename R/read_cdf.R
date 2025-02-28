#' Read CDF
#'
#' Reads 'Analytical Data Interchange' (ANDI) netCDF (\code{.cdf}) files.
#'
#' @param path Path to ANDI netCDF file.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{\link[data.table]{data.table}}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' For 2D files, "long" format returns the retention time as the first column of
#' the data.frame or matrix while "wide" format returns the retention time as the
#' rownames of the object.
#' @param what For ANDI chrom files, whether to extract \code{chroms}
#' and/or \code{peak_table}. For ANDI ms files, whether to extract MS1 scans
#' (\code{MS1}) or the total ion chromatogram (\code{TIC}).
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element.
#' @param ... Additional arguments to parser. The \code{ms_format} argument
#' can be used here to specify whether to return mass spectra in \code{list}
#' format or as a \code{data.frame}.
#' @return A chromatogram in the format specified by the \code{format_out} and
#' \code{data_format} arguments.
#' @author Ethan Bass
#' @export

read_cdf <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                     data_format = c("wide", "long"),
                     what = NULL, read_metadata = TRUE,
                     metadata_format = c("chromconverter", "raw"),
                     collapse = TRUE, ...){
  check_for_pkg("ncdf4")
  data_format <- match.arg(data_format, c("wide", "long"))
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  nc <- ncdf4::nc_open(path)
  if ("ordinate_values" %in% names(nc$var)){
    format <- "chrom"
  } else if (all(c("intensity_values", "mass_values",
                   "scan_index", "scan_acquisition_time") %in% names(nc$var))){
    format <- "ms"
  } else {
    format <- "unknown"
  }
  ncdf4::nc_close(nc)
  fn <- switch(format, chrom = read_andi_chrom, ms = read_andi_ms,
               unknown = function(...){
                 stop("The format of the provided cdf file could not be recognized.")
               })
  fn(path = path, data_format = data_format, format_out = format_out,
     what = what, read_metadata = read_metadata,
     metadata_format = metadata_format, collapse = collapse, ...)
}

#' Read ANDI chrom file
#' @param path Path to file.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' For 2D files, "long" format returns the retention time as the first column of
#' the data.frame or matrix while "wide" format returns the retention time as the
#' rownames of the object.
#' @param what Whether to extract \code{chromatogram} and/or \code{peak_table}.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in the format specified by the \code{format_out} and
#' \code{data_format} arguments (retention time x wavelength).
#' @author Ethan Bass
#' @noRd
read_andi_chrom <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                            data_format = c("wide", "long"),
                            what = "chroms", read_metadata = TRUE,
                            metadata_format = "chromconverter",
                            collapse = TRUE){
  metadata_format <- switch(metadata_format,
                            chromconverter = "andi_chrom", raw = "raw")
  what <- if(is.null(what)) "chroms" else what
  if (any(what == "chromatogram")){
    warning("The `chromatogram` argument to `what` is deprecated. Please use `chroms` instead.")
    what[which(what == "chromatogram")] <- "chroms"
  }
  what <- match.arg(what, c("chroms", "peak_table"), several.ok = TRUE)
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  if (any(what == "chroms")){
    y <- ncdf4::ncvar_get(nc, "ordinate_values")
    nvals <- ncdf4::ncvar_get(nc, "actual_run_time_length")
    n_interval <- ncdf4::ncvar_get(nc, "actual_sampling_interval")
    n_start <- ncdf4::ncvar_get(nc, "actual_delay_time")
    x <- seq(from = n_start, to = nvals, length.out = length(y))
    chroms <- format_2d_chromatogram(rt = x, int = y,
                                           data_format = data_format,
                                           format_out = format_out)
  }
  if (any(what == "peak_table")){
    peak_table_vars <- names(which(sapply(nc$var, function(x){
      x$dim[[1]]$name
      }) == "peak_number"))
    if (length(peak_table_vars) > 0){
      peak_table <- sapply(peak_table_vars, function(var){
        ncdf4::ncvar_get(nc, varid = var)
      })
      peak_table <- as.data.frame(peak_table)
    }
  }
  data <- mget(what)
  if (collapse) data <- collapse_list(data)
  if (read_metadata){
    meta <- ncdf4::ncatt_get(nc, varid = 0)
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
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return the total ion chromatogram in \code{wide}
#' or \code{long} format. The "long" format returns the retention time as the
#' first column of the data.frame or matrix while "wide" format returns the
#' retention time as the rownames of the object.
#' @param what Whether to extract MS1 scans \code{MS1} and/or the total ion
#' chromatogram \code{TIC}.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param ms_format Whether to return mass spectral data as a (long)
#' \code{data.frame} or a \code{list}.
#' @return A chromatogram in the format specified by the \code{format_out} and
#' \code{data_format} arguments and MS spectra as either a long-format
#' \code{data.frame} or a \code{list} of spectra, according to the value of
#' \code{ms_format}.
#' @author Ethan Bass
#' @noRd

read_andi_ms <- function(path, format_out = c("matrix", "data.frame"),
                         data_format = c("wide", "long"),
                         what = c("MS1", "TIC"),
                         ms_format = c("data.frame", "list"),
                         read_metadata = TRUE,
                         metadata_format = "chromconverter",
                         collapse = TRUE){
  metadata_format <- switch(metadata_format,
                            chromconverter = "andi_ms", raw = "raw")
  ms_format <- match.arg(ms_format, c("data.frame", "list"))
  what <- if(is.null(what)) c("MS1", "TIC") else what
  what <- match.arg(toupper(what), c("MS1", "TIC"), several.ok = TRUE)
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  if (any(what == "TIC")){
    y <- ncdf4::ncvar_get(nc, "total_intensity")
    x <- ncdf4::ncvar_get(nc, "scan_acquisition_time")
    data = data.frame(rt = x, intensity = y)
    if (data_format == "wide"){
      rownames(data) <- data[, 1]
      data <- data[, -1, drop = FALSE]
    }
    if (format_out == "matrix"){
      data <- as.matrix(data)
    }
    TIC <- data
  }
  if (any(what == "MS1")){
    int <- ncdf4::ncvar_get(nc, "intensity_values")
    mz <- ncdf4::ncvar_get(nc, "mass_values")
    scan_idx <- ncdf4::ncvar_get(nc, "scan_index")
    n_scans <- ncdf4::ncvar_get(nc, "point_count")
    rt_scan <- ncdf4::ncvar_get(nc, "scan_acquisition_time")
    zeros <- as.list(rep(NA, length(which(scan_idx == 0)) - 1))
    if (ms_format == "data.frame"){
      rts <- unlist(sapply(seq_along(rt_scan), function(i){
        rep(rt_scan[i], n_scans[i])
      }))
      MS1 <- data.frame(rt = rts, mz = mz, intensity = int)
    } else if (ms_format == "list"){
      scans <- mapply(function(x, y){
        cbind(mz = x, int = y)
      }, split_at(mz, scan_idx + 1), split_at(int, scan_idx + 1))
      MS1 <- c(zeros, scans)
      names(MS1) <- rt_scan
    }
  }

  data <- mget(what)
  if (collapse) data <- collapse_list(data)
  if (read_metadata){
    meta <- ncdf4::ncatt_get(nc, varid = 0)
    meta$detector <- "MS"
    if (inherits(data, "list")){
      data <- lapply(data, function(xx){
        attach_metadata(xx, meta = meta, format_in = metadata_format,
                        format_out = format_out, data_format = data_format,
                        parser = "chromconverter", source_file = path,
                        source_file_format = "andi_ms")
      })
    } else{
      data <- attach_metadata(data, meta = meta, format_in = metadata_format,
                              format_out = format_out, data_format = data_format,
                              parser = "chromconverter", source_file = path,
                              source_file_format = "andi_ms")
    }
  }
  data
}
