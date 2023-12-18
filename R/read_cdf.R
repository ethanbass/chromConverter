#' Read CDF file
#' @param file Path to file.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' For 2D files, "long" format returns the retention time as the first column of
#' the data.frame or matrix while "wide" format returns the retention time as the
#' rownames of the object.
#' @param what Whether to extract \code{chromatogram} and/or \code{peak_table}.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element.
#' @return A chromatogram in the format specified by the \code{format_out} and
#' \code{data_format} arguments (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_cdf <- function(file, format_out = c("matrix", "data.frame"),
                     data_format = c("wide","long"),
                     what = "chromatogram", read_metadata = TRUE,
                     metadata_format = c("chromconverter", "raw"),
                     collapse = TRUE){
  check_for_pkg("ncdf4")
  nc <- ncdf4::nc_open(file)
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
  fn(file = file, data_format = data_format, format_out = format_out,
     what = what, read_metadata = read_metadata,
     metadata_format = metadata_format, collapse = collapse)
}

#' Read ANDI chrom file
#' @param file Path to file.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
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
read_andi_chrom <- function(file, format_out = c("matrix", "data.frame"),
                            data_format = c("wide", "long"),
                            what = "chromatogram", read_metadata = TRUE,
                            metadata_format = c("chromconverter", "raw"),
                            collapse = TRUE){
  data_format <- match.arg(data_format, c("wide","long"))
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "cdf", raw = "raw")
  what <- match.arg(what, c("chromatogram", "peak_table"), several.ok = TRUE)
  nc <- ncdf4::nc_open(file)
  if (any(what == "chromatogram")){
    y <- ncdf4::ncvar_get(nc, "ordinate_values")
    nvals <- ncdf4::ncvar_get(nc, "actual_run_time_length")
    n_interval <- ncdf4::ncvar_get(nc, "actual_sampling_interval")
    n_start <- ncdf4::ncvar_get(nc, "actual_delay_time")
    x <- seq(from = n_start, to = nvals, length.out=length(y))
    data = data.frame(RT = x, Intensity = y)
    if (data_format == "wide"){
      rownames(data) <- data[,1]
      data <- data[,-1, drop = FALSE]
    }
    if (format_out == "matrix"){
      data <- as.matrix(data)
    }
    chromatogram <- data
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
  # if ("peak_table" %in% what & "chromatogram" %in% what){
  #   what <- "both"
  # }
  data <- mget(what)
  if (collapse) data <- collapse_list(data)
  if (read_metadata){
    meta <- ncdf4::ncatt_get(nc, varid = 0)
    if (inherits(data, "list")){
      data <- lapply(data, function(xx){
        attach_metadata(xx, meta = meta, format_in = "cdf",
                        format_out = format_out, data_format = data_format,
                        parser = "chromconverter", source_file = file)
      })
    } else{
    data <- attach_metadata(data, meta = meta, format_in = metadata_format,
                            format_out = format_out, data_format = data_format,
                            parser = "chromconverter", source_file = file)
    }
  }
  ncdf4::nc_close(nc)
  data
}

#' Read ANDI MS file
#' @param file Path to file.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return the total ion chromatogram in \code{wide}
#' or \code{long} format. The "long" format returns the retention time as the
#' first column of the data.frame or matrix while "wide" format returns the
#' retention time as the rownames of the object.
#' @param what Whether to extract \code{chromatogram} and/or \code{ms_spectra}.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param ms_format Whether to return mass spectral data as a (long)
#' \code{data.frame} or a list.
#' @return A chromatogram in the format specified by the \code{format_out} and
#' \code{data_format} arguments (retention time x wavelength).
#' @author Ethan Bass
#' @noRd

read_andi_ms <- function(file, format_out = c("matrix", "data.frame"),
                         data_format = c("wide", "long"),
                         what = "chromatogram", ms_format = c("data.frame", "list"),
                         read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw"),
                         collapse = TRUE){
  data_format <- match.arg(data_format, c("wide","long"))
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  ms_format <- match.arg(ms_format, c("data.frame","list"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "cdf", raw = "raw")
  what <- match.arg(what, c("chromatogram", "ms_spectra"), several.ok = TRUE)
  nc <- ncdf4::nc_open(file)
  if (any(what == "chromatogram")){
    y <- ncdf4::ncvar_get(nc, "total_intensity")
    x <- ncdf4::ncvar_get(nc, "scan_acquisition_time")
    data = data.frame(RT = x, Intensity = y)
    if (data_format == "wide"){
      rownames(data) <- data[, 1]
      data <- data[,-1, drop = FALSE]
    }
    if (format_out == "matrix"){
      data <- as.matrix(data)
    }
    chromatogram <- data
  }
  if (any(what == "ms_spectra")){
    int <- ncdf4::ncvar_get(nc, "intensity_values")
    mz <- ncdf4::ncvar_get(nc, "mass_values")
    scan_idx <- ncdf4::ncvar_get(nc, "scan_index")
    rt <- ncdf4::ncvar_get(nc, "scan_acquisition_time")
    zeros <- as.list(rep(NA, length(which(scan_idx==0)) - 1))
    if (ms_format == "data.frame"){
      n_scans <- diff(c(scan_idx, length(mz)))
      rts <- unlist(sapply(seq_along(rt), function(i){rep(rt[i], n_scans[i])}))
      ms_spectra <- data.frame(rt = rts, mz = mz, intensity = int)
    } else if (ms_format == "list"){
      scans <- mapply(function(x,y){
        cbind(mz = x, intensity = y)
      }, split_at(mz, scan_idx+1), split_at(int, scan_idx+1))
      ms_spectra <- c(zeros, scans)
      names(ms_spectra) <- rt
    }
  }

  data <- mget(what)
  if (collapse) data <- collapse_list(data)
  if (read_metadata){
    meta <- ncdf4::ncatt_get(nc, varid = 0)
    if (inherits(data, "list")){
      data <- lapply(data, function(xx){
        attach_metadata(xx, meta = meta, format_in = "cdf",
                        format_out = format_out, data_format = data_format,
                        parser = "chromconverter", source_file = file)
      })
    } else{
      data <- attach_metadata(data, meta = meta, format_in = metadata_format,
                              format_out = format_out, data_format = data_format,
                              parser = "chromconverter", source_file = file)
    }
  }
  ncdf4::nc_close(nc)
  data
}
