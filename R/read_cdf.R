#' Read CDF file
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' For 2D files, "long" format returns the retention time as the first column of
#' the data.frame or matrix while "wide" format returns the retention time as the
#' rownames of the object.
#' @param read_metadata Whether to read metadata from file.
#' @param what Whether to extract \code{chromatogram} and/or \code{peak_table}.
#' @return A chromatogram in the format specified by the \code{format_out} and
#' \code{data_format} arguments (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_cdf <- function(file, format_out = c("matrix", "data.frame"),
                     data_format = c("wide","long"),
                     what = "chromatogram", read_metadata = TRUE){
  check_for_pkg("ncdf4")
  nc <- ncdf4::nc_open(file)
  if ("ordinate_values" %in% names(nc$var)){
    format = "chrom"
  } else {
    format = "ms"
  }
  ncdf4::nc_close(nc)
  fn <- switch(format, chrom = read_andi_chrom, ms = andi_ms_error)
  fn(file = file, data_format = data_format, format_out = format_out,
     what = what, read_metadata = read_metadata)
}

#' @noRd
andi_ms_error <- function(...){
  stop("The `cdf` MS format is not yet supported by an internal `chromConverter` parser.
           Please try the OpenChrom `msd` parser instead.")
}

#' Read ANDI chrom file
#' @noRd
read_andi_chrom <- function(file, format_out = c("matrix", "data.frame"),
                            data_format = c("wide", "long"),
                           what = "chromatogram", read_metadata = TRUE,
                           metadata_format = c("chromconverter", "raw")){
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
  }
  if (any(what == "peak_table")){
    peak_table_vars <- names(which(sapply(nc$var, function(x){
      x$dim[[1]]$name
      }) == "peak_number"))
    if (length(peak_table_vars) > 0){
      peak_tab <- sapply(peak_table_vars, function(var){
        ncdf4::ncvar_get(nc, varid = var)
      })
      peak_tab <- as.data.frame(peak_tab)
    }
  }
  if ("peak_table" %in% what & "chromatogram" %in% what){
    what <- "both"
  }
  data <- switch(what, "chromatogram" = data,
               "peak_table" = peak_tab,
               "both" = list(chromatogram=data, peak_table = peak_tab))
  if (read_metadata){
    meta <- ncdf4::ncatt_get(nc, varid = 0)
    # data <- attach_metadata(data, meta = meta, format_in = format_in,
    #                 format_out = format_out, data_format = data_format,
    #                 parser = "chromconverter", source_file = file)
    if (inherits(data, "list")){
      data <- lapply(data, function(xx){
        attach_metadata(data, meta = meta, format_in = "cdf",
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
