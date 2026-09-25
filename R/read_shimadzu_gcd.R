#' Read 'Shimadzu' GCD
#'
#' Read chromatogram data streams from 'Shimadzu' `.gcd` files.
#'
#' A parser to read chromatogram data streams from 'Shimadzu' `.gcd` files.
#' GCD files are encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The chromatogram data is encoded in streams titled
#' `LSS Raw Data:Chromatogram Ch<#>`. Each stream begins with a 24-byte
#' header:
#' * 4 bytes: segment label (`17234`).
#' * 4 bytes: Little-endian integer specifying the sampling interval in
#' milliseconds.
#' * 4 bytes: Little-endian integer specifying the number of values in the
#' stream.
#' * 4 bytes: Little-endian integer specifying a byte count, which does not
#' match the size of the stream exactly.
#' * 8 bytes of `00`s
#'
#' After the header, the data are encoded as 64-bit (little-endian)
#' floating-point numbers. Retention times are derived from the number of
#' values and the sampling interval encoded in the header, rather than read
#' from the file: the `n`th value is placed at `n` times the sampling
#' interval.
#'
#' @inheritParams shared_params
#' @param path Path to 'Shimadzu' `.gcd` file.
#' @param what What stream to get: current options are chromatograms
#' (`chroms`) and/or peak lists (`peak_table`). If a stream
#' is not specified, the function will default to `chroms`.
#' @examples \dontrun{
#' read_shimadzu_gcd("path/to/file.gcd")
#' }
#' @author Ethan Bass
#' @inherit generic_return_2D return
#' @family 'Shimadzu' parsers
#' @export

read_shimadzu_gcd <- function(path, what = "chroms",
                              format_out = c("matrix", "data.frame",
                                             "data.table"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE,
                              metadata_format = c("chromconverter","raw"),
                              collapse = TRUE){
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "shimadzu_lcd")
  check_py_module("olefile")

  if (read_metadata){
    meta <- read_sz_file_properties(path)
  }
  if (any(what == "chroms")){
    existing_streams <- check_streams(path, what = "chroms")

    chroms <- lapply(existing_streams, function(stream){

      idx <- as.numeric(gsub("\\D", "", stream[2]))
      DI <- read_sz_2DDI(path, idx = idx)

      x <- decode_shimadzu_gcd(path, stream = stream)
      x <- format_2d_chromatogram(rt = x$rt, int = x$int,
                                  data_format = data_format,
                                  format_out = format_out)
      if (read_metadata){
        x <- attach_metadata(x, c(meta,DI), format_in = metadata_format,
                             source_file = path, source_file_format = "shimadzu_gcd",
                             data_format = data_format,
                             format_out = format_out)
      }
      x
    })
    # infer times from "PDA.1.Method" stream:
    # method_metadata <- read_sz_method(path,
    #                                   stream = c("GUMM_Information",
                                          # "ShimadzuGC.1","GUC.1.METHOD"))
    if (length(chroms) == 1){
      chroms <- chroms[[1]]
    }
  }
  if (any(what == "peak_table")){
    peak_table <- read_sz_tables(path, format_out = format_out)
    if (read_metadata){
      peak_table <- attach_metadata(peak_table, meta, format_in = metadata_format,
                           source_file = path, source_file_format = "shimadzu_gcd",
                             data_format = data_format,
                           format_out = "data.frame")
    }
  }
  dat <- mget(what, ifnotfound = NA)
  null <- sapply(dat, is.null)
  if (any(null)) dat <- dat[-which(sapply(dat, is.null))]
  if (collapse) dat <- collapse_list(dat)
  dat
}

#' Decode 'Shimadzu' GCD data stream
#' @author Ethan Bass
#' @noRd
decode_shimadzu_gcd <- function(path, stream){
  path_stream <- export_stream(path, stream = stream)
  on.exit(unlink_stream(path_stream), add = TRUE)

  f <- file(path_stream, "rb")
  on.exit(close(f), add = TRUE)

  block_start <- seek(f, NA, "current")

  readBin(f, what = "integer", n = 1, size = 4) #skip
  interval <- readBin(f, what = "integer",size = 4,endian = "little")
  nval <- readBin(f, what = "integer", size = 4, endian = "little")

  readBin(f, what = "double", size = 4, n = 3, endian = "little") #skip

  signal <- readBin(f, what = "double", n = nval, endian = "little")

  times <- seq(interval, nval*interval, interval)/60000
  data.frame(rt = times, int = signal)
}
