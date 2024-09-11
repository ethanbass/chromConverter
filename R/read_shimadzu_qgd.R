#' Read 'Shimadzu' QGD
#' Parser for 'Shimadzu GCMSsolution' \code{.qgd} files
#' The MS data is stored in the "GCMS Raw Data" storage, which contains a
#' \code{MS Raw Data} stream with MS scans, a \code{TIC Data} stream containing
#' the total ion chromatogram, and a \code{Retention Time} stream containing the
#' retention times. All known values are little-endian. The retention time
#' stream is a simple array of 4-byte integers. The TIC stream is a simple array
#' of 8-byte integers corresponding to retention times stored in the
#' retention time stream. The MS Raw Data stream is blocked by retention time.
#' Each block begins with a header consisting of the following elements:
#' * scan number (4-byte integer)
#' * retention time (4-byte integer)
#' * unknown (12-bytes)
#' * number of bytes in intensity values (2-byte integer)
#' * unknown (8-bytes)
#'
#' After the header, the rest of the block consists of an array of mz values and
#' intensities. The mz values are encoded as 2-byte integers where each mz value
#' is scaled by a factor of 20. Intensities are encoded as (unsigned) integers
#' with variable byte-length defined by the value in the header.
#'
#' @param path Path to 'Shimadzu' QGD file.
#' @param what What stream to get: current options are \code{tic} or
#' \code{ms}. If a stream is not specified, the function will return both
#' streams.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A 2D chromatogram from the chromatogram stream in \code{matrix} or
#' \code{data.frame} format, according to the value of \code{format_out}.
#' The chromatograms will be returned in \code{wide} or \code{long} format
#' according to the value of \code{data_format}.
#' @note This parser is experimental and may still need some work. It is not
#' yet able to interpret much metadata from the files.
#' @author Ethan Bass
#' @export

read_shimadzu_qgd <- function(path, what = c("tic", "ms"),
                              format_out = c("matrix", "data.frame"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE,
                              metadata_format = c("chromconverter", "raw")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  what <- match.arg(what, c("tic","ms"), several.ok = TRUE)
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, "chromconverter" = "shimadzu_lcd",
                            "raw")
  olefile_installed <- reticulate::py_module_available("olefile")
  if (!olefile_installed){
    configure_python_environment(parser = "olefile")
  }

  if ("tic" %in% what){
    tic <- read_qgc_tic(path, format_out = format_out, data_format = data_format)
  }
  if ("ms" %in% what){
    ms <- read_qgd_ms_stream(path, format_out = format_out)
  }
  dat <- mget(what)

  if (read_metadata){
    meta <- try(read_qgd_fp(path))
    dat <- attach_metadata(dat, meta, format_in = metadata_format,
                         source_file = path, data_format = data_format,
                         format_out = format_out)
  }
  dat
}

#' Read QGC total ion chromatogram
#' @author Ethan Bass
#' @noRd
read_qgc_tic <- function(path, format_out = c("matrix", "data.frame"),
                        data_format = c("wide", "long"),
                        read_metadata = TRUE){

  path_tic <- export_stream(path, c("GCMS Raw Data", "TIC Data"))

  f <- file(path_tic, "rb")
  on.exit(close(f))

  seek(f, where = 0, origin = "end")
  bytes <- seek(f, where = 0, origin = "end")

  nval <- (bytes)/8

  seek(f, 0, "start")

  int <- readBin(f, "integer", size = 8, n = nval, endian = "little")

  rts <- read_qgd_retention_times(path)

  if (data_format == "wide"){
    dat <- matrix(int, nrow = nval, ncol = 1, dimnames = list(rts, "int"))
  } else if (data_format == "long"){
    dat <- cbind(rts, int)
    colnames(dat) <- c("rt", "int")
  }
  if (format_out == "data.frame"){
    dat <- as.data.frame(dat)
  }
  dat
}

#' Read 'Shimadzu' QGD MS block
#' @author Ethan Bass
#' @noRd
read_qgd_ms_block <- function(f){
  scan <- readBin(f, "integer", size = 4, endian = "little")
  rt <- readBin(f, "integer", size = 4, endian = "little")
  u1 <- readBin(f, "integer", size = 4, endian = "little")
  readBin(f, "integer", size = 4, endian = "little", n = 2) #skip

  # number of bytes in intensity value
  n_bytes <- readBin(f, "integer", size = 2, endian = "little")

  # number of values in block
  nval <- readBin(f, "integer", size = 2, endian = "little")
  readBin(f, "integer", size = 4, endian = "little", n = 2) #skip

  mat <- matrix(NA, nrow = nval, ncol = 4,
                dimnames = list(NULL, c("scan", "rt", "mz", "int")))
  # we have to add a byte of 00s for odd numbers of bytes because R can't deal
  # with integers that have odd numbers of bytes
  add_byte <- n_bytes %% 2 == 1
  nb <- ifelse(add_byte, n_bytes + 1, n_bytes)
  signed <- ifelse(n_bytes == 2, FALSE, TRUE)
  for (i in seq_len(nval)){
    buffer <- readBin(f, what = "raw", n = (2 + n_bytes))
    if (add_byte){
      buffer <- c(buffer, as.raw(0x00))
    }
    mat[i,] <- c(scan, rt,
      readBin(buffer[1:2], what = "integer", size = 2,
            endian = "little", n = 1),
      readBin(buffer[3:(3 + nb)], what = "integer", size = (nb),
            endian = "little", n = 1, signed = signed)
    )
  }
  mat[,3] <- mat[,3]/20
  mat
}

# what are time units?

#' Read 'Shimadzu QGD' retention times
#' Retention times are stored in the "GCMS Raw Data/Retention Time" stream as
#' a series of 4-byte, little-endian integers.
#' @noRd
read_qgd_retention_times <- function(path){
  path_rts <- export_stream(path, c("GCMS Raw Data", "Retention Time"))
  f <- file(path_rts, "rb")
  on.exit(close(f))

  seek(f, 0, origin = "end")
  last_byte <- seek(f, 0, origin = "end")

  n_val <- last_byte/4
  seek(f, 0, origin = "start")
  rts <- readBin(f, what = "integer", size = 4, n = n_val, endian = "little")/60
  rts
}

#' Read 'Shimadzu' QGD MS stream
#' Read MS data from 'Shimadzu' QGD file
#' The MS data is stored in the
#' The
#' @param path Path to 'Shimadzu' QGD file.
#' @author Ethan Bass
#' @noRd
read_qgd_ms_stream <- function(path, format_out = c("matrix", "data.frame")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))

  rts <- read_qgd_retention_times(path)

  path_ms <- export_stream(path, c("GCMS Raw Data", "MS Raw Data"))
  f <- file(path_ms, "rb")
  on.exit(close(f))

  xx <- lapply(seq_along(rts), function(i){
    read_qgd_ms_block(f)
  })
  mat <- do.call(rbind, xx)
  if (format_out == "data.frame"){
    mat <- as.data.frame(mat)
  }
  mat
}


#' Read QGD file property stream
#' The file properties stream is not XML unlike 'Shimadzu LCD' and 'Shimadzu GCD'
#' files.
#' @author Ethan Bass
#' @noRd
read_qgd_fp <- function(path){
  path_fp <- export_stream(path, "File Property")

  f <- file(path_fp, "rb")
  on.exit(close(f))

  qgd_offsets <- get_sz_qgd_offsets()

  xx <- lapply(seq_len(nrow(qgd_offsets)), function(i){
    seek(f, as.numeric(qgd_offsets[i, "offset"]))
    switch(qgd_offsets[i, "type"],
           "character" = readBin(f, what = "character"),
           "integer" = readBin(f, what = "integer", size = 4)
    )
  })
  names(xx) <- qgd_offsets$field
  xx$time_gen <- sztime_to_unixtime(xx$time_gen_low, xx$time_gen_high)
  xx$time_mod <- sztime_to_unixtime(xx$time_mod_low, xx$time_mod_high)
  xx$time_acq <- sztime_to_unixtime(xx$time_acq_low, xx$time_acq_high)
  xx <- xx[-which(names(xx) %in% c("time_gen_low","time_gen_high",
                                   "time_mod_low", "time_mod_high",
                                   "time_acq_low", "time_acq_high"))]
  xx
}

#' Get 'Shimadzu' QGD file property offsets
#' @author Ethan Bass
#' @noRd
get_sz_qgd_offsets <- function(){
  rbind(data.frame(field = "DataFileProperty.szVersion", offset = 4, type = "character"),
        c(field = "generated_by", offset = 20, type = "character"),
        c(field = "time_gen_low", offset = 52, type = "integer"),
        c(field = "time_gen_high", offset = 56, type = "integer"),
        c(field = "modified_by", offset = 60, type = "character"),
        c(field = "time_mod_low", offset = 92, type = "integer"),
        c(field = "time_mod_high", offset = 96, type = "integer"),
        c(field = "SampleInfo.smpl_type", offset = 172, type = "character"),
        c(field = "SampleInfo.smpl_name", offset = 204, type = "character"),
        c(field = "SampleInfo.operator_name", offset = 300, type = "character"),
        c(field = "time_acq_low", offset = 508, type = "integer"),
        c(field = "time_acq_high", offset = 512, type = "integer"),
        c(field = "SampleInfoFile.datafile", offset = 580, type = "character"),
        c(field = "SampleInfoFile.batchfile", offset = 1604, type = "character"),
        c(field = "SampleInfoFile.methodfile", offset = 2116, type = "character"))
}
