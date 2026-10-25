#' Read 'Shimadzu' QGD files
#'
#' Reads 'Shimadzu GCMSsolution' `.qgd` GC-MS data files.
#'
#' The MS data is stored in the `GCMS Raw Data` storage, which contains an
#' `MS Raw Data` stream with MS scans, a `TIC Data` stream containing the total
#' ion chromatogram, and a `Retention Time` stream containing the retention
#' times. All known values are little-endian. The retention time stream is an
#' array of 4-byte integers giving the time of each scan in milliseconds, and
#' the TIC stream an array of 8-byte integers, one per retention time.
#'
#' The MS Raw Data stream holds one block per scan, located through the
#' `Spectrum Index` stream. The index is an array of byte offsets, held as
#' 4-byte integers in some files and as 8-byte integers after a 2-byte header in
#' others, such as the MRM files seen so far. Each block begins with a 32-byte
#' header:
#' * scan number (4-byte integer)
#' * retention time in milliseconds (4-byte integer)
#' * unknown (12 bytes)
#' * number of bytes in each intensity value (2-byte integer)
#' * number of m/z values in the block (2-byte integer)
#' * MRM event number (2-byte integer)
#' * number of MRM transitions in the block (2-byte integer)
#' * unknown (4 bytes)
#'
#' In a full scan, each m/z value is followed by its intensity. The m/z values
#' are 2-byte integers holding m/z times 20. Intensities are unsigned integers
#' of the width given in the header, except that the top bit of a 4-byte
#' intensity is dropped.
#'
#' An MRM scan leaves the width and the number of m/z values at zero and
#' instead holds one record per transition: the precursor and product m/z, each
#' stored like the m/z of a full scan, followed by an unsigned intensity. The
#' header doesn't give the intensity width, so the reader works it out from
#' the size of the block.
#'
#' @inheritParams shared_params
#' @param path Path to 'Shimadzu' `.qgd` file.
#' @param what What stream to get: current options are full scans (`MS1`), MRM
#' scans (`MS2`) and the `TIC`. If a stream is not specified, the function
#' returns all three.
#' @inherit generic_return_2D return
#' @return A list holding whichever of the requested spectra the file has
#' (`MS1`, `MS2`) and the `TIC`, or the one requested where `collapse` is
#' `TRUE`. The TIC is returned in the format specified by
#' `data_format` and `format_out`. If `data_format` is `wide`, it is returned
#' with retention times as rows and a single column for the intensity. If
#' `long` format is requested, two columns are returned: one for the retention
#' time and one for the intensity. The `format_out` argument determines
#' whether it is returned as a `matrix`, `data.frame`, or `data.table`. The
#' spectra are always long, with columns
#' `scan`, `rt`, `mz` and `intensity`, plus `precursor_mz` before `mz` in the
#' MRM spectra, and a `matrix` resolves to a
#' `data.table`. Metadata are attached as [attributes] if `read_metadata` is
#' `TRUE`.
#' @examples \dontrun{
#' read_shimadzu_qgd("path/to/file.qgd")
#' }
#' @author Ethan Bass
#' @family 'Shimadzu' parsers
#' @export

read_shimadzu_qgd <- function(path, what = c("MS1", "MS2", "TIC"),
                              format_out = c("matrix", "data.frame",
                                             "data.table"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE,
                              metadata_format = c("chromconverter", "raw"),
                              collapse = TRUE){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  what <- match.arg(toupper(what), c("MS1", "MS2", "TIC"), several.ok = TRUE)
  metadata_format <- check_metadata_format(metadata_format, "shimadzu_lcd")
  check_py_module("olefile")

  dat <- list()
  if (any(c("MS1", "MS2") %in% what)){
    ms <- read_qgd_ms_stream(path, format_out = check_format_out_table(format_out))
    dat <- ms[intersect(c("MS1", "MS2"), what)]
    dat <- dat[lengths(dat) > 0]
  }
  if ("TIC" %in% what){
    dat$TIC <- read_qgd_tic(path, format_out = format_out,
                            data_format = data_format)
  }
  if (read_metadata){
    meta <- try(read_qgd_fp(path))
    meta$time.unit <- "Minutes"
    meta <- c(meta, sz_method_metadata(path))
    dat <- purrr::imap(dat, function(x, h){
      attach_metadata(x, meta, format_in = metadata_format,
                      source_file = path, source_file_format = "shimadzu_qgd",
                      data_format = ifelse(h == "TIC", data_format, "long"),
                      format_out = ifelse(h == "TIC", format_out,
                                          check_format_out_table(format_out)))
    })
  }
  if (collapse){
    dat <- collapse_list(dat)
  }
  dat
}

#' Read 'Shimadzu' QGD total ion chromatogram
#' @author Ethan Bass
#' @noRd
read_qgd_tic <- function(path, format_out = "data.frame",
                        data_format = c("wide", "long"),
                        read_metadata = TRUE){

  path_tic <- export_stream(path, c("GCMS Raw Data", "TIC Data"))
  on.exit(unlink_stream(path_tic), add = TRUE)

  f <- file(path_tic, "rb")
  on.exit(close(f), add = TRUE)

  seek(f, where = 0, origin = "end")
  bytes <- seek(f, where = 0, origin = "end")

  nval <- (bytes)/8

  seek(f, 0, "start")

  int <- readBin(f, "integer", size = 8, n = nval, endian = "little")

  rts <- read_qgd_retention_times(path)

  dat <- format_2d_chromatogram(rt = rts, int = int, format_out = format_out,
                         data_format = data_format)
  dat
}

#' Read 'Shimadzu' QGD MS scan
#' @author Ethan Bass
#' @noRd
read_qgd_ms_scan <- function(f, offsets, scan_no){
  header <- readBin(f, "integer", size = 4, endian = "little", n = 5)
  scan <- header[1]
  rt <- header[2]

  # number of bytes in intensity value & number of values in block, then the
  # MRM event and number of transitions
  header <- readBin(f, "integer", size = 2, endian = "little", n = 4)
  n_bytes <- header[1]
  nval <- header[2]
  n_transitions <- header[4]

  readBin(f, "integer", size = 4, endian = "little", n = 1) #skip

  actual_block_size <- offsets[scan_no+1] - offsets[scan_no]
  if (n_bytes == 0 && nval == 0 && n_transitions > 0){
    return(read_qgd_mrm_scan(f, scan = scan, rt = rt, n = n_transitions,
                             data_size = actual_block_size - 32,
                             scan_no = scan_no))
  }

  expected_block_size <- 32 + nval * (2 + n_bytes)
  if (expected_block_size != actual_block_size){
    data_size <- actual_block_size - 32
    n_bytes <- match(data_size, nval * (2 + 1:5))
    if (is.na(n_bytes)){
      stop(sprintf("Cannot resolve block size mismatch at scan `%d`: offset `%d`.",
                   scan_no, offsets[scan_no]))
    }
  }

  if (!n_bytes %in% 1:8){
    stop(sprintf("Unsupported intensity width (%d bytes) at scan `%d`.",
                 n_bytes, scan_no))
  }

  # read the whole block at once and reshape so that each column is one record
  record_size <- 2L + n_bytes
  block <- matrix(readBin(f, what = "raw", n = nval * record_size),
                  nrow = record_size, ncol = nval)

  mz <- readBin(as.vector(block[1:2, , drop = FALSE]), what = "integer",
                size = 2, endian = "little", n = nval)

  intensity <- decode_qgd_intensity(block[3:record_size, , drop = FALSE])

  out <- matrix(c(rep.int(scan, nval), rep.int(rt/60000, nval), mz/20,
                  intensity), nrow = nval, ncol = 4,
                dimnames = list(NULL, c("scan", "rt", "mz", "intensity")))
  if (n_bytes > 4){
    attr(out, "unvalidated_width") <- n_bytes
  }
  attr(out, "ms_level") <- 1L
  out
}

#' Read 'Shimadzu' QGD MRM scan
#'
#' Reads the `n` transitions of an MRM scan, each a precursor m/z and a product
#' m/z (2-byte integers holding m/z times 20) followed by an intensity. The
#' intensity width is not in the header, so it is worked out from the size of
#' the block.
#' @noRd
read_qgd_mrm_scan <- function(f, scan, rt, n, data_size, scan_no){
  n_bytes <- data_size/n - 4
  if (n_bytes %% 1 != 0 || !n_bytes %in% 1:8){
    stop(sprintf("Cannot resolve MRM record size at scan `%d`.", scan_no))
  }
  record_size <- 4L + n_bytes
  block <- matrix(readBin(f, what = "raw", n = n * record_size),
                  nrow = record_size, ncol = n)
  mz <- readBin(as.vector(block[1:4, , drop = FALSE]), what = "integer",
                size = 2, endian = "little", n = 2 * n, signed = FALSE)
  dim(mz) <- c(2L, n)
  intensity <- decode_qgd_intensity(block[5:record_size, , drop = FALSE])
  out <- matrix(c(rep.int(scan, n), rep.int(rt/60000, n), mz[1, ]/20,
                  mz[2, ]/20, intensity), nrow = n, ncol = 5,
                dimnames = list(NULL, c("scan", "rt", "precursor_mz", "mz",
                                        "intensity")))
  if (n_bytes > 4){
    attr(out, "unvalidated_width") <- n_bytes
  }
  attr(out, "ms_level") <- 2L
  out
}

#' Decode 'Shimadzu' QGD intensities
#'
#' @param x Raw matrix with one column per intensity, least significant byte
#' first.
#' @return Unsigned intensities. The top bit of a 4-byte intensity is dropped.
#' @noRd
decode_qgd_intensity <- function(x){
  n_bytes <- nrow(x)
  if (n_bytes > 4){
    return(colSums(matrix(as.numeric(x), nrow = n_bytes) *
                     256^(0:(n_bytes - 1))))
  }
  # we have to add a byte of 00s for odd numbers of bytes because R can't deal
  # with integers that have odd numbers of bytes
  nb <- ifelse(n_bytes %% 2 == 1, n_bytes + 1L, n_bytes)
  signed <- ifelse(n_bytes == 2, FALSE, TRUE)
  if (nb > n_bytes){
    x <- rbind(x, matrix(as.raw(0x00), nrow = nb - n_bytes, ncol = ncol(x)))
  }
  intensity <- readBin(as.vector(x), what = "integer", size = nb,
                       endian = "little", n = ncol(x), signed = signed)
  if (n_bytes == 4){
    intensity <- bitwAnd(intensity, 0x7FFFFFFF)
  }
  intensity
}

#' Read 'Shimadzu QGD' retention times
#' Retention times are stored in the "GCMS Raw Data/Retention Time" stream as
#' a series of 4-byte, little-endian integers.
#' @noRd
read_qgd_retention_times <- function(path){
  path_rts <- export_stream(path, c("GCMS Raw Data", "Retention Time"))
  on.exit(unlink_stream(path_rts), add = TRUE)
  f <- file(path_rts, "rb")
  on.exit(close(f), add = TRUE)

  seek(f, 0, origin = "end")
  last_byte <- seek(f, 0, origin = "end")

  n_val <- last_byte/4
  seek(f, 0, origin = "start")
  rts <- readBin(f, what = "integer", size = 4, n = n_val, endian = "little")
  rts/60000
}

#' Read 'Shimadzu' QGD MS stream
#' Read MS data from 'Shimadzu' QGD file
#' @param path Path to 'Shimadzu' QGD file.
#' @author Ethan Bass
#' @noRd
read_qgd_ms_stream <- function(path, format_out = "data.frame"){
  format_out <- check_format_out(format_out)

  rts <- read_qgd_retention_times(path)

  path_ms <- export_stream(path, c("GCMS Raw Data", "MS Raw Data"))
  on.exit(unlink_stream(path_ms), add = TRUE)

  offsets <- get_spectrum_offsets(path)
  offsets <- c(offsets, file.info(path_ms)$size)

  f <- file(path_ms, "rb")
  on.exit(close(f), add = TRUE)

  xx <- lapply(seq_along(rts), function(i){
    tryCatch({
      read_qgd_ms_scan(f, offsets = offsets, scan_no = i)
    }, error = function(e) stop(sprintf("Failed to read scan %d: %s",
                                        i, conditionMessage(e)))
    )
  })
  widths <- lapply(xx, attr, "unvalidated_width")
  odd <- which(lengths(widths) > 0)
  if (length(odd) > 0){
    warning(sprintf(paste0("%d scan(s) store intensities %s bytes wide (e.g. ",
                           "scan %d), a width not yet checked against a ",
                           "'LabSolutions' export. Check these intensities."),
                    length(odd),
                    paste(sort(unique(unlist(widths[odd]))), collapse = "/"),
                    odd[1]), call. = FALSE)
  }
  level <- vapply(xx, attr, "ms_level", FUN.VALUE = integer(1))
  dat <- lapply(c(MS1 = 1L, MS2 = 2L), function(lvl){
    if (!any(level == lvl)) return(NULL)
    convert_chrom_format(do.call(rbind, xx[level == lvl]),
                         format_out = format_out)
  })
  dat[lengths(dat) > 0]
}


#' Read QGD file property stream
#' The file properties stream is not XML unlike 'Shimadzu LCD' and 'Shimadzu GCD'
#' files.
#' @author Ethan Bass
#' @noRd
read_qgd_fp <- function(path){
  path_fp <- export_stream(path, "File Property")
  on.exit(unlink_stream(path_fp), add = TRUE)

  f <- file(path_fp, "rb")
  on.exit(close(f), add = TRUE)

  qgd_offsets <- get_sz_qgd_offsets()

  xx <- lapply(seq_len(nrow(qgd_offsets)), function(i){
    seek(f, as.numeric(qgd_offsets[i, "offset"]))
    switch(qgd_offsets[i, "type"],
           "character" = trimws(clean_vendor_string(readBin(f, what = "character"))),
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
  rbind(data.frame(field = "FileProperty.szVersion", offset = 4, type = "character"),
        c(field = "generated_by", offset = 20, type = "character"),
        c(field = "time_gen_low", offset = 52, type = "integer"),
        c(field = "time_gen_high", offset = 56, type = "integer"),
        c(field = "modified_by", offset = 60, type = "character"),
        c(field = "time_mod_low", offset = 92, type = "integer"),
        c(field = "time_mod_high", offset = 96, type = "integer"),
        c(field = "DataFileProperty.szVersion", offset = 148, type = "character"),
        c(field = "SampleInfo.smpl_type", offset = 172, type = "character"),
        c(field = "SampleInfo.smpl_name", offset = 204, type = "character"),
        c(field = "SampleInfo.operator_name", offset = 300, type = "character"),
        c(field = "time_acq_low", offset = 508, type = "integer"),
        c(field = "time_acq_high", offset = 512, type = "integer"),
        c(field = "SampleInfoFile.datafile", offset = 580, type = "character"),
        c(field = "SampleInfoFile.batchfile", offset = 1604, type = "character"),
        c(field = "SampleInfoFile.methodfile", offset = 2116, type = "character"))
}

#' Get 'Shimadzu' QGD spectrum index
#' @noRd
get_spectrum_offsets <- function(path){
  path_spectrum_index <- export_stream(path, c("GCMS Raw Data", "Spectrum Index"))
  on.exit(unlink_stream(path_spectrum_index), add = TRUE)
  f <- file(path_spectrum_index, "rb")
  on.exit(close(f), add = TRUE)
  size <- file.info(path_spectrum_index)$size
  if (size %% 8 == 2){
    readBin(f, what = "raw", n = 2)
    return(readBin(f, what = "integer", size = 8, endian = "little",
                   n = (size - 2)/8))
  }
  readBin(f, what = "integer", size = 4, endian = "little", n = size/4)
}
