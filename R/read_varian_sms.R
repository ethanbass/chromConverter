#' Read 'Varian' SMS
#'
#' Reads 'Varian Workstation' SMS files.
#'
#' Varian SMS files begin with a "DIRECTORY" with offsets for each section. The
#' first section (in all the files I've been able to inspect) is "MSData"
#' generally beginning at byte 3238. This MSdata section is in turn divided into
#' two sections. The first section (after a short header) contains chromatogram
#'  data. Some of the information found in this section includes scan numbers,
#' retention times, (as 64-bit
#' floats), the total ion chromatogram (TIC), the base peak chromatogram (BPC),
#' ion time (µsec), as well as some other unidentified information. The scan
#' numbers and intensities for the TIC and BPC are stored at 4-byte
#' little-endian integers. Following this section, there is a series of null
#' bytes, followed by a series of segments containing the mass spectra.
#'
#' The encoding scheme for the mass spectra is somewhat more complicated. Each
#' scan is represented by a series of values of variable length separated from
#' the next scan by two null bytes. Within these segments, values are paired.
#' The first value in each pair represents the delta-encoded mass-to-charge ratio,
#' while the second value represents the intensity of the signal. Values in this
#' section are variable-length, big-endian integers that are encoded using a
#' selective bit masking based on the leading digit (\code{d}) of each value.
#' The length of each integer seems to be determined as 1 + (d %/% 4). Integers
#' beginning with digits 0-3 are simple 2-byte integers. If d >= 4, values are
#' determined by masking to preserve the lowest \code{n} bits according to the
#' following scheme:
#'
#' * d = 4-5 -> preserve lowest 13 bits
#' * d = 6-7 -> preserve lowest 14 bits
#' * d = 8-9 -> preserve lowest 21 bits
#' * d = 10-11 (A-B) -> preserve lowest 22 bits
#' * d = 12-13 (C-D) -> preserve lowest 27 bits
#' * d = 14-15 (E-F) -> preserve lowest 28 bits (?)
#'
#' @param path Path to 'Varian' \code{.SMS} files.
#' @param what Whether to extract chromatograms (\code{chroms}) and/or
#' \code{MS1} data. Accepts multiple arguments.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file. (This is just a
#' placeholder for now as there is not yet support for parsing metadata).
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element.
#' @return A chromatogram or list of chromatograms from the specified
#' \code{file},  according to the value of \code{what}. Chromatograms are
#' returned in the format specified by \code{format_out}.
#' @author Ethan Bass
#' @note There is still only limited support for the extraction of metadata from
#' this file format. Also, the timestamp conversions aren't quite right.
#' @examples \dontrun{
#' read_varian_sms(path)
#' }
#' @family 'Varian' parsers
#' @export

read_varian_sms <- function(path, what = c("MS1", "TIC", "BPC"),
                            format_out = c("matrix", "data.frame", "data.table"),
                            data_format = c("wide", "long"),
                            read_metadata = TRUE, collapse = TRUE){

  what <- match.arg(what, c("MS1", "TIC", "BPC", "chroms"), several.ok = TRUE)
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))

  f <- file(path, "rb")
  on.exit(close(f))

  meta <- read_varian_msdata_header(f)

  chroms <- read_varian_chromatograms(f, n_time = meta$n_scan,
                                      format_out = format_out,
                                      data_format = "long")

  skip_null_bytes(f)

  acq_delay <- max(which(chroms[, "tic"] == 0))
  n_scans <- nrow(chroms) - acq_delay
  if ("MS1" %in% what){
    MS1 <- read_varian_ms_stream(f, n_scans = n_scans, format_out = format_out)

    MS1[,1] <- chroms[(MS1[,1] + acq_delay), "rt"]
    colnames(MS1) <- c("rt", "mz", "intensity")
  }

  if (any(what == "TIC")){
    TIC <- format_2d_chromatogram(rt = chroms[,"rt"], int = chroms[,"tic"],
                                  data_format = "long",
                                  format_out = format_out)
  }
  if (any(what == "BPC")){
    BPC <- format_2d_chromatogram(rt = chroms[,"rt"], int = chroms[,"bpc"],
                                  data_format = "long",
                                  format_out = format_out)
  }
  dat <- mget(what)
  if (collapse)
    dat <- collapse_list(dat)
  if (read_metadata){
    offsets <- read_varian_offsets(f)

    prep_offset <- offsets[grep("SamplePrep", offsets$name), "start"]
    seek(f, prep_offset)
    meta$sample_name <-  readBin(f, "character")

    meta <- read_mod_metadata(f, offsets, meta)

    dat <- lapply(dat, function(x){
      attach_metadata(x, meta, format_in = "varian_sms",
                           format_out = format_out, data_format = "long",
                           source_file = path, source_file_format = "varian_sms")
    })
  }
  dat
}

#' Read 'Varian' Mod Attribute metadata
#' @noRd
read_mod_metadata <- function(f, offsets, meta){
  mod_offset <- offsets[grep("ModAttr", offsets$name), "start"]
  seek(f, mod_offset)
  readBin(f, "raw", n = 2)
  meta$software <- readBin(f, "character")
  skip_null_bytes(f)

  meta$version <- readBin(f, "character")
  skip_null_bytes(f)

  readBin(f, "raw", n = 3) # skip
  skip_null_bytes(f)

  meta$temp_trap <- readBin(f, "integer", size = 2,
                            signed = FALSE, endian = "little")

  meta$temp_manifold <- readBin(f, "integer", size = 2,
                                signed = FALSE, endian = "little")

  meta$temp_transferline <- readBin(f, "integer", size = 2,
                                    signed = FALSE, endian = "little")

  readBin(f, "integer", size = 2,
          signed = FALSE, endian = "little")

  meta$axial_modulation <- readBin(f, "integer", size = 2,
                                   signed = FALSE, endian = "little")/10
  # unknown date
  # meta$date <- as.POSIXct(readBin(f, "integer", size=4, endian = "little"))

  # seek(f, 12, origin = "current") # skip 12 bytes
  # readBin(f, "double", size=8) #air water check
  meta
}

#' Read 'Varian Workstation' Chromatograms
#' @param f Connection to a 'Varian' SMS file opened to the beginning of the
#' chromatogram.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @author Ethan Bass
#' @noRd

read_varian_chromatograms <- function(f, n_time, format_out = "data.frame",
                                      data_format = "wide"){
  dat <- matrix(NA, nrow = n_time, ncol = 5)
  colnames(dat) <- c("scan", "rt", "tic", "bpc", "ion_time")
  for (i in seq_len(n_time)){
    dat[i, "scan"] <- readBin(f, what = "integer", size = 4, endian = "little")
    dat[i, "rt"] <- readBin(f, what = "double", size = 8, endian = "little")
    dat[i, "ion_time"] <- readBin(f, what = "integer", size = 2, signed = FALSE,
                                 endian = "little")
    dat[i, "tic"] <- readBin(f, what = "integer", size = 4, endian = "little")
    readBin(f, what = "raw", n = 6) # skip six unidentified bytes
    dat[i, "bpc"] <- readBin(f, what = "integer", size = 4, endian = "little")
    readBin(f, what = "raw", n = 11) # skip 11 unidentified bytes
  }
  if (data_format == "wide"){
    rownames(dat) <- dat[,"rt"]
    dat <- dat[,-2]
  }
  dat <- convert_chrom_format(dat, format_out = format_out)
  dat
}

#' Read 'Varian' MS stream
#' @param f Connection to a 'Varian' SMS file opened to the beginning of the
#' mass spectra stream.
#' @author Ethan Bass
#' @noRd
read_varian_ms_stream <- function(f, n_scans, format_out = "data.frame",
                                  data_format = "wide"){
  xx <- lapply(seq_len(n_scans), function(i){
    xx <- read_varian_ms_block(f)
    cbind(scan = i, xx)
  })
  dat <- do.call(rbind, xx)
  dat <- convert_chrom_format(dat, format_out = format_out)
  dat
}



#' Read 'Varian' MS block
#' @author Ethan Bass
#' @noRd
read_varian_ms_block <- function(f){
  buffer <- list(0,0,0,0)
  mat <- matrix(nrow = 1000, ncol = 2)
  i = 1
  buffer[[3]] <- readBin(f, "raw", n = 1)
  while (buffer[[3]] != "00"){
    for (j in c(1:2)){
      hex1 <- extract_sign(buffer[[3]])
      if (hex1 < 4){
        buffer[[2]] <- strtoi(buffer[[3]], base = 16)
      } else if (hex1 >= 4){
        buffer[[4]] <- readBin(f, "raw", n = hex1 %/% 4)
        buffer[[2]] <- decode_sms_val(c(buffer[[3]], buffer[[4]]))
      }
      if (j == 1){
        buffer[[1]] <- buffer[[1]] + buffer[[2]]
        mat[i,j] <- buffer[[1]]
      } else if (j == 2){
        mat[i,j] <- buffer[[2]]
      }
      buffer[[3]] <- readBin(f, "raw", n = 1)
    }
    i <- i + 1
  }
  readBin(f, "raw", n = 1) # skip null byte
  mat <- mat[!is.na(mat[,1]),]
  mat[,1] <- mat[,1]/10
  mat
}

#' Skip null bytes
#' @author Ethan Bass
#' @noRd
skip_null_bytes <- function(f){
  while(TRUE){
    bin <- readBin(f, "raw", n = 1)
    if (as.character(bin) != "00"){
      seek(f, -1, origin = "current")
      break
    }
  }
}

#' Decode 'Varian SMS' value
#' @author Ethan Bass
#' @noRd
decode_sms_val <- function(hex) {
  num <- hex_to_int(hex)
  d <- extract_sign(hex, num)

  mask <- generate_mask(d)

  result <- bitwAnd(num, mask)
  return(result)
}

#' Generate mask for 'Varian' MS values
#' @author Ethan Bass
#' @noRd
generate_mask <- function(d) {
  # Define the mapping from leading digit to the number of bits
  bit_map <- c(
    '4' = 13, '5' = 13,
    '6' = 14, '7' = 14,
    '8' = 21, '9' = 21,
    '10' = 22, '11' = 22,
    '12' = 27, '13' = 27,
    '14' = 28, '15' = 28 # predicted
  )
  n_bits <- bit_map[[as.character(d)]]
  (2^n_bits) - 1
}

#' Extract leading digit from 'Varian' MS values
#' @author Ethan Bass
#' @noRd
extract_sign <- function(hex, num) {
  if (missing(num)){
    num <- hex_to_int(hex)
  }
  # Calculate the number of value bits
  value_bits <- 8*length(hex) - 4

  # Extract the sign (leftmost 4 bits)
  bitwAnd(bitwShiftR(num, n = value_bits), b = 0xF)
}

#' Translate hex (raw) to integer
#' @noRd
hex_to_int <- function(hex){
  x <- 0
  for (i in seq_along(hex)) {
    x <- bitwOr(bitwShiftL(x, n = 8), as.integer(hex[i]))
  }
  x
}

#' Read 'Varian SMS' MSdata header
#' The header contains 66 bytes of general information about the mass spectrum,
#' followed by 55 byte headers for each MS segment containing information
#' specific to each segment, such as the start and end times and maximum
#' ionization time.
#' @param f Connection to a 'Varian' SMS file.
#' @author Ethan Bass
#' @noRd
read_varian_msdata_header <- function(f){

  seek(f, 3238)

  readBin(f, "raw", n = 10) #skip

  ion_time <- readBin(f, what = "integer", size = 2, endian = "little",
                      signed = FALSE)

  emission_current <- readBin(f, what = "integer", size = 2, endian = "little",
                              signed = FALSE)

  dac <- readBin(f, what = "integer", size = 2, endian = "little",
                 signed = FALSE)

  u1 <- readBin(f, what = "integer", size = 2, endian = "little",
                signed = FALSE)

  t2 <- readBin(f, what = "raw", n = 4, endian = "little")
  t2 <- as.POSIXct(strtoi(paste(c(t2[2], t2[1], t2[3:4]), collapse = ""), 16),
                   tz = "UTC")

  t1 <- readBin(f, what = "raw", n = 4, endian = "little")
  t1 <- as.POSIXct(strtoi(paste(c(t1[2], t1[1], t1[3:4]), collapse = ""), 16),
                   tz = "UTC")

  u2 <- readBin(f, what = "integer", size = 2, endian = "little",
                signed = FALSE)
  readBin(f, what = "integer", size = 2, endian = "little") #skip

  n_scan <- readBin(f, what = "integer", size = 2, endian = "little",
                    signed = FALSE)
  readBin(f, what = "integer", size = 2, endian = "little") #skip

  max_ric_scan <- readBin(f, what = "integer", size = 2, endian = "little",
                          signed = FALSE)
  readBin(f, what = "integer", size = 2, endian = "little", signed = FALSE) #skip

  max_ric_val <- readBin(f, what = "integer", size = 2, endian = "little",
                         signed = FALSE)
  readBin(f, what = "integer", size = 2, endian = "little", signed = FALSE) #skip

  u3 <- readBin(f, what = "integer", size = 2, endian = "little",
                signed = FALSE)
  readBin(f, what = "integer", size = 2, endian = "little", signed = FALSE) #skip

  u4 <- readBin(f, what = "integer", size = 2, endian = "little",
                signed = FALSE)
  readBin(f, what = "integer", size = 2, endian = "little") #skip

  u5 <- readBin(f, what = "integer", size = 2, endian = "little",
                signed = FALSE)

  readBin(f, what = "integer", size = 2, endian = "little") #skip
  readBin(f, what = "raw", n = 12) #skip

  # reader segment headers
  seg_no <- readBin(f, what = "integer", size = 2)
  segment_metadata <- list()
  i <- 1
  while(seg_no == i){
    start_time <- readBin(f, what = "double", size = 8)

    end_time <- readBin(f, what = "double", size = 8)

    readBin(f, what = "raw", n = 1) #01

    start_scan <- readBin(f, what = "integer", size = 2, endian = "little",
                          signed = FALSE)
    readBin(f, what = "raw", n = 2)

    end_scan <- readBin(f, what = "integer", size = 2, endian = "little",
                        signed = FALSE)
    readBin(f, what = "raw", n = 2) # skip

    us1 <- readBin(f, what = "integer", size = 2, endian = "little",
                   signed = FALSE)
    readBin(f, what = "raw", n = 2) #skip

    us2 <- readBin(f, what = "integer", size = 2, endian = "little",
                   signed = FALSE)
    readBin(f, what = "raw", n = 2) # skip

    max_ionization_time <- readBin(f, what = "integer", size = 2,
                                   endian = "little", signed = FALSE)

    readBin(f, what = "raw", n = 2) # skip
    readBin(f, what = "raw", n = 16) # skip

    segment_metadata[[i]] <- mget(c("start_time", "end_time", "start_scan", "end_scan",
           "us1", "us2", "max_ionization_time"))
    seg_no <- readBin(f, what = "integer", size = 2,
                      endian = "little", signed = FALSE)
    i <- i + 1
  }
  readBin(f, what = "raw", n = 6)
  mget(c("ion_time", "emission_current", "dac", "u1", "t1", "t2", "u2", "n_scan",
  "max_ric_scan", "max_ric_val", "u3", "u4", "u5", "segment_metadata"))
}

#' Read 'Varian SMS' offsets from header
#' @param f Connection to a 'Varian SMS' file.
#' @author Ethan Bass
#' @noRd
read_varian_offsets <- function(f){
  seek(f, 28)
  readBin(f, "raw", n = 10)
  mat <- matrix(NA, 20, 4)
  i <- 1
  name <- ""
  while (name != "InjectionLog"){
    mat[i,1] <- readBin(f, "integer", size = 4)

    mat[i,2] <- readBin(f, "integer", size = 4)

    mat[i,3] <- readBin(f, "integer", size = 2)

    readBin(f, "raw", n = 8)
    name <- readBin(f, "character")
    mat[i,4] <- name
    skip_null_bytes(f)
    i <- i + 1
  }
  mat <- mat[!is.na(mat[,1]),]
  data.frame(start = as.numeric(mat[,1]), end = as.numeric(mat[,2]),
             number = as.numeric(mat[,3]), name = mat[,4])
}
