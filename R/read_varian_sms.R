#' Read 'Varian' SMS
#'
#' Parser for reading 'Varian Workstation' SMS file.
#'
#' Varian SMS files begin with a "DIRECTORY" with offsets for each section. The
#' first section (in all the files I've been able to inspect) is "MSData"
#' generally beginning at byte 3238. This MSdata section is in turn divided into
#' two sections. The first section (after a short header) contains chromatogram
#'  data. Some of the information found in this section includes scan numbers,
#' retention times, (as 64-bit
#' floats), the total ion chromatogram (TIC), the base peak chromatogram (BPC),
#' as well as some other unidentified information. The scan numbers and
#' intensities for the TIC and BPC are stored at 4-byte little-endian integers.
#' Following this section, there is a series of null bytes, followed by a series
#' of segments containing the mass spectra.
#'
#' The encoding scheme for the mass spectra is somewhat more complicated. Each
#' scan is represented by a series of values of variable length separated from
#' the next scan by two null bytes. Within these segments, values are paired.
#' The first value in each pair represents the delta-encoded mass-to-charge ratio,
#' while the second value represents the intensity of the signal. All values in
#' this section are offset encoded using a base of value of 4096 (16^3). Thus,
#' integers beginning with digits 0-3 are simple 2-byte integers. However, values
#' with a prefix digit >= 4 are 4-byte integers with offset encoding, where the
#' offset is (n-4) * 4096. So values with a prefix digit of 4 have an offset of 0,
#' values with a prefix digit of 5 have an offset of 8192 (4096 * 2), and so on.
#'
#' @param path Path to \code{.SMS} files.
#' @param what Whether to extract chromatograms (\code{chroms}) and/or
#' \code{MS1} data. Accepts multiple arguments.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file. (This is just a
#' placeholder for now as there is not yet support for parsing metadata).
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element.
#' @return A nested list of elements from the specified \code{file}, where the
#' top levels are chromatograms and/or mass spectra according to
#' the value of \code{what}. Chromatograms are returned in the format specified
#' by \code{format_out} (retention time x wavelength).
#' @author Ethan Bass
#' @note There is not yet support for the extraction of metadata from this file
#' format.

read_varian_sms <- function(path, what = c("chrom", "MS1"),
                            format_out = c("matrix", "data.frame"),
                            data_format = c("long", "wide"),
                            read_metadata = TRUE, collapse = TRUE){

  what <- match.arg(what, c("chroms", "MS1"), several.ok = TRUE)
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))

  f <- file(path, "rb")
  on.exit(close(f))

  chroms <- read_varian_chromatograms(f)

  skip_null_bytes(f)

  acq_delay <- max(which(chroms[,"tic"] == 0))
  n_scans <- nrow(chroms) - acq_delay
  if ("MS1" %in% what){
    MS1 <- read_varian_ms_stream(f, n_scans = n_scans)

    MS1[,1] <- chroms[(MS1[,1] + acq_delay), "rt"]
    # all.equal(MS1[,1],ms1$rt/1000)
    colnames(MS1) <- c('rt', 'mz', 'int')
  }

  dat <- mget(what)
  if (collapse)
    dat <- collapse_list(dat)
  dat
}

#' Read 'Varian Workstation' Chromatograms
#' @author Ethan Bass
#' @noRd
read_varian_chromatograms <- function(f){
  offsets <- list(scan_no = 3268, ms_start = 3422)
  seek(f, offsets$scan_no)
  n_time <- readBin(f, "integer", size = 4)
  seek(f, offsets$ms_start)
  mat <- matrix(NA, nrow = n_time, ncol = 5)
  colnames(mat) <- c("scan", "rt", "tic", "bpc", "unk")
  for (i in seq_len(n_time)){
    mat[i,"scan"] <- readBin(f, what="integer", size = 4)
    mat[i,"rt"] <- readBin(f, what = "double", size = 8)
    mat[i,"unk"] <- readBin(f, what = "integer",size = 2, signed = FALSE)
    mat[i,"tic"] <- readBin(f, what = "integer", size = 4)
    readBin(f, what="raw", n = 6) # skip six unidentified bytes
    mat[i,"bpc"] <- readBin(f, what="integer", size = 4)
    readBin(f, what="raw", n = 11) # skip 11 unidentified bytes
  }
  mat
}

#' Read 'Varian' MS stream
#' @author Ethan Bass
#' @noRd
read_varian_ms_stream <- function(f, n_scans){
  xx <- lapply(seq_len(n_scans), function(i){
    xx <- read_varian_ms_block(f)
    cbind(scan = i, xx)
  })
  do.call(rbind, xx)
}

#' Read 'Varian' MS block
#' @author Ethan Bass
#' @noRd
read_varian_ms_block <- function(f){
  buffer <- list(0, 0, 0, 0)
  mat <- matrix(nrow = 1000, ncol = 2)
  i=1
  buffer[[3]] <- readBin(f, "raw", n = 1)
  while (buffer[[3]] != "00"){
    for (j in c(1:2)){
      hex1 <- as.numeric(substr(buffer[[3]], start = 1, stop = 1))
      if (hex1 < 4){
        buffer[[2]] <- strtoi(buffer[[3]], base = 16)
      } else if (hex1 >= 4){
        buffer[[4]] <- readBin(f, "raw", n = 1)
        buffer[[2]] <- strtoi(paste0(substr(buffer[[3]], 2, 2), buffer[[4]]),
                              base = 16)
        if (hex1 >= 5) buffer[[2]] <- buffer[[2]] + (hex1-4)*4096
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
