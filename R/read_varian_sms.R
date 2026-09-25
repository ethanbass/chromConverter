#' Read 'Varian' SMS
#'
#' Reads 'Varian Workstation' SMS files.
#'
#' Varian SMS files begin with a `DIRECTORY` holding the offsets of each
#' section. The first section is `MSData`, which begins at byte `3238` in every
#' file seen so far, and is itself divided in two. The first part, after a short
#' header, holds chromatogram data, one record per scan: the scan number, the
#' retention time (as a 64-bit float), the ion time (µsec, as a 2-byte unsigned
#' integer), the total ion chromatogram (TIC), the base peak chromatogram
#' (BPC), and further unidentified fields. The scan numbers and the
#' TIC and BPC intensities are stored as 4-byte little-endian integers. A run of
#' null bytes then separates this part from the segments holding the mass
#' spectra.
#'
#' The mass spectra are encoded differently. Each scan is a series of
#' variable-length values, separated from the next scan by two null bytes.
#' Within a scan the values are paired: the first of each pair is the
#' delta-encoded mass-to-charge ratio and the second is the intensity. Each
#' value is a big-endian integer whose length, `1 + (d %/% 4)` bytes, and bit
#' width are set by its leading hexadecimal digit (`d`). Values beginning with
#' `0-3` are single bytes. For `d >= 4`, the lowest `n` bits are preserved
#' according to the following scheme:
#'
#' * d = 4-5 -> preserve lowest 13 bits
#' * d = 6-7 -> preserve lowest 14 bits
#' * d = 8-9 -> preserve lowest 21 bits
#' * d = 10-11 (A-B) -> preserve lowest 22 bits
#' * d = 12-13 (C-D) -> preserve lowest 27 bits
#' * d = 14-15 (E-F) -> preserve lowest 28 bits
#'
#' No file seen so far carries a leading digit above `C`, so the rules for `D`
#' and for `E`-`F` are extrapolated from the others rather than observed.
#'
#' @inheritParams shared_params
#' @param path Path to a 'Varian' `.SMS` file.
#' @param what Which streams to get: mass spectra (`MS1`), the total ion
#' chromatogram (`TIC`) and/or the base peak chromatogram (`BPC`). Accepts
#' multiple arguments. Defaults to all three.
#' @param data_format Whether to return data in `long` (default) or `wide`
#' format. Mass spectra are always returned in `long` format.
#' @return A chromatogram or list of chromatograms from the specified file,
#' according to the value of `what`. Chromatograms are returned in the format
#' specified by `format_out`, except that mass spectra are returned as a
#' `data.table` when `format_out` is `matrix`.
#' @author Ethan Bass
#' @note There is still only limited support for the extraction of metadata from
#' this file format.
#' @examples \dontrun{
#' read_varian_sms(path)
#' }
#' @family 'Varian' parsers
#' @export

read_varian_sms <- function(path, what = c("MS1", "TIC", "BPC"),
                            format_out = c("matrix", "data.frame", "data.table"),
                            data_format = "long",
                            read_metadata = TRUE, collapse = TRUE){

  what <- match.arg(what, c("MS1", "TIC", "BPC"), several.ok = TRUE)
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)

  f <- file(path, "rb")
  on.exit(close(f))

  # Read the directory up front so the mass spectra stream can be bounded by the
  # end of the MSData section instead of the end of the file. Some files carry a
  # large tail of peak tables and results after the spectra. Tolerate a
  # unreadable directory here and fall back to the file size; if metadata is
  # actually requested the error is raised below instead.
  offsets <- tryCatch(read_varian_offsets(f), error = function(e) NULL)
  flen <- file.size(path)
  if (!is.null(offsets)){
    ms_end <- offsets$end[offsets$name == "MSData"]
    if (length(ms_end) == 1 && !is.na(ms_end) && ms_end > 3238){
      flen <- min(ms_end, flen)
    }
  }

  meta <- read_varian_msdata_header(f)

  chroms <- read_varian_chromatograms(f, n_time = meta$n_scan,
                                      format_out = format_out,
                                      data_format = "long")

  skip_null_bytes(f)

  acq_delay <- max(which(get_column(chroms, "tic") == 0))
  n_scans <- nrow(chroms) - acq_delay
  if ("MS1" %in% what){
    MS1 <- read_varian_ms_stream(f, n_scans = n_scans, flen = flen,
                                 format_out = format_out)
    # the stream numbers its scans from the start of acquisition, so the scan
    # column is translated into the retention time of the corresponding row of
    # the chromatograms
    rt <- get_column(chroms, "rt")[get_column(MS1, 1) + acq_delay]
    if (inherits(MS1, "data.table")){
      data.table::set(MS1, j = 1L, value = rt)
      data.table::setnames(MS1, c("rt", "mz", "intensity"))
    } else {
      MS1[, 1] <- rt
      colnames(MS1) <- c("rt", "mz", "intensity")
    }
  }

  if (any(what == "TIC")){
    TIC <- format_2d_chromatogram(rt = get_column(chroms, "rt"),
                                  int = get_column(chroms, "tic"),
                                  data_format = data_format,
                                  format_out = format_out)
  }
  if (any(what == "BPC")){
    BPC <- format_2d_chromatogram(rt = get_column(chroms, "rt"),
                                  int = get_column(chroms, "bpc"),
                                  data_format = data_format,
                                  format_out = format_out)
  }
  dat <- mget(what)
  if (read_metadata){
    if (is.null(offsets)){
      offsets <- read_varian_offsets(f)
    }

    meta <- utils::modifyList(meta, read_varian_injection_log(f, offsets))

    # the SamplePrep section is the more specific source for the sample name
    prep_offset <- offsets[grep("SamplePrep", offsets$name), "start"]
    seek(f, prep_offset)
    meta$sample_name <-  readBin(f, "character")

    meta <- read_mod_metadata(f, offsets, meta)

    dat <- purrr::imap(dat, function(x, h){
      attach_metadata(x, meta, format_in = "varian_sms",
                      format_out = ifelse(h == "MS1",
                                          check_format_out_table(format_out),
                                          format_out),
                      data_format = ifelse(h == "MS1", "long", data_format),
                      source_file = path, source_file_format = "varian_sms")
    })
  }
  if (collapse){
    dat <- collapse_list(dat)
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
#' @param data_format Either `wide` (default) or `long`.
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
    rownames(dat) <- dat[, "rt"]
    dat <- dat[,-2]
  }
  dat <- convert_chrom_format(dat, format_out = format_out)
  dat
}

#' Decode tables for the 'Varian SMS' mass spectra stream
#'
#' Values in the MS stream are variable-length big-endian integers whose length
#' and bit-width are a pure function of the leading nibble (`d`) of the first
#' byte, per the scheme documented in the details section of [read_varian_sms()]:
#' `len = 1 + (d %/% 4)`, preserving the lowest `bits` of the value.
#'
#' `.sms_lead_mask` masks the *lead byte* rather than the assembled integer.
#' This is equivalent -- in every case the preserved width exceeds
#' `8 * (len - 1)`, so the mask only ever clips bits inside the lead byte -- but
#' it keeps every intermediate within 28 bits. Assembling first and masking
#' afterwards overflows `.Machine$integer.max` on 4-byte values and yields `NA`
#' ("NAs introduced by coercion to integer range"), so do not reorder these.
#' Both tables are indexed `[d + 1L]`.
#' @noRd
.sms_val_len   <- 1L + (0:15) %/% 4L
.sms_lead_mask <- bitwShiftL(1L, c(rep(8L, 4), 13L, 13L, 14L, 14L,
                                   21L, 21L, 22L, 22L,
                                   27L, 27L, 28L, 28L) -
                                 8L * ((0:15) %/% 4L)) - 1L

#' Read 'Varian' MS stream
#'
#' Reads the whole stream into memory and decodes it with a single pass. Each
#' scan is a run of (delta-encoded m/z, intensity) pairs terminated by two null
#' bytes.
#'
#' A fully vectorized tokenizer is possible -- build `nxt[i] = i + len[i]` over
#' every byte and recover all token starts by pointer doubling, then decode in
#' bulk -- and benchmarks about 2x faster again (0.71s vs 1.66s on STRD15.SMS)
#' at ~1.6x the peak memory. It is not used here because the format is still
#' only partly reverse-engineered: this loop can be stepped through with
#' `browser()` to find the exact offset where an unfamiliar file derails,
#' whereas a wrong length rule in a vectorized walk silently misaligns every
#' token downstream of it. Worth revisiting if batch conversion becomes a
#' bottleneck.
#'
#' @param f Connection to a 'Varian' SMS file opened to the beginning of the
#' mass spectra stream.
#' @param n_scans Number of scans to read.
#' @param flen Length of the file in bytes.
#' @author Ethan Bass
#' @noRd
read_varian_ms_stream <- function(f, n_scans, flen, format_out = "data.frame"){
  format_out <- check_format_out_table(format_out)
  b <- as.integer(readBin(f, "raw", n = flen - seek(f)))
  cap <- length(b) %/% 2L # upper bound: each pair spans at least two bytes
  sc <- integer(cap); mzv <- numeric(cap); iv <- numeric(cap)
  k <- 0L; p <- 1L
  for (s in seq_len(n_scans)){
    mz <- 0
    repeat {
      lead <- b[p]
      if (lead == 0L){
        p <- p + 2L # two null bytes terminate the scan
        break
      }
      d <- lead %/% 16L; len <- .sms_val_len[d + 1L]
      v <- bitwAnd(lead, .sms_lead_mask[d + 1L])
      if (len > 1L) for (j in seq_len(len - 1L)) v <- bitwShiftL(v, 8L) + b[p + j]
      p <- p + len
      mz <- mz + v # m/z is delta-encoded within each scan

      lead <- b[p]
      d <- lead %/% 16L; len <- .sms_val_len[d + 1L]
      w <- bitwAnd(lead, .sms_lead_mask[d + 1L])
      if (len > 1L) for (j in seq_len(len - 1L)) w <- bitwShiftL(w, 8L) + b[p + j]
      p <- p + len

      k <- k + 1L
      sc[k] <- s; mzv[k] <- mz; iv[k] <- w
    }
  }
  i <- seq_len(k)
  dat <- cbind(scan = sc[i], mz = mzv[i]/10, intensity = iv[i])
  convert_chrom_format(dat, format_out = format_out)
}

#' Read 'Varian' InjectionLog metadata
#'
#' The InjectionLog section is a dump of an in-memory structure, most of which
#' is stale pointers, but a handful of null-terminated strings sit at stable
#' offsets from the start of the section. The offsets below were confirmed
#' against the sample files available to us, which come from the same
#' instrument and software build, so each field is validated before use and
#' dropped if it does not look like text.
#'
#' Offset 2 holds the injection time as a little-endian Unix timestamp, which
#' matches the acquisition start recorded in the MSData header.
#'
#' @param f Connection to a 'Varian' SMS file.
#' @param offsets Directory returned by `read_varian_offsets`.
#' @author Ethan Bass
#' @noRd
read_varian_injection_log <- function(f, offsets){
  i <- which(offsets$name == "InjectionLog")
  if (length(i) != 1) return(list())
  n <- offsets$end[i] - offsets$start[i]
  if (is.na(n) || n < 700) return(list())

  seek(f, offsets$start[i])
  b <- readBin(f, "raw", n = n)

  fields <- c(sample_name = 6, control_software = 28,
              control_software_version = 70, os_version = 112,
              instrument = 196, method = 415, sample_list = 675)
  out <- lapply(fields, function(off) read_null_terminated(b, off))
  out$injection_time <- {
    x <- readBin(b[3:6], "integer", size = 4, endian = "little")
    if (is.na(x) || x <= 0) NA else as.POSIXct(x, origin = "1970-01-01",
                                               tz = "UTC")
  }
  out[!vapply(out, is.null, logical(1))]
}

#' Extract a null-terminated string from a raw vector
#'
#' Returns `NULL` rather than a garbled string if the bytes at `offset` are not
#' printable text, so that a file laid out differently than expected drops the
#' field instead of reporting nonsense.
#' @author Ethan Bass
#' @noRd
read_null_terminated <- function(b, offset, min_length = 1){
  if (offset >= length(b)) return(NULL)
  tail <- b[(offset + 1L):length(b)]
  z <- which(tail == as.raw(0))[1]
  if (is.na(z) || z <= min_length) return(NULL)
  chars <- as.integer(tail[seq_len(z - 1L)])
  if (any(chars < 32 | chars > 126)) return(NULL)
  rawToChar(tail[seq_len(z - 1L)])
}

#' Read a little-endian 32-bit Unix timestamp
#' @author Ethan Bass
#' @noRd
read_unix_time <- function(f){
  x <- readBin(f, what = "integer", size = 4, endian = "little")
  if (is.na(x) || x <= 0) return(NA)
  as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
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

  # Acquisition start and end, as little-endian 32-bit Unix timestamps. The
  # start matches the `startTimeStamp` OpenChrom writes for the same sample, and
  # the interval between the two matches the span of the chromatogram.
  acquisition_start <- read_unix_time(f)
  acquisition_end <- read_unix_time(f)

  u1 <- readBin(f, what = "integer", size = 2, endian = "little",
                signed = FALSE)

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
  mget(c("ion_time", "emission_current", "dac", "u1", "acquisition_start",
  "acquisition_end", "u2", "n_scan", "max_ric_scan", "max_ric_val", "u3", "u4",
  "u5", "segment_metadata"))
}

#' Read 'Varian SMS' offsets from header
#'
#' The DIRECTORY is a fixed-width table of 50-byte records running from byte 38
#' to the end offset given by its own (first) record. Each record holds a 4-byte
#' start offset, a 4-byte end offset, a 2-byte number, 8 bytes of timestamps and
#' a 32-byte null-padded name. Unused slots are zero-filled.
#'
#' The records must be indexed by their fixed stride rather than by scanning for
#' the null padding after each name: a start offset that is a multiple of 256
#' begins with a null byte that is indistinguishable from padding, which shifts
#' every field in that record by one byte (e.g. record 3 of `STRD15.SMS`).
#'
#' @param f Connection to a 'Varian SMS' file.
#' @author Ethan Bass
#' @noRd
read_varian_offsets <- function(f){
  seek(f, 0, origin = "end")
  flen <- seek(f)

  seek(f, 38) # 28-byte file header + 10 bytes
  hdr <- readBin(f, "raw", n = 50) # the first record describes the DIRECTORY
  dir_end <- readBin(hdr[5:8], "integer", size = 4, endian = "little")
  if (is.na(dir_end) || dir_end <= 38){
    stop("Could not read the directory of this 'Varian SMS' file.")
  }
  # don't trust a corrupt end offset to size the read
  n <- min(dir_end, flen) %/% 50 - 38 %/% 50

  seek(f, 38)
  b <- readBin(f, "raw", n = n * 50)
  o <- (seq_len(n) - 1L) * 50L

  read_field <- function(idx, size){
    vapply(o, function(p) readBin(b[p + idx], "integer", size = size,
                                  endian = "little"), numeric(1))
  }
  name <- vapply(o, function(p){
    nm <- b[p + 19:50]
    z <- which(nm == as.raw(0))
    if (length(z) && z[1] > 1) rawToChar(nm[seq_len(z[1] - 1L)]) else ""
  }, character(1))

  keep <- nzchar(name)
  data.frame(start = read_field(1:4, 4)[keep], end = read_field(5:8, 4)[keep],
             number = read_field(9:10, 2)[keep], name = name[keep])
}
