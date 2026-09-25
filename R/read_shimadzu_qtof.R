#' Read 'Shimadzu' QTOF stream
#'
#' Read QTOF stream from 'Shimadzu LabSolutions' `.lcd` files.
#'
#' Data for each scan is stored in three contiguous blocks: a 64-byte header,
#' a block of flight times, followed by a block of intensities.
#'
#' **Scan Header** (64 bytes, little-endian):
#'
#' | **Offset**  | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3| `uint32` | Data-dependent acquisition (DDA) cycle: sequential over survey scans, repeated by the product-ion scans acquired from each |
#' | 4–7 | `uint32` | Retention time (milliseconds) |
#' | 8–15 | `uint64` | Base peak flight time (0 if empty scan) |
#' | 16–19 | `uint32` | Base peak raw intensity (0 if empty scan) |
#' | 20–23 | `uint32` | Scan index (0-based; increments for every scan including empty ones) |
#' | 24–27 | `uint32` | Data block size in bytes (`n_peaks * (8 + int_width)`; 0 if empty scan) |
#' | 28–31 | `uint32` | MS level in the upper 16 bits, acquisition event in the lower 16 bits (e.g. `0x00010001` = MS1 event 1, `0x00020003` = MS2 event 3) |
#' | 32–35 | `uint32` | Padding |
#' | 36–39 | `uint32` | Intensity width in bytes (`int_width`); 1, 2 and 4 all occur on scans with data |
#' | 40–43 | `uint32` | Instrument constant (scan window / detector setting) |
#' | 44–47 | `uint32` | Instrument constant |
#' | 48–51 | `uint32` | Instrument constant |
#' | 52–55 | `uint32` | Instrument constant |
#' | 56–63 | — | Padding |
#'
#' **m/z block** (n x 8 bytes)
#' Each peak's flight time is an unsigned 64-bit little-endian integer. R has
#' no 64-bit integer type, so it is read as two 32-bit halves and recombined as
#' `low + high * 2^32`. The low half is unsigned and has to be corrected as
#' such, since R has no unsigned 32-bit type either: read as signed, it shifts
#' half of all flight times by `2^32` (~4 ppm of flight time, so ~8 ppm of
#' m/z), and `0x80000000` comes back as `NA`. Conversion is
#' \deqn{mz = ((t-B)/A)^2}
#'
#' `A` and `B` are fitted per file from the `TOF Calibration Table` stream (see
#' `read_sz_qtof_calibration`),
#' which stores calibration points for both polarities; the polarity of the
#' acquisition selects between them. This matters: the coefficients differ by
#' ~1.5% between polarities on one instrument, which is ~3% in m/z, so fixed
#' coefficients are not usable. The tuning calibration alone is still a few
#' ppm out, because it is recorded before the run; folding in the mass
#' correction the file caches for the run closes that gap (see
#' `read_sz_qtof_mass_correction`).
#' Measured against 'ProteoWizard' conversions of three files from two
#' instruments --- which read the file through Shimadzu's own library --- the
#' m/z then agree to 0.03-0.1 ppm (median). That figure is the resolution of
#' the comparison rather than of this parser: the vendor reports m/z to four
#' decimal places, which is 0.5 ppm at the bottom of the mass range and 0.05
#' ppm at the top.
#'
#' **Intensity block** (n x m bytes)
#' Each peak's intensity is stored as a little-endian unsigned integer of
#' `m` bytes, where `m` is the `int_width` field from the scan
#' header (2 for most scans, 4 for scans containing large values). The stored
#' values are raw detector counts summed over every TOF accumulation in the
#' scan --- the individual time-of-flight spectra the instrument adds together
#' to make one. LabSolutions (and the 'Shimadzu' library used by
#' 'ProteoWizard') normalizes them to a nominal 100 accumulations:
#' \deqn{intensity = round(raw \times 100 / n)}
#' where `n` is the number of accumulations per scan (rounding is
#' half-up). `n` is read from the `Status` stream, which stores it
#' once per acquisition event; it is 376 in the reference file, giving a
#' divisor of 3.76.
#'
#' @param path Path to `.lcd` file.
#' @param format_out Class of output. Either `matrix`,
#' `data.frame`, or `data.table`.
#' @param data_format Format of output. Spectra have no wide representation, so
#' this is always `long`.
#' @param read_metadata Logical. Whether to read metadata from the file.
#' @param metadata_format Format to output metadata in.
#' @param scale Logical. Whether to scale raw detector counts to the
#' intensities reported by 'LabSolutions'. Defaults to `TRUE`, which errors if
#' the accumulation count is missing from the file, since there is no safe
#' divisor to guess; `FALSE` returns the raw counts.
#' @param levels Which MS levels to return, spelled `MS1` and `MS2`. Both are
#' decoded either way, since the level of a scan is recorded in the scan.
#' @return A named list holding whichever of `MS1` and `MS2` the file
#' contains, each a `data.table`, `data.frame` or `matrix` in
#' long format with columns `scan`, `rt`, `mz` and
#' `intensity`, preceded by `precursor_mz` for product-ion
#' spectra. Each carries a `scan_info` attribute giving one row per
#' spectrum of that level, including the ones that hold no peaks, with the
#' acquisition event, MS level, DDA cycle, ion polarity, selected precursor
#' and peak count.
#' @md
#' @keywords internal

read_sz_qtof <- function(path, format_out = c("matrix", "data.frame",
                                              "data.table"),
                         data_format = "long", levels = c("MS1", "MS2"),
                         read_metadata = TRUE,
                         metadata_format = "shimadzu_lcd",
                         scale = TRUE){
  format_out <- check_format_out_table(format_out)
  # spectra have no wide representation, so they are always returned long
  data_format <- "long"
  existing_streams <- check_streams(path, what = "qtof")
  if (length(existing_streams) == 0){
    stop("QTOF data stream could not be detected.")
  }

  rts <- read_qtof_retention_times(path)

  # polarity and the scan window come out of one `Mass Parameters` walk, and
  # the polarity then selects the calibration block and the reference masses,
  # so reading it once here saves three more walks of the same stream
  mass_params <- read_qtof_mass_params(path)
  polarity <- read_qtof_polarity(path, fields = mass_params)
  mz_range <- read_qtof_mz_range(path, fields = mass_params)

  int_scale <- if (scale) read_qtof_int_scale(path) else 1
  cal <- read_sz_qtof_calibration(path, polarity = polarity)
  offsets <- read_qtof_spectrum_index(path)
  if (is.null(offsets)){
    stop("The 'Centroid Index' stream could not be read.")
  }

  path_centroid <- export_stream(path, existing_streams[[1]])
  on.exit(unlink_stream(path_centroid))

  # a cached correction already carries the vendor's mass correction, so the
  # reference ions are only wanted where there is none to fold in
  corrected <- isTRUE(attr(cal, "corrected"))
  lock_mass <- if (corrected) numeric(0) else
    read_qtof_lock_mass(path, polarity = polarity)
  if (!corrected && length(lock_mass) < 2){
    warning("No mass correction was found in the file, and fewer than two ",
            "reference ions are available to fit one. The mass axis rests on ",
            "the tuning calibration alone, which is typically a few ppm out.",
            call. = FALSE)
  }

  decoded <- decode_qtof_stream(path_centroid, offsets = offsets,
                                n_expected = length(rts), A = cal[["A"]],
                                B = cal[["B"]], int_scale = int_scale,
                                lock_mass = lock_mass,
                                dda = read_qtof_dda(path),
                                polarity = polarity,
                                mz_range = mz_range)
  dat <- decoded$dat
  dat[, rt := rt/60000]
  # `scan_info` mixes numeric and character columns, so it stays a data.frame
  # whatever `format_out` is asked for
  sz_split_ms_levels(dat, decoded$scan_info, levels = levels,
                     level = decoded$level,
                     meta = if (read_metadata){
                       read_qtof_metadata(path, polarity = polarity,
                                          mz_range = mz_range,
                                          time_range = range(rts)/60000)
                     } else NULL,
                     path = path, format_out = format_out,
                     metadata_format = metadata_format, scale = scale)
}

#' Read a varint from a protobuf-encoded stream
#'
#' Accumulates by multiplication rather than bit shifting, because these
#' streams carry values well past 32 bits (flight times reach ~2e15, which is
#' still exact in a double).
#' @noRd
sz_pb_varint <- function(bytes, i){
  value <- 0
  scale <- 1
  n <- 0L
  repeat {
    if (i > length(bytes)) return(NULL)
    b <- bytes[i]
    i <- i + 1L
    n <- n + 1L
    value <- value + bitwAnd(b, 0x7FL) * scale
    if (bitwAnd(b, 0x80L) == 0L) break
    scale <- scale * 128
    if (n > 10L) return(NULL)
  }
  list(value = value, pos = i, nbytes = n)
}

#' Walk a protobuf-encoded 'Shimadzu' parameter stream
#'
#' No schema is published for these streams, so this walks the wire format and
#' reports every field as a dotted path of field numbers.
#'
#' Without a schema there is no telling a nested message from a string or a
#' blob --- on the wire they are the same length-delimited field --- so both
#' are guessed at. A payload of entirely printable bytes is reported as a
#' string, a longer one is walked as a message, and what is left over (a
#' payload of one byte or none, or anything more than six levels deep) is
#' dropped rather than reported. A short message that happens to be printable
#' will therefore come back as a string, and a field that goes missing here is
#' most likely one of the dropped cases.
#'
#' @return A list of `list(path, wire, value, nbytes)`.
#' @noRd
sz_pb_walk <- function(bytes, path = "", depth = 0L, out = list()){
  i <- 1L
  n <- length(bytes)
  while (i <= n){
    tag <- sz_pb_varint(bytes, i)
    if (is.null(tag)) break
    i <- tag$pos
    field <- tag$value %/% 8
    wire <- tag$value %% 8
    p <- if (nzchar(path)) paste0(path, ".", field) else as.character(field)
    if (wire == 0){
      v <- sz_pb_varint(bytes, i)
      if (is.null(v)) break
      i <- v$pos
      out[[length(out) + 1L]] <- list(path = p, wire = "varint",
                                      value = v$value, nbytes = v$nbytes)
    } else if (wire == 2){
      len <- sz_pb_varint(bytes, i)
      if (is.null(len)) break
      i <- len$pos
      if (len$value < 0 || i + len$value - 1L > n) break
      sub <- if (len$value == 0) integer(0) else bytes[i:(i + len$value - 1L)]
      i <- i + len$value
      if (length(sub) > 0 && all(sub >= 32L & sub < 127L)){
        out[[length(out) + 1L]] <- list(path = p, wire = "string",
                                        value = rawToChar(as.raw(sub)),
                                        nbytes = length(sub))
      } else if (depth < 6L && length(sub) > 1){
        out <- c(out, sz_pb_walk(sub, p, depth + 1L))
      }
    } else if (wire == 1){
      if (i + 7L > n) break
      out[[length(out) + 1L]] <- list(path = p, wire = "fixed64",
        value = readBin(as.raw(bytes[i:(i + 7L)]), "double", size = 8L,
                        endian = "little"), nbytes = 8L)
      i <- i + 8L
    } else if (wire == 5){
      if (i + 3L > n) break
      out[[length(out) + 1L]] <- list(path = p, wire = "fixed32",
        value = readBin(as.raw(bytes[i:(i + 3L)]), "double", size = 4L,
                        endian = "little"), nbytes = 4L)
      i <- i + 4L
    } else break
  }
  out
}

#' Read a protobuf 'Shimadzu' parameter stream
#'
#' These streams begin with a 64-byte ASCII name padded with NULs, followed by
#' the message itself, with no length prefix or enclosing field around it. A
#' few (e.g. `Mass Correction Parameter`) have no such header, so the header is
#' only skipped when it is actually present.
#' @noRd
read_sz_pb_stream <- function(path, stream, records = FALSE){
  path_pb <- export_stream(path, stream)
  if (length(path_pb) != 1 || is.na(path_pb)) return(NULL)
  on.exit(unlink_stream(path_pb))
  raw <- readBin(path_pb, what = "raw", n = file.size(path_pb))
  bytes <- as.integer(raw)
  if (length(bytes) > 64L){
    head <- bytes[1:64]
    nul <- which(head == 0L)
    if (length(nul) > 0 && nul[1] > 1L &&
        all(head[1:(nul[1] - 1L)] >= 32L & head[1:(nul[1] - 1L)] < 127L) &&
        all(head[nul[1]:64] == 0L)){
      bytes <- bytes[-(1:64)]
    }
  }
  if (records) sz_pb_records(bytes) else sz_pb_walk(bytes)
}

#' Split a protobuf stream into its top-level records
#'
#' Some 'Shimadzu' streams are a repeated length-delimited field at the top
#' level, one entry per scan. `sz_pb_walk` would flatten those: every record
#' carries the same field numbers, so the boundaries between them would be
#' lost. Anything that is not a length-delimited field at the top level ends
#' the walk, as does a record that runs past the end of the stream.
#'
#' @return A list holding one `sz_pb_walk` field list per record.
#' @noRd
sz_pb_records <- function(bytes){
  out <- list()
  i <- 1L
  n <- length(bytes)
  while (i <= n){
    tag <- sz_pb_varint(bytes, i)
    if (is.null(tag) || tag$value %% 8 != 2) break
    i <- tag$pos
    len <- sz_pb_varint(bytes, i)
    if (is.null(len)) break
    i <- len$pos
    # the record ends `len` bytes after the *payload* starts, not after the
    # length varint starts, and a zero-length record must not be turned into
    # the backwards range `i:(i - 1)`
    if (len$value < 0 || i + len$value - 1L > n) break
    sub <- if (len$value == 0) integer(0) else bytes[i:(i + len$value - 1L)]
    out[[length(out) + 1L]] <- sz_pb_walk(sub)
    i <- i + len$value
  }
  out
}

#' Read the 'Shimadzu' QTOF `Mass Parameters` stream
#'
#' Both the ion polarity and the acquisition m/z window come out of this one
#' protobuf message, and walking it costs an OLE open, a temp file and a full
#' interpreted pass, so the readers below take an already-parsed field list.
#' Each still defaults to reading it, which keeps them usable on their own.
#' @noRd
read_qtof_mass_params <- function(path){
  tryCatch(read_sz_pb_stream(path, c("QTFL RawData", "Mass Parameters")),
           error = function(e) NULL)
}

#' Read the ion polarity of a 'Shimadzu' QTOF acquisition
#'
#' The `Mass Parameters` stream records a voltage whose sign follows the ion
#' polarity (field `120.10.230.220`, +/-1007276000 in every file examined).
#' A negative `int64` is emitted by protobuf as a full 10-byte varint, which is
#' how the sign is recovered without 64-bit arithmetic.
#'
#' @return `"positive"`, `"negative"`, or `NA` if the field is absent.
#' @noRd
read_qtof_polarity <- function(path, fields = read_qtof_mass_params(path)){
  if (is.null(fields)) return(NA_character_)
  hit <- Filter(function(f) identical(f$path, "120.10.230.220") &&
                            identical(f$wire, "varint"), fields)
  if (length(hit) == 0) return(NA_character_)
  if (hit[[1]]$nbytes >= 10L) "negative" else "positive"
}

#' Decode every varint in a protobuf stream at once
#'
#' The `DDA` stream holds one record per product-ion spectrum and runs to
#' hundreds of kilobytes, so walking it a byte at a time is the slowest part of
#' reading a QTOF file. It contains only varints and nested messages --- no
#' strings or fixed-width fields --- which means every byte belongs to some
#' varint and the whole stream can be split on the continuation bit in one
#' vectorized pass.
#'
#' @return A list of `value`, `start` and `end` (byte positions, 1-based), or
#'   `NULL` if the stream does not decompose cleanly.
#' @noRd
sz_pb_varints <- function(bytes){
  if (length(bytes) == 0) return(NULL)
  ends <- which(bitwAnd(bytes, 0x80L) == 0L)
  # a trailing continuation bit means the stream is not varints all the way
  if (length(ends) == 0 || ends[length(ends)] != length(bytes)) return(NULL)
  starts <- c(1L, ends[-length(ends)] + 1L)
  lens <- ends - starts + 1L
  idx <- sequence(lens, from = starts)
  parts <- bitwAnd(bytes[idx], 0x7FL) * 2^(7*(sequence(lens) - 1L))
  value <- as.numeric(rowsum(parts, rep.int(seq_along(lens), lens),
                             reorder = FALSE))
  list(value = value, start = starts, end = ends)
}

#' Read the acquisition m/z window of a 'Shimadzu' QTOF file
#'
#' `Mass Parameters` records the m/z range the instrument was told to scan, as
#' m/z scaled by 1e9 (fields `120.10.80` and `120.10.90`). Where an event
#' narrows the range the widest bound is taken, so the window is never
#' understated.
#'
#' @return Numeric `c(min, max)`, or `NULL` if the fields are absent.
#' @noRd
read_qtof_mz_range <- function(path, fields = read_qtof_mass_params(path)){
  if (is.null(fields)) return(NULL)
  grab <- function(suffix){
    v <- vapply(fields, function(f){
      if (identical(f$wire, "varint") && endsWith(f$path, suffix)) f$value else NA_real_
    }, numeric(1))
    v[!is.na(v) & v > 0]/1e9
  }
  lo <- grab("120.10.80")
  hi <- grab("120.10.90")
  if (length(lo) == 0 || length(hi) == 0) return(NULL)
  c(min(lo), max(hi))
}

#' Check decoded m/z against the acquisition window
#'
#' A wrong calibration shows up as an m/z range that runs outside the range the
#' instrument was set to scan. Using the calibration block for the wrong
#' polarity, for instance, is a ~3% error, which on a 100-1700 window puts the
#' top of the range at ~1750 --- far outside anything the instrument could have
#' recorded. Correctly calibrated files land inside the window to within a few
#' parts per million, so the tolerance here is loose enough to only fire on a
#' real fault.
#'
#' This matters because the calibration table holds a block per polarity and
#' the polarity is read from the acquisition method: if a file ever switches
#' polarity mid-run, or the method field is missing, half the spectra would be
#' calibrated with the wrong block and nothing else would notice.
#'
#' @noRd
check_qtof_mz_range <- function(mz, window, tol = 0.01){
  if (is.null(window) || length(window) != 2 || !all(is.finite(window)) ||
      length(mz) == 0) return(invisible(FALSE))
  # `finite = TRUE` does in one pass what filtering and then ranging does in
  # three, which matters when `mz` runs to tens of millions of peaks
  obs <- suppressWarnings(range(mz, finite = TRUE))
  if (!all(is.finite(obs))) return(invisible(FALSE))
  if (obs[2] > window[2]*(1 + tol) || obs[1] < window[1]*(1 - tol)){
    warning("Decoded m/z (", signif(obs[1], 6), " - ", signif(obs[2], 6),
            ") fall outside the acquisition range recorded in the file (",
            signif(window[1], 6), " - ", signif(window[2], 6),
            "), which points to a wrong TOF calibration.", call. = FALSE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' Read the data-dependent acquisition table from a 'Shimadzu' QTOF file
#'
#' `QTFL RawData/DDA` is a protobuf message holding a repeated field `10`, one
#' record per MS2 spectrum, giving the precursor that was selected for it. The
#' paths below are relative to a record: `60.10` is the
#' acquisition event, `60.20` the retention time in milliseconds, `60.30` the
#' 1-based scan number, `60.40` the DDA cycle (the same value the scan header
#' carries at bytes 0-3), `60.70.10` the precursor m/z scaled by 1e9 and
#' `60.70.20` its intensity.
#'
#' The record count matches the number of MS2 spectra exactly, and the scan
#' number, retention time and event all agree with the scan headers. The
#' precursor m/z is corroborated independently of any vendor conversion: every
#' one checked falls within 20 ppm of an actual peak in the survey spectrum of
#' its own cycle (median 0.39 ppm). 'ProteoWizard' does not report precursor
#' m/z for this format, so there is no external reference for it.
#'
#' @return A data.table of `scan`, `rt`, `event`, `cycle`, `precursor_mz` and
#'   `precursor_intensity`, or `NULL` if there is no DDA stream.
#' @noRd
read_qtof_dda <- function(path){
  path_pb <- export_stream(path, c("QTFL RawData", "DDA"))
  if (length(path_pb) != 1 || is.na(path_pb)) return(NULL)
  on.exit(unlink_stream(path_pb))
  bytes <- as.integer(readBin(path_pb, what = "raw", n = file.size(path_pb)))
  v <- sz_pb_varints(bytes)
  if (is.null(v)) return(NULL)

  field <- v$value %/% 8
  wire <- v$value %% 8
  vstart <- v$start
  vend <- v$end
  val <- v$value
  nv <- length(val)

  n_max <- sum(field == 10L & wire == 2L)
  if (n_max == 0L) return(NULL)
  scan <- rt <- event <- cycle <- prec <- pint <- rep(NA_real_, n_max)
  n <- 0L
  i <- 1L
  while (i < nv){
    # one length-delimited field 10 per product-ion spectrum
    if (field[i] != 10L || wire[i] != 2L) break
    rec_end <- vend[i + 1L] + val[i + 1L]
    n <- n + 1L
    j <- i + 2L
    # fields 10 and 20 occur at the record level as well as inside the nested
    # messages, so the nesting has to be tracked rather than guessed
    ctx <- 0L
    end60 <- end70 <- 0
    while (j < nv && vstart[j] <= rec_end){
      if (ctx == 70L && vstart[j] > end70) ctx <- 60L
      if (ctx == 60L && vstart[j] > end60) ctx <- 0L
      f <- field[j]
      if (wire[j] == 2L){
        sub_end <- vend[j + 1L] + val[j + 1L]
        if (ctx == 0L && f == 60L){
          ctx <- 60L; end60 <- sub_end; j <- j + 2L; next
        }
        if (ctx == 60L && f == 70L){
          ctx <- 70L; end70 <- sub_end; j <- j + 2L; next
        }
        j <- j + 2L
        while (j < nv && vstart[j] <= sub_end) j <- j + 1L
        next
      }
      x <- val[j + 1L]
      if (ctx == 60L){
        if (f == 10L) event[n] <- x
        else if (f == 20L) rt[n] <- x
        else if (f == 30L) scan[n] <- x
        else if (f == 40L) cycle[n] <- x
      } else if (ctx == 70L){
        if (f == 10L) prec[n] <- x
        else if (f == 20L) pint[n] <- x
      }
      j <- j + 2L
    }
    i <- j
  }
  if (n == 0L) return(NULL)
  keep <- seq_len(n)
  out <- data.table::data.table(
    scan = scan[keep], rt = rt[keep], event = event[keep], cycle = cycle[keep],
    precursor_mz = prec[keep]/1e9, precursor_intensity = pint[keep])
  out <- out[!is.na(out$scan) & !is.na(out$precursor_mz) & out$precursor_mz > 0, ]
  if (nrow(out) == 0) return(NULL)
  out
}

#' Read the mass-correction reference masses from a 'Shimadzu' QTOF file
#'
#' `Mass Data Processing/Mass Correction Parameter` lists the reference
#' compounds used to correct the mass axis, as a protobuf message with one
#' block per polarity (field `10` positive, `20` negative, as in the
#' calibration table). Each entry holds the reference m/z scaled by 1e9 and the
#' compound name, e.g. Betaine at 118.086255 or
#' Hexakis(1H, 1H, 3H-tetrafluoropropoxy)phosphazene at 922.009798.
#'
#' @return Numeric vector of reference m/z, or `numeric(0)`.
#' @noRd
read_qtof_lock_mass <- function(path, polarity = read_qtof_polarity(path)){
  if (length(polarity) != 1 || is.na(polarity)) return(numeric(0))
  fields <- tryCatch(
    read_sz_pb_stream(path, c("Mass Data Processing", "Mass Correction Parameter")),
    error = function(e) NULL)
  if (is.null(fields)) return(numeric(0))
  prefix <- if (identical(polarity, "positive")) "10." else "20."
  mz <- vapply(fields, function(f){
    if (identical(f$wire, "varint") && startsWith(f$path, prefix) &&
        endsWith(f$path, ".20.20")) f$value/1e9 else NA_real_
  }, numeric(1))
  mz <- mz[!is.na(mz) & mz > 0]
  unique(mz)
}

#' Read the cached mass correction from a 'Shimadzu' QTOF file
#'
#' Reads the mass correction 'LabSolutions' fitted for the run, where the file
#' carries one.
#'
#' `Mass Data Load Format/Mass Correction Cache` holds the mass correction
#' 'LabSolutions' applied to the run: the reference ions it found and the
#' coefficients it fitted to them. Like the other QTOF parameter streams it is
#' a protocol buffer with a block per polarity (field `10` positive, `20`
#' negative), the reference compounds under `10` and the result under `30`:
#'
#' - `30.<pol>.10.10` --- offset of the correction, in flight-time units
#' - `30.<pol>.10.20` --- scale factor of the correction, near 1
#' - `30.<pol>.20.10.10`, `.20` --- id of a reference compound and the m/z at
#'   which it was measured in this run, scaled by 1e9
#'
#' The correction is applied to the flight time, as `t' = scale * t + offset`,
#' which is the same as shifting the calibration in the square root of the
#' mass:
#' \deqn{\sqrt{mz'} = scale \sqrt{mz} + shift}
#' where `shift` is the mean of `sqrt(theoretical) - scale * sqrt(measured)`
#' over the reference ions. Reading the vendor's own correction out reproduces
#' its mass axis exactly, where refitting on the reference ions found in the
#' data leaves a few tenths of a ppm.
#'
#' @param path Path to 'Shimadzu' .lcd file.
#' @param polarity Ion polarity, either `positive` or `negative`, selecting
#' which of the two blocks to read. Defaults to the polarity recorded in the
#' file.
#' @return A list of `scale`, `offset` and `shift`, or `NULL` if the stream is
#' missing or holds no usable result.
#' @md
#' @keywords internal

read_sz_qtof_mass_correction <- function(path, polarity = read_qtof_polarity(path)){
  if (length(polarity) != 1 || is.na(polarity)) return(NULL)
  fields <- tryCatch(read_sz_pb_stream(path, c("Mass Data Load Format",
                                               "Mass Correction Cache")),
                     error = function(e) NULL)
  if (is.null(fields) || length(fields) == 0) return(NULL)
  pol <- if (identical(polarity, "positive")) "10" else "20"
  pick <- function(...){
    p <- paste(..., sep = ".")
    unlist(lapply(fields[vapply(fields, function(f) identical(f$path, p), TRUE)],
                  `[[`, "value"))
  }
  scale <- pick("30", pol, "10.20")
  offset <- pick("30", pol, "10.10")
  measured <- pick("30", pol, "20.10.20")/1e9
  theoretical <- pick("10", pol, "20.10.20.20")[
    match(pick("30", pol, "20.10.10"), pick("10", pol, "20.10.10"))]/1e9
  if (length(scale) != 1L || length(offset) != 1L || length(measured) == 0 ||
      length(theoretical) != length(measured) || anyNA(theoretical) ||
      !all(is.finite(c(scale, offset, measured, theoretical))) ||
      any(measured <= 0) || abs(scale - 1) > 1e-3){
    return(NULL)
  }
  list(scale = scale, offset = offset,
       shift = mean(sqrt(theoretical) - scale*sqrt(measured)))
}

#' Apply the cached mass correction to the tuning calibration
#'
#' See `read_sz_qtof_calibration` for why the cached correction identifies the
#' replicate set 'LabSolutions' fitted against, which averaging them cannot.
#'
#' @param mz,flight,rep_id Calibration points and the replicate set each
#'   belongs to, as read from the `TOF Calibration Table` stream.
#' @param mc Cached correction from `read_sz_qtof_mass_correction`.
#' @return Named numeric vector of `A` and `B`, with the correction folded in,
#'   or `NULL` if no replicate set matches the cached offset.
#' @noRd
qtof_correct_calibration <- function(mz, flight, rep_id, mc){
  ok <- !is.na(mz) & !is.na(flight) & flight > 0
  sets <- split(which(ok), rep_id[ok])
  sets <- sets[vapply(sets, function(i) length(unique(mz[i])) > 1L, TRUE)]
  if (length(sets) == 0) return(NULL)
  fits <- lapply(sets, function(i) fit_tof_calibration(mz[i], flight[i]))
  err <- vapply(fits, function(co){
    abs((mc$offset - mc$shift*co[["A"]])/(1 - mc$scale)/co[["B"]] - 1)
  }, numeric(1))
  best <- which.min(err)
  # the set the correction was written against matches to ~1e-7, the others to
  # 1e-4 at best. Anything in between means the offset does not belong to this
  # table, so leave the correction to the reference ions in the data instead.
  if (!is.finite(err[best]) || err[best] > 1e-5 ||
      (length(err) > 1L && sort(err)[2] < 10*err[best])){
    return(NULL)
  }
  co <- fits[[best]]
  if (!all(is.finite(co)) || co[["A"]] <= 0) return(NULL)
  structure(c(A = unname(co[["A"]]/mc$scale),
              B = unname(co[["B"]] - mc$shift*co[["A"]]/mc$scale)),
            corrected = TRUE)
}

#' Read TOF calibration coefficients from a 'Shimadzu' QTOF file
#'
#' Fits the flight-time-to-m/z coefficients `A` and `B` from the calibration
#' points stored in the file.
#'
#' The `TOF Calibration Table` stream holds the calibration points themselves:
#' pairs of a known m/z and the flight time at which the instrument actually
#' observed it. It is encoded as a protocol buffer, a binary format in which
#' every value is tagged with a number rather than a name, so the fields are
#' referred to below by those numbers. The top-level ones are:
#'
#' - `10` -- positive mode (`Na(NaI)n` calibrants)
#' - `20` -- negative mode (`I(NaI)n` calibrants)
#'
#' Both polarities are written to every file, so the table is the same in a
#' positive and a negative run on the same instrument.
#'
#' Within each entry are the theoretical m/z scaled by 1e9 (`.10`), the measured
#' flight time in the same units as the `Centroid Data` stream (`.20`) and a
#' label (`.30`). Five calibrants are stored per polarity, in three replicate
#' sets. The second is a factory default rather than a measurement --- its
#' flight times are byte-identical across different instruments --- and
#' including it inflates the error by two orders of magnitude.
#'
#' 'LabSolutions' fits *one* of the remaining sets rather than their average,
#' and which one varies between files. The mass correction the file caches for
#' the run (see `read_sz_qtof_mass_correction`) says which, because its offset is
#' tied to that set's intercept by
#' \deqn{offset = B(1 - scale) + shift \times A}
#' Solving that for `B` reproduces one set's own intercept to ~1e-7 and no
#' other's to better than 1e-4. That set is fitted and the correction folded
#' in, as \eqn{A/scale} and \eqn{B - shift \times A/scale}, which reproduces
#' the m/z 'LabSolutions' reports to the four decimal places it writes them
#' with, leaving 0.03 to 0.1 ppm (median) over three files from two
#' instruments.
#'
#' Failing that --- no cached correction, or an offset that fits no set --- the
#' sets named by `drop_rep` are dropped and the rest fitted together, which
#' lands within about 5 ppm. `read_sz_qtof` then refines that fit against the
#' reference ions it finds in the data.
#'
#' @param path Path to 'Shimadzu' .lcd file.
#' @param drop_rep Which of the three replicate sets to exclude from the
#' fit. Defaults to `2`, the factory default set described above.
#' Ignored when the cached correction is used, since that identifies a single
#' set on its own.
#' @param correct Logical. Whether to fold in the mass correction
#' 'LabSolutions' applied to the run, where the file carries one. Defaults to
#' `TRUE`; the result then reproduces the vendor's mass axis, and carries a
#' `corrected` attribute to say so. `FALSE` returns the tuning calibration
#' alone, which is a few ppm out but is what the correction is expressed
#' against.
#' @param polarity Ion polarity, either `positive` or `negative`, selecting
#' which of the two top-level entries to fit. Defaults to the polarity
#' recorded in the file.
#' @return Named numeric vector with elements `A` and `B`.
#' @md
#' @keywords internal

read_sz_qtof_calibration <- function(path, drop_rep = 2L, correct = TRUE,
                                     polarity = read_qtof_polarity(path)){
  fail <- function(reason){
    stop("Could not read the TOF calibration from the file (", reason,
         "), so flight times cannot be converted to m/z.", call. = FALSE)
  }
  if (length(polarity) != 1 || is.na(polarity)){
    fail("ion polarity not found")
  }
  fields <- tryCatch(read_sz_pb_stream(path,
                                       c("LCMSQTOF Tuning", "TOF Calibration Table")),
                     error = function(e) NULL)
  if (is.null(fields) || length(fields) == 0){
    fail("no 'TOF Calibration Table' stream")
  }
  prefix <- if (identical(polarity, "positive")) "10." else "20."
  mz <- flight <- numeric(0)
  rep_id <- integer(0)
  seen <- character(0)
  rep <- 1L
  cur_mz <- cur_t <- NA_real_
  for (f in fields){
    if (!startsWith(f$path, prefix)) next
    if (endsWith(f$path, ".10") && identical(f$wire, "varint")){
      cur_mz <- f$value/1e9
    } else if (endsWith(f$path, ".20") && identical(f$wire, "varint")){
      cur_t <- f$value
    } else if (endsWith(f$path, ".30") && identical(f$wire, "string")){
      if (f$value %in% seen){
        rep <- rep + 1L
        seen <- character(0)
      }
      seen <- c(seen, f$value)
      mz <- c(mz, cur_mz)
      flight <- c(flight, cur_t)
      rep_id <- c(rep_id, rep)
      # an entry missing its m/z or flight time must not inherit the last one
      cur_mz <- cur_t <- NA_real_
    }
  }
  # the vendor's own correction, where the file has one, picks out the
  # replicate set it was fitted against, which averaging the sets cannot
  mc <- if (isTRUE(correct)) read_sz_qtof_mass_correction(path, polarity) else NULL
  if (!is.null(mc)){
    coefs <- qtof_correct_calibration(mz, flight, rep_id, mc)
    if (!is.null(coefs)) return(coefs)
    # the file carries a correction but none of the replicate sets matches the
    # offset it was written against, so it cannot be folded in. The reference
    # ions in the data still correct the axis, to a few tenths of a ppm rather
    # than exactly, but reaching this means the match failed on a file that
    # should have worked.
    warning("The mass correction stored in the file could not be matched to a ",
            "calibration replicate set, so it has been ignored. The mass axis ",
            "is instead corrected against the reference ions in the data.",
            call. = FALSE)
  }
  keep <- !is.na(mz) & !is.na(flight) & flight > 0 & !(rep_id %in% drop_rep)
  if (sum(keep) < 3){
    fail("too few calibration points")
  }
  coefs <- fit_tof_calibration(mz[keep], flight[keep])
  if (!all(is.finite(coefs)) || coefs[["A"]] <= 0){
    fail("calibration fit failed")
  }
  coefs
}

#' Read metadata from 'Shimadzu' QTOF streams
#'
#' The QTFL `Status` stream carries acquisition settings rather than
#' instrument identification, so the descriptive metadata comes from the
#' `File Property` stream shared by all `.lcd` files.
#'
#' @noRd
read_qtof_metadata <- function(path, polarity = read_qtof_polarity(path),
                               mz_range = read_qtof_mz_range(path),
                               time_range =
                                 range(read_qtof_retention_times(path))/60000){
  meta <- read_sz_file_properties(path)
  # `attach_metadata` builds `time_range` from DLT and AT
  meta$DLT <- time_range[1]
  meta$AT <- time_range[2]
  meta$`time.unit` <- "Minutes"
  meta$DETN <- "MS"
  # a QTOF run has one polarity throughout, which is why the calibration is
  # chosen by it; MS level varies from scan to scan and stays in `scan_info`
  meta$polarity <- polarity
  meta$mz_range <- mz_range
  meta
}

#' Read the spectrum offsets of a 'Shimadzu' QTOF file
#'
#' `Centroid Index` holds one 24-byte record per spectrum, the first four bytes
#' of which are the spectrum's byte offset into `Centroid Data`. Using it
#' avoids having to guess the record layout: the header is 64 bytes in some
#' files and 72 in others, so walking `64 + data_size` desynchronises on the
#' latter from the second record onwards.
#'
#' @return Numeric vector of byte offsets, one per spectrum.
#' @noRd
read_qtof_spectrum_index <- function(path){
  x <- read_ole_uint32(path, c("QTFL RawData", "Centroid Index"))
  if (is.null(x)) return(NULL)
  if (length(x) %% 6L != 0L){
    stop("The 'Centroid Index' stream is truncated: ", length(x)*4L,
         " bytes is not a whole number of 24-byte records.", call. = FALSE)
  }
  matrix(x, ncol = 6L, byrow = TRUE)[, 1]
}

#' Decode a 'Shimadzu' QTOF centroid stream
#'
#' Every field is read in one batch: decoding spectrum by spectrum costs ~30 us
#' per peak, almost all of it per-spectrum overhead, because the median
#' spectrum holds only seven peaks.
#'
#' @param path_ms Path to the extracted `Centroid Data` stream.
#' @param offsets Byte offsets from `read_qtof_spectrum_index`.
#' @param n_expected Spectrum count from the `Retention Time` stream, used
#'   only as a cross-check.
#' @param lock_mass Reference m/z from `read_qtof_lock_mass`. When at
#'   least two of them are found in the data, the calibration is refitted on
#'   them, which is what takes the m/z from ~5 ppm of the values reported by
#'   'LabSolutions' to ~0.15 ppm.
#' @param lock_tol Half-width of the window used to match a reference mass, as
#'   a fraction. Deliberately generous: candidates are trimmed by residual
#'   afterwards, which makes the result insensitive to this value (20-100 ppm
#'   all give 0.08-0.19 ppm on the files measured). It has to stay well below
#'   ~200 ppm, where unrelated analyte peaks start to outnumber the reference
#'   ions and the trimming can no longer recover.
#' @param dda Table from `read_qtof_dda`, used to label each MS2
#'   spectrum with the precursor that was selected for it.
#' @param polarity Ion polarity, recorded in `scan_info`.
#' @return A list of `dat` (long spectra), `scan_info` (one row per spectrum)
#'   and `level` (the MS level of every row of `dat`).
#' @noRd
decode_qtof_stream <- function(path_ms, offsets, A, B, int_scale,
                               n_expected = NULL, lock_mass = numeric(0),
                               lock_tol = 50e-6, dda = NULL,
                               polarity = NA_character_, mz_range = NULL){
  ms <- readBin(path_ms, what = "raw", n = file.size(path_ms))
  n_bytes <- length(ms)
  off <- offsets[!is.na(offsets) & offsets >= 0 & offsets + 64 <= n_bytes]
  n_scans <- length(off)
  if (n_scans == 0L){
    stop("The 'Centroid Index' stream holds no usable spectrum offsets.")
  }
  if (!is.null(n_expected) && n_expected != n_scans){
    warning("The 'Centroid Index' stream holds ", n_scans, " spectra but the ",
            "'Retention Time' stream holds ", n_expected, ".", call. = FALSE)
  }

  # every header field, in one read
  h <- readBin(ms[rep(off, each = 64L) + 1:64], what = "integer",
               n = 16L * n_scans, size = 4L, endian = "little")
  dim(h) <- c(16L, n_scans)
  rt_ms <- h[2, ]
  scan_index <- h[6, ]
  data_size <- h[7, ]
  # bytes 0-3 count DDA cycles: sequential over the survey spectra, and each
  # product-ion spectrum repeats the value of the survey spectrum it came from
  cycle <- h[1, ]
  # bytes 28-31 pack the MS level in the high word and the acquisition event in
  # the low word. The high word matches the level reported by 'ProteoWizard'
  # for all 16,018 spectra of the reference file, and the low word matches
  # column 6 of `Centroid Index`.
  ms_level <- bitwAnd(bitwShiftR(h[8, ], 16L), 0xFFFFL)
  event <- bitwAnd(h[8, ], 0xFFFFL)
  # the high byte of this field is not part of the width: values of 258 and 260
  # (0x0102, 0x0104) occur alongside the bare 1, 2 and 4
  int_width <- bitwAnd(h[10, ], 0xFFL)

  # the header is 64 bytes in some files and 72 in others, so take its length
  # from the gap between consecutive records rather than assuming
  record_len <- diff(c(off, n_bytes))
  hdr_len <- unique(record_len - data_size)
  if (length(hdr_len) != 1L || is.na(hdr_len) || hdr_len < 64){
    hdr_len <- as.numeric(names(sort(table(record_len - data_size),
                                     decreasing = TRUE))[1])
    if (is.na(hdr_len) || hdr_len < 64){
      stop("Could not determine the QTOF spectrum header length.")
    }
  }

  n_peaks <- data_size %/% (8L + int_width)
  keep <- !is.na(n_peaks) & n_peaks > 0L
  # out-of-range indexing of a raw vector yields 00 rather than an error, so a
  # truncated stream would decode as zero-intensity peaks at zero flight time
  if (any(off[keep] + hdr_len + data_size[keep] > n_bytes)){
    stop("The 'Centroid Data' stream is truncated: a spectrum runs past the ",
         "end of the stream.", call. = FALSE)
  }

  precursor <- rep(NA_real_, n_scans)
  if (!is.null(dda) && nrow(dda) > 0){
    # `dda` numbers spectra from 1, the scan header from 0
    j <- match(scan_index, dda$scan - 1)
    precursor <- dda$precursor_mz[j]
  }
  scan_info <- data.table::data.table(
    scan = scan_index,
    rt = rt_ms/60000,
    event = event,
    ms_level = ms_level,
    cycle = cycle,
    polarity = polarity,
    precursor_mz = precursor,
    n_peaks = n_peaks)
  # a wrong `int_width` would silently reframe the whole stream
  if (any(data_size[keep] %% (8L + int_width[keep]) != 0L)){
    stop("QTOF data block size is not a multiple of the peak size.")
  }
  if (!any(keep)){
    return(list(dat = data.table::data.table(scan = integer(), rt = integer(),
                                             mz = numeric(),
                                             intensity = numeric()),
                scan_info = scan_info, level = integer(0)))
  }

  off <- off[keep]
  int_width <- int_width[keep]
  np <- n_peaks[keep]
  n_tot <- sum(np)

  # every flight time, in one read
  ft <- readBin(ms[sequence(8L * np, from = off + hdr_len + 1)],
                what = "integer", n = 2L * n_tot, size = 4L, endian = "little")
  # the 8 bytes are an unsigned 64-bit counter, and R has no unsigned 32-bit
  # type: reading the low word as signed shifts half of all m/z by 2^32, which
  # is ~4 ppm of flight time. 0x80000000 additionally comes back as NA.
  lo <- as_uint32(ft[c(TRUE, FALSE)])
  hi <- as.numeric(ft[c(FALSE, TRUE)])
  if (anyNA(hi) || any(hi >= 2097152)){
    warning("QTOF flight times exceed the range representable exactly as a ",
            "double; m/z may lose precision.", call. = FALSE)
  }
  flight_times <- lo + hi * 2^32

  # every intensity, in one read per distinct width
  raw_intensities <- numeric(n_tot)
  for (w in unique(int_width)){
    sel <- int_width == w
    v <- readBin(ms[sequence(w * np[sel],
                             from = off[sel] + hdr_len + 8L*np[sel] + 1)],
                 what = "integer", n = sum(np[sel]), size = w,
                 endian = "little", signed = (w > 2L))
    raw_intensities[rep(sel, np)] <- as_uint32(v)
  }

  # Correct the mass axis against the reference compounds infused for that
  # purpose. The calibration table is recorded at tune time, so on its own it
  # leaves a near-constant bias of a few ppm; refitting on reference ions
  # measured in this run removes it.
  mz <- NULL
  if (length(lock_mass) > 1){
    mz0 <- ((flight_times - B) / A)^2
    # reused unless the refit below actually moves the coefficients
    mz <- mz0
    hit_mz <- hit_ft <- vector("list", length(lock_mass))
    for (i in seq_along(lock_mass)){
      ref <- lock_mass[i]
      k <- which(abs(mz0 - ref) < ref * lock_tol)
      if (length(k) > 0){
        hit_mz[[i]] <- rep.int(ref, length(k))
        hit_ft[[i]] <- flight_times[k]
      }
    }
    hit_mz <- unlist(hit_mz)
    hit_ft <- unlist(hit_ft)
    if (length(hit_mz) >= 20L && length(unique(hit_mz)) > 1L){
      # A wide window catches unrelated analyte peaks near a reference mass, so
      # trim by residual rather than trying to pick the window precisely. Three
      # rounds is enough to settle; the cut is on the median absolute deviation
      # so a few bad matches cannot drag it.
      for (i in 1:3){
        coefs <- fit_tof_calibration(hit_mz, hit_ft)
        if (!all(is.finite(coefs)) || coefs[["A"]] <= 0) break
        resid <- ((hit_ft - coefs[["B"]])/coefs[["A"]])^2 - hit_mz
        resid <- resid/hit_mz
        mad <- stats::mad(resid)
        if (!is.finite(mad) || mad <= 0) break
        ok <- abs(resid - stats::median(resid)) < 3*mad
        if (sum(ok) < 10L || length(unique(hit_mz[ok])) < 2L) break
        hit_mz <- hit_mz[ok]
        hit_ft <- hit_ft[ok]
      }
      coefs <- fit_tof_calibration(hit_mz, hit_ft)
      if (all(is.finite(coefs)) && coefs[["A"]] > 0){
        A <- coefs[["A"]]
        B <- coefs[["B"]]
        mz <- NULL
      }
    }
  }

  if (is.null(mz)) mz <- ((flight_times - B) / A)^2
  check_qtof_mz_range(mz, mz_range)

  dat <- data.table::data.table(
    scan = rep.int(scan_index[keep], np),
    rt = rep.int(rt_ms[keep], np),
    mz = mz,
    intensity = if (int_scale == 1){
      if (max(raw_intensities) <= .Machine$integer.max)
        as.integer(raw_intensities) else raw_intensities
    } else floor(raw_intensities / int_scale + 0.5)
  )
  if (any(!is.na(precursor[keep]))){
    data.table::set(dat, j = "precursor_mz",
                    value = rep.int(precursor[keep], np))
    data.table::setcolorder(dat, c("scan", "rt", "precursor_mz", "mz",
                                   "intensity"))
  }
  list(dat = dat, scan_info = scan_info,
       level = rep.int(ms_level[keep], np))
}

#' Read intensity scale factor from 'Shimadzu' QTOF stream
#'
#' Returns the divisor needed to convert raw accumulated detector counts into
#' the intensities reported by LabSolutions. The `Status` stream stores a
#' 48-byte record for each acquisition event, beginning with the number of TOF
#' accumulations per scan (376 in the reference file) followed by the number of
#' scans acquired for that event. Intensities are normalized to a nominal 100
#' accumulations, so the divisor is `accumulations / 100`. There is no safe
#' value to fall back on --- the count varies by method, and guessing one
#' scales every intensity in the file by the wrong factor --- so a missing or
#' unreadable count is an error, as is a file whose events disagree, since one
#' divisor cannot then be right for all of them. Callers who want the raw counts can ask for
#' them with `scale = FALSE`.
#'
#' @noRd
read_qtof_int_scale <- function(path){
  n_acc <- tryCatch({
    path_status <- export_stream(path, c("QTFL RawData", "Status"))
    f <- file(path_status, "rb")
    on.exit(close(f))
    on.exit(unlink_stream(path_status), add = TRUE)
    n_rec <- (file.size(path_status) - 64) %/% 48
    seek(f, 64, origin = "start")
    rec <- readBin(f, what = "integer", size = 4L, n = 12L * n_rec,
                   endian = "little")
    unique(rec[seq_len(n_rec)*12L - 11L])
  }, error = function(e) NA_integer_)
  if (length(n_acc) > 1L){
    stop("The 'Status' stream reports more than one accumulation count (",
         paste(n_acc, collapse = ", "), "), so a single divisor cannot be ",
         "right for every acquisition event. Use `scale = FALSE` to return ",
         "raw detector counts.", call. = FALSE)
  }
  if (length(n_acc) != 1L || is.na(n_acc) || n_acc <= 0){
    stop("Could not read the number of TOF accumulations from the file, so ",
         "intensities cannot be scaled to match 'LabSolutions'. Use ",
         "`scale = FALSE` to return raw detector counts.", call. = FALSE)
  }
  n_acc / 100
}

#' Read retention times from Shimadzu QTOF stream
#'
#' Errors rather than returning nothing: `NULL` survives the subset below
#' unchanged, and would surface downstream as a spectrum-count mismatch and an
#' infinite `time_range` in the metadata rather than as a missing stream.
#'
#' @return Numeric vector of retention times in milliseconds, one per spectrum.
#' @noRd
read_qtof_retention_times <- function(path){
  rts <- read_ole_uint32(path, c("QTFL RawData", "Retention Time"))
  if (is.null(rts)){
    stop("The 'Retention Time' stream could not be read.", call. = FALSE)
  }
  if (length(rts) %% 3L != 0L){
    stop("The 'Retention Time' stream is truncated: ", length(rts)*4L,
         " bytes is not a whole number of 12-byte records.", call. = FALSE)
  }
  # three values per scan; the first is the retention time in milliseconds
  rts[c(TRUE, FALSE, FALSE)]
}

#' Fit TOF calibration coefficients A and B from calibrant data
#'
#' Solves t = A*sqrt(mz) + B via least squares. Choosing which calibration
#' points to pass in is the caller's job --- `read_sz_qtof_calibration` drops
#' the factory replicate set before calling this.
#'
#' @param theoretical_mz Numeric vector. Theoretical m/z of calibrant ions.
#' @param flight_times    Numeric vector. Measured flight times, in the units
#'   used by the `Centroid Data` stream.
#' @return Named numeric vector with elements `A` and `B`.
#' @noRd
fit_tof_calibration <- function(theoretical_mz, flight_times) {
  X <- cbind(sqrt(theoretical_mz), 1)
  fit <- stats::.lm.fit(X, flight_times)
  stats::setNames(fit$coefficients, c("A", "B"))
}
