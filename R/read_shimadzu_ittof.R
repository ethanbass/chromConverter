#' Read 'Shimadzu' IT-TOF stream
#'
#' Read the `TTFL Raw Data` mass spectrometry stream written by the 'Shimadzu'
#' LCMS-IT-TOF to `.lcd` files.
#'
#' `Data Index` holds one 16-byte record per scan: the scan's byte offset
#' into `MS Raw Data` (`uint64`), the cycle number and a scan counter. Each
#' scan in `MS Raw Data` is a 256-byte header followed by a run-length-encoded
#' profile. The header fields read here are:
#'
#' | **Offset** | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3     | `uint32` | Scan index (0-based) |
#' | 4–7     | `uint32` | Cycle number |
#' | 8–11    | `uint32` | Start of the cycle (milliseconds), as in `Retention Time` |
#' | 12–15   | `uint32` | Acquisition time of the scan (milliseconds) |
#' | 16–19   | `uint32` | Acquisition event (0-based) |
#' | 20–23   | `uint32` | MS level in the upper 16 bits, polarity in the lower 16 (0 = positive, 1 = negative) |
#' | 144–147 | `uint32` | Last flight-time bin of the scan range |
#' | 148–151 | `uint32` | Flight time of bin 0 |
#' | 154–155 | `uint16` | Intensity multiplier |
#' | 164–167 | `uint32` | Length of the profile in 16-bit words |
#' | 168–171 | `uint32` | Total ion current |
#' | 176–179 | `uint32` | Precursor m/z x 10000 (MS2 and above) |
#'
#' **Profile.** The profile is a sequence of 16-bit words. A word with bit 15
#' clear is a gap: the number of empty flight-time bins to skip. A word with
#' bit 15 set opens a run, and its lower 15 bits give the number of
#' intensities that follow, one per consecutive bin; the intensities
#' themselves use all 16 bits. A run of length 0 ends the scan.
#'
#' **Intensities.** The header's total ion current, and the `TIC Data`
#' streams, equal the sum of the stored values times the scan's intensity
#' multiplier, so `scale = TRUE` multiplies them by it. The multiplier is
#' usually 128, but reaches 4096 in some intense scans of PXD025121.
#' 'LabSolutions' displays and exports the profile
#' after a 5-point moving average, which is not applied here.
#'
#' **Mass axis.** Bin `b` of a scan has flight time `t = b + t0`, where `t0`
#' is read from the header. `TTFL Tuning/Tuning Result 00` stores, for each
#' polarity, the reference masses of the calibrant (sodium trifluoroacetate
#' clusters) with their measured flight times. Between two reference ions,
#' `sqrt(mz)` is the quadratic in `t` through that pair and the next ion up;
#' below the first, it is the line through the first two. The last interval
#' and anything above it reuse the quadratic through the last three ions.
#'
#' Checked against the vendor's own `TTFLDataExport` profile export of an
#' MTBLS688 file, this reproduces every intensity of all 3002 scans exactly,
#' once the 5-point moving average is applied, and every m/z to the four
#' decimal places the export keeps. The centroids of an MTBLS432 file
#' (vendor ANDI-MS exports) and of a PXD020792 file (Mascot Distiller peak
#' lists) agree to within a fraction of a flight-time bin. Above the last
#' reference ion, the median error against the Mascot Distiller centroids of
#' PXD025121 files stays within 2 ppm up to m/z 1500.
#'
#' @param path Path to `.lcd` file.
#' @param format_out Class of output. Either `data.table` or `data.frame`.
#' Spectra are long and mixed-type, so `matrix` resolves to `data.table`.
#' @param data_format Ignored: spectra are always returned in `long` format.
#' @param levels Which MS levels to return, spelled `MS1` and `MS2`. Both are
#' decoded either way.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata.
#' @param scale Logical. Whether to scale the stored values to the units of
#' the total ion current. `FALSE` returns them as stored.
#' @return A named list holding whichever of the requested `levels` the file
#' has, each a long table with columns `scan`, `rt` (minutes), `mz` and
#' `intensity`, and a `precursor_mz` column before `mz` for MS2. A per-scan
#' summary (retention time, event, MS level, polarity, precursor m/z, point
#' count) is attached to each as a `scan_info` attribute.
#' @author Ethan Bass
#' @md
#' @keywords internal
read_sz_ittof <- function(path, format_out = c("matrix", "data.frame",
                                               "data.table"),
                          data_format = "long", levels = c("MS1", "MS2"),
                          read_metadata = TRUE,
                          metadata_format = "shimadzu_lcd", scale = TRUE){
  format_out <- check_format_out_table(format_out)
  data_format <- "long"

  x <- read_ittof_scans(path)
  h <- x$header
  cal <- read_ittof_calibration(path)
  runs <- decode_ittof_runs(x$words, start = x$offsets/2 + 129,
                            n_words = h[, "n_words"])

  row <- rep.int(runs$scan, runs$n)
  n_points <- tabulate(row, nbins = nrow(h))
  bin <- sequence(runs$n, from = runs$bin)
  intensity <- x$words[sequence(runs$n, from = runs$word)]
  if (scale) intensity <- intensity * h[row, "multiplier"]

  # every scan of an event shares a polarity and a bin origin, so the mass axis
  # is computed once per pair and looked up by bin
  key <- paste(h[, "polarity"], h[, "t0"])
  group <- match(key, unique(key))
  group_row <- group[row]
  mz <- numeric(length(bin))
  for (k in unique(group_row)){
    i <- which(group_row == k)
    s <- which(group == k)[1]
    lut <- ittof_tof_to_mz(seq.int(0, max(bin[i])) + h[s, "t0"],
                           cal[[h[s, "polarity"] + 1L]])
    mz[i] <- lut[bin[i] + 1L]
  }

  scan_info <- ittof_scan_info(h, n_points)
  higher <- sum(scan_info$ms_level > 2L)
  if (higher > 0){
    warning(sprintf("%d spectra of MS3 or higher are not returned.", higher),
            call. = FALSE)
  }
  dat <- data.table::setDT(list(
    scan = as.integer(h[row, "scan"]),
    rt = h[row, "rt"]/60000,
    mz = mz,
    intensity = intensity))
  if (any(scan_info$ms_level > 1L)){
    data.table::set(dat, j = "precursor_mz",
                    value = scan_info$precursor_mz[row])
    data.table::setcolorder(dat, c("scan", "rt", "precursor_mz", "mz",
                                   "intensity"))
  }

  sz_split_ms_levels(dat, scan_info, levels = levels,
                     level = scan_info$ms_level[row],
                     meta = if (read_metadata){
                       read_ittof_metadata(path, range(h[, "rt"])/60000)
                     } else NULL,
                     path = path, format_out = format_out,
                     metadata_format = metadata_format, scale = scale)
}

#' Read the scans of a 'Shimadzu' IT-TOF stream
#'
#' @return A list with the scan offsets into `MS Raw Data`, the stream as
#' unsigned 16-bit words, and a matrix of header fields, one row per scan.
#' @noRd
read_ittof_scans <- function(path){
  index <- read_ole_uint32(path, c("TTFL Raw Data", "Data Index"))
  if (is.null(index) || length(index) %% 4L != 0L){
    stop("The 'Data Index' stream could not be read.", call. = FALSE)
  }
  index <- matrix(index, ncol = 4L, byrow = TRUE)
  offsets <- index[, 1] + index[, 2] * 2^32

  path_ms <- export_stream(path, c("TTFL Raw Data", "MS Raw Data"))
  on.exit(unlink_stream(path_ms), add = TRUE)
  ms <- readBin(path_ms, what = "raw", n = file.size(path_ms))
  words <- readBin(ms, what = "integer", size = 2L, signed = FALSE,
                   endian = "little", n = length(ms) %/% 2L)

  hdr <- readBin(ms[rep(offsets, each = 256L) + seq_len(256L)],
                 what = "integer", size = 4L, endian = "little",
                 n = 64L * length(offsets))
  hdr <- matrix(hdr, ncol = 64L, byrow = TRUE)
  header <- cbind(scan = hdr[, 1], rt = hdr[, 4], event = hdr[, 5],
                  ms_level = bitwShiftR(hdr[, 6], 16L),
                  polarity = bitwAnd(hdr[, 6], 0xFFFFL),
                  last_bin = hdr[, 37], t0 = hdr[, 38],
                  multiplier = bitwShiftR(hdr[, 39], 16L), n_words = hdr[, 42],
                  tic = as_uint32(hdr[, 43]), precursor = hdr[, 45])
  list(offsets = offsets, words = words, header = header)
}

#' Decode the run-length-encoded profiles of a 'Shimadzu' IT-TOF stream
#'
#' A run's length sits in its first word and its values can use bit 15, so the
#' stream cannot be split into tokens without walking it. Every scan is walked
#' at once instead, one token per iteration, so the loop runs as many times as
#' the longest scan has tokens rather than once per token in the file.
#'
#' @param words The `MS Raw Data` stream as unsigned 16-bit words.
#' @param start Index into `words` of the first word of each profile.
#' @param n_words Length of each profile in words.
#' @return A list with one element per run: the scan it belongs to (as a row
#' of `start`), the index of its first value in `words`, its first bin and its
#' length.
#' @noRd
decode_ittof_runs <- function(words, start, n_words){
  ptr <- start
  end <- start + n_words
  pos <- numeric(length(start))
  active <- which(n_words > 0)
  out <- list()
  while (length(active) > 0){
    p <- ptr[active]
    w <- words[p]
    run <- w >= 32768L
    n <- w - 32768L
    a <- active[run]
    if (length(a) > 0){
      out[[length(out) + 1L]] <- list(scan = a, word = p[run] + 1,
                                      bin = pos[a], n = n[run])
      pos[a] <- pos[a] + n[run]
      ptr[a] <- p[run] + 1 + n[run]
    }
    g <- active[!run]
    pos[g] <- pos[g] + w[!run]
    ptr[g] <- p[!run] + 1
    active <- active[!(run & n == 0L) & ptr[active] < end[active]]
  }
  runs <- lapply(c(scan = "scan", word = "word", bin = "bin", n = "n"),
                 function(f) unlist(lapply(out, `[[`, f), use.names = FALSE))
  keep <- runs$n > 0
  ord <- order(runs$scan[keep], runs$word[keep])
  lapply(runs, function(v) v[keep][ord])
}

#' Read the mass calibration of a 'Shimadzu' IT-TOF file
#'
#' `TTFL Tuning/Tuning Result 00` is a fixed 26796-byte structure. For each
#' polarity it holds up to ten reference masses (`uint32`, m/z x 100000) and
#' their flight times (`double`); unused slots are zero. The positive table
#' starts at offsets 3022 and 3150, the negative one at 7356 and 7484.
#'
#' @return A list of two data frames, positive then negative, each with the
#' columns `mz` and `t`.
#' @noRd
read_ittof_calibration <- function(path){
  path_tr <- export_stream(path, c("TTFL Tuning", "Tuning Result 00"))
  on.exit(unlink_stream(path_tr), add = TRUE)
  if (length(path_tr) != 1 || is.na(path_tr) ||
      file.size(path_tr) != 26796){
    stop("The IT-TOF mass calibration ('TTFL Tuning/Tuning Result 00') ",
         "could not be read.", call. = FALSE)
  }
  x <- readBin(path_tr, what = "raw", n = 26796)
  lapply(list(c(3022, 3150), c(7356, 7484)), function(o){
    mz <- as_uint32(readBin(x[o[1] + 1:40], what = "integer", size = 4L,
                            n = 10L, endian = "little"))/1e5
    t <- readBin(x[o[2] + 1:80], what = "double", size = 8L, n = 10L,
                 endian = "little")
    keep <- mz > 0
    data.frame(mz = mz[keep], t = t[keep])
  })
}

#' Convert 'Shimadzu' IT-TOF flight times to m/z
#'
#' See `read_sz_ittof` for the interpolation.
#'
#' @param t Flight times.
#' @param cal Calibration table for one polarity, as returned by
#' `read_ittof_calibration`.
#' @noRd
ittof_tof_to_mz <- function(t, cal){
  if (nrow(cal) < 3){
    stop("The IT-TOF mass calibration holds fewer than three reference ",
         "ions.", call. = FALSE)
  }
  x <- cal$t
  y <- sqrt(cal$mz)
  n <- length(x)
  seg <- findInterval(t, x)
  s <- numeric(length(t))
  below <- seg == 0L
  s[below] <- y[1] + (t[below] - x[1]) * (y[2] - y[1])/(x[2] - x[1])
  for (k in unique(seg[!below])){
    i <- which(seg == k)
    j <- min(k, n - 2L) + 0:2
    l <- vapply(1:3, function(a){
      b <- setdiff(1:3, a)
      (t[i] - x[j[b[1]]]) * (t[i] - x[j[b[2]]]) /
        ((x[j[a]] - x[j[b[1]]]) * (x[j[a]] - x[j[b[2]]]))
    }, numeric(length(i)))
    s[i] <- matrix(l, ncol = 3L) %*% y[j]
  }
  s^2
}

#' Summarize 'Shimadzu' IT-TOF scan headers
#' @noRd
ittof_scan_info <- function(h, n_points){
  precursor <- h[, "precursor"]/1e4
  precursor[h[, "ms_level"] < 2L] <- NA_real_
  data.table::data.table(
    scan = as.integer(h[, "scan"]),
    rt = h[, "rt"]/60000,
    event = as.integer(h[, "event"]) + 1L,
    ms_level = as.integer(h[, "ms_level"]),
    polarity = ifelse(h[, "polarity"] == 0L, "positive", "negative"),
    precursor_mz = precursor,
    n_points = n_points)
}

#' Read metadata from 'Shimadzu' IT-TOF streams
#' @noRd
read_ittof_metadata <- function(path, time_range){
  meta <- read_sz_file_properties(path)
  meta$DLT <- time_range[1]
  meta$AT <- time_range[2]
  meta$`time.unit` <- "Minutes"
  meta$DETN <- "MS"
  meta
}

#' Read TIC from 'Shimadzu' IT-TOF stream
#'
#' Reads the total ion current of every scan from the scan headers of
#' `TTFL Raw Data/MS Raw Data` (see `read_sz_ittof`), where it sits beside the
#' scan's own acquisition time. Each acquisition event is returned as a
#' separate chromatogram, named `Event 1`, `Event 2` and so on.
#'
#' @param path Path to `.lcd` file.
#' @param format_out Class of output. Either `matrix`, `data.frame`,
#' or `data.table`.
#' @param data_format Either `wide` (default) or `long`.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata.
#' @return A named list of chromatograms, one per acquisition event.
#' @author Ethan Bass
#' @keywords internal
read_sz_ittof_tic <- function(path, format_out = c("matrix", "data.frame",
                                                   "data.table"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE,
                              metadata_format = "shimadzu_lcd"){
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format)
  h <- read_ittof_scans(path)$header
  events <- sort(unique(h[, "event"]))
  meta <- if (read_metadata){
    read_ittof_metadata(path, range(h[, "rt"])/60000)
  } else NULL
  dat <- lapply(events, function(ev){
    i <- which(h[, "event"] == ev)
    x <- format_2d_chromatogram(rt = h[i, "rt"]/60000, int = h[i, "tic"],
                                data_format = data_format,
                                format_out = format_out)
    if (is.null(meta)) return(x)
    m <- meta
    m$ms_level <- as.integer(h[i[1], "ms_level"])
    m$polarity <- if (h[i[1], "polarity"] == 0L) "positive" else "negative"
    attach_metadata(x, m, format_in = metadata_format, source_file = path,
                    data_format = data_format, format_out = format_out,
                    source_file_format = "shimadzu_lcd")
  })
  names(dat) <- paste("Event", events + 1L)
  dat
}
