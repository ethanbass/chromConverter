#' Read 'Shimadzu' single quadrupole stream
#'
#' Read the `Mass Raw Data` mass spectrometry stream written by 'Shimadzu'
#' single quadrupole instruments, such as the LCMS-2020, to `.lcd` files.
#'
#' Spectra are centroided and returned as stored, except that each spectrum is
#' sorted by m/z, as ProteoWizard's `msconvert` also does, because the stored
#' m/z values do not always increase within a scan. The intensities of a scan
#' sum to its value in `TIC Data`, so no scaling is applied.
#'
#' Some methods record profile scans as a separate event. With
#' `centroided = FALSE` these are returned instead of the centroids, so events
#' recorded only as centroids, such as the negative scans of the one file
#' checked, are absent. The stored values are multiplied by 2/3, which puts their sum
#' in the units of `TIC Data` in the one file checked. The vendor's export
#' applies a 9-point Savitzky-Golay filter to them, which is not applied here.
#'
#' The file stores no m/z values for profile points, and it's unknown how the
#' vendor's library computes them. Profile points are instead spread
#' evenly over the method's m/z range, from `lo` to `hi`, so that point `j`
#' (0-based) of `n` lies at `lo + (j - 12) * (hi - lo)/(n - 27)`. Against a
#' `msconvert` export of the one file checked (150-2000 at 15 points per m/z),
#' these m/z values are up to 0.012 low, about a fifth of the point spacing.
#' The offsets 12 and 27 come from that file alone. Profile scans with any
#' other number of points than 15 per m/z of the range plus 26 give an error.
#'
#' Polarity is read from bit 2 of the event flags. In the one method seen,
#' which alternates positive and negative scans, the method's event table
#' stores the same flags beside a signed interface voltage, and bit 2 is set on
#' the event with the negative voltage. `msconvert`, which reads the file
#' through the vendor's library, reports the same polarity as this reader for
#' every scan.
#'
#' No file with SIM events has been seen, and no header field is known to give
#' the scan type. A SIM event whose records match the layout below would be
#' returned in `MS1` as its own event, with nothing marking it as SIM.
#'
#' @section File format:
#' Values are little-endian unsigned integers.
#'
#' * `Mass Raw Data/Spectrum Index`: the byte offset of each scan in
#'   `MS Raw Data` (uint32).
#' * `Mass Raw Data/TIC Data`: the total ion current of each scan (uint64).
#' * `Mass Raw Data/MS Raw Data`: one block per scan, a 64-byte header followed
#'   by one record per centroid. Header:
#'   * 0: scan index, 0-based (uint32).
#'   * 4: retention time in milliseconds, as in `Retention Time` (uint32).
#'   * 8: acquisition event, 0-based (uint16).
#'   * 44: event flags; bit 2 is the polarity, 0 positive and 1 negative
#'     (uint16).
#'   * 52: intensity width code, w: the width in bytes, 2 or 3, or 1 for 4
#'     bytes (uint16).
#'   * 54: number of centroids, n (uint16).
#'   * 60: number of profile points, 0 in centroid scans (uint32).
#'
#'   Each of the n records is the m/z times 20 (uint16) followed by the
#'   intensity. Bit 31 of a 4-byte intensity is a flag and is masked off.
#'   Scans with bit 4 of the event flags set hold profile data instead: n is 0
#'   and the block holds the profile points (uint16) with no m/z values. A
#'   point with bit 15 set holds its lower 15 bits times 32. Bytes not listed
#'   are not read.
#' * `Mass Raw Data/Status`: the m/z range of the method (uint16 at bytes 14
#'   and 16).
#'
#' @param path Path to `.lcd` file.
#' @param format_out Class of output. Either `data.table` or `data.frame`.
#' Spectra are long and mixed-type, so `matrix` resolves to `data.table`.
#' @param data_format Ignored: spectra have no wide representation and are
#' always returned in `long` format.
#' @param levels Which MS levels to return, spelled `MS1` and `MS2`. A single
#' quadrupole records only `MS1`.
#' @param centroided Logical. Whether to return the centroided scans (`TRUE`)
#' or the profile scans (`FALSE`).
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata.
#' @return A named list holding `MS1` if it was requested, a long table with
#' columns `scan`, `rt` (minutes), `mz` and `intensity`. A per-scan summary
#' (retention time, event, MS level, polarity, point count) is attached as a
#' `scan_info` attribute.
#' @author Ethan Bass
#' @md
#' @keywords internal
read_sz_sq <- function(path, format_out = c("matrix", "data.frame",
                                            "data.table"),
                       data_format = "long", levels = c("MS1", "MS2"),
                       centroided = TRUE, read_metadata = TRUE,
                       metadata_format = "shimadzu_lcd"){
  format_out <- check_format_out_table(format_out)

  if (!is.logical(centroided) || length(centroided) != 1 ||
      is.na(centroided)){
    stop("`centroided` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  x <- read_sq_scans(path)
  i <- which(sq_profile(x$header[, "flags"]) != centroided)
  if (!centroided && length(i) == 0){
    stop("The file holds no profile scans.", call. = FALSE)
  }
  h <- x$header[i, , drop = FALSE]
  dat <- if (centroided){
    read_sq_centroids(x, i)
  } else read_sq_profiles(x, i, read_sq_mz_range(path))
  data.table::setorderv(dat, c("scan", "mz"))

  scan_info <- data.table::data.table(
    scan = as.integer(h[, "scan"]),
    rt = h[, "rt"]/60000,
    event = as.integer(h[, "event"]) + 1L,
    ms_level = 1L,
    polarity = sq_polarity(h[, "flags"]),
    n_points = as.integer(h[, if (centroided) "n_points" else "n_profile"]))

  meta <- if (read_metadata){
    c(read_sq_metadata(path, range(x$header[, "rt"])/60000),
      centroided = centroided)
  }
  sz_split_ms_levels(dat, scan_info, levels = levels,
                     level = rep.int(1L, nrow(dat)), meta = meta,
                     path = path, format_out = format_out,
                     metadata_format = metadata_format)
}

#' Read the scans of a 'Shimadzu' single quadrupole stream
#'
#' @return A list with the scan offsets into `MS Raw Data`, the stream as a raw
#' vector, and a matrix of header fields, one row per scan.
#' @noRd
read_sq_scans <- function(path){
  offsets <- read_ole_uint32(path, c("Mass Raw Data", "Spectrum Index"))
  if (is.null(offsets)){
    stop("The 'Spectrum Index' stream could not be read.", call. = FALSE)
  }
  path_ms <- export_stream(path, c("Mass Raw Data", "MS Raw Data"))
  on.exit(unlink_stream(path_ms), add = TRUE)
  ms <- readBin(path_ms, what = "raw", n = file.size(path_ms))

  hdr <- readBin(ms[rep(offsets, each = 64L) + seq_len(64L)],
                 what = "integer", size = 2L, signed = FALSE,
                 endian = "little", n = 32L * length(offsets))
  hdr <- matrix(hdr, ncol = 32L, byrow = TRUE)
  header <- cbind(scan = hdr[, 1] + hdr[, 2] * 65536,
                  rt = hdr[, 3] + hdr[, 4] * 65536,
                  event = hdr[, 5], flags = hdr[, 23],
                  width = ifelse(hdr[, 27] == 1L, 4L, hdr[, 27]),
                  n_points = hdr[, 28],
                  n_profile = hdr[, 31] + hdr[, 32] * 65536)

  block <- diff(c(offsets, length(ms))) - 64
  expected <- ifelse(sq_profile(header[, "flags"]), 2 * header[, "n_profile"],
                     header[, "n_points"] * (2 + header[, "width"]))
  bad <- which(block != expected)
  if (length(bad) > 0){
    stop(sprintf("The block size of scan %d does not match its header.",
                 bad[1] - 1L), call. = FALSE)
  }
  list(offsets = offsets, ms = ms, header = header)
}

#' Decode 'Shimadzu' single quadrupole centroid scans
#'
#' @param x The output of `read_sq_scans`.
#' @param i Rows of `x$header` holding centroid scans.
#' @return A long table with columns `scan`, `rt`, `mz` and `intensity`.
#' @noRd
read_sq_centroids <- function(x, i){
  h <- x$header[i, , drop = FALSE]
  w <- h[, "width"]
  n <- h[, "n_points"]

  start <- sequence(n, from = x$offsets[i] + 65, by = 2L + w)
  rec_w <- rep.int(w, n)
  mz <- (as.integer(x$ms[start]) + 256L * as.integer(x$ms[start + 1L]))/20
  intensity <- numeric(length(start))
  for (k in seq_len(max(w, 0L))){
    j <- which(rec_w >= k)
    b <- as.integer(x$ms[start[j] + 1L + k])
    if (k == 4L) b <- bitwAnd(b, 0x7FL)
    intensity[j] <- intensity[j] + b * 256^(k - 1)
  }

  row <- rep.int(seq_len(nrow(h)), n)
  data.table::setDT(list(
    scan = as.integer(h[row, "scan"]),
    rt = h[row, "rt"]/60000,
    mz = mz,
    intensity = intensity))
}

#' Decode 'Shimadzu' single quadrupole profile scans
#'
#' @param x The output of `read_sq_scans`.
#' @param i Rows of `x$header` holding profile scans.
#' @param mz_range The m/z range of the method, from `read_sq_mz_range`.
#' @return A long table like the centroids'. A scan whose point count does not
#' fit the one method the axis was measured on gives an error.
#' @noRd
read_sq_profiles <- function(x, i, mz_range){
  h <- x$header[i, , drop = FALSE]
  n <- h[, "n_profile"]
  if (length(mz_range) != 2){
    stop("The m/z range could not be read from `Mass Raw Data/Status`.",
         call. = FALSE)
  }
  if (any(n - 26 != 15 * diff(mz_range))){
    stop(paste("The m/z axis of these profile scans is unknown: their point",
               "count does not match the m/z range."), call. = FALSE)
  }
  intensity <- unlist(lapply(seq_along(i), function(k){
    readBin(x$ms[x$offsets[i[k]] + 64 + seq_len(2 * n[k])], what = "integer",
            size = 2L, signed = FALSE, endian = "little", n = n[k])
  }), use.names = FALSE)
  big <- intensity >= 32768L
  intensity[big] <- (intensity[big] - 32768L) * 32L
  j <- sequence(n, from = 0L)
  row <- rep.int(seq_along(i), n)
  data.table::setDT(list(
    scan = as.integer(h[row, "scan"]),
    rt = h[row, "rt"]/60000,
    mz = mz_range[1] + (j - 12) * diff(mz_range)/(n[row] - 27),
    intensity = intensity * 2/3))
}

#' Read the m/z range of a 'Shimadzu' single quadrupole stream
#'
#' The `Status` stream opens with the m/z range of the scans (uint16 at bytes
#' 14 and 16).
#' @noRd
read_sq_mz_range <- function(path){
  path_status <- export_stream(path, c("Mass Raw Data", "Status"))
  on.exit(unlink_stream(path_status), add = TRUE)
  if (length(path_status) != 1 || is.na(path_status)) return(NULL)
  readBin(path_status, what = "integer", size = 2L, signed = FALSE,
          endian = "little", n = 9L)[8:9]
}

#' Flag 'Shimadzu' single quadrupole profile scans from the event flags
#' @noRd
sq_profile <- function(flags){
  bitwAnd(flags, 0x10L) != 0L
}

#' Label 'Shimadzu' single quadrupole polarity from the event flags
#' @noRd
sq_polarity <- function(flags){
  ifelse(bitwAnd(flags, 4L) == 0L, "positive", "negative")
}

#' Read metadata from 'Shimadzu' single quadrupole streams
#' @noRd
read_sq_metadata <- function(path, time_range){
  meta <- read_sz_file_properties(path)
  meta$DLT <- time_range[1]
  meta$AT <- time_range[2]
  meta$`time.unit` <- "Minutes"
  meta$DETN <- "MS"
  meta$mz_range <- read_sq_mz_range(path)
  meta
}

#' Read TIC from 'Shimadzu' single quadrupole stream
#'
#' Reads the total ion current of every scan from `Mass Raw Data/TIC Data`
#' (uint64, see `read_sz_sq`). Each acquisition event is returned as a separate
#' chromatogram, named `Event 1`, `Event 2` and so on.
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
read_sz_sq_tic <- function(path, format_out = c("matrix", "data.frame",
                                                "data.table"),
                           data_format = c("wide", "long"),
                           read_metadata = TRUE,
                           metadata_format = "shimadzu_lcd"){
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format)
  h <- read_sq_scans(path)$header
  tic <- read_ole_uint32(path, c("Mass Raw Data", "TIC Data"))
  if (length(tic) != 2L * nrow(h)){
    stop("The 'TIC Data' stream does not hold one value per scan.",
         call. = FALSE)
  }
  tic <- matrix(tic, nrow = 2L)
  tic <- tic[1, ] + tic[2, ] * 2^32
  events <- sort(unique(h[, "event"]))
  meta <- if (read_metadata){
    read_sq_metadata(path, range(h[, "rt"])/60000)
  } else NULL
  dat <- lapply(events, function(ev){
    i <- which(h[, "event"] == ev)
    x <- format_2d_chromatogram(rt = h[i, "rt"]/60000, int = tic[i],
                                data_format = data_format,
                                format_out = format_out)
    if (is.null(meta)) return(x)
    m <- meta
    m$ms_level <- 1L
    m$polarity <- sq_polarity(h[i[1], "flags"])
    attach_metadata(x, m, format_in = metadata_format, source_file = path,
                    data_format = data_format, format_out = format_out,
                    source_file_format = "shimadzu_lcd")
  })
  names(dat) <- paste("Event", events + 1L)
  dat
}
