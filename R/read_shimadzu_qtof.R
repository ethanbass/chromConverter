#' Read 'Shimadzu' QTOF stream
#'
#' Read QTOF stream from 'Shimadzu LabSolutions' \code{.lcd} files.
#'
#' Data for each scan is stored in three contiguous blocks: a 64-byte header,
#' a block of flight times, followed by a block of intensities.
#'
#' **Scan Header** (64 bytes, little-endian):
#'
#' | **Offset**  | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3| `uint32` | MS level (1 = MS1, 2 = MS2) |
#' | 4–7 | `uint32` | Retention time (milliseconds) |
#' | 8–15 | `int64` | Base peak flight time (TOF ticks; 0 if empty scan) |
#' | 16–19 | `uint32` | Base peak raw intensity (0 if empty scan) |
#' | 20–23 | `uint32` | Scan index (0-based; increments for every scan including empty ones) |
#' | 24–27 | `uint32` | Data block size in bytes (`n_peaks * 10`; 0 if empty scan) |
#' | 28–31 | `uint32` | Flags: `0x00010001` = scan with data; `0x000200xx` = empty scan |
#' | 32–35 | `uint32` | Padding |
#' | 36–39 | `uint32` | Spectrum count (2 = has data, 1 = empty) |
#' | 40–43 | `uint32` | Instrument constant (scan window / detector setting) |
#' | 44–47 | `uint32` | Instrument constant |
#' | 48–51 | `uint32` | Instrument constant |
#' | 52–55 | `uint32` | Instrument constant |
#' | 56–63 | — | Padding |
#'
#' **m/z block** (n x 8 bytes)
#' Each peak's m/z is stored as a signed 64-bit little-endian integer
#' representing a TOF flight time in instrument ticks. Conversion uses a
#' two-parameter calibration derived from the TOF \code{Calibration Table}
#' stream:
#' \deqn{mz = ((t-B)/A)^2}
#' where \code{A} and \code{B} are derived from the calibrant flight times.
#'
#' **Intensity block** (n x m bytes)
#' Each peak's intensity is stored as an unsigned 16-bit little-endian integer
#' (raw detector counts). To convert to the intensity values reported by
#' LabSolutions, values must be divide by a fixed (???) hardware gain factor of
#' approximately 3.761.
#'
#' @param mz_hex     Character. Hex string of the m/z block (int64 flight times).
#' @param int_hex    Character. Hex string of the intensity block (uint16 counts).
#' @param n          Integer. Number of peaks. If NULL, inferred from block sizes.
#' @param A          Numeric. TOF calibration coefficient A (default fitted from calibrants).
#' @param B          Numeric. TOF calibration coefficient B (default fitted from calibrants).
#' @param int_scale  Numeric. Intensity scale factor (default 3.761).
#' @return A data.table with columns `mz` and `intensity`.
#' @md
#' @export

read_sz_qtof <- function(path, format_out = "data.frame",
                         data_format = "wide",
                         read_metadata = TRUE,
                         metadata_format = "shimadzu_lcd",
                         scale = TRUE){
  if (data_format == "long" && format_out == "matrix"){
    format_out <- "data.frame"
  }
  existing_streams <- check_streams(path, what = "qtof")
  if (length(existing_streams) == 0){
    stop("QTOF data stream could not be detected.")
  }

  rts <- read_lcd_retention_times(path)

  path_centroid <- export_stream(path, existing_streams[[1]])
  f <- file(path_centroid, "rb")
  on.exit(close(f))

  # offsets <- get_spectrum_offsets(path)
  # offsets <- c(offsets, file.info(path_ms)$size)

  dat <- lapply(seq_along(rts), function(i){
    tryCatch({
      decode_qtof_scan(f)
    }, error = function(e) stop(sprintf("Failed to read scan %d: %s",
                                        i, conditionMessage(e)))
    )
  })
  dat <- do.call(rbind, dat)
  dat
}

#' Decode QTOF scan
#'
#' Decode a single Shimadzu QTOF centroid scan
#'
#' @noRd

decode_qtof_scan <- function(f, A = 4.690116e+13, B = 7.448160e+11,
                             int_scale   = 3.761) {
  header <- read_scan_header(f)
  n_peaks <- header$data_size %/% (8L + header$int_width)
  if (n_peaks == 0L){
    return(NULL)
  }
  # data_start <- seek(f, NA)
  flight_times <- read_flight_times(f, n_peaks)
  # int_start <- seek(f, NA)
  raw_intensities <- readBin(f, what = "integer", n = n_peaks,
                             size = header$int_width, endian = "little")
  data.table::data.table(
    scan = header$scan_index,
    rt = header$rt_ms,
    mz  = ((flight_times - B) / A)^2,
    intensity = raw_intensities / 3.761
  )
}

#' Read flight times from Shimadzu QTOF stream
#' @noRd
read_flight_times <- function(con, n_peaks){
  raw <- readBin(con, what = "integer", n = n_peaks * 2L, size = 4L,
                 endian = "little")
  lo <- bit64::as.integer64(raw[seq(1, length(raw), 2)])
  hi <- bit64::as.integer64(raw[seq(2, length(raw), 2)])
  flight_times <- lo + hi * bit64::as.integer64(2^32)
  flight_times
}

#' Read retention times from Shimadzu QTOF stream
#' @noRd
read_lcd_retention_times <- function(path){
  path_rts <- export_stream(path, c("QTFL RawData", "Retention Time"))
  f <- file(path_rts, "rb")
  on.exit(close(f))

  seek(f, 0, origin = "end")
  last_byte <- seek(f, 0, origin = "end")

  n_val <- last_byte/4
  seek(f, 0, origin = "start")
  rts <- readBin(f, what = "integer", size = 4, n = n_val, endian = "little")
  scan <- rts[seq(2,length(rts),by=3)]
  rts <- rts[seq(1,length(rts),by=3)]
  # rts/60
  rts
}

#' Read header from Shimadzu QTOF stream
#' @noRd
read_scan_header <- function(con) {
  raw <- readBin(con, what = "raw", n = 64L)

  header_info <- list(
    ms_level      = readBin(raw[1:4],   "integer", size = 4L, endian = "little"),
    rt_ms         = readBin(raw[5:8],   "integer", size = 4L, endian = "little"),
    bp_flight_time = ((read_flight_times(raw[9:16], 1) - B) / A)^2,
    bp_raw_intensity = readBin(raw[17:20], "integer", size = 4L, endian = "little"),
    scan_index    = readBin(raw[21:24], "integer", size = 4L, endian = "little"),
    data_size     = readBin(raw[25:28], "integer", size = 4L, endian = "little"),
    flags         = readBin(raw[29:32], "integer", size = 4L, endian = "little"),
    int_width = readBin(raw[37:40], "integer", size = 4L, endian="little")
  )
  header_info
}

#' Fit TOF calibration coefficients A and B from calibrant data
#'
#' Solves t = A*sqrt(mz) + B via least squares, excluding outlier calibration
#' sets (rep 2 in the Shimadzu TOF Calibration Table is typically an outlier).
#'
#' @param theoretical_mz  Numeric vector. Theoretical m/z of calibrant ions.
#' @param flight_times    Numeric vector. Measured flight times (int64 ticks).
#' @return Named numeric vector with elements `A` and `B`.
#' @noRd
fit_tof_calibration <- function(theoretical_mz, flight_times) {
  X <- cbind(sqrt(theoretical_mz), 1)
  fit <- stats::.lm.fit(X, flight_times)
  stats::setNames(fit$coefficients, c("A", "B"))
}
