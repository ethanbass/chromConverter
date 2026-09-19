#' Read 'Shimadzu' TLM stream
#'
#' Read the `TLM Raw Data` mass spectrometry stream from 'Shimadzu
#' LabSolutions' `.lcd` files.
#'
#' Spectra live in the `MS Raw Data` stream and are located through
#' `Spectrum Index`, which holds one 24-byte entry per spectrum:
#'
#' | **Offset** | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3   | `uint32` | Record size in bytes |
#' | 4–7   | `uint32` | Event number |
#' | 8–11  | `uint32` | Offset into `MS Raw Data` |
#' | 12–15 | `uint32` | Padding |
#' | 16–19 | `uint32` | Cycle number |
#' | 20–23 | `uint32` | Scan number |
#'
#' Each record begins with a 12-byte wrapper (`0xFFFFFFFF`, uncompressed size,
#' compressed size) followed by a zlib stream. The decompressed block opens
#' with a 44-byte header shared by every scan type:
#'
#' | **Offset** | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3   | `uint32` | Retention time (milliseconds) |
#' | 4–7   | `uint32` | Retention time of the first scan in the cycle |
#' | 8–11  | `uint32` | Event number |
#' | 12–15 | `uint32` | Scan counter within the event |
#' | 16–19 | `uint32` | Scan index (0-based) |
#' | 20–21 | `uint16` | Scan type: 10 = MS1 profile, 14 = MS2 profile, 15 = MRM/SIM |
#' | 22–23 | `uint16` | MS level + 1 (0 in a truncated final scan) |
#' | 24–27 | `uint32` | Constant (`0x00010000`) |
#' | 28–31 | `uint32` | Last precursor m/z x 100 (stale outside MS2 scans) |
#' | 32–35 | `uint32` | Instrument state bit field; bit 0 follows polarity |
#' | 36–39 | `uint32` | Polarity (0 = positive, 1 = negative) |
#' | 40–43 | `uint32` | Number of data points (profile) or transitions (MRM) |
#'
#' **Profile scans** (type 10 and 14) follow the header with two m/z pairs
#' stored as m/z x 100 — the isolation window (equal low and high values for
#' MS2; the scan range for MS1), then the scan range — and then `n`
#' unsigned 32-bit intensities. Bit 31 of an intensity marks detector
#' saturation and is not part of the value. Intensities are already in the
#' units LabSolutions reports, so no scaling is applied.
#'
#' Points lie on an evenly spaced m/z grid running from the low end of the scan
#' range, with a bin width of `(high - low) / (100 * n)` (0.1 in the files
#' seen so far). The grid overhangs the acquired range by 10 bins at the bottom
#' and 9 at the top; those are dropped, which is what makes the summed
#' intensity match the `TIC Data` stream exactly.
#'
#' **MRM and SIM scans** (type 15) instead follow the header with `n`
#' 12-byte transitions: Q1 m/z x 100, Q3 m/z x 100, and intensity.
#'
#' @param path Path to `.lcd` file.
#' @param format_out Class of output. Either `data.table` or
#' `data.frame`. Spectra are long and mixed-type, so `matrix`
#' resolves to `data.table`.
#' @param data_format Ignored: spectra have no wide representation and are
#' always returned in `long` format.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata.
#' @param sparse Logical. Whether to return spectra in sparse format
#' (excluding zeros), as [call_rainbow()] does. Defaults to `TRUE`, since a
#' profile scan is mostly empty and keeping its zeros costs roughly an order
#' of magnitude more memory.
#' @param levels Which MS levels to return, spelled `MS1` and `MS2`. Both are
#' decoded either way, since the level of a scan is recorded inside its own
#' compressed record.
#' @return A named list holding whichever of `MS1` and `MS2` the file has, each
#' a long table with columns `scan`, `rt` (minutes), `mz` and `intensity`,
#' preceded by `precursor_mz` where the scans carry one. A per-scan summary
#' (retention time, event, MS level, polarity, precursor m/z, point count) is
#' attached to each as a `scan_info` attribute.
#' @author Ethan Bass
#' @md
#' @keywords internal
read_sz_tlm <- function(path, format_out = c("matrix", "data.frame",
                                             "data.table"),
                        data_format = "long", levels = c("MS1", "MS2"),
                        sparse = TRUE, read_metadata = TRUE,
                        metadata_format = "shimadzu_lcd"){
  format_out <- check_format_out_table(format_out)
  data_format <- "long"

  index <- read_tlm_spectrum_index(path)
  path_ms <- export_stream(path, c("TLM Raw Data", "MS Raw Data"))
  on.exit(unlink_stream(path_ms), add = TRUE)
  ms <- readBin(path_ms, what = "raw", n = file.size(path_ms))
  n_scans <- nrow(index)

  mz <- intensity <- precursor <- vector("list", n_scans)
  header <- matrix(0L, nrow = n_scans, ncol = 11L,
                   dimnames = list(NULL, tlm_header_fields()))

  # gather every record's compressed length in one pass rather than seeking
  # back for four bytes per scan
  clens <- readBin(ms[rep(index[, "offset"] + 9L, each = 4L) + 0:3],
                   what = "integer", n = n_scans, size = 4L, endian = "little")

  # m/z grids are shared by every scan of an event, so they are built once per
  # distinct (range, n) combination rather than once per scan
  grids <- list()

  for (i in seq_len(n_scans)){
    offset <- index[i, "offset"]
    x <- memDecompress(ms[(offset + 13L):(offset + 12L + clens[i])],
                       type = "gzip")

    h <- read_tlm_scan_header(x)
    header[i, ] <- h
    n <- h[["n_points"]]
    if (n == 0L) next
    scan_type <- bitwAnd(h[["type_flags"]], 0xFFFFL)

    if (scan_type %in% tlm_transition_types()){
      tr <- decode_tlm_transitions(x, n)
      precursor[[i]] <- tr[1, ]/100
      mz[[i]] <- tr[2, ]/100
      intensity[[i]] <- tr[3, ]
    } else {
      # 10 points are trimmed from the head of the grid and 9 from the tail, so
      # anything shorter than that would index backwards
      if (n < 20L) next
      range <- decode_tlm_scan_range(x)
      # the stored grid overhangs the acquired range at both ends, so read only
      # the bytes for points 11..n-9 rather than decoding all `n` and subsetting
      int <- readBin(x[101:(4L*n + 24L)], what = "integer", n = n - 19L,
                     size = 4L, endian = "little")
      key <- paste(range[1], range[2], n, sep = "/")
      grid <- grids[[key]]
      if (is.null(grid)){
        bin_width <- (range[2] - range[1])/(100*n)
        grid <- range[1]/100 + bin_width * seq.int(10L, n - 10L)
        grids[[key]] <- grid
      }
      keep <- if (sparse) which(int != 0L) else seq_along(int)
      mz[[i]] <- grid[keep]
      # bit 31 flags detector saturation rather than contributing to the value
      intensity[[i]] <- bitwAnd(int[keep], 0x7FFFFFFFL)
      # `last_precursor` describes a product-ion scan (type 14) and is stale in
      # any other scan, so only those rows take it. Reporting it here keeps
      # `precursor_mz` meaning the same thing in both containers: the precursor
      # the spectrum came from.
      if (scan_type == 14L){
        precursor[[i]] <- rep.int(h[["last_precursor"]]/100, length(keep))
      }
    }
  }

  scan_info <- tlm_scan_info(header)
  n_peaks <- lengths(intensity)
  dat <- data.table::setDT(list(
    scan = rep.int(header[, "scan"], n_peaks),
    rt = rep.int(header[, "rt"]/60000, n_peaks),
    mz = unlist(mz, use.names = FALSE),
    intensity = unlist(intensity, use.names = FALSE)))
  has_precursor <- !vapply(precursor, is.null, logical(1))
  if (any(n_peaks > 0L & has_precursor)){
    # A file can mix scan types that carry a precursor with ones that do not --
    # SIM or MRM alongside product-ion scans, say -- so the scans without one
    # need placeholders or the column will not line up with the peaks.
    gaps <- which(!has_precursor & n_peaks > 0L)
    for (i in gaps) precursor[[i]] <- rep(NA_real_, n_peaks[i])
    data.table::set(dat, j = "precursor_mz",
                    value = unlist(precursor, use.names = FALSE))
    data.table::setcolorder(dat, c("scan", "rt", "precursor_mz", "mz",
                                   "intensity"))
  }

  # `scan_info` mixes numeric and character columns, so it stays a data.frame
  # whatever `format_out` is asked for
  sz_split_ms_levels(dat, scan_info, levels = levels,
                     level = rep.int(unname(scan_info$ms_level), n_peaks),
                     meta = if (read_metadata){
                       read_tlm_metadata(path, index = index,
                                         path_ms = path_ms)
                     } else NULL,
                     path = path, format_out = format_out,
                     metadata_format = metadata_format)
}

#' Read spectrum index from 'Shimadzu' TLM stream
#' @noRd
read_tlm_spectrum_index <- function(path){
  path_index <- export_stream(path, c("TLM Raw Data", "Spectrum Index"))
  on.exit(unlink_stream(path_index), add = TRUE)
  index <- matrix(as_uint32(readBin(path_index, what = "integer", size = 4L,
                                    endian = "little",
                                    n = file.size(path_index) %/% 4L)),
                  ncol = 6L, byrow = TRUE,
                  dimnames = list(NULL, c("size", "event", "offset", "padding",
                                          "cycle", "scan")))
  index
}

#' Field names of the 44-byte 'Shimadzu' TLM scan header
#' @noRd
tlm_header_fields <- function(){
  c("rt", "cycle_rt", "event", "event_scan", "scan", "type_flags", "const",
    "last_precursor", "state", "polarity", "n_points")
}

#' Read header from 'Shimadzu' TLM scan
#' @noRd
read_tlm_scan_header <- function(x){
  h <- readBin(x[1:44], what = "integer", n = 11L, size = 4L, endian = "little")
  names(h) <- tlm_header_fields()
  h
}

#' Scan types that store transitions rather than a profile
#'
#' MRM (15) and SIM (11) share a record layout of
#' `n x (Q1 x 100, Q3 x 100, intensity)`. SIM records have `Q1 == Q3`.
#' @noRd
tlm_transition_types <- function() c(11L, 15L)

#' Decode the transition block of a 'Shimadzu' TLM scan
#'
#' @return A 3 x `n` integer matrix of Q1 x 100, Q3 x 100 and intensity.
#' @noRd
decode_tlm_transitions <- function(x, n){
  tr <- readBin(x[45:(44L + 12L*n)], what = "integer", n = 3L*n, size = 4L,
                endian = "little")
  dim(tr) <- c(3L, n)
  tr
}

#' Decode the scan range of a 'Shimadzu' TLM profile scan
#'
#' @return The two ends of the range, as m/z x 100.
#' @noRd
decode_tlm_scan_range <- function(x){
  readBin(x[53:60], what = "integer", n = 2L, size = 4L, endian = "little")
}

#' Label a 'Shimadzu' TLM polarity flag
#' @noRd
tlm_polarity <- function(x) ifelse(x == 0L, "positive", "negative")

#' Map a 'Shimadzu' TLM scan type to an MS level and a label
#'
#' Observed types: 10 full scan, 11 SIM, 14 product-ion scan, 15 MRM. SIM and
#' MRM share a record layout of `n x (Q1, Q3, intensity)`; SIM is taken to be
#' MS1 because its Q1 and Q3 are equal, i.e. nothing is selected after the
#' collision cell. Unknown types give `NA` rather than a guess.
#' @noRd
sz_tlm_ms_level <- function(scan_type){
  c("10" = 1L, "11" = 1L, "14" = 2L, "15" = 2L)[as.character(scan_type)]
}

#' @noRd
sz_tlm_scan_type <- function(scan_type){
  c("10" = "scan", "11" = "SIM", "14" = "product ion scan",
    "15" = "MRM")[as.character(scan_type)]
}

#' Summarize decoded 'Shimadzu' TLM scan headers
#'
#' @param header Integer matrix of scan headers, one row per scan, with the
#' columns named by `tlm_header_fields`.
#' @return One row per scan, with the fields that describe the scan rather than
#' its peaks.
#' @noRd
tlm_scan_info <- function(header){
  scan_type <- bitwAnd(header[, "type_flags"], 0xFFFFL)
  precursor <- header[, "last_precursor"]/100
  # `last_precursor` only describes a product-ion scan. MRM and SIM records
  # carry a Q1 per transition, which can differ within one scan, so there is no
  # single per-scan value to report here --- those are in the `precursor_mz`
  # column of the spectra instead.
  precursor[scan_type != 14L] <- NA_real_
  data.table::data.table(
    scan = header[, "scan"],
    rt = header[, "rt"]/60000,
    event = header[, "event"],
    ms_level = sz_tlm_ms_level(scan_type),
    polarity = tlm_polarity(header[, "polarity"]),
    precursor_mz = precursor,
    n_points = header[, "n_points"])
}

#' Read TIC from 'Shimadzu' TLM stream
#'
#' Reads total ion current chromatograms from the `TLM Raw Data` streams
#' of 'Shimadzu LabSolutions' `.lcd` files.
#'
#' `TIC Data` holds two little-endian `uint32`s per spectrum: the
#' total ion current and a flag that is `0x80000000` when any point in
#' the spectrum saturated the detector. The retention times come from the
#' `Retention Time` stream. Neither stream needs the spectra themselves to
#' be decompressed, so this is much cheaper than `read_sz_tlm`.
#'
#' Each acquisition event is returned as a separate chromatogram, since events
#' can differ in polarity, MS level and scan range, and are interleaved in
#' acquisition order.
#'
#' The `SumTIC Data` stream (one 12-byte record per cycle: retention time,
#' value, saturation flag) is a curve computed by the instrument. It is not a
#' plain sum of the per-spectrum TICs — in MRM files it matches the cycle sum
#' exactly, but in scan files it comes out as the cycle mean divided by six —
#' so it is returned as stored rather than recomputed.
#'
#' @param path Path to `.lcd` file.
#' @param format_out Class of output. Either `matrix`, `data.frame`,
#' or `data.table`.
#' @param data_format Either `wide` (default) or `long`.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata.
#' @param what Which curve to return: `tic` (default) for the per-spectrum
#' total ion current, or `sumtic` for the per-cycle curve.
#' @return A named list of chromatograms, one per acquisition event, or a
#' single chromatogram for `what = "sumtic"`.
#' @author Ethan Bass
#' @keywords internal
read_sz_tlm_tic <- function(path, format_out = c("matrix", "data.frame",
                                                 "data.table"),
                            data_format = c("wide", "long"),
                            read_metadata = TRUE,
                            metadata_format = "shimadzu_lcd",
                            what = c("tic", "sumtic")){
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format)
  what <- match.arg(what)

  if (what == "sumtic"){
    x <- matrix(read_ole_uint32(path, c("TLM Raw Data", "SumTIC Data")),
                ncol = 3L, byrow = TRUE)
    dat <- format_2d_chromatogram(rt = x[, 1]/60000, int = x[, 2],
                                  data_format = data_format,
                                  format_out = format_out)
    if (read_metadata){
      dat <- attach_metadata(dat, read_tlm_metadata(path),
                             format_in = metadata_format,
                             source_file = path, data_format = data_format,
                             format_out = format_out,
                             source_file_format = "shimadzu_lcd")
    }
    return(dat)
  }

  tic <- matrix(read_ole_uint32(path, c("TLM Raw Data", "TIC Data")),
                ncol = 2L, byrow = TRUE)
  rts <- read_ole_uint32(path, c("TLM Raw Data", "Retention Time"))/60000
  index <- read_tlm_spectrum_index(path)
  events <- index[, "event"]
  ev_ids <- sort(unique(events))
  # which rows belong to each event, wanted once for the chromatograms and
  # again for their metadata
  ev_rows <- lapply(ev_ids, function(ev) which(events == ev))
  dat <- lapply(ev_rows, function(idx){
    format_2d_chromatogram(rt = rts[idx], int = tic[idx, 1],
                           data_format = data_format,
                           format_out = format_out)
  })
  names(dat) <- paste("Event", ev_ids)
  if (read_metadata){
    meta <- read_tlm_metadata(path, index = index, time_range = range(rts))
    # Each event monitors its own transition over its own slice of the run,
    # so attaching the same file-level metadata to all of them would leave
    # nothing to tell them apart but their names.
    ev_tab <- meta$events
    dat <- lapply(seq_along(ev_ids), function(i){
      m <- meta
      row <- if (is.null(ev_tab)) NULL else
        ev_tab[ev_tab$event == ev_ids[i], , drop = FALSE]
      if (!is.null(row) && nrow(row) == 1L){
        m$ms_level <- row$ms_level
        m$scan_type <- row$scan_type
        m$polarity <- row$polarity
        tr <- if (is.null(row$transitions)) NULL else row$transitions[[1]]
        if (is.null(tr)){
          # a full scan or a product-ion scan sweeps a range of masses
          # rather than monitoring a fixed set of them, so the range is
          # what describes the event
          m$precursor_mz <- row$precursor_mz
          m$mz_range <- c(row$mz_min, row$mz_max)
        } else {
          # an MRM event can monitor more than one transition, in which case
          # the whole set is reported rather than the range it spans
          m$precursor_mz <- sort(unique(tr$precursor_mz))
          # SIM selects its ions in Q1 and passes them through undissociated,
          # so its Q3 masses only repeat the precursors
          if (!identical(row$scan_type, "SIM")){
            m$product_mz <- sort(unique(tr$product_mz))
          }
        }
      }
      # `time_range` comes from DLT and AT, which describe the whole run;
      # a scheduled event covers only a window of it
      rt_ev <- rts[ev_rows[[i]]]
      if (length(rt_ev) > 0){
        m$DLT <- min(rt_ev)
        m$AT <- max(rt_ev)
      }
      attach_metadata(dat[[i]], m, format_in = metadata_format,
                      source_file = path, data_format = data_format,
                      format_out = format_out,
                      source_file_format = "shimadzu_lcd")
    })
    names(dat) <- paste("Event", ev_ids)
  }
  dat
}

#' Read metadata from 'Shimadzu' TLM streams
#'
#' Combines the sample and method information shared by all `.lcd` files
#' with the mass spectrometer fields from `TLM Raw Data/Status`:
#'
#' | **Offset** | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3   | `uint32` | Struct version (202 in all files seen) |
#' | 4–7   | `uint32` | Retention time of the last scan (milliseconds) |
#' | 8–11  | `uint32` | Number of spectra |
#' | 18–33 | `char`   | Firmware version (e.g. `5.98SP1`) |
#' | 34–49 | `char`   | Instrument configuration (e.g. `TQ8030-60_M1.66`) |
#' | 182–183 | `uint16` | Number of acquisition events |
#'
#' The instrument configuration names the control platform the whole triple
#' quadrupole line shares --- `TQ8030`, whatever the model, followed by a
#' hardware and a firmware revision --- so it is reported as
#' `instrument_config` and the instrument itself is taken from the
#' `SystemInformation` stream (see `read_sz_system_info`).
#'
#' A 24-byte record per event follows at offset 184, but is empty apart from an
#' event number and one further counter. `TLM Raw Data/Mass Parameters`
#' (a packed `CTLM3030Parameters` struct) should hold the rest of the
#' method — collision energies and dwell times among them — but is not parsed:
#' its fields are unaligned and none of the available files provide a reference
#' to check an interpretation against.
#'
#' @param path Path to `.lcd` file.
#' @return A list of metadata fields.
#' @author Ethan Bass
#' @md
#' @noRd
read_tlm_metadata <- function(path, index = read_tlm_spectrum_index(path),
                              path_ms = NULL,
                              time_range = range(read_ole_uint32(path,
                                c("TLM Raw Data", "Retention Time")))/60000){
  meta <- read_sz_file_properties(path)

  path_status <- export_stream(path, c("TLM Raw Data", "Status"))
  on.exit(unlink_stream(path_status), add = TRUE)
  status <- readBin(path_status, what = "raw", n = file.size(path_status))

  nums <- readBin(status[1:12], what = "integer", n = 3L, size = 4L,
                  endian = "little")
  meta$instrument_config <- readBin(status[35:50], what = "character")
  meta$firmware_version <- readBin(status[19:34], what = "character")
  meta$n_scans <- nums[3]
  meta$n_events <- readBin(status[183:184], what = "integer", n = 1L,
                           size = 2L, endian = "little", signed = FALSE)
  # `attach_metadata` builds `time_range` from DLT and AT
  meta$DLT <- time_range[1]
  meta$AT <- time_range[2]
  meta$`time.unit` <- "Minutes"
  meta$DETN <- "MS"
  meta$events <- read_tlm_events(path, index = index, path_ms = path_ms)
  meta
}

#' Summarize acquisition events in a 'Shimadzu' TLM stream
#'
#' Decodes the first spectrum of each event to recover the scan parameters that
#' the event header carries. Cheap enough to call routinely, since only one
#' record per event is decompressed. The m/z range and the transition list are
#' fixed by the method and so are the same in every scan of an event, but a
#' data-dependent event picks a new precursor each cycle --- in the files
#' examined, as many as 54 across 165 scans --- and its first scan is no more
#' representative than any other. Such an event reports `precursor_mz` as
#' `NA`; the per-scan values are in the `scan_info` attribute returned by
#' `read_sz_tlm`.
#'
#' `mz_min` and `mz_max` mean different things by scan type. A full
#' scan or product-ion scan sweeps a range, and they are its two ends; an MRM
#' or SIM event monitors a fixed set of masses, and they only span it. The set
#' itself is in `transitions`, which carries every `Q1 > Q3` pair
#' the first scan lists, since the first transition alone does not describe an
#' event monitoring several. `precursor_mz` is `NA` for the rare
#' event whose transitions do not share one precursor.
#'
#' @noRd
read_tlm_events <- function(path, index = read_tlm_spectrum_index(path),
                            path_ms = NULL){
  # only four records are read here, so reuse a stream the caller has already
  # extracted rather than writing out a second copy of a 30 MB stream
  own_stream <- is.null(path_ms)
  if (own_stream){
    path_ms <- export_stream(path, c("TLM Raw Data", "MS Raw Data"))
  }
  f <- file(path_ms, "rb")
  on.exit(close(f))
  if (own_stream) on.exit(unlink_stream(path_ms), add = TRUE)

  events <- sort(unique(index[, "event"]))
  out <- do.call(rbind, lapply(events, function(ev){
    scans <- which(index[, "event"] == ev)
    i <- scans[1]
    seek(f, index[i, "offset"], origin = "start")
    record <- readBin(f, what = "raw", n = index[i, "size"])
    x <- memDecompress(record[13:index[i, "size"]], type = "gzip")
    h <- read_tlm_scan_header(x)
    scan_type <- bitwAnd(h[["type_flags"]], 0xFFFFL)
    n <- h[["n_points"]]
    transitions <- NULL
    if (scan_type %in% tlm_transition_types()){
      # an MRM or SIM event lists all of its transitions in every scan
      tr <- decode_tlm_transitions(x, n)/100
      transitions <- data.frame(precursor_mz = tr[1, ], product_mz = tr[2, ],
                                row.names = NULL)
      precursor <- unique(tr[1, ])
      if (length(precursor) > 1) precursor <- NA_real_
      range <- range(tr[2, ])
    } else {
      # the upper half of `type_flags` records how the event was acquired: 1 for
      # a targeted one (MRM, SIM, or a product-ion scan on a precursor the
      # method fixes), 2 for the survey scan of a data-dependent run and 3 for
      # the product-ion scans that run selects. Only 3 retunes Q1 per cycle, so
      # only there is the first scan's precursor unrepresentative of the event.
      dda <- bitwShiftR(h[["type_flags"]], 16L) == 3L
      precursor <- if (scan_type == 14L && !dda){
        h[["last_precursor"]]/100
      } else NA_real_
      range <- decode_tlm_scan_range(x)/100
    }
    out <- data.frame(event = ev, n_scans = length(scans),
                      ms_level = sz_tlm_ms_level(scan_type),
                      polarity = tlm_polarity(h[["polarity"]]),
                      scan_type = sz_tlm_scan_type(scan_type),
                      precursor_mz = precursor,
                      mz_min = range[1], mz_max = range[2], row.names = NULL)
    # always present, so that a file mixing MRM or SIM events with profile
    # ones still binds into one table
    out$transitions <- list(transitions)
    out
  }))
  if (all(vapply(out$transitions, is.null, logical(1)))){
    out$transitions <- NULL
  }
  out
}
