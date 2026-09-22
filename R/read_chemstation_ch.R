#' Read 'Agilent ChemStation' CH files
#'
#' Reads 'Agilent ChemStation' `.ch` files.
#'
#' 'Agilent' `.ch` files come in several different formats. This parser
#' can automatically detect and read several versions of these files from
#' 'Agilent ChemStation' and 'Agilent OpenLab', including versions `30` and
#' `130`, which are generally produced by ultraviolet detectors, as well as
#' `81`, `179`, and `181` which are generally produced by flame ionization (FID)
#' detectors.
#'
#' @importFrom bitops bitAnd bitShiftL
#' @inheritParams shared_params
#' @param path Path to 'Agilent' `.ch` file.
#' @param scale Whether to scale the data by the scaling factor present in the
#' file. Defaults to `TRUE`. 'MassHunter' seems to ignore the scaling
#' factor in at least some types of 'ChemStation' files.
#' @param source_file Source file from which chromatogram data was originally
#' derived.
#' @author Ethan Bass
#' @inherit generic_return_2D return
#' @note This function was adapted from the [Chromatography Toolbox](
#' https://github.com/chemplexity/chromatography)
#' (© James Dillon 2014).
#' @examplesIf interactive()
#' read_chemstation_ch("tests/testthat/testdata/chemstation_130.ch")
#' @family 'Agilent' parsers
#' @export
#' @md

read_chemstation_ch <- function(path, format_out = c("matrix", "data.frame",
                                                     "data.table"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                scale = TRUE, source_file = NULL){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "chemstation")
  source_file <- ifelse(is.null(source_file), path, source_file)
  f <- file(path, "rb")
  on.exit(close(f))

  # HEADER
  seek(f, 0, "start")
  version <- read_cs_string(f)
  version <- match.arg(version,
                       choices = c("8", "81", "30", "130", "179", "181"))
  offsets <- get_agilent_offsets(version)
  if (version == "179"){
    seek(f, 347)
    filetype <- substr(read_cs_string(f, type = 2), 1, 2)
    if (filetype == "OL"){
      bytes = "8b"
    } else if (filetype == "GC"){
      seek(f, offsets$software)
      soft <- read_cs_string(f, type = 2)
      chemstation_version <- strsplit(soft, " ")[[1]][1]
      bytes <- ifelse(chemstation_version == "Mustang", "8b", "4b")
    }
    version <- paste(version, bytes, sep = "_")
  }

  decoder <- switch(version,
                    "8" = decode_delta,
                    "81" = decode_double_delta,
                    "30" = decode_delta,
                    "130" = decode_delta,
                    "181" = decode_double_delta,
                    "179_4b" = decode_double_array_4byte,
                    "179_8b" = decode_double_array_8byte)

  seek(f, 264, "start")
  offset <- (readBin(f, "integer", n = 1, endian = "big", size = 4) - 1) * 512

  data <- decoder(f, offset)

  seek(f, where = 282, origin = "start")
  if (version %in% c("8", "30", "130")){
    xmin <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE,
                              endian = "big")) / 60000
    xmax <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE,
                              endian = "big")) / 60000
  } else {
    xmin <- readBin(f, "numeric", n = 1, endian = "big", size = 4) / 60000
    xmax <- readBin(f, "numeric", n = 1, endian = "big", size = 4) / 60000
  }
    times <- seq(xmin, xmax, length.out = length(data))

    seek(f, offsets$intercept, "start")
    intercept <- readBin(f, "double", n = 1, endian = "big", size = 8)
    if (is.na(intercept)){
      intercept <- 0
    }

    seek(f, offsets$scaling_factor, "start")
    scaling_factor <- readBin(f, "double", n = 1, endian = "big", size = 8)

    if (version == "8"){
      seek(f, offsets$scaling_toggle, "start")
      st <- readBin(f, "integer", n = 1, size = 4, endian = "big")
      scaling_factor <- ifelse(st %in% c(1,2,3), 1.33321110047553, scaling_factor)
    }

    if (scale){
      data <- data * scaling_factor + intercept
    }
    data <- format_2d_chromatogram(rt = times, int = data,
                                   data_format = data_format,
                                   format_out = format_out)

    if (read_metadata){
      meta <- read_chemstation_string_fields(f, offsets,
                type = ifelse(version %in% c("8", "30", "81"), 1, 2))
    meta$intensity_multiplier <- scaling_factor
    meta$time_range <- c(xmin, xmax)

    metadata_from_file <- try(read_chemstation_metadata(path), silent = TRUE)
    if (!inherits(metadata_from_file, "try-error")){
      meta <- c(meta, metadata_from_file)
    }
    datetime_regex <- "(\\d{2}-[A-Za-z]{3}-\\d{2}, \\d{2}:\\d{2}:\\d{2})|(\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} (?:AM|PM)?)"
    meta$date <- regmatches(meta$date, gregexpr(datetime_regex, meta$date))[[1]]
    data <- attach_metadata(data, meta, format_in = metadata_format,
                            data_format = data_format, format_out = format_out,
                            parser = "chromconverter", source_file = source_file,
                            source_file_format = paste0("chemstation_", version),
                            scale = scale)
  }
  data
}

#' Decode double delta array
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
#' @noRd
decode_double_delta <- function(file, offset){
  seek(file, 0, "end")
  fsize <- seek(file, NA, "current")
  seek(file, offset, "start")

  rw <- readBin(file, "raw", n = fsize - offset)
  n16 <- length(rw) %/% 2L
  v <- readBin(rw, "integer", n = n16, size = 2, signed = TRUE, endian = "big")

  esc <- resolve_escape_positions(v, 32767L, 3L)
  esc <- esc[esc + 3L <= n16]

  keep <- rep(TRUE, n16)
  if (length(esc)) keep[as.vector(outer(1:3, esc, "+"))] <- FALSE
  pos <- which(keep)

  is_esc <- pos %in% esc
  d <- as.numeric(v[pos])
  d[is_esc] <- 0

  absv <- if (length(esc)){
    v[esc + 1L] * 4294967296 +
      readBin(rw[as.vector(outer(3:6, esc * 2L, "+"))], "integer",
              n = length(esc), size = 4, endian = "big")
  } else numeric(0)

  b2 <- cumsum_with_resets(d, is_esc, rep(0, length(esc)))
  cumsum_with_resets(b2, is_esc, absv)
}

#' Decode double array
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
#' @noRd
decode_double_array_4byte <- function(file, offset){
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  offset <- 6144
  # Read data
  seek(file, offset, "start")
  signal <- readBin(file, what = "double", size = 4, endian = "little",
                    n = (fsize - offset) %/% 4L)
  signal <- signal[seq(2, length(signal), 2)]
  return(signal)
}

#' Decode double array
#' @noRd
decode_double_array_8byte <- function(file, offset){
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  offset <- 6144
  # Read data
  seek(file, offset, "start")
  signal <- readBin(file, what = "double", size = 8, endian = "little",
                    n = (fsize - offset) %/% 8L)
  return(signal)
}

#' Decode delta array
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
#' @noRd
decode_delta <- function(file, offset){
  seek(file, 0, "end")
  fsize <- seek(file, NA, "current")
  seek(file, offset, "start")

  rw <- readBin(file, "raw", n = fsize - offset)
  n16 <- length(rw) %/% 2L
  v <- readBin(rw, "integer", n = n16, size = 2, signed = TRUE, endian = "big")

  signal <- numeric(n16)
  index <- 1L
  s <- 1L
  acc <- 0

  while (s <= n16){
    h <- v[s]
    if (is.na(h) || h < 0x1000L || h > 0x10FFL) break
    len <- h - 0x1000L
    if (len == 0L){
      s <- s + 1L
      next
    }
    sl <- v[(s + 1L):min(n16, s + 3L * len)]
    esc <- which(sl == -32768L)
    if (!length(esc) || esc[1L] > len){
      vals <- acc + cumsum(sl[seq_len(len)])
      consumed <- len
    } else {
      vals <- numeric(len)
      j <- 1L
      for (i in seq_len(len)){
        if (sl[j] == -32768L){
          acc <- readBin(rw[(s * 2L + j * 2L + 1L):(s * 2L + j * 2L + 4L)],
                         "integer", n = 1, size = 4, endian = "big")
          j <- j + 3L
        } else {
          acc <- acc + sl[j]
          j <- j + 1L
        }
        vals[i] <- acc
      }
      consumed <- j - 1L
    }
    acc <- vals[len]
    signal[index:(index + len - 1L)] <- vals
    index <- index + len
    s <- s + 1L + consumed
  }
  signal[seq_len(index - 1L)]
}

#' Read 'ChemStation' IT file
#' @noRd
read_chemstation_it <- function(path, format_out = c("matrix", "data.frame",
                                                     "data.table"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                scale = TRUE, source_file = NULL){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "chemstation")
  source_file <- ifelse(is.null(source_file), path, source_file)

  f <- file(path, "rb")
  on.exit(close(f))

  # HEADER
  seek(f, 0, "start")
  version <- read_cs_string(f)
  if (version != "179"){
    stop("The parser currently only supports `.IT` files in the version 179 format.")
  }
  offsets <- get_agilent_offsets(version)

  seek(f, 347)
  filetype <- substr(read_cs_string(f, type = 2), 1, 2)

  if (filetype != "OL"){
    stop("The parser currently only supports `.IT` files from OpenLab CDS.")
  }

  decoder <- decode_double_array_8byte

  seek(f, 264, "start")
  offset <- (readBin(f, "integer", n = 1, endian = "big", size = 4) - 1) * 512

  data <- decoder(f, offset)
  data <- split(data, seq_along(data) %% 2)
  vals <- data[[1]]
  rt <- data[[2]]/60000

  seek(f, offsets$intercept, "start")
  intercept <- readBin(f, "double", n = 1, endian = "big", size = 8)
  if (is.na(intercept)){
    intercept <- 0
  }

  seek(f, offsets$scaling_factor, "start")
  scaling_factor <- readBin(f, "double", n = 1, endian = "big", size = 8)

  if (scale){
    vals <- vals * scaling_factor + intercept
  }
  data <- format_2d_chromatogram(rt = rt, int = vals,
                                 data_format = data_format,
                                 format_out = format_out)

  if (read_metadata){
    meta <- read_chemstation_string_fields(f, offsets)
    meta$time_range <- c(head(rt, 1), tail(rt, 1))
    # the Latin-1 degree sign in `units` is now re-encoded by
    # `clean_vendor_string`, which `read_cs_string` applies to every field
    meta <- c(meta, intensity_multiplier = scaling_factor)
    datetime_regex <- "(\\d{2}-[A-Za-z]{3}-\\d{2}, \\d{2}:\\d{2}:\\d{2})|(\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} (?:AM|PM)?)"
    meta$date <- regmatches(meta$date, gregexpr(datetime_regex, meta$date))[[1]]
    data <- attach_metadata(data, meta, format_in = metadata_format,
                            data_format = data_format, format_out = format_out,
                            parser = "chromconverter", source_file = source_file,
                            source_file_format = paste0("chemstation_", version),
                            scale = scale)
  }
  data
}

