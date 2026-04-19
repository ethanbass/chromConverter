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
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chemstation",
                            raw = "raw")
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
      meta_slots <- switch(version, "8" = 10,
                                    "81" = 10,
                                    "30" = 13,
                                    "130" = 14,
                                    "179_4b" = 10,
                                    "179_8b" = 10,
                                    "181" = 10)

      meta <- lapply(offsets[seq_len(meta_slots)], function(offset){
        seek(f, where = offset, origin = "start")
        if (version %in% c("8", "30", "81")){
          read_cs_string(f, type = 1)
        } else{
          read_cs_string(f, type = 2)
        }
      })
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
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")

  # Read data

  seek(file, offset, "start")
  seek(file, offset, "start")

  signal <- numeric(fsize/2)
  count <- 1
  buffer <- numeric(3)

  while (seek(file, NA, "current") < fsize){
    buffer[3] <- readBin(file, "integer", n = 1, endian = "big", size = 2)

    if (buffer[3] != 32767) {
      buffer[2] <- buffer[2] + buffer[3]
      buffer[1] <- buffer[1] + buffer[2]
    } else {
      buffer[1] <- readBin(file, "integer", n=1, endian = "big", size = 2) * 4294967296
      buffer[1] <- readBin(file, "integer", n=1, endian = "big", size = 4) + buffer[1]
      buffer[2] <- 0
    }

    signal[count] <- buffer[1]
    count <- count + 1
  }

  signal <- signal[1:(count - 1)]
  return(signal)
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
                    n = (fsize - offset))
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
                    n = (fsize - offset))
  return(signal)
}

#' Decode delta array
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
#' @noRd
decode_delta <- function(file, offset){
    seek(file, 0, 'end')
    fsize <- seek(file, NA, "current")

    seek(file, offset, "start")
    start <- seek(file, NA, "current")

  signal <- rep(NA, round((fsize - start)/2))
  buffer <- rep(0, 4)
  index <- 1

    while (TRUE){
      head <- readBin(file, "integer", n = 1, size = 1, endian = "big")
      if (head != 0x10) {
        break
      }
      buffer[2] <- buffer[4]

      segment_length <- readBin(file, "integer", n = 1, size = 1, endian = "big")
      for (i in seq_len(segment_length)){
        buffer[3] <- readBin(file, "integer", n = 1, size = 2, endian = "big")
        if (buffer[3] != -32768L) {
          buffer[2] <- buffer[2] + buffer[3]
        } else {
          buffer[2] <- readBin(file, "integer", n = 1, size = 4 ,endian = "big")
        }

        signal[index] <- buffer[2]
        index <- index + 1
      }
      buffer[4] <- buffer[2]
  }
  signal <- signal[!is.na(signal)]
  return(signal)
}

#' Read Chemstation IT file
#' @noRd
read_chemstation_it <- function(path, format_out = c("matrix", "data.frame",
                                                     "data.table"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                scale = TRUE, source_file = NULL){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chemstation",
                            raw = "raw")
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
    meta <- lapply(offsets[seq_len(10)], function(offset){
      seek(f, where = offset, origin = "start")
        read_cs_string(f, type = 2)
    })
    meta$time_range <- c(head(rt, 1), tail(rt, 1))
    meta$units <- gsub("\xb0", "\u00b0", meta$units, useBytes = TRUE)
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

