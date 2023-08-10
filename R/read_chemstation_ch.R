#' Parser for reading Agilent ('.ch') files into R
#' @importFrom bitops bitAnd bitShiftL
#' @param path Path to \code{.ch} file
#' @param format_out Matrix or data.frame.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to attach metadata.
#' @author Ethan Bass
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
#' @export

read_chemstation_ch <- function(path, format_out = c("matrix","data.frame"),
                                data_format = c("wide","long"),
                                read_metadata = TRUE){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))

  f <- file(path, "rb")
  on.exit(close(f))

  # HEADER
  seek(f, 1, "start")
  version <- readBin(f, "character", n = 1)
  version <- match.arg(version, choices = c("8", "81", "30", "130", "179", "181"))
  offsets <- get_agilent_offsets(version)
  decoder <- switch(version,
                    "8" = decode_delta,
                    "81" = decode_double_delta,
                    "30" = decode_delta,
                    "130" = decode_delta,
                    "181" = decode_double_delta,
                    "179" = decode_double_array)

  # Sample Info
  # offsets <- list(sample = 858, description = 1369, method = 2574,
  #                 operator = 1880, date = 2391, instrument = 2533,
  #                inlet = 2492, units = 4172)

  seek(f, 264, "start")
  offset <- (readBin(f, "integer", n = 1, endian = "big", size = 4) - 1) * 512

  data <- decoder(f, offset)

  seek(f, where = 282, origin = "start")
  seek(f, where = 282, origin = "start")

  if (version %in% c("8", "30", "130")){
    xmin <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE, endian = "big")) / 60000
    xmax <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE, endian = "big")) / 60000
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

    data <- data * scaling_factor + intercept

    if (data_format == "wide"){
      data <- data.frame(Intensity = data, row.names = times)
    } else if (data_format == "long"){
      data <- data.frame(RT = times, Intensity = data)
    }
    if (format_out == "matrix"){
      data <- as.matrix(data)
    }
    if (read_metadata){
      meta_slots <- switch(version, "8" = 9,
                                    "81" = 9,
                                    "30" = 11,
                                    "130" = 12,
                                    "181" = 9,
                                    "179" = 9)

    meta <- lapply(offsets[seq_len(meta_slots)], function(offset){
      seek(f, where = offset, origin = "start")
      n <- get_nchar(f)
      if (version == "30"){
        readBin(f, what = "character")
      } else{
        cc_collapse(readBin(f, "character", n = n))
      }
    })
  if (read_metadata){
    datetime_regex <- "(\\d{2}-[A-Za-z]{3}-\\d{2}, \\d{2}:\\d{2}:\\d{2})|(\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} (?:AM|PM)?)"
    datetime <- regmatches(meta$date, gregexpr(datetime_regex, meta$date))[[1]]
    datetime_formats <- c("%d-%b-%y, %H:%M:%S", "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %I:%M:%S %p")
    datetime <- as.POSIXct(datetime, tz = "UTC", tryFormats = datetime_formats)
    data <- structure(data, file_version = version, sample_name = meta$sample_name,
                      run_date = datetime,
                      instrument = meta$instrument,
                      method = meta$method, software_version = meta$software_version,
                      software = meta$software, software_rev = meta$software_revision,
                      signal = meta$signal, detector_unit = meta$unit,
                      time_range = c(xmin, xmax), time_interval = mean(diff(times)),
                      time_unit = "Minutes", source_file = path,
                      data_format = data_format, parser = "chromConverter")
  }
  }
  data
}

#' @noRd
cc_collapse <- function(x){
  paste(x, collapse="")
}

#' @noRd
cc_trim_str <- function(x, len=2){
  substr(x, len, nchar(x))
}

#' Find .D folder
#' @noRd
get_chemstation_dir_name <- function(path){
  dir <- gsub(basename(path), "", path)
  sp <- str_split_fixed(dir, "/", stringr::str_count(dir,"/")+1)[1,]
  grep("\\.D|\\.d$", sp, ignore.case = TRUE,value = TRUE)
}

#' @noRd
get_nchar <- function(f){
  as.numeric(readBin(f, what = "raw", n = 1))
}

#' Decode double delta array
#' @noRd
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
decode_double_delta <- function(file, offset) {

  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")

  # Read data

  seek(file, offset, "start")
  seek(file, offset, "start")

  signal <- numeric(fsize/2)
  count <- 1
  buffer <- numeric(3)

  while (seek(file, NA, "current") < fsize) {
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
#' @noRd
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
decode_double_array <- function(file, offset) {
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  offset <- 0x1800
  # Read data
  seek(file, offset, "start")
  signal <- readBin(file, what = "double", size = 4, endian = "little",
                    n = (fsize - offset))
  signal <- signal[seq(2, length(signal), 2)]
  return(signal)
}

#' Decode delta array
#' @noRd
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
#' ((c) James Dillon 2014).
decode_delta <- function(file, offset) {
    seek(file, 0, 'end')
    fsize <- seek(file, NA, "current")

    seek(file, offset, "start")
    start <- seek(file, NA, "current")

  signal <- rep(NA, round((fsize - start)/2))
  buffer <- rep(0, 4)
  index <- 1

    while (TRUE) {
      head <- readBin(file, "integer", n = 1, size = 1, endian = "big")
      if (head != 0x10) {
        break
      }
      buffer[2] <- buffer[4]

      segment_length <- readBin(file, "integer", n = 1, size = 1, endian = "big")
      for (i in seq_len(segment_length)){
      # for (i in (1:bitwAnd(buffer[1], 4095L))) {
        buffer[3] <- readBin(file, "integer", n = 1, size = 2, endian = "big")
        if (buffer[3] != -32768L) {
          buffer[2] <- buffer[2] + buffer[3]
        } else {
          buffer[2] <- readBin(file, "integer", n = 1, size =4 ,endian = "big")
        }

        signal[index] <- buffer[2]
        index <- index + 1
      }
      buffer[4] <- buffer[2]
  }
  signal <- signal[!is.na(signal)]
  return(signal)
}

#' Get Agilent offsets
#' @noRd
get_agilent_offsets <- function(version){
  if (version %in% c("179","181")){
    offsets <- list(
      file_type = 347, #0x15B
      sample_name = 858, #0x35A
      operator = 1880, #0x758
      date = 2391, # 0x957
      instrument = 2492, # 0x9BC
      method = 2574, # 0xA0E
      software = 3089, # 0xC11
      unit = 4172, # 0x104C
      signal = 4213, # 0x1075
      num_times = 278, # 0x116
      rt_first = 282, # 0x11A
      rt_last = 286, # 0x11E
      scaling_factor = 4732, # 0x127C
      intercept = 4724,
      data_start = 4096 # 0x1000
    )
  } else if (version == "130"){
    offsets <- list(
      # sequence_line_or_injection = 252, #UINT16
      # injection_or_sequence_line = 256, #UINT16
      # data_offset = 264, # UINT32
      start_time = 282,
      # end_time = 286,
      # version_string = 326, # utf16
      file_type = 347, # utf16
      sample_name = 858, # utf16
      operator = 1880, # utf16
      date = 2391, # utf16
      inlet = 2492, # utf16
      instrument = 2533, # utf16'
      method = 2574, # utf16
      software_version = 3601, #utf16'
      software = 3089, # 'utf16'
      software_revision = 3802, #'utf16'
      units = 4172, # 'utf16'
      signal = 4213, # 'utf16'
      intercept = 4110, # INT32
      scaling_factor = 4732) #ENDIAN + 'd'
  } else if (version == 30){
    offsets <- list(
      file_type = 4, # utf16
      sample_name = 24, # utf16
      operator = 148, # utf16
      date = 178, # utf16
      # inlet = 2492, # utf16
      instrument = 208, # utf16'
      method = 228, # utf16
      software_version = 355, #utf16'
      software = 322, # 'utf16'
      software_revision = 405, #'utf16'
      units = 580, # 'utf16'
      signal = 596, # 'utf16'
      intercept = 636, # INT32
      scaling_factor = 644,
      data_start = 1024 #ENDIAN + 'd'
    )
  } else if (version %in% c("8","81")){
    offsets <- list(sample_name = 24,
                    description = 86,
                    operator = 148,
                    date = 178,
                    instrument = 218,
                    inlet = 208,
                    method = 228,
                    # software = 0xC11,
                    unit = 580,
                    num_times = 0x116,
                    rt_first = 0x11A,
                    rt_last = 0x11E,
                    scaling_factor = 644,
                    intercept = 636,
                    data_start = 4096)
  }
  offsets
}
