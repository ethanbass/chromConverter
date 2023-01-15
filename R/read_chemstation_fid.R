#' Parser for reading Agilent FID (.ch) files into R
#' @param path Path to \code{.ch} file
#' @param read_metadata Logical. Whether to attach metadata.
#' @param format_out Matrix or data.frame
#' @author Ethan Bass
#' @note This function was adapted from the \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox} ((c) James Dillon 2014).
#' @export
read_chemstation_fid <- function(path, read_metadata = TRUE,
                                 format_out = c("matrix","data.frame")){
  format_out <- match.arg(format_out, c("matrix","data.frame"))

  f <- file(path, "rb")
  on.exit(close(f))

  # HEADER
  seek(f, 1, "start")
  version <- readBin(f, "character", n = 1)
  version <- match.arg(version, choices = c("8", "81", "179","181"))
  # endian <- switch(version,
  #                  "179" = "little",
  #                  "180" = "big",
  #                  "181" = "big")
  if (version %in% c("179","181")){
    offsets <- list(file_type = 0x15B,
                    sample_name = 0x35A,
                    operator = 0x758,
                    date = 0x957,
                    instrument = 0x9BC,
                    method = 0xA0E,
                    software = 0xC11,
                    unit = 0x104C,
                    signal = 0x1075,
                    num_times = 0x116,
                    rt_first = 0x11A,
                    rt_last = 0x11E,
                    scaling_factor = 0x127C,
                    intercept = 4724,
                    data_start = 0x1000)
  } else if (version %in% c("8","81")){
    offsets <- list(sample_name = 24,
                    description = 86,
                    operator = 148,
                    date = 178,
                    instrument = 218,
                    inlet = 208,
                    method = 228,
                    software = 0xC11,
                    unit = 580,
                    num_times = 0x116,
                    rt_first = 0x11A,
                    rt_last = 0x11E,
                    scaling_factor = 644,
                    intercept = 636,
                    data_start = 0x1000)
  }

  decoder <- switch(version,
                    "8" = decode_delta,
                    "81" = decode_double_delta,
                    "181" = decode_double_delta,
                    "179" = decode_double_array)

  # Sample Info
  # offsets <- list(sample = 858, description = 1369, method = 2574,
  #                 operator = 1880, date = 2391, instrument = 2533,
  #                inlet = 2492, units = 4172)

  seek(f, 264, "start")
  offset <- (readBin(f, "integer", n = 1, endian = "big", size = 4) - 1) * 512

  # data <- FileInfo(file, data, options)
  data <- decoder(f, offset)

  seek(f, 282, "start")
  seek(f, 282, "start")
  # seek(f, where = 0x11A, origin="start")
  if (version == "8"){
    xmin <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE, endian = "big")) / 60000
    xmax <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE, endian = "big")) / 60000
  } else {
    xmin <- readBin(f, "numeric", n=1, endian = "big", size=4) / 60000
    xmax <- readBin(f, "numeric", n=1, endian = "big", size=4) / 60000

    times <- seq(xmin, xmax, length.out = length(data))

    seek(f, offsets$intercept, "start")
    intercept <- readBin(f, "double", n=1, endian = "big", size = 8)

    seek(f, offsets$scaling_factor, "start")
    scaling_factor <- readBin(f, "double", n=1, endian = "big", size = 8)

    data <- data * scaling_factor + intercept
  }
  data <- data.frame(Intensity=data,row.names=times)
  if (format_out == "matrix"){
    data <- as.matrix(data)
  }
  if (read_metadata){
    meta <- lapply(offsets[1:9], function(offset){
      seek(f, where = offset, origin = "start")
      n <- get_nchar(f)
      cc_collapse(readBin(f, "character", n = n))
    })
    data <- structure(data, version = version, sample_name = meta$sample_name,
                      run_date = meta$date, instrument = meta$instrument, method = meta$method,
                      software = meta$software, unit = meta$unit, signal=meta$signal,
                      time_range = c(xmin, xmax),
                      data_format = "long", parser="chromConverter")
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

#' @noRd
# check for .D folder
get_chemstation_dir_name <- function(path){
  dir <- gsub(basename(path), "", path)
  sp <- str_split_fixed(dir, "/", stringr::str_count(dir,"/")+1)[1,]
  grep("\\.D|\\.d$", sp, ignore.case = TRUE,value = TRUE)
}

#' @noRd
get_nchar <- function(f){
  as.numeric(readBin(f, what = "raw", n = 1))*2
}

#' @noRd
#' @note This function was adapted from the \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox} ((c) James Dillon 2014).

decode_double_delta <- function(file, offset) {

  seek(file, 0, 'end');
  fsize = seek(file, NA, "current");

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

#' @noRd
#' @note This function was adapted from the \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox} ((c) James Dillon 2014).

decode_double_array <- function(file, offset) {

  seek(file, 0, 'end');
  fsize = seek(file, NA, "current");

  # Read data
  seek(file, offset, "start")
  # signal <- readBin(file, (fsize - offset) / 8, "double", "l")
  signal <- readBin(file, what = "double", size = 4, endian = "little",
                    n = (fsize - offset)/8)
  signal <- signal[seq(2,length(signal),2)]
  return(signal)
}

#' @noRd
#' @note This function was adapted from the \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox} ((c) James Dillon 2014).

# decode_delta <- function(file, offset) {
#   if (ftell(file) == -1) {
#     signal <- c()
#     return(signal)
#   } else {
#     fseek(file, 0, "start")
#     stop <- ftell(file)
#
#     fseek(file, offset, "end")
#     start <- ftell(file)
#   }
#
#   signal <- rep(0, round((stop - start)/2))
#   buffer <- rep(0, 4)
#   index <- 1
#
#   while (seek(file, NA, "current") < fsize) {
#     buffer[1] <- readBin(file, "integer", n=1, endian = "big", size = 2)
#     buffer[2] <- buffer[4]
#
#     if (bitshift(buffer[1], 12, "int16") == 0) {
#       signal <- signal[1:index-1]
#       break
#     }
#
#     for (i in 1:bitand(buffer[1], 4095, "int16")) {
#       buffer[3] <- readBin(file, "integer", n=1, endian = "big", size = 2)
#
#       if (buffer[3] != -32768) {
#         buffer[2] <- buffer[2] + buffer[3]
#       } else {
#         buffer[2] <- readBin(file, "integer", n = 1, endian = "big", size = 4)
#       }
#
#       signal[index] <- buffer[2]
#       index <- index + 1
#     }
#
#     buffer[4] <- buffer[2]
#   }
#
#   return(signal)
# }
decode_delta <-function(){
  stop("Unfortunately, version 8 FID files are not yet supported.")
}
