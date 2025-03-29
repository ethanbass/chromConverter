#' Read 'Agilent ChemStation' CH files
#'
#' Reads 'Agilent ChemStation' \code{.ch} files.
#'
#' 'Agilent' \code{.ch} files come in several different formats. This parser
#' can automatically detect and read several versions of these files from
#' 'Agilent ChemStation' and 'Agilent OpenLab', including versions \code{30} and
#' \code{130}, which are generally produced by ultraviolet detectors, as well as
#' \code{81}, \code{179}, and \code{181} which are generally produced by flame
#' ionization (FID) detectors.
#'
#' @importFrom bitops bitAnd bitShiftL
#' @param path Path to 'Agilent' \code{.ch} file.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to attach metadata. Defaults to \code{TRUE}.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param scale Whether to scale the data by the scaling factor present in the
#' file. Defaults to \code{TRUE}. 'MassHunter' seems to ignore the scaling
#' factor in at least some types of 'ChemStation' files.
#' @author Ethan Bass
#' @return A 2D chromatogram in the format specified by \code{data_format} and
#' \code{format_out}. If \code{data_format} is \code{wide}, the chromatogram will
#' be returned with retention times as rows and a single column for the intensity.
#' If \code{long} format is requested, two columns will be returned: one for the
#' retention time and one for the intensity. The \code{format_out} argument
#' determines whether the chromatogram is returned as a \code{matrix},
#' \code{data.frame}, or \code{data.table}. Metadata can be attached to the
#' chromatogram as \code{\link{attributes}} if \code{read_metadata} is \code{TRUE}.
#' @note This function was adapted from the
#' \href{https://github.com/chemplexity/chromatography}{Chromatography Toolbox}
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
                                scale = TRUE){
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chemstation",
                            raw = "raw")

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
    meta <- c(meta, intensity_multiplier = scaling_factor)
    metadata_from_file <- try(read_chemstation_metadata(path), silent = TRUE)
    if (!inherits(metadata_from_file, "try-error")){
      meta <- c(meta, metadata_from_file)
    }
    datetime_regex <- "(\\d{2}-[A-Za-z]{3}-\\d{2}, \\d{2}:\\d{2}:\\d{2})|(\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} (?:AM|PM)?)"
    meta$date <- regmatches(meta$date, gregexpr(datetime_regex, meta$date))[[1]]
    data <- attach_metadata(data, meta, format_in = metadata_format,
                            data_format = data_format, format_out = format_out,
                            parser = "chromconverter", source_file = path,
                            source_file_format = paste0("chemstation_", version),
                            scale = scale)
  }
  data
}

#' Read ChemStation string
#' @noRd
read_cs_string <- function(f, type = 1, pos = NULL){
  if (!is.null(pos)){
    seek(f, where = pos, origin = "start")
  }
  n <- get_nchar(f)
  if (type == 1){
    tryCatch(rawToChar(readBin(f, what = "raw", n = n)), error = function(e) NA)
  } else if (type == 2){
    tryCatch(rawToChar(readBin(f, what = "raw", n = n*2)[c(TRUE, FALSE)]),
             error = function(e) NA)
  }
}

#' @noRd
cc_collapse <- function(x){
  paste(x, collapse="")
}

#' @noRd
cc_trim_str <- function(x, len = 2){
  substr(x, len, nchar(x))
}

#' Find .D folder
#' @noRd
get_chemstation_dir_name <- function(path){
  dir <- gsub(basename(path), "", path)
  sp <- str_split_fixed(dir, "/", stringr::str_count(dir, "/") + 1)[1,]
  grep("\\.D|\\.d$", sp, ignore.case = TRUE, value = TRUE)
}

#' Get number of characters for Agilent segment
#' @noRd
get_nchar <- function(f){
  as.numeric(readBin(f, what = "raw", n = 1))
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

#' Get Agilent offsets
#' @noRd
get_agilent_offsets <- function(version){
  if (version == "131_LC"){
    offsets <- list(version = 326,
                     file_type = 347,
                     sample_name = 858,
                     operator = 1880,
                     date = 2391,
                     detector_model = 2492,
                     method = 2574,
                     software = 3089,
                     units = 3093,
                     sample_id = 4055,
                     num_times = 278, #big-endian
                     rt_first = 282,
                     rt_last = 286,
                     scaling_factor = 3085,
                     data_start = 4096
                    )
  } else if (version == "131_OL"){
    offsets <- list(version = 326,
                    file_type = 347,
                    sample_name = 858,
                    operator = 1880,
                    date = 2391,
                    # detector = 2492,
                    method = 2574,
                    # software = 3089,
                    units = 3093,
                    sample_id = 4055,
                    num_times = 278, #big-endian
                    rt_first = 282,
                    rt_last = 286,
                    scaling_factor = 3085,
                    data_start = 4096
                  )
  } else if (version == "31"){
    offsets <- list(version = 0,
              file_type = 4,
              sample_name = 24,
              operator = 148,
              date = 178,
              detector_model = 208,
              instrument = 218,
              method = 228,
              # unknown = 260,
              num_times = 278, # big-endian
              scaling_factor = 318,
              units = 326,
              data_start = 512
            )
  } else if (version == "2"){
    offsets <- list(version = 0,
              file_type = 4,
              sample_name = 40,
              operator = 148,
              date = 178,
              detector_model = 208,
              instrument = 218,
              method = 228,
              # unknown = 260,
              signal = 326,
              header_length = 266,
              num_times = 280, # big-endian
              start_time = 282, #big-endian, 4 bytes
              end_time = 286 #big-endian, 4 bytes
              # scaling_factor = 318,
            )
  } else if (version %in% c("179","179_4b", "179_8b", "181")){
    offsets <- list(
      version = 326,
      file_type = 347, #0x15B
      sample_name = 858, #0x35A
      operator = 1880, #0x758
      date = 2391, # 0x957
      instrument = 2492, # 0x9BC
      method = 2574, # 0xA0E
      software = 3089, # 0xC11
      units = 4172, # 0x104C
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
      # start_time = 282,
      # end_time = 286,
      version = 326, # utf16
      file_type = 347, # utf16
      sample_name = 858, # utf16
      operator = 1880, # utf16
      date = 2391, # utf16
      inlet = 2492, # utf16
      instrument = 2533, # utf16'
      method = 2574, # utf16
      software = 3089, # 'utf16'
      software_version = 3601, #utf16'
      software_revision = 3802, #'utf16'
      sample_id = 4054,
      units = 4172, # 'utf16'
      signal = 4213, # 'utf16'
      intercept = 4110, # INT32
      scaling_factor = 4732) #ENDIAN + 'd'
  } else if (version == 30){
    offsets <- list(
      version = 0,
      file_type = 4, # utf16
      sample_name = 24, # utf16
      operator = 148, # utf16
      date = 178, # utf16
      detector_model = 208, # utf16'
      instrument = 218,
      method = 228, # utf16
      software = 322, # 'utf16'
      software_version = 355, #utf16'
      software_revision = 405, #'utf16'
      units = 580, # 'utf16'
      signal = 596, # 'utf16'
      intercept = 636, # INT32
      scaling_factor = 644,
      data_start = 1024 #ENDIAN + 'd'
    )
  } else if (version %in% c("8", "81")){
    offsets <- list(version = 0,
                    file_type = 4,
                    sample_name = 24,
                    description = 86,
                    operator = 148,
                    date = 178,
                    detector_model = 208,
                    instrument = 218,
                    method = 228,
                    units = 580,
                    num_times = 278,
                    rt_first = 282,
                    rt_last = 286,
                    scaling_toggle = 542,
                    scaling_factor = 644,
                    intercept = 636,
                    data_start = switch(version, "8" = 1024, "81" = 4096)
    )
  }
  offsets
}

#' Read 'Agilent' DX files
#'
#' Reads 'Agilent' \code{.dx} files.
#'
#' This function unzips 'Agilent'  \code{.dx} into a temporary directory using
#' \code{\link{unzip}} and calls \code{\link{read_chemstation_ch}}.
#'
#' @importFrom utils unzip
#' @param path Path to \code{.dx} file.
#' @param path_out Path to directory to export unzipped files.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to attach metadata.
#' @author Ethan Bass
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_agilent_dx <-  function(path, path_out = NULL,
                             format_out = c("matrix", "data.frame", "data.table"),
                              data_format = c("wide","long"),
                              read_metadata = TRUE){
    format_out <- check_format_out(format_out)
    data_format <- match.arg(data_format, c("wide","long"))
    files <- unzip(path, list = TRUE)
    files.idx <- grep(".ch$", files$Name, ignore.case = TRUE)
    # make temp directory
    if (is.null(path_out)){
      path_out <- tempdir()
      on.exit(unlink(path_out), add = TRUE)
    }
    # copy .dx file to directory
    file.copy(path, path_out)
    path <- fs::path(path_out, basename(path))
    # unzip .dx file
    unzip(path, files = files$Name[files.idx], exdir = path_out)
    # read in `.ch` files
    read_chemstation_ch(fs::path(path_out, files$Name[files.idx]),
                        format_out = format_out, data_format = data_format,
                        read_metadata = read_metadata)
}
