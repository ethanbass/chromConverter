#' Shimadzu LCD parser
#'
#' Read 3D PDA or 2D chromatogram streams from 'Shimadzu' LCD files.
#'
#' A parser to read data from 'Shimadzu' \code{.lcd} files. LCD files are
#' encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The PDA data is encoded in a stream called \code{PDA 3D Raw Data:3D Raw Data}.
#' The PDA data stream contains a segment for each retention time, beginning
#' with a 24-byte header.
#'
#' The 24 byte header consists of the following fields:
#' * 4 bytes: segment label (\code{17234}).
#' * 4 bytes: Little-endian integer specifying the sampling rate along the time
#' axis for 2D streams or along the spectral axis (?) for PDA streams.
#' * 4 bytes: Little-endian integer specifying the number of values in the file
#' (for 2D data) or the number of wavelength values in each segment (for 3D data).
#' * 4 bytes: Little-endian integer specifying the total number of bytes in the segment.
#' * 8 bytes of \code{00}.
#'
#' For 3D data, Each time point is divided into two sub-segments, which begin and
#' end with an integer specifying the length of the sub-segment in bytes. 2D data
#' are structured similarly but with more segments. All known values
#' in this the LCD data streams are little-endian and the data are delta-encoded.
#' The first hexadecimal digit of each value is a sign digit specifying the
#' number of bytes in the delta and whether the value is positive
#' or negative. The sign digit represents the number of hexadecimal digits used
#' to encode each value. Even numbered sign digits correspond to positive deltas,
#' whereas odd numbers indicate negative deltas. Positive values are encoded as
#' little-endian integers, while negative values are encoded as two's
#' complements. The value at each position is derived by subtracting the delta
#' at each position from the previous value.
#'
#' @param path Path to LCD file.
#' @param what What stream to get: current options are \code{pda},
#' \code{chromatogram}, or \code{tic}. If a stream is not specified,
#' the function will default to \code{pda} if the PDA stream is present.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param scale Whether to scale the data by the value factor. Defaults to
#' \code{TRUE}.
#' @author Ethan Bass
#' @return A 3D chromatogram from the PDA stream in \code{matrix} or
#' \code{data.frame} format, according to the value of \code{format_out}.
#' The chromatograms will be returned in \code{wide} or \code{long} format
#' according to the value of \code{data_format}.
#' @note My parsing of the date-time format seems to be a little off, since
#' the acquisition times diverge slightly from the ASCII file.
#' @export

read_shimadzu_lcd <- function(path, what, format_out = c("matrix", "data.frame"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                scale = TRUE){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "shimadzu_lcd", raw = "raw")

  if (missing(what)){
    what <- ifelse(check_streams(path, "pda", boolean = TRUE),
                   "pda", "chromatogram")
  }
  what <- match.arg(what, c("pda", "chromatogram", "tic"))

  olefile_installed <- reticulate::py_module_available("olefile")
  if (!olefile_installed){
    configure_python_environment(parser = "olefile")
  }

  read_sz <- switch(what, "pda" = read_sz_lcd_3d,
                          "chromatogram" = read_sz_lcd_2d,
                          "tic" = read_sz_tic)

  read_sz(path, format_out = format_out, data_format = data_format,
           read_metadata = read_metadata, metadata_format = metadata_format,
            scale = scale)
}

#' Shimadzu LCD 3D parser
#'
#' Read 3D PDA data stream from 'Shimadzu' LCD files.
#'
#' A parser to read PDA data from 'Shimadzu' \code{.lcd} files. LCD files are
#' encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The PDA data is encoded in a stream called \code{PDA 3D Raw Data:3D Raw Data}.
#' The PDA data stream contains a segment for each retention time, beginning
#' with a 24-byte header.
#'
#' The 24 byte header consists of the following fields:
#' * 4 bytes: segment label (\code{17234}).
#' * 4 bytes: Little-endian integer specifying the wavelength bandwidth (?).
#' * 4 bytes: Little-endian integer specifying the number of wavelength values
#' in the segment.
#' * 4 bytes: Little-endian integer specifying the total number of bytes in the segment.
#' * 8 bytes of \code{00}s
#'
#' Each segment is divided into two sub-segments, which begin and end with an
#' integer specifying the length of the sub-segment in bytes. All known values
#' in this data stream are little-endian and the data are delta-encoded. The
#' first hexadecimal digit of each value is a sign digit
#' specifying the number of bytes in the delta and whether the value is positive
#' or negative. The sign digit represents the number of hexadecimal digits used
#' to encode each value. Even numbered sign digits correspond to positive deltas,
#' whereas odd numbers indicate negative deltas. Positive values are encoded as
#' little-endian integers, while negative values are encoded as two's
#' complements. The value at each position is derived by subtracting the delta
#' at each position from the previous value.
#'
#' @param path Path to LCD file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param scale Whether to scale the data by the value factor.
#' @author Ethan Bass
#' @return A 3D chromatogram from the PDA stream in \code{matrix} or
#' \code{data.frame} format, according to the value of \code{format_out}.
#' The chromatograms will be returned in \code{wide} or \code{long} format
#' according to the value of \code{data_format}.
#' @export

read_sz_lcd_3d <- function(path, format_out = "matrix",
                            data_format = "wide",
                            read_metadata = TRUE,
                            metadata_format = "shimadzu_lcd",
                            scale = TRUE){
  check_streams(path, what = "pda")

  # read wavelengths from "Wavelength Table" stream
  lambdas <- read_sz_wavelengths(path)
  n_lambdas <- length(lambdas)

  # read data from "3D Raw Data" stream
  dat <- read_sz_pda(path, n_lambdas = n_lambdas)
  colnames(dat) <- lambdas

  DI <- read_sz_3DDI(path)
  times <- seq(DI$DLT, DI$AT, by = DI$Rate)
  if (length(times) != nrow(dat)){
    times <- seq(DI$DLT, DI$AT, length.out = nrow(dat))
    warning("Length of the inferred time axis does not match the number of rows
            in the data.")
  }
  if (inherits(times, "numeric")){
    rownames(dat) <- times
  }
  if (data_format == "long"){
    dat <- reshape_chrom(dat, data_format = "wide")
  }

  if (format_out == "data.frame"){
    dat <- as.data.frame(dat)
  }
  if (read_metadata){
    meta <- read_sz_file_properties(path)
    meta <- c(meta, DI)
    dat <- attach_metadata(dat,meta, format_in = metadata_format,
                           source_file = path, data_format = data_format,
                           format_out = format_out)
  }
  dat
}

#' Shimadzu LCD 2D parser
#'
#' Read 2D PDA data stream from 'Shimadzu' LCD files.
#'
#' A parser to read chromatogram data streams from 'Shimadzu' \code{.lcd} files.
#' LCD files are encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The chromatogram data is encoded in streams titled
#' \code{LSS Raw Data:Chromatogram Ch<#>}. The chromatogram data streams begin
#' with a 24-byte header.
#'
#' The 24 byte header consists of the following fields:
#' * 4 bytes: segment label (\code{17234}).
#' * 4 bytes: Little-endian integer specifying the sampling rate (in milliseconds).
#' * 4 bytes: Little-endian integer specifying the number of values
#' in the file.
#' * 4 bytes: Little-endian integer specifying the total number of bytes
#' in the file.
#' * 8 bytes of \code{00}s
#'
#' Each segment is divided into multiple sub-segments, which begin and end with an
#' integer specifying the length of the sub-segment in bytes. All known values
#' in this data stream are little-endian and the data are delta-encoded. The
#' first hexadecimal digit of each value is a sign digit
#' specifying the number of bytes in the delta and whether the value is positive
#' or negative. The sign digit represents the number of hexadecimal digits used
#' to encode each value. Even numbered sign digits correspond to positive deltas,
#' whereas odd numbers indicate negative deltas. Positive values are encoded as
#' little-endian integers, while negative values are encoded as two's
#' complements. The value at each position is derived by subtracting the delta
#' at each position from the previous value.
#'
#' @param path Path to LCD file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param scale Whether to scale the data by the value factor.
#' @author Ethan Bass
#' @return One or more 2D chromatograms from the chromatogram streams in
#' \code{matrix} or \code{data.frame} format, according to the value of
#' \code{format_out}. If multiple chromatograms are found, they will be returned
#' as a list of matrices or data.frames. The chromatograms will be returned in
#' \code{wide} or \code{long} format according to the value of \code{data_format}.
#' @export

read_sz_lcd_2d <- function(path, format_out = "matrix",
                            data_format = "wide",
                            read_metadata = TRUE,
                            metadata_format = "shimadzu_lcd",
                            scale = TRUE){
  if (data_format == "long"){
    format_out <- "data.frame"
  }
  existing_streams <- check_streams(path, what = "chromatogram")
  if (length(existing_streams) == 0){
    stop("Chromatogram streams not detected.")
  }

  if (read_metadata){
    meta <- read_sz_file_properties(path)
  }

  dat <- lapply(existing_streams, function(stream){
    dat <- read_sz_chrom(path, stream = stream)

    idx <- as.numeric(gsub("\\D", "", stream[2]))
    DI <- read_sz_2DDI(path, idx = idx)

    times <- seq(DI$DLT, DI$AT, length.out = nrow(dat))
    rownames(dat) <- times

    if (scale){
      dat <- dat*DI$detector.vf
    }
    if (data_format == "long"){
      dat <- data.frame(rt = times, int = dat$int, detector = DI$DETN,
                   channel = DI$DSCN, wavelength = DI$ADN,
                   unit = DI$detector.unit)
    }
    if (format_out == "matrix"){
      dat <- as.matrix(dat)
    }
    if (read_metadata){
      dat <- attach_metadata(dat, c(meta, DI), format_in = metadata_format,
                             source_file = path, data_format = data_format,
                             format_out = format_out, scale = scale)
    }
    dat
  })

  names(dat) <- sapply(dat, function(x){
    det <- gsub("Detector ", "", attr(x,"detector"))
    wv <- attr(x, "wavelength")
    ifelse(wv == "", det, paste(det, wv, sep = ", "))
  })

  if (data_format == "long"){
    dat <- do.call(rbind, c(dat, make.row.names = FALSE))
  }
  if (length(dat) == 1){
    dat <- dat[[1]]
  }
  dat
}

#' A parser to read total ion chromatogram data streams from 'Shimadzu'
#' \code{.lcd} files. LCD files are encoded as 'Microsoft' OLE documents. The
#' parser relies on the [olefile](https://pypi.org/project/olefile/) package in
#' Python to unpack the files. The TIC data is encoded in a stream called
#' \code{Centroid:3D Raw Data}.
#' The PDA data stream contains a segment for each retention time, beginning
#' with a 8-byte header. After the header, the file consists of a series of
#' 4-byte little-endian integers in blocks of 3 (16-bytes per block), followed by
#' a 4-byte spacer (\code{00000000}) The first integer is the retention time
#' (scaled by 1000), the second integer is the scan number, and the third integer
#' is the intensity.
#'
#' @param path Path to LCD file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @author Ethan Bass
#' @return A 2D chromatogram from the SumTIC stream in \code{matrix} or
#' \code{data.frame} format, according to the value of \code{format_out}.
#' The chromatograms will be returned in \code{wide} or \code{long} format
#' according to the value of \code{data_format}.
#' @note This parser is experimental and may still need some work. It is not
#' yet able to interpret much metadata from the files.
#' @export

read_sz_tic <- function(path, format_out = c("matrix", "data.frame"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE){
  path_tic <- check_streams(path, what = "tic")
  path_tic <- export_stream(path, c("QTFL RawData", "Centroid SumTIC"))
  f <- file(path_tic, "rb")
  on.exit(close(f))
  dat <- decode_sz_tic(f)
  if (data_format == "wide"){
    row.names(dat) <- dat[, "rt"]
    dat <- dat[,"int", drop=FALSE]
  }
  if (format_out == "data.frame"){
    dat <- as.data.frame(dat)
  }
  dat
}

#' Decode 'Shimadzu' total ion chromatogram
#' @noRd
decode_sz_tic <- function(f){
  seek(f, where = 0, origin = "end")
  bytes <- seek(f, where = 0, origin = "end")

  nval <- (bytes-8)/16
  seek(f, 0, "start")
  seek(f, 0, "start")

  mat <- matrix(nrow = nval, ncol = 3)
  count <- 1
  readBin(f, what = "integer", size = 4, n = 2) # skip 2
  while(count < nval){
    mat[count,] <- readBin(f, what = "integer", size = 4, n = 3)
    readBin(f, what = "integer", size = 4, n = 1) # skip 1
    count <- count + 1
  }
  mat[,1] <- mat[,1]/1000
  colnames(mat) <- c("rt", "index", "int")
  mat
}

#' @noRd
read_sz_chrom <- function(path, stream){
  path_raw <- export_stream(path, stream = stream)
  f <- file(path_raw, "rb")
  on.exit(close(f))
  data.frame(int = decode_sz_block(f))
}

#' Read 'Shimadzu' "Method" stream
#' This function is called internally by \code{read_shimadzu_lcd}.
#' @author Ethan Bass
#' @noRd
read_sz_method <- function(path, stream = c("GUMM_Information", "ShimadzuPDA.1",
                                            "PDA.1.METHOD")){
  method_path <- export_stream(path, stream = stream,
                                   remove_null_bytes = TRUE)
  if (is.na(method_path)){
    warning("Method stream could not be found -- unable to infer retention times.")
    return(NA)
  } else{
    method_stream <- xml2::read_xml(method_path)

    sz_extract_upd_elements <- function(method_stream, xpath,
                                        data_format = c("list", "data.frame")){
      data_format <- match.arg(data_format, c("list", "data.frame"))
      upd_elements <- xml2::xml_find_all(method_stream, xpath)

      vals <- suppressWarnings(as.numeric(xml2::xml_text(
        xml2::xml_find_first(upd_elements, ".//Val"))))
      data <- as.list(vals)
      names(data) <- xml2::xml_attr(upd_elements, "ID")

      if (data_format == "data.frame"){
        data <- as.data.frame(do.call(rbind, data))
        colnames(data) <- "Val"
      }
      data
    }
    sz_extract_upd_elements(method_stream, xpath = "/GUD/UP/UPD")
  }
}

#' Infer times from 'Shimadzu' Method stream
#' This function is called internally by \code{read_shimadzu_lcd}.
#' @note This function is no longer needed because the times can be inferred
#' (more reliably?) from the 2D Data Item.
#' @author Ethan Bass
#' @noRd
get_sz_times <- function(sz_method, what = c("pda","chromatogram"), nval){
  what <- match.arg(what, c("pda", "chromatogram"))
  fields <- switch(what, "pda" = c("StTm", "EdTm"),
                         "chromatogram" = c("ACQ$StartTm#1", "ACQ$EndTm#1"))
  start_time <- try(get_metadata_field(sz_method, fields[1])/60000, silent = TRUE)
  end_time <- try(get_metadata_field(sz_method, fields[2])/60000, silent = TRUE)
  if (inherits(start_time, "numeric") & inherits(end_time, "numeric")){
    seq(from = start_time, to = end_time, length.out = nval)
  } else NA
}

#' Get number of rows from LCD 3D Data item
#' @author Ethan Bass
#' @noRd

get_shimadzu_rows <- function(path){
  metadata_path <- export_stream(path, stream =  c("PDA 3D Raw Data", "3D Data Item"))
  if (is.na(metadata_path)){
    warning("3D Data Item stream could not be found -- unable to infer number of rows in stream.")
    return(NA)
  } else {
    meta <- xml2::read_xml(metadata_path)
    cn_node <- xml2::xml_find_first(meta, "//CN")
    as.numeric(xml2::xml_text(cn_node))
  }
}

#' Read 'Shimadzu' LCD 3D Raw Data
#' @author Ethan Bass
#' @noRd
read_sz_pda <- function(path, n_lambdas = NULL){
  path_raw <- export_stream(path, stream =  c("PDA 3D Raw Data", "3D Raw Data"),
                            verbose = TRUE)
  f <- file(path_raw, "rb")
  on.exit(close(f))

  seek(f, 0, 'end')
  fsize <- seek(f, NA, "current")

  # Read data

  seek(f, 0, "start")
  seek(f, 0, "start")

  nrows <- get_shimadzu_rows(path)
  nrows <- ifelse(is.na(nrows), fsize/(n_lambdas * 1.5), nrows)

  mat <- matrix(NA, nrow = nrows, ncol = n_lambdas)
  i <- 1
  while (seek(f, NA, "current") < fsize) {
    mat[i,] <- decode_sz_block(f)
    i <- i + 1
  }
  if (any(is.na(mat[,1]))){
    mat <- mat[-which(is.na(mat[,1])),]
  }
  mat
}


#' Extract wavelengths from Shimadzu LCD
#' This function is called internally by \code{read_shimadzu_lcd}.
#' @author Ethan Bass
#' @noRd
read_sz_wavelengths <- function(path){
  path_wavtab <- export_stream(path, stream =  c("PDA 3D Raw Data",
                                                 "Wavelength Table"))
  f <- file(path_wavtab, "rb")
  on.exit(close(f))
  n_lambda <- readBin(f, what="integer", size = 4)
  count <- 1
  lambdas <- sapply(seq_len(n_lambda), function(i){
    readBin(f, what = "integer", size = 4)/100
  })
  lambdas
}

#' Read 'Shimadzu' LCD data block
#' This function is called internally by \code{read_shimadzu_lcd}.
#' @author Ethan Bass
#' @noRd
decode_sz_block <- function(f) {
  block_start <- seek(f, NA, "current")

  readBin(f, what = "integer", n = 6, size = 1) #skip
  readBin(f, what = "integer", n = 1, size = 2)

  n_lambda <- readBin(f, what = "integer", n = 1,
                        size = 2, endian = "little")

  readBin(f, what = "integer", n = 1, size = 2)
  block_length <- readBin(f, what = "integer", n = 1, size = 2)
  readBin(f, what = "integer", n = 5, size = 2)

  signal <- numeric(n_lambda)
  count <- 1
  buffer <- list(0,0,0,0)

  while (count < length(signal)){
    n_bytes <- readBin(f, "integer", n = 1, size = 2)
    start <- seek(f, NA, "current")
    if (length(n_bytes) == 0) break

    while(seek(f, NA, "current") < start + n_bytes){
      buffer[[3]] <- readBin(f, "raw", n = 1, size = 1)
      hex1 <- as.numeric(substr(buffer[[3]], start = 1, stop = 1))
      hex1
      if (buffer[[3]] == "82"){
        next
      } else if (buffer[[3]] == "00"){
        buffer[[2]] <- 0
      } else if (hex1 == 0){
        buffer[[2]] <- as.integer(buffer[[3]])
      } else if (hex1 == 1){
        buffer[[2]] <- decode_sz_val(buffer[[3]])
      } else if (hex1 > 1){
        buffer[[4]] <- readBin(f, "raw", n = (hex1 %/% 2), size = 1)
        buffer[[2]] <- decode_sz_val(c(buffer[[3]], buffer[[4]]))
    }
      buffer[[1]] <- buffer[[1]] + buffer[[2]]
      signal[count] <- buffer[[1]]
      count <- count + 1
    }
    end <- readBin(f, "integer", n = 1, size = 2)
    n_bytes == end
    buffer[[1]] <- 0
  }
  signal
}

#' Return twos complement from binary string
#' This function is called internally by \code{read_shimadzu_lcd}.
#' @noRd
twos_complement <- function(bin, exp){
  if (missing(exp)){
    exp <- nchar(bin)
  }
  strtoi(bin, 2) - 2^exp
}

#' Convert integer to binary
#' @author Stuart K. Grange
#' @note This function is borrowed from the threadr package
#' \url{https://github.com/skgrange/threadr/} where it's licensed under GPL3.
#' @noRd
as_binary <- function(x, n = 32) {
  # Check type
  if (!is.integer(x)) stop("Input must be an integer.", call. = FALSE)
  # Do
  x <- sapply(x, function(x) integer_to_binary(x, n))
  # Return
  x
}

#' Convert integer to binary
#' @author Stuart K. Grange
#' @note This function is borrowed from the threadr package
#' \url{https://github.com/skgrange/threadr/} where it's licensed under GPL3.
#' @noRd
integer_to_binary <- function(x, n) {
  # Convert to a vector of integers
  x <- intToBits(x)
  # Drop leading zeros
  x <- as.integer(x)
  # Filter to a certain number of bits
  x <- x[1:n]
  # Reverse order of vector
  x <- rev(x)
  # Collapse vector into string
  x <- stringr::str_c(x, collapse = "")
  # Return
  x
}

#' Convert hexadecimal string to raw format
#' @param x A hexadecimal string
#' @noRd
char_to_raw <- function(x){
  hex <- strsplit(x, "")[[1]]
  hex <- paste(hex[c(TRUE, FALSE)], hex[c(FALSE, TRUE)], sep = "")
  as.raw(strtoi(hex, 16L))
}

#' Read float from 'Shimadzu' metadata
#' @noRd
sz_float <- function(x, size = 8, endian = "little"){
  readBin(char_to_raw(x), "double", n = 1, size = size, endian = endian)
}

#' Decode 'Shimadzu' metadata 'FtoX' floats
#' @noRd
sz_decode_fto <- Vectorize(
  function(x){
    x <- gsub("@FtoX@", "", x)
    if (x == "1"){
      return(1)
    } else{
      return(sz_float(x, size = 4, endian = "big"))
    }
  }
)

#' Decode 'Shimadzu' metadata 'StoX' strings
#' @noRd
sz_decode_sto <- Vectorize(
  function(x){
    x <- gsub("@StoX@", "", x)
    tryCatch({raw_bytes <- as.raw(strtoi(substring(x, seq(1, nchar(x), 2),
                                                   seq(2, nchar(x), 2)), 16L))
    rawToChar(raw_bytes)
    }, error = function(err) NA)
  }
)


#' Convert 'Shimadzu' time to Unix time
#' 'Shimadzu' LCD files seem to store times in 'Windows FILETIME' structure,
#' where the "low" time and "high" times must be combined into a 64-bit integer
#' representing the number of 100-nanosecond intervals since 1601-01-01.
#' Assuming that this interpretation is correct, there seems to be something
#' wrong with my conversion, since the times don't quite match the ones from the
#' ASCII files exported from 'Lab Solutions'.
#' @importFrom bit64 as.integer64
#' @noRd
sztime_to_unixtime <- function(low, high, tz = "UTC") {
  if (tz!="UTC"){
    tz <- -as.numeric(gsub("'00'", "", tz))
    if (tz > 0){
      tz <- paste0("+",tz)
    }
    tz <- paste0("Etc/GMT", tz)
  }
  filetime <- bit64::as.integer64(high) * 2^32 + bit64::as.integer64(low)
  unix_time <- (filetime / 10000000) - 11644473600
  as.POSIXct(unix_time, origin = "1970-01-01", tz = tz)
}

#' Read 'Shimadzu' file properties
#' @noRd
read_sz_file_properties <- function(path){
  path_prop <- export_stream(path, "File Property")
  raw_xml <- readLines(path_prop, skipNul = TRUE)
  raw_xml <- sub("^\037\004|^o\004", "", raw_xml)
  xml_headers <- grep("xml version", raw_xml)

  # Combine lines and parse
  props <- lapply(seq_along(xml_headers[-1]), function(i){
    xml_content <- paste(raw_xml[xml_headers[[i]]:(xml_headers[[i+1]] - 1)],
                         collapse = "\n")
    xml_doc <- xml2::read_xml(xml_content)
  })
  names(props) <- sapply(props, xml2::xml_name)
  meta <- suppressWarnings(unlist(lapply(props, sz_decode_props),
                                  recursive = FALSE))
  meta
}

#' Decode 'Shimadzu' file properties
#' @noRd
sz_decode_props <- function(x){
  nodes <- xml2::xml_children(x)
  meta <- xml2::xml_text(nodes)
  names(meta) <- xml2::xml_name(nodes)
  fto.idx <- grep("@FtoX@", meta)
  meta[fto.idx] <- sz_decode_fto(meta[fto.idx])
  sto.idx <- grep("@StoX@", meta)
  meta[sto.idx] <- sz_decode_sto(meta[sto.idx])
  as.list(meta)
}

#' Read 'Shimadzu' 3D Data Item
#' @noRd
read_sz_3DDI <- function(path){
  path_meta <- export_stream(path, c('PDA 3D Raw Data', '3D Data Item'))
  doc <- xml2::read_xml(path_meta)

  nodes <- xml2::xml_children(doc)
  rm <- which(xml2::xml_name(nodes) %in% c("ELE", "GUD", "DataItem", "SPR"))
  meta <- as.list(xml2::xml_text(nodes[-rm]))
  names(meta) <- xml2::xml_name(nodes[-rm])

  meta[c("WVB","WVE","WLS")] <-
    lapply(meta[c("WVB","WVE","WLS")], function(x){
      sz_float(x)/100
  })
  meta <- c(meta, read_sz_2DDI(xml2::xml_find_all(doc,
                                                  ".//GUD[@Type='2DDataItem']"),
                               read_file = FALSE))
  meta
}

#' Read 'Shimadzu' 2D Data Item
#' @noRd
read_sz_2DDI <- function(path, read_file = TRUE, idx = 1){
  if(read_file){
    path_meta <- export_stream(path, c('LSS Data Processing', '2D Data Item'))
    doc <- xml2::read_xml(path_meta)
  } else{
    doc <- path
  }

  nodes <- xml2::xml_child(doc, search = idx) |> xml2::xml_children()
  ddi_idx <- which(xml2::xml_name(nodes) == "DDI")

  meta <- xml2::xml_text(nodes[-ddi_idx])
  names(meta) <- xml2::xml_name(nodes[-ddi_idx])

  meta[c("CF","GF","AT","DLT")] <-
    lapply(meta[c("CF","GF","AT","DLT")], function(x) sz_float(x))

  meta <- c(meta, extract_axis_metadata(nodes))

  meta$time.vf <- ifelse(is.na(meta$time.vf), 60000, meta$time.vf)
  meta$detector.vf <- 1/meta$detector.vf

  meta[c("AT","DLT","Rate")] <-
    lapply(meta[c("AT","DLT","Rate")], function(x) as.numeric(x)/meta$time.vf)

  meta
}

#' Extract axis metadata
#' @noRd
extract_axis_metadata <- function(x){
  idx <- c(0, 1)
  ax <- lapply(idx, function(i){
    ax <- xml2::xml_find_all(x, paste0(".//Axis[@ID='", i, "']"))
    dus <- as.numeric(xml2::xml_attr(ax, "DUS"))
    if (dus != 0){
      xml2::xml_find_all(ax, "US")[[dus]]
    } else NA

  })
  names(ax) <- c("detector","time")
  unlist(lapply(ax, function(x){
    if (inherits(x, "xml_node")){
      list(vf = xml2::xml_find_all(x, "VF") |> xml2::xml_text() |> sz_float(),
           unit = xml2::xml_find_all(x, "Unit") |> xml2::xml_text()
           )
    } else list(vf = NA, unit = NA)
  }), recursive = FALSE)
}

#' Decode 'Shimadzu' values
#' @noRd
decode_sz_val <- function(hex) {
  # Convert raw vector to integer
  total_bits <- 8*length(hex)
  x <- 0
  for (i in seq_along(hex)) {
    x <- bitwOr(bitwShiftL(x, n = 8), as.integer(hex[i]))
  }

  # Calculate the number of value bits
  value_bits <- total_bits - 4

  # Extract the sign (leftmost 4 bits)
  sign <- bitwAnd(bitwShiftR(x, n = value_bits), b = 0xF)

  # Extract the value part
  value_mask <- bitwShiftL(1, value_bits) - 1
  value <- bitwAnd(x, b = value_mask)

  if (sign %% 2 == 1) {
    return(-(bitwShiftL(1, value_bits) - value))
  } else {
    return(value)
  }
}
