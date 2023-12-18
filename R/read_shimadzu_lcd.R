#' Shimadzu LCD parser
#'
#' Read 3D PDA data stream from 'Shimadzu' LCD files.
#'
#' A parser to read PDA data from 'Shimadzu' \code{.lcd} files. LCD files are
#' encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The PDA data is encoded in a stream called \code{PDA 3D Raw Data:3D Data Item}.
#' The PDA data stream contains a segment for each retention time, beginning
#' with a 24-byte header.
#'
#' The 24 byte header consists of the following fields:
#' * 4 bytes: segment label (\code{17234}).
#' * 4 bytes: ???
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
#' from the previous value.
#'
#' @param path Path to LCD file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @author Ethan Bass
#' @note This parser is experimental and may still
#' need some work. It is not yet able to interpret much metadata from the files.
#' @export

read_shimadzu_lcd <- function(path, format_out = c("matrix", "data.frame"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))

  olefile_installed <- reticulate::py_module_available("olefile")
  if (!olefile_installed){
    configure_python_environment(parser = "olefile")
  }

  # read wavelengths from "Wavelength Table" stream
  lambdas <- read_shimadzu_wavelengths(path)
  n_lambdas <- length(lambdas)

  # read data from "3D Raw Data" stream
  dat <- read_shimadzu_raw(path, n_lambdas = n_lambdas)
  colnames(dat) <- lambdas

  # infer times from "PDA.1.Method" stream
  method_metadata <- read_sz_method(path)
  times <- get_sz_times(method_metadata, nval = nrow(dat))
  if (inherits(times, "numeric")){
    rownames(dat) <- times
  }

  if (data_format == "long"){
    dat <- reshape_chrom(dat, data_format = "wide")
  }

  if (format_out == "data.frame"){
    data <- as.data.frame(data)
  }
  dat
}

#' Read Shimadzu "Method" stream
#' @author Ethan Bass
#' @noRd
read_sz_method <- function(path){
  method_path <- export_stream(path,
                                   stream = c("GUMM_Information", "ShimadzuPDA.1",
                                              "PDA.1.METHOD"),
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
#' @author Ethan Bass
#' @noRd
get_sz_times <- function(sz_method, nval){
    start_time <- try(get_metadata_field(sz_method, "StTm")/60000, silent = TRUE)
    end_time <- try(get_metadata_field(sz_method, "EdTm")/60000, silent = TRUE)
    if (inherits(start_time, "numeric") & inherits(end_time, "numeric")){
      seq(from = start_time, to = end_time, length.out = nval)
    } else NA
}

#' Read 'Shimadzu' LCD 3D raw data
#' @author Ethan Bass
#' @noRd

read_shimadzu_raw <- function(path, n_lambdas = NULL){
  path_raw <- export_stream(path, stream =  c("PDA 3D Raw Data", "3D Raw Data"),
                            verbose = TRUE)
  f <- file(path_raw, "rb")
  on.exit(close(f))

  seek(f, 0, 'end')
  fsize <- seek(f, NA, "current")

  # Read data

  seek(f, 0, "start")
  seek(f, 0, "start")

  mat <- matrix(NA, nrow = fsize/(n_lambdas*1.5), ncol = n_lambdas)
  i <- 1
  while (seek(f, NA, "current") < fsize) {
    mat[i,] <- decode_shimadzu_block(f)
    i <- i + 1
  }
  if (any(is.na(mat[,1]))){
    mat <- mat[-which(is.na(mat[,1])),]
  }
  mat
}

#' Export OLE stream
#' Use olefile to export te specified stream.
#' @param file Path to ole file.
#' @author Ethan Bass
#' @noRd
export_stream <- function(path_in, stream, path_out, remove_null_bytes = FALSE,
                          verbose = FALSE){
  reticulate::py_run_string('import olefile')
  reticulate::py_run_string(paste0('ole = olefile.OleFileIO("', path_in, '")'))
  python_stream <- paste0("[", paste(paste0("'", stream, "'"), collapse = ', '),"]")
  stream_exists <- reticulate::py_eval(paste0("ole.exists(", python_stream, ")"))
  if (!stream_exists){
    if (verbose){
      warning(paste0("The stream ", sQuote(python_stream), " could not be found."),
              immediate. = TRUE)
    }
    return(NA)
  } else{
    reticulate::py_run_string(paste0("st = ole.openstream(", python_stream, ")"))
    reticulate::py_run_string('data = st.read()')

    if (missing(path_out)){
      path_out <- tempfile()
    }
    if (remove_null_bytes){
      reticulate::py_run_string("data = data.replace(b'\\x00', b'')")
    }
    reticulate::py_run_string(paste0('with open("', path_out ,'", "wb") as binary_file:
      binary_file.write(data)'))
    path_out
  }
}

#' Read 'Shimadzu' LCD data block
#' @author Ethan Bass
#' @noRd
decode_shimadzu_block <- function(file) {
  block_start <- seek(file, NA, "current")

  readBin(file, what = "integer", n = 6, size=1) #skip
  readBin(file, what = "integer", n = 1, size=2)

  n_lambda <- readBin(file, what = "integer", n = 1, size = 2, endian = "little")

  readBin(file, what = "integer", n = 1, size = 2)
  block_length <- readBin(file, what = "integer", n = 1, size = 2)
  readBin(file, what = "integer", n = 5, size = 2)

  signal <- numeric(n_lambda)
  count <- 1
  buffer <- list(0,0,0,0)

  for (i in c(1:2)){
    n_bytes <- readBin(file, "integer", n = 1, size = 2)
    start <- seek(file, NA, "current")

    while(seek(file, NA, "current") < start + n_bytes){
      buffer[[3]] <- readBin(file, "raw", n = 1, size = 1)
      hex1 <- as.numeric(substr(buffer[[3]], 1, 1))
      if (hex1 == 0){
        buffer[[2]] <- strtoi(buffer[[3]], 16)
      } else if (hex1 == 1){
        bin <- as_binary(strtoi(buffer[[3]], 16), 8)
        buffer[[2]] <- twos_complement(substr(bin, 5, nchar(bin)))
      } else if (hex1 > 1){
        buffer[[4]] <- readBin(file, "raw", n = (hex1 %/% 2), size = 1)
        bin <- paste(as_binary(strtoi(c(buffer[[3]], buffer[[4]]), 16), 8),
                     collapse = "")
        if (hex1 %% 2 == 0){
          buffer[[2]] <- strtoi(substr(bin, 5, nchar(bin)), 2)
        } else {
          buffer[[2]] <- twos_complement(substr(bin, 5, nchar(bin)))
        }
      }
      buffer[[1]] <- buffer[[1]] + buffer[[2]]
      signal[count] <- buffer[[1]]
      count <- count + 1
    }
    end <- readBin(file, "integer", n = 1, size = 2)
    n_bytes == end
    buffer[[1]] <- 0
  }
  signal
}

#' Return twos complement from binary string
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

#' Extract wavelengths from Shimadzu LCD
#' @author Ethan Bass
#' @noRd
read_shimadzu_wavelengths <- function(path){
  path_wavtab <- export_stream(path, stream =  c("PDA 3D Raw Data", "Wavelength Table"))
  f <- file(path_wavtab, "rb")
  on.exit(close(f))
  n_lambda <- readBin(f, what="integer", size = 4)
  count <- 1
  # lambdas <- numeric(n_lambda)
  lambdas <- sapply(seq_len(n_lambda), function(i){
    readBin(f, what="integer", size = 4)/100
  })
  lambdas
}

