#' Read 'Shimadzu' LCD
#' @param path Path to LCD file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @author Ethan Bass
#' @note This parser is experimental and may still need some work.
#' @export
read_shimadzu_lcd <- function(path,format_out = c("matrix","data.frame"),
                              data_format = c("wide","long"),
                              read_metadata = TRUE){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))

  path_meta <- export_stream(path, stream =  c("PDA 3D Raw Data", "3D Data Item"))
  meta <- xml2::read_xml(path_meta)
  xpath_expression <- "//CN"
  result <- xml_find_all(meta, xpath_expression)
  nval <- as.numeric(xml_text(result[[1]]))

  dat <- read_shimadzu_raw(path, nval = nval)

  lambdas <- read_shimadzu_wavelengths(path)
  colnames(dat) <- lambdas
  if (data_format == "long"){
    data <- reshape_chrom(data)
  }
  if (format_out == "data.frame"){
    data <- as.data.frame(data)
  }
  dat
}

#' Read 'Shimadzu' LCD 3D raw data
#' @author Ethan Bass
#' @noRd
read_shimadzu_raw <- function(path, nval){
  path_raw <- export_stream(path, stream =  c("PDA 3D Raw Data", "3D Raw Data"))
  f <- file(path_raw, "rb")
  on.exit(close(f))
  mat <- pbapply::pbsapply(seq_len(nval), function(i){
    decode_shimadzu_block(f)
  })
  mat <- t(mat)
}

#' Export OLE stream
#' @author Ethan Bass
#' @noRd
export_stream <- function(path, stream, path_out){
  py_run_string('import olefile')
  py_run_string(paste0('ole = olefile.OleFileIO("', path, '")'))
  python_stream <- paste(paste0("'",stream, "'"), collapse=', ')
  py_run_string(paste0("st = ole.openstream([", python_stream, "])"))
  py_run_string('dat = st.read()')
  if (missing(path_out)){
    path_out = tempfile()
  }
  py_run_string(paste0('with open("', path_out ,'", "wb") as binary_file:
    binary_file.write(dat)'))
  path_out
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
        bin <- paste(as_binary(strtoi(c(buffer[[3]], buffer[[4]]), 16), 8), collapse="")
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
