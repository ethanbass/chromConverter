#' Shimadzu GCD parser
#'
#' Read 2D PDA data stream from 'Shimadzu' GCD files.
#'
#' A parser to read chromatogram data streams from 'Shimadzu' \code{.gcd} files.
#' GCD files are encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The PDA data is encoded in a stream called \code{PDA 3D Raw Data:3D Raw Data}.
#' The GCD data stream contains a segment for each retention time, beginning
#' with a 24-byte header.
#'
#' The 24 byte header consists of the following fields:
#' * 4 bytes: segment label (\code{17234}).
#' * 4 bytes: Little-endian integer specifying the sampling interval in milliseconds.
#' * 4 bytes: Little-endian integer specifying the number of values in the file.
#' * 4 bytes: Little-endian integer specifying the total number of bytes in the file
#' (However, this seems to be off by a few bytes?).
#' * 8 bytes of \code{00}s
#'
#' After the header, the data are simply encoded as 64-bit (little-endian)
#' floating-point numbers. The retention times can be (approximately?) derived
#' from the number of values and the sampling interval encoded in the header.
#' @param path Path to GCD file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @author Ethan Bass
#' @return A 2D chromatogram from the chromatogram stream in \code{matrix} or
#' \code{data.frame} format, according to the value of \code{format_out}.
#' The chromatograms will be returned in \code{wide} or \code{long} format
#' according to the value of \code{data_format}.
#' @note This parser is experimental and may still need some work. It is not
#' yet able to interpret much metadata from the files.
#' @export

read_shimadzu_gcd <- function(path, format_out = c("matrix", "data.frame"),
                                   data_format = c("wide", "long"),
                                   read_metadata = TRUE,
                                    metadata_format = c("chromconverter","raw")){
    format_out <- match.arg(format_out, c("matrix", "data.frame"))
    data_format <- match.arg(data_format, c("wide", "long"))
    metadata_format <- match.arg(metadata_format, c("chromconverter","raw"))

    olefile_installed <- reticulate::py_module_available("olefile")
    if (!olefile_installed){
      configure_python_environment(parser = "olefile")
    }

    existing_streams <- check_streams(path, what = "chromatogram")

    dat <- lapply(existing_streams, function(stream){
      decode_shimadzu_gcd(path, stream = stream)
    })

    # infer times from "PDA.1.Method" stream
    # method_metadata <- read_sz_method(path, stream = c("GUMM_Information", "ShimadzuGC.1",
    #                                                    "GUC.1.METHOD"))

    if (data_format == "wide"){
      dat <- lapply(dat, function(x){
        data.frame(int = x$int, row.names = x$rt)
      })
    }

    if (format_out == "matrix"){
      dat <- lapply(dat, function(x){
        as.matrix(x)
      })
    }
    if (length(dat) == 1){
      dat <- dat[[1]]
    }
    dat
  }

#' @noRd
decode_shimadzu_gcd <- function(path, stream){
  path_stream <- export_stream(path, stream = stream)

  f <- file(path_stream, "rb")
  on.exit(close(f))

  block_start <- seek(f, NA, "current")

  readBin(f, what = "integer", n = 1, size = 4) #skip
  interval <- readBin(f, what = "integer",size = 4,endian = "little")
  nval <- readBin(f, what = "integer", size = 4, endian = "little")

  readBin(f, what="double", size=4, n = 3, endian = "little") #skip

  signal <- readBin(f, what = "double", n = nval, endian = "little")

  times <- seq(40, nval*interval, interval)/60000
  data.frame(rt = times, int=signal)
}
