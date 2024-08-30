#' Read 'ChemStation' DAD files
#'
#' Agilent \code{.uv} files come in several different varieties. This parser can
#' automatically detect and read several versions of these files from
#' 'Agilent ChemStation' and 'Agilent OpenLab', including versions \code{31} and
#' \code{131}.
#'
#' @importFrom utils head tail
#' @param path Path to \code{.uv} file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param scale Whether to scale the data by the scaling factor present in the
#' file. Defaults to \code{TRUE}.
#' @return A 3D chromatogram in the format specified by \code{data_format} and
#' \code{format_out}. If \code{data_format} is \code{wide}, the chromatogram will
#' be returned with retention times as rows and wavelengths as columns. If
#' \code{long} format is requested, three columns will be returned: one for the
#' retention time, one for the wavelength and one for the intensity. The
#' \code{format_out} argument determines whether the chromatogram is returned as
#' a \code{matrix} or \code{data.frame}. Metadata can be attached to the
#' chromatogram as \code{\link{attributes}} if \code{read_metadata} is \code{TRUE}.
#' @author Ethan Bass
#' @note This function was adapted from the parser in the rainbow project
#' licensed under GPL 3 by Evan Shi
#' \url{https://rainbow-api.readthedocs.io/en/latest/agilent/uv.html}.
#' @export

read_chemstation_uv <- function(path, format_out = c("matrix", "data.frame"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                scale = TRUE){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "chemstation_uv", raw = "raw")

  f <- file(path, "rb")
  on.exit(close(f))

  version <- read_cs_string(f)
  seek(f, 348, "start")
  file_type_code <- paste(file_type_name = readBin(f, "character", n = 2),
                          collapse = "")
  version <- match.arg(version, choices = c("31", "131"))

  if (version == "131"){
    version <- paste(version, file_type_code, sep = "_")
  }

  offsets <- get_agilent_offsets(version)

  n_metadata_fields <- switch(version, "131_LC" = 10,
                                       "131_OL" = 8,
                                       "31" = 8)

  meta <- lapply(offsets[seq_len(n_metadata_fields)], function(offset){
    type <- switch(version, "31" = 1, 2)
    seek(f, where = offset, origin = "start")
    read_cs_string(f, type = type)
  })

  # Number of data values
  seek(f, where = offsets$num_times, origin = "start")
  nval <- readBin(f, "int", n = 1, endian = "big", signed = 2)

  # Scaling factor
  seek(f, where = offsets$scaling_factor, origin = "start")
  scaling_value <- readBin(f, "double", n = 1, endian = "big")

  # Seek to the start of data segment header
  seek(f, offsets$data_start + 0x8)

  # Read and unpack wavelength information
  wave_info <- readBin(f, integer(), n = 3, size = 2, endian = "little")
  lambda_start <- wave_info[1] %/% 20
  lambda_end <- wave_info[2] %/% 20
  delta_lambda <- wave_info[3] %/% 20

  # Compute wavelengths and number of wavelengths
  lambdas <- seq(lambda_start, lambda_end, by = delta_lambda)
  n_lambdas <- length(lambdas)

  # BODY
  seek(f, where = offsets$data_start, origin = "start")

  # Read data and populate arrays
  decode_array <- switch(version, "131_OL" = decode_uv_array,
                    "131_LC" = decode_uv_delta,
                    "31" = decode_uv_delta)

  data <- decode_array(f = f, nval = nval, ncol = n_lambdas)
  if (scale){
    data <- data*scaling_value
  }
  colnames(data) <- lambdas

  if (data_format == "long"){
    data <- reshape_chrom(data)
  }

  if (format_out == "data.frame"){
    data <- as.data.frame(data)
  }

  if (read_metadata){
    metadata_from_file <- try(read_chemstation_metadata(path), silent = TRUE)
    if (!inherits(metadata_from_file, "try-error")){
      meta <- c(meta, metadata_from_file)
    }
    meta$signal <- as.numeric(c(lambda_start, lambda_end))
    meta$time_range = as.numeric(c(head(rownames(data), 1), tail(rownames(data), 1)))
    meta$intensity_multiplier <- scaling_value
    data <- attach_metadata(data, meta, format_in = metadata_format,
                    data_format = data_format, format_out = format_out,
                    parser = "chromconverter", source_file = path,
                    scale = scale)
  }
  data
}

#' @author Ethan Bass
#' @noRd
decode_uv_delta <- function(f, nval, ncol){
  # Initialize empty arrays
  times <- integer(nval)
  data <- array(0, dim = c(nval, ncol))
  for (i in seq_len(nval)) {
    readBin(f, integer(), n = 1, size = 4)  # Discard first 4 bytes
    times[i] <- readBin(f, integer(), n = 1, size = 4)
    readBin(f, raw(), n = 14)  # Discard 14 bytes
    absorb_accum <- 0
    for (j in seq_len(ncol)) {
      check_int <- readBin(f, integer(), n = 1, size = 2)
      if (check_int == -0x8000) {
        absorb_accum <- readBin(f, integer(), n = 1, size = 4)
      } else {
        absorb_accum <- absorb_accum + check_int
      }
      data[i, j] <- absorb_accum
    }
  }
  times <- times/60000
  rownames(data) <- times
  data
}

#' @author Ethan Bass
#' @noRd
decode_uv_array <- function(f, nval, ncol){
  # Initialize empty arrays
  times <- integer(nval)
  data <- array(0, dim = c(nval, ncol))
  for (i in seq_len(nval)) {
    readBin(f, integer(), n = 1, size = 4)  # Discard first 4 bytes
    times[i] <- readBin(f, integer(), n = 1, size = 4)
    readBin(f, raw(), n = 14)  # Discard 14 bytes
    data[i,] <- readBin(f, what = "double", size = 8, n = ncol)
  }
  times <- times / 60000
  rownames(data) <- times
  data
}
