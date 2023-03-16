#' Parser for reading Agilent UV (.uv) files into R
#' @param path Path to \code{.uv} file
#' @param read_metadata Logical. Whether to attach metadata.
#' @param format_out Matrix or data.frame
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @importFrom utils head tail
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @note This function was adapted from the parser in the rainbow project
#' licensed under GPL 3 by Evan Shi
#' (https://rainbow-api.readthedocs.io/en/latest/agilent/uv.html).
#' @export

read_chemstation_uv <- function(path, read_metadata = TRUE,
                                format_out = c("matrix","data.frame"),
                                data_format = c("wide","long")){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))

  f <- file(path, "rb")
  on.exit(close(f))

  seek(f, 1, "start")
  version <- readBin(f, "character", n = 1)
  version <- match.arg(version, choices = c("31", "131"))
  if (version != "131"){
    stop("Only type 131 files are currently supported by this parser.")
  }
  if (version == "131"){
    offsets <- list(file_type = 326,
                    sample_name = 858,
                    operator = 1880,
                    date = 2391,
                    detector = 2492,
                    method = 2574,
                    software = 3089,
                    units = 3093,
                    vial = 4055,
                    num_times = 278,
                    rt_first = 282,
                    rt_last = 286,
                    scaling_factor = 3085,
                    data_start = 4096)
  } else if (version == "31"){
    offsets <- list(file_type = 4,
                    # sample_name = 858,
                    operator = 148,
                    date = 178,
                    detector = 208,
                    method = 228,
                    # software = 3089,
                    units = 326,
                    # vial = 4055,
                    num_times = 278,
                    rt_first = 282,
                    rt_last = 286,
                    scaling_factor = 3085,
                    data_start = 0x202)
  }


  meta <- lapply(offsets[seq_len(9)], function(offset){
    seek(f, where = offset, origin = "start")
    n <- get_nchar(f)
    cc_collapse(readBin(f, "character", n = 1))
  })

  #Number of data values
  seek(f, where = offsets$num_times, origin="start")
  nval <- readBin(f, "int", n=1, endian = "big", signed = 2)

  #Scaling factor
  seek(f, where = offsets$scaling_factor, origin="start")
  scaling_value <- readBin(f, "double", n=1, endian = "big")

  # Seek to the start of data segment header
  seek(f, offsets$data_start + 0x8)

  # Read and unpack wavelength information
  wave_info <- readBin(f, integer(), n=3, size=2, endian="little")
  lambda_start <- wave_info[1] %/% 20
  lambda_end <- wave_info[2] %/% 20
  delta_lambda <- wave_info[3] %/% 20

  # Compute wavelengths and number of wavelengths
  lambdas <- seq(lambda_start, lambda_end, by=delta_lambda)
  n_lambdas <- length(lambdas)

  # BODY
  seek(f, where = offsets$data_start, origin="start")
  seek(f, where = offsets$data_start, origin="start")

  # Initialize empty arrays
  times <- integer(nval)
  data <- array(0, dim=c(nval, n_lambdas))

  # Read data and populate arrays
  for (i in seq_len(nval)) {
    readBin(f, integer(), n=1, size=4)  # Discard first 4 bytes
    times[i] <- readBin(f, integer(), n=1, size=4)
    readBin(f, raw(), n=14)  # Discard 14 bytes
    absorb_accum <- 0
    for (j in seq_len(n_lambdas)) {
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
  data <- data*scaling_value
  rownames(data) <- times
  colnames(data) <- lambdas

  if (data_format == "long"){
    data <- reshape_chrom(data)
  }

  if (format_out == "matrix"){
    data <- as.matrix(data)
  }

  if (read_metadata){
    data <- structure(data, file_version = meta$file_type, sample_name = meta$sample_name,
                      operator = meta$operator, run_date = meta$date,
                      instrument = meta$detector,
                      method = meta$method, software_version = NA,
                      software = NA, software_rev = NA,
                      signal = NA, unit = meta$units,
                      vial = meta$vial,
                      time_range = c(head(times,1), tail(times,1)),
                      data_format = "wide", parser = "chromConverter")
  }
  data
}

