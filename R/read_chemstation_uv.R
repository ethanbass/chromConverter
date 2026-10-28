#' Read 'Agilent ChemStation' DAD files
#'
#' Agilent `.uv` files come in several formats. This parser detects the
#' version from the file and reads versions `31` and `131` from 'Agilent
#' ChemStation' and 'Agilent OpenLab'; any other version is an error.
#'
#' Retention times are read from each spectrum, so they are unevenly spaced
#' when the detector did not store every spectrum. Interpolate onto a common
#' time axis before comparing runs.
#'
#' @importFrom utils head tail
#' @inheritParams shared_params
#' @param path Path to 'Agilent' `.uv` file.
#' @param scale Whether to scale the data by the scaling factor present in the
#' file. Defaults to `TRUE`.
#' @param source_file Source file from which UV data was originally derived.
#' @inherit generic_return_3D return
#' @examplesIf interactive()
#' read_chemstation_uv("tests/testthat/testdata/dad1.uv")
#' @author Ethan Bass
#' @note This function was adapted from the parser in the rainbow project
#' licensed under GPL 3 by Evan Shi
#' \url{https://rainbow-api.readthedocs.io/en/latest/agilent/uv.html}.
#' @family 'Agilent' parsers
#' @export

read_chemstation_uv <- function(path, format_out = c("matrix", "data.frame",
                                                     "data.table"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                scale = TRUE, source_file = NULL){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "chemstation")
  source_file <- ifelse(is.null(source_file), path, source_file)
  f <- file(path, "rb")
  on.exit(close(f))

  hdr <- read_cs_uv_header(f)
  file_version <- hdr$file_version
  nval <- hdr$nval
  scaling_value <- hdr$scaling_value
  lambdas <- hdr$lambdas
  n_lambdas <- length(lambdas)

  # BODY
  seek(f, where = hdr$offsets$data_start, origin = "start")

  # Read data and populate arrays
  decode_array <- switch(file_version, "131_OL" = decode_uv_array,
                    "131_LC" = decode_uv_delta,
                    "31" = decode_uv_delta)

  data <- decode_array(f = f, nval = nval, ncol = n_lambdas)
  if (file_version == "131_OL" &&
      all(abs(data*scaling_value - round(data*scaling_value)) < 1e-3)){
    scaling_value <- scaling_value^2
  }
  if (scale){
    data <- data*scaling_value
  }
  colnames(data) <- lambdas
  time_range <- as.numeric(c(head(rownames(data), 1), tail(rownames(data), 1)))

  if (data_format == "long"){
    data <- reshape_chrom_long(data)
  }
  data <- convert_chrom_format(data, format_out = format_out,
                               data_format = data_format)

  if (read_metadata){
    meta <- cs_uv_metadata(path, hdr, time_range = time_range,
                           scaling_value = scaling_value)
    data <- attach_metadata(data, meta, format_in = metadata_format,
                    data_format = data_format, format_out = format_out,
                    parser = "chromconverter", source_file = source_file,
                    source_file_format = paste0("chemstation_", file_version),
                    scale = scale)
  }
  data
}

#' Read the header of an 'Agilent' UV file
#' @noRd
read_cs_uv_header <- function(f){
  file_version <- read_cs_string(f)
  seek(f, 348, "start")
  file_type_code <- paste(file_type_name = readBin(f, "character", n = 2),
                          collapse = "")
  file_version <- match.arg(file_version, choices = c("31", "131"))

  if (file_version == "131"){
    file_version <- paste(file_version, file_type_code, sep = "_")
  }

  offsets <- get_agilent_offsets(file_version)

  meta <- read_chemstation_string_fields(f, offsets,
                                         type = switch(file_version, "31" = 1, 2))

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

  list(file_version = file_version, offsets = offsets, meta = meta,
       nval = nval, scaling_value = scaling_value,
       lambda_range = c(lambda_start, lambda_end),
       lambdas = seq(lambda_start, lambda_end, by = delta_lambda))
}

#' Metadata of an 'Agilent' UV file
#' @noRd
cs_uv_metadata <- function(path, hdr, time_range, scaling_value){
  meta <- hdr$meta
  metadata_from_file <- try(read_chemstation_metadata(path), silent = TRUE)
  if (!inherits(metadata_from_file, "try-error")){
    meta <- c(meta, metadata_from_file)
  }
  meta$signal <- as.numeric(hdr$lambda_range)
  meta$time_range <- time_range
  meta$intensity_multiplier <- scaling_value
  meta$detector <- "DAD"
  meta$detector_x_unit <- "nm"
  meta
}

#' Read the metadata of an 'Agilent' UV file without its data
#'
#' The records of a delta-encoded file are walked to find the times of the first
#' and last spectra. The scaling of a `131_OL` file depends on its data, which
#' are read in full.
#' @noRd
read_chemstation_uv_metadata <- function(path,
                                         metadata_format = c("chromconverter",
                                                             "raw"),
                                         scale = TRUE){
  f <- file(path, "rb")
  on.exit(close(f))
  hdr <- read_cs_uv_header(f)
  if (hdr$file_version == "131_OL"){
    x <- read_chemstation_uv(path, metadata_format = metadata_format,
                             scale = scale)
    x <- transfer_metadata(matrix(numeric(0), 0, 0), x)
    attr(x, "format_out") <- NULL
    attr(x, "data_format") <- NULL
    return(x)
  }
  metadata_format <- check_metadata_format(metadata_format, "chemstation")
  seek(f, where = hdr$offsets$data_start, origin = "start")
  times <- read_uv_delta_index(f, hdr$nval)$times / 60000
  meta <- cs_uv_metadata(path, hdr, time_range = times[c(1, hdr$nval)],
                         scaling_value = hdr$scaling_value)
  metadata_stub(meta, format_in = metadata_format, parser = "chromconverter",
                source_file = path,
                source_file_format = paste0("chemstation_", hdr$file_version),
                scale = scale)
}

#' Locate the records of an 'Agilent' delta-encoded DAD array
#' @noRd
read_uv_delta_index <- function(f, nval){
  start <- seek(f, NA, "current")
  seek(f, 0, "end")
  fsize <- seek(f, NA, "current")
  seek(f, start, "start")

  rw <- readBin(f, "raw", n = fsize - start)
  v <- readBin(rw, "integer", n = length(rw) %/% 2L, size = 2, signed = TRUE,
               endian = "little")

  starts <- integer(nval)
  lens <- integer(nval)
  p <- 0L
  for (i in seq_len(nval)){
    starts[i] <- p
    lens[i] <- v[p + 2L]
    p <- p + lens[i] %/% 2L
  }

  tidx <- rep(starts * 2L + 4L, each = 4L) + rep(1:4, nval)
  times <- readBin(rw[tidx], "integer", n = nval, size = 4, endian = "little")
  list(rw = rw, v = v, starts = starts, lens = lens, times = times)
}

#' Decode 'Agilent' delta-encoded DAD array
#' @author Ethan Bass
#' @noRd
decode_uv_delta <- function(f, nval, ncol){
  index <- read_uv_delta_index(f, nval)
  rw <- index$rw
  v <- index$v
  u <- readBin(rw, "integer", n = length(rw) %/% 2L, size = 2, signed = FALSE,
               endian = "little")
  starts <- index$starts
  lens <- index$lens
  times <- index$times

  nslots <- lens %/% 2L - 11L
  idx <- sequence(nslots, from = starts + 12L)
  d <- v[idx]

  esc <- resolve_escape_positions(d, -32768L, 2L)
  absval <- if (length(esc)){
    u[idx[esc + 1L]] + v[idx[esc + 2L]] * 65536
  } else numeric(0)

  is_val <- rep(TRUE, length(d))
  if (length(esc)) is_val[c(esc + 1L, esc + 2L)] <- FALSE

  reset <- logical(length(d))
  reset[c(1L, head(cumsum(nslots), -1L) + 1L)] <- TRUE
  reset[esc] <- TRUE

  delta <- as.numeric(d)
  delta[!is_val] <- 0
  delta[reset] <- 0

  rp <- which(reset)
  rval <- numeric(length(rp))
  rval[match(esc, rp)] <- absval
  rs <- setdiff(rp, esc)
  rval[match(rs, rp)] <- as.numeric(d[rs])

  data <- matrix(cumsum_with_resets(delta, reset, rval)[is_val],
                 nrow = nval, byrow = TRUE)
  rownames(data) <- times / 60000
  data
}

#' Decode 'Agilent ChemStation' DAD array
#' @author Ethan Bass
#' @noRd
decode_uv_array <- function(f, nval, ncol){
  start <- seek(f, NA, "current")
  seek(f, 0, "end")
  fsize <- seek(f, NA, "current")
  seek(f, start, "start")

  stride <- 22L + 8L * ncol
  rw <- readBin(f, "raw", n = fsize - start)
  if (length(rw) < nval * stride){
    stop("'Agilent' UV file is shorter than its header declares.")
  }
  m <- matrix(rw[seq_len(nval * stride)], nrow = stride)

  reclen <- readBin(as.vector(m[3:4, ]), "integer", n = nval, size = 2,
                    signed = FALSE, endian = "little")
  if (any(reclen != stride)){
    stop("Unexpected record length in 'Agilent' UV file.")
  }

  times <- readBin(as.vector(m[5:8, ]), "integer", n = nval, size = 4,
                   endian = "little")
  data <- matrix(readBin(as.vector(m[-seq_len(22L), ]), "double",
                         n = nval * ncol, size = 8, endian = "little"),
                 nrow = nval, byrow = TRUE)
  rownames(data) <- times / 60000
  data
}
