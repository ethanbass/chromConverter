#' Read 'Agilent ChemStation' MS files
#'
#' Reads 'Agilent ChemStation MSD Spectral Files' beginning with
#' `x01/x32/x00/x00`.
#'
#' @inheritParams shared_params
#' @param path Path to 'Agilent' `.ms` file.
#' @param what Which streams to return: `MS1`, `BPC` and/or `TIC`. Defaults to
#' all three.
#' @param data_format Whether to return the `BPC` and `TIC` in `long` (default)
#' or `wide` format. Mass spectra are always returned in `long` format.
#' @return A list of the streams in `what`, in the format specified by
#' `format_out`. In wide format, the `BPC` and `TIC` have retention times as
#' rows and a single intensity column; in long format, a retention time column
#' and an intensity column. MS data will always be returned in long format. The
#' `format_out` argument determines whether the chromatogram is returned as a
#' `matrix`, `data.frame`, or `data.table`. Metadata are attached as [attributes] if
#' `read_metadata` is `TRUE`. With `collapse = TRUE`, a list of one stream is
#' replaced by that stream.
#' @author Ethan Bass
#' @note Many thanks to Evan Shi and Eugene Kwan for providing helpful
#' information on the structure of these files in the [rainbow documentation](
#' https://rainbow-api.readthedocs.io/en/latest/agilent/ms.html).
#' @family 'Agilent' parsers
#' @examples \dontrun{
#' read_chemstation_ms(path)
#' }
#' @export

read_chemstation_ms <- function(path, what = c("MS1", "BPC", "TIC"),
                                format_out = c("matrix", "data.frame",
                                                     "data.table"),
                                data_format = "long",
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                collapse = TRUE){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "chemstation")
  what <- match.arg(what, c("MS1", "BPC", "TIC"), several.ok = TRUE)
  f <- file(path, "rb")
  on.exit(close(f))

  # HEADER
  version <- read_cs_string(f, pos = 0)
  # detector <- read_cs_string(f, type = 1, pos=4)
  # version <- paste(version, strsplit(detector, " ")[[1]][1],sep ="_")

  version <- match.arg(version, choices = c("2"))

  offsets <- get_agilent_offsets(version)

  # decoder <- switch(version,
  #                   "2" = )
  seek(f, offsets$num_times)
  n_rt <- readBin(f, what = "integer", size = 2, signed = FALSE, endian = "big")

  seek(f, offsets$header_length)
  header_len <- (readBin(f, what = "integer", size = 2,
                        signed = FALSE, endian = "big") - 1)*2

  seek(f, 0, "end")
  fsize <- seek(f, NA, "current")
  seek(f, header_len, "start")

  rw <- readBin(f, "raw", n = fsize - header_len)
  nw <- length(rw) %/% 2L
  wu <- readBin(rw, "integer", n = nw, size = 2, signed = FALSE, endian = "big")
  wi <- readBin(rw, "integer", n = nw, size = 2, signed = TRUE, endian = "big")

  starts <- integer(n_rt)
  pos <- 1L
  for (i in seq_len(n_rt)){
    starts[i] <- pos
    pos <- pos + wu[pos]
  }
  if (pos > nw + 1L){
    stop("'Agilent' MS scan blocks overrun the end of the file.")
  }

  rt <- int32_from_words(wu[starts + 1L], wu[starts + 2L])/60000
  n_row <- int32_from_words(wu[starts + 5L], wu[starts + 6L])
  if (any(wu[starts] != 14L + 2L*n_row)){
    stop("'Agilent' MS block length does not match its peak count.")
  }

  if (any(what == "MS1")){
    pairs <- wu[sequence(2L*n_row, from = starts + 9L)]
    mz <- seq.int(1L, length(pairs), by = 2L)
    MS1 <- cbind(rt = rep.int(rt, n_row), mz = pairs[mz]/20,
                 intensity = ms_bit_shift(pairs[mz + 1L]))
  }
  if (any(what == "BPC")){
    BPC <- cbind(rt = rt, mz = wi[starts + 7L]/20,
                 intensity = ms_bit_shift(wi[starts + 8L]))
  }

  if (any(what == "TIC")){
    TIC <- format_2d_chromatogram(rt = rt,
                                  int = int32_from_words(
                                    wu[starts + 12L + 2L*n_row],
                                    wu[starts + 13L + 2L*n_row]),
                                  data_format = data_format,
                                  format_out = format_out)
  }

  dat <- mget(what)
  dat <- purrr::imap(dat, function(x, h){
    convert_chrom_format(x, data_format = data_format,
                         format_out = ifelse(h != "TIC",
                                             check_format_out_table(format_out),
                                             format_out))
  })

  if (read_metadata){
    meta <- read_chemstation_string_fields(f, offsets, type = 1)
    meta$detector <- "MS"
    dat <- purrr::imap(dat, function(x, h){
      attach_metadata(x, meta, format_in = metadata_format,
                      data_format = ifelse(h != "TIC", "long", data_format),
                      format_out = ifelse(h != "TIC",
                                          check_format_out_table(format_out),
                                          format_out),
                      parser = "chromconverter",
                      source_file = path,
                      source_file_format = paste0("chemstation_", version),
                      scale = FALSE)
    })
  }
  if (collapse){
    dat <- collapse_list(dat)
  }
  dat
}

#' 'ChemStation' MS bit shift
#' @noRd
ms_bit_shift <- function(int){
  int_heads <- bitwShiftR(int, 14)
  shifted <- int_heads != 0
  out <- as.numeric(int)
  out[shifted] <- 8^int_heads[shifted] * bitwAnd(int[shifted], 0x3FFF)
  out
}
