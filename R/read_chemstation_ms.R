#' Read 'Agilent ChemStation' MS file.
#'
#' Reads 'Agilent ChemStation MSD Spectral Files' beginning with
#' \code{x01/x32/x00/x00}.
#'
#' @param path Path to 'Agilent' \code{.ms} file.
#' @param what What stream to get: current options are \code{MS1}, \code{BPC}
#' and/or \code{TIC}. If a stream is not specified, the function will return all
#' streams.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to attach metadata. Defaults to \code{TRUE}.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element. Defaults to \code{TRUE}.
#' @author Ethan Bass
#' @return A 2D chromatogram in the format specified by \code{data_format} and
#' \code{format_out}. If \code{data_format} is \code{wide}, the chromatogram will
#' be returned with retention times as rows and a single column for the intensity.
#' If \code{long} format is requested, two columns will be returned: one for the
#' retention time and one for the intensity. The \code{format_out} argument
#' determines whether the chromatogram is returned as a \code{matrix},
#' \code{data.frame}, or \code{data.table}. Metadata can be attached to the
#' chromatogram as \code{\link{attributes}} if \code{read_metadata} is \code{TRUE}.
#' @author Ethan Bass
#' @note Many thanks to Evan Shi and Eugene Kwan for providing helpful
#' information on the structure of these files in the
#' \href{https://rainbow-api.readthedocs.io/en/latest/agilent/ms.html}{rainbow documentation}.
#' @family 'Agilent' parsers
#' @examples \dontrun{
#' read_chemstation_ms(path)
#' }
#' @export

read_chemstation_ms <- function(path, what = c("MS1", "BPC", "TIC"),
                                format_out = c("matrix", "data.frame",
                                                     "data.table"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                collapse = TRUE){
  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chemstation",
                            raw = "raw")
  match.arg(what, c("MS1", "BPC", "TIC"), several.ok = TRUE)
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

  seek(f, header_len, "start")

  dat <- lapply(seq_len(n_rt), function(i){
    read_cs_ms_block(f)
  })
  if (any(what == "MS1"))
    MS1 <- do.call(rbind, lapply(dat, "[[", 1))

  if (any(what == "BPC")){
    BPC <- do.call(rbind, lapply(dat, "[[", 2))
    BPC[,2] <- BPC[,2]/20
    BPC[,3] <- sapply(BPC[,3], ms_bit_shift)
    colnames(BPC) <- c("rt", "mz", "intensity")
  }

  if (any(what == "TIC")){
    TIC <- do.call(rbind, lapply(dat, "[[", 3))
    colnames(TIC) <- c("rt", "intensity")
  }

  dat <- mget(what)
  dat <- lapply(dat, function(x){
    convert_chrom_format(x, format_out = format_out)
  })

  if (read_metadata){
    meta_slots <- switch(version, "2" = 9)

    meta <- lapply(offsets[seq_len(meta_slots)], function(offset){
      seek(f, where = offset, origin = "start")
      read_cs_string(f, type = 1)
    })
    meta$date <- as.POSIXct(strptime(meta$date, format="%d %b %y %I:%M %p %z"),
                            tz="UTC")
    meta$detector <- "MS"
    dat <- lapply(dat, function(x){
      attach_metadata(x, meta, format_in = metadata_format,
                            data_format = data_format, format_out = format_out,
                            parser = "chromconverter", source_file = path,
                            source_file_format = paste0("chemstation_", version),
                            scale = FALSE)
    })
  }
  if (collapse) dat <- collapse_list(dat)
  dat
}

#' Read 'Agilent Chemstation' MS block
#' @author Ethan Bass
#' @noRd
#' @note Many thanks to the rainbow team for providing helpful information on the
#' structure of this file.
read_cs_ms_block <- function(f){
  start <- seek(f, NA)
  block_length <- readBin(f, what = "integer", size = 2,
                          signed = FALSE, endian = "big")*2
  rt <- readBin(f, what = "integer", size = 4, endian = "big")/60000
  u1 <- readBin(f, what = "integer", size = 4, endian = "big")
  n_row <- readBin(f, what = "integer", size = 4, endian = "big")
  bpc <- c(rt, readBin(f, what = "integer", n = 2, size = 2, endian = "big"))
  mat <- matrix(NA, nrow = n_row, ncol = 2, dimnames = list(NULL, c("mz", "intensity")))
  for (i in seq_len(n_row)){
    mat[i,] <- readBin(f, what = "integer", size = 2, n = 2,
                       signed = FALSE, endian = "big")
  }
  mat[,"mz"] <- mat[,"mz"]/20
  mat[,"intensity"] <- sapply(mat[,"intensity"], ms_bit_shift)

  u3 <- readBin(f, what="raw", n=6)
  tic <- c(rt, readBin(f, what = "integer", size = 4, endian = "big"))
  end <- seek(f, NA)
  stopifnot(start + block_length == end)
  list(MS1 = cbind(rt, mat), BPC = bpc, TIC = tic)
}

#' Chemstation MS bit shift
#' @noRd
ms_bit_shift <- function(int){
  int_heads <- bitwShiftR(int, 14)
  if (int_heads != 0){
    int_tails <- bitwAnd(int, 0x3FFF)
    8^int_heads * int_tails
  }
  else int
}
