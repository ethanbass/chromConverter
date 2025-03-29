#' Read 'Waters' RAW
#'
#' Reads 'Waters MassLynx' (\code{.raw}) files into R.
#'
#' @importFrom stats setNames
#' @param path Path to Waters \code{.raw} file.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @note For now this parser only reads 1D chromatograms (not mass spectra or
#' DAD data) and does not support parsing of metadata from 'Waters' RAW files.
#' @author Ethan Bass
#' @family 'Waters' parsers
#' @export

read_waters_raw <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                             data_format = c("wide", "long"),
                             read_metadata = TRUE,
                             metadata_format = c("chromconverter", "raw")){

  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(tolower(metadata_format),
                               c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "waters_raw", raw = "raw")
  uv_paths <- list.files(path, pattern="_CHRO", full.names = TRUE, ignore.case = TRUE)
  meta_path <- grep("\\.INF$", uv_paths, value = TRUE, ignore.case = TRUE)
  uv_paths <- grep("\\.INF$", uv_paths, invert = TRUE, value = TRUE, ignore.case = TRUE)

  if (read_metadata){
    hdr_path <- list.files(path, pattern="_HEADER.TXT",
                           full.names = TRUE, ignore.case = TRUE)
    hdr <- readLines(hdr_path)
    hdr <- gsub("\\$\\$ ", "", hdr)
    hdr <- stringr::str_split_fixed(hdr, ":", n = 2)
    hdr[,2] <- gsub("^ ", "", hdr[,2])
    hdr[hdr[,2] == "", 2] <- NA
    hdr[,1] <- gsub(" ", "_", hdr[,1])
    hdr <- as.list(setNames(hdr[,2], hdr[,1]))
  }

  dat <- lapply(uv_paths, read_waters_chro, format_out = format_out,
                data_format = data_format)

  meta <- readLines(meta_path, skipNul = TRUE, warn = FALSE,
                    encoding = "Latin-1")
  meta <- iconv(meta, sub = "")
  meta <- strsplit(meta, "\001")[[1]][-c(1:3)]
  nms <- gsub("^ |\\$CC\\$", "", sapply(strsplit(meta, ","), `[`, 1))

  if (read_metadata){
    detector_unit <- sapply(strsplit(meta, ","), `[`, 6)
    dat <- lapply(seq_along(dat), function(i){
      attach_metadata(x = dat[[i]], meta = c(hdr, Detector_Unit = detector_unit[i]),
                       format_in = metadata_format,
                       format_out = format_out,
                       data_format = data_format,
                       parser = "chromconverter",
                       source_file = path,
                       source_file_format = "waters_raw",
                       scale = FALSE)
    })
  }
  names(dat) <- gsub("^\\([0-9]+\\)\\s*", "", nms)
  dat
}

#' Read 'Waters' chromatograms
#'
#' Parser for reading 'Waters MassLynx' CHRO (.dat) files into R.
#'
#' @importFrom utils head tail
#' @param path Path to \code{.dat} file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @noRd

#magic 80000100 08000200

read_waters_chro <- function(path, format_out = "data.frame",
                            data_format = c("wide", "long")){

  data_format <- match.arg(data_format, c("wide", "long"))

  f <- file(path, "rb")
  on.exit(close(f))

  seek(f, 0, "end")
  end <- seek(f, 0, "end")

  seek(f, 128, "start")
  start <- seek(f, 128, "start")

  x <- readBin(f, "numeric", size = 4, n = (end - start)/4)
  times <- x[seq(1, length(x), by = 2)]
  int <- x[seq(2, length(x), by = 2)]
  dat <- format_2d_chromatogram(rt = times, int = int,
                         data_format = data_format, format_out = format_out)
  dat
}
