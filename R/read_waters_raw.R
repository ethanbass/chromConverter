#' Read 'Waters' RAW
#'
#' Parser for reading 'Waters MassLynx (.raw) files into R.
#'
#' @param path Path to \code{.raw} file.
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
#' @export

read_waters_raw <- function(path, format_out = c("matrix", "data.frame", "data.table"),
                             data_format = c("wide", "long"),
                             read_metadata = TRUE,
                             metadata_format = c("chromconverter", "raw")){

  format_out <- check_format_out(format_out)
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))

  uv_paths <- list.files(path, pattern="_CHRO", full.names = TRUE)
  meta_path <- grep("\\.INF", uv_paths, value = TRUE)
  uv_paths <- grep("\\.INF", uv_paths, invert = TRUE, value = TRUE)

  dat <- lapply(uv_paths, read_waters_chro, format_out = format_out,
                data_format = data_format, read_metadata = read_metadata,
                metadata_format = metadata_format)

  meta <- readLines(meta_path, skipNul = TRUE, warn = FALSE,
                    encoding = "Latin-1")
  meta <- iconv(meta, sub = "")
  meta <- strsplit(meta,"\\([0-9]\\)")[[1]][-1]
  meta <- gsub("^ |\\$CC\\$", "", sapply(strsplit(meta, ","), function(x) x[1]))

  names(dat) <- meta
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
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @noRd

#magic 80000100 08000200

read_waters_chro <- function(path, format_out = "data.frame",
                            data_format = c("wide", "long"),
                            read_metadata = TRUE,
                            metadata_format = c("chromconverter", "raw")){

  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  # metadata_format <- switch(metadata_format,
  #                           chromconverter = "waters_uv", raw = "raw")

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
