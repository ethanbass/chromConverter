#' Read 'Chromatotec' file
#'
#' Reads 'Chromatotec' \code{.Chrom} files.
#'
#' @param path Path to 'Chromatotec' \code{.Chrom} file.
#' @param what Whether to extract chromatograms (\code{chrom}) and/or
#' \code{peak_table} data. Accepts multiple arguments.
#' @param format_out R format. Either \code{matrix}, \code{data.frame}, or
#' \code{data.table}.
#' @param data_format Either \code{wide} (default) or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element.
#' @return A chromatogram and/or peak table from the specified
#' \code{path},  according to the value of \code{what}. Chromatograms are
#' returned in the format specified by \code{format_out}.
#' @author Ethan Bass
#' @note This function is a work in progress and the accuracy of the results is
#' not guaranteed.
#' @examples \dontrun{
#' read_chromatotec(path)
#' }
#' @export

read_chromatotec <- function(path, what = c("chrom", "peak_table"),
                             format_out = c("matrix", "data.frame",
                                            "data.table"),
                             data_format = c("wide","long"),
                             read_metadata = TRUE,
                             metadata_format = c("chromconverter", "raw"),
                             collapse = TRUE){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chromatotec",
                            raw = "raw")
  what <- match.arg(what, c("chrom", "peak_table"), several.ok = TRUE)
  f <- file(path, "rb")
  on.exit(close(f))
  magic <- readBin(f, "raw", n = 10)
  if (rawToChar(magic) != "CHROMFILEV"){
    stop("The supplied file does not appear to be in Chromatotec CHROM format")
  }
  version <- readBin(f, "raw", n = 2)
  version <- paste0(version,collapse=".")
  if (version != "06.01"){
    warning(sprintf("Version %s not recognized. Proceed with caution.",
                    version), immediate. = TRUE)
  }
  offsets <- readBin(f, what = "int", n = 10, size = 4)
  meta <- read_chromatotec_metadata(f, offset = offsets[[1]])
  meta$version <- version
  if (any(what == "chrom")){
    chrom <- read_chromatotec_chrom(f, offset = 318)
    rts <- seq(from = 0, length.out = length(chrom), by=1/meta$Sampling_rate)
    chrom <- format_2d_chromatogram(rts, chrom, data_format = data_format,
                           format_out = format_out)
  }
  if (any(what == "peak_table")){
    peak_table <- read_chromatotec_table(f, offset = offsets[[3]])
  }
  dat <- mget(what)
  if (read_metadata){
    dat <- lapply(dat, function(x) attach_metadata(x, meta = meta,
                                                   format_in = metadata_format,
                                                   format_out = format_out,
                                                   data_format = data_format,
                                                   parser = "chromConverter",
                                                   source_file = path,
                                                   source_file_format = "chromatotec",
                                                   scale = FALSE))
  }

  if (collapse){
    dat <- collapse_list(dat)
  }
  dat
}

#' Read Chromatotec metadata
#' @author Ethan Bass
#' @noRd
read_chromatotec_metadata <- function(f, offset){
  metadata_offsets <- c(Serial_no = 58,
                        Location = 80,
                        Operator = 107,
                        Method = 168,
                        Description = 177,
                        SubstanceTableName = 204,
                        Sampling_rate = 249,
                        Sampling_duration = 301
                        )
  meta <- purrr::imap(metadata_offsets, function(offset, n){
    seek(f, offset)
    readBin(f,
            what = ifelse(n %in% c("Sampling_rate", "Sampling_duration"),
                          "int", "character"),
            size = 2, endian = switch(n, "Sampling_rate" = "big",
                                      "Sampling_duration" = "little",
                                      "little"))
  })
  meta
}

#' Skip Chromatotec NULL bytes (5b5d)
#' @noRd
skip_null_bytes_chromatotec <- function (f) {
  while (TRUE) {
    bin <- readBin(f, "raw", n = 2)
    if (paste0(as.character(bin), collapse = "") != "5b5d") {
      seek(f, -2, origin = "current")
      break
    }
  }
}

#' Read Chromatotec table row
#' @author Ethan Bass
#' @noRd
read_chromatotec_table_row <- function(f){
  dat <- readBin(f, what = "double", n = 9, size = 4)
  readBin(f, what = "raw", n = 2) # skip
  name <- readBin(f, what = "char")
  # seek(f,NA)
  skip_null_bytes(f)
  # seek(f,NA)
  unit <- readBin(f, what = "char")
  # seek(f,NA)
  readBin(f, what = "raw", n = 3)
  c(dat, name, unit)
}

# seek(f, 154164)
#' Read Chromatotec table
#' @noRd
#' @author Ethan Bass
read_chromatotec_table <- function(f, offset){
  seek(f, offset)
  readBin(f, what="raw", n=4) # section header
  skip_null_bytes_chromatotec(f)
  n_rows <- readBin(f, what = "int", size=2)
  df <- as.data.frame(do.call(rbind, lapply(seq_len(n_rows), function(x){
    read_chromatotec_table_row(f)
  })))
  colnames(df) <- c("U1","U2","U3","Rt","U4","U5",
                    "Area","U6","Result","Substance","Unit")
  df[,c(1:9)] <- apply(df[,c(1:9)], 2, as.numeric)
  df$Unit <- iconv(df$Unit, from = "latin1", to = "UTF-8")
  df
}

#' Read Chromatotec chromatogram
#' @author Ethan Bass
#' @noRd
read_chromatotec_chrom <- function(f, offset){
  seek(f, offset)
  readBin(f, what="raw", n=4) # section header
  skip_null_bytes_chromatotec(f)
  n_val <- readBin(f, what = "int", size = 2,
                   signed = FALSE, endian = "little")
  unk <- readBin(f, what = "int", size = 2, n = 4,
                 signed = FALSE, endian = "little")
  chrom <- readBin(f, "int", n = (n_val - 1), size = 4, endian = "little")
}
