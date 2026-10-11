#' Read 'Chromatotec' file
#'
#' Reads 'Chromatotec' `.Chrom` files.
#'
#' @inheritParams shared_params
#' @param path The path to 'Chromatotec' `.Chrom` file.
#' @param what Whether to extract chromatograms (`chrom`) and/or `peak_table`
#' data. Accepts multiple arguments, and defaults to both.
#' @return A chromatogram and/or peak table from the specified `path`, according
#' to the value of `what`, with times in minutes. Chromatograms are returned in
#' the format specified by `format_out`. The rows of the peak table that
#' report a total over most of the run rather than a peak, such as
#' `NMTHC-EqC6` and `THC_PAMS`, are only kept with
#' `peaktable_format = "original"`. In the original table, `Start` and `End`
#' are where integration of the peak starts and ends, and `SubstanceIndex`
#' appears to be the position of its substance in the run's substance table
#' (`0` for an unnamed peak); `U1`, `U3` and `U4` have not been identified.
#' @author Ethan Bass
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
                             collapse = TRUE,
                             peaktable_format = c("chromatographr",
                                                  "original")){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "chromatotec")
  peaktable_format <- match.arg(peaktable_format, c("chromatographr",
                                                    "original"))
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
    rts <- seq(from = 0, length.out = length(chrom),
               by = 1 / meta$Sampling_rate) / 60
    chrom <- format_2d_chromatogram(rts, chrom, data_format = data_format,
                           format_out = format_out)
  }
  if (any(what == "peak_table")){
    peak_table <- read_chromatotec_table(f, offset = offsets[[3]])
    if (peaktable_format == "chromatographr"){
      peak_table <- chromatotec_chromatographr(peak_table)
    }
  }
  dat <- mget(what)
  if (read_metadata){
    dat <- lapply(dat, function(x) attach_metadata(x, meta = meta,
                                                   format_in = metadata_format,
                                                   format_out = format_out,
                                                   data_format = data_format,
                                                   parser = "chromconverter",
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
  colnames(df) <- c("U1", "Start", "U3", "Rt", "U4", "End",
                    "Area", "SubstanceIndex", "Result", "Substance", "Unit")
  df[, c(1:9)] <- apply(df[, c(1:9)], 2, as.numeric)
  df[c("Start", "Rt", "End")] <- df[c("Start", "Rt", "End")] / 60
  df$SubstanceIndex <- vapply(df$SubstanceIndex, function(x){
    readBin(writeBin(x, raw(), size = 4), "integer", size = 4) %/% 65536L
  }, integer(1))
  df$Unit <- iconv(df$Unit, from = "latin1", to = "UTF-8")
  df
}

#' Convert a 'Chromatotec' peak table to the `chromatographr` layout
#'
#' Rows whose retention time is not inside their own integration window, such
#' as the `NMTHC` and `THC_PAMS` totals, are not peaks and are dropped.
#' @noRd
chromatotec_chromatographr <- function(x){
  peak <- which(x$Start < x$Rt & x$Rt < x$End)
  out <- chromatographr_peak_table(x[peak, , drop = FALSE],
                                   list(rt = "Rt", start = "Start",
                                        end = "End", area = "Area"))
  rownames(out) <- NULL
  out
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
