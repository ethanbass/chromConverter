#' Parser for reading Agilent FID (.ch) files into R
#' @param path Path to \code{.ch} file
#' @param read_metadata Logical. Whether to attach metadata.
#' @param format_out Matrix or data.frame
#' @author Ethan Bass
#' @note This function benefited greatly from the schematic shared by the developers
#' of the rainbow project (https://rainbow-api.readthedocs.io/en/latest/agilent/ch_fid.html).
#' @export
read_chemstation_fid <- function(path, read_metadata=TRUE,
                                 format_out = c("matrix","data.frame")){

  f <- file(path, "rb")
  on.exit(close(f))

  # HEADER
  version<-readBin(f, "raw", n = 4)
  version <- paste(sapply(version[2:4], rawToChar),collapse="")

  endian <- switch(version,
                   "179" = "little",
                   "180" = "big",
                   "181" = "big")
  seek(f, where = 0x15C, origin="start")
  filetype <- readBin(f, "character", n=20)
  filetype <- as.character(paste(filetype, collapse=""))

  #notebook name
  seek(f, where = 0x35A, origin="start")
  notebook <- cc_trim_str(cc_collapse(readBin(f, "character", n=100)))

  #Parent directory
  seek(f, where = 0x758, origin="start")
  dir <- cc_trim_str(cc_collapse(readBin(f, "character", n=100)))

  #date
  seek(f, where = 0x957, origin="start")
  date <- cc_trim_str(cc_collapse(readBin(f, "character", n=30)))

  seek(f, where = 0x9BC, origin="start")
  instrument <- cc_trim_str(cc_collapse(readBin(f, "character", n=100)))

  # seek(f, where = 0x9E5, origin="start")
  # GC<-readBin(f, "character", n=2)

  seek(f, where = 0xA0E, origin="start")
  acq_method <- cc_trim_str(cc_collapse(readBin(f, "character", n=100)))

  seek(f, where = 0xC11, origin="start")
  software <- cc_trim_str(cc_collapse(readBin(f, "character", n=100)))

  seek(f, where = 0x104C, origin="start")
  unit <- cc_trim_str(cc_collapse(readBin(f, "character", n=2)))

  seek(f, where = 0x1075, origin="start")
  signal <- cc_trim_str(cc_collapse(readBin(f, "character", n=5)))

  #Number of data values
  seek(f, where = 0x116, origin="start")
  nval <- readBin(f, "int", n=1, endian = "big", signed=2)

  #First retention time (ms)
  seek(f, where = 0x11A, origin="start")
  # readBin(f, "numeric", n=100, endian = "big")
  start <- readBin(f, "numeric", n=1, endian = "big", size=4)*1.6667E-5

  #Last retention time (ms)
  seek(f, where = 0x11E, origin="start")
  # readBin(f, "numeric", n=100, endian = "big")
  end <- readBin(f, "numeric", n=1, endian = "big", size=4)*1.6667E-5

  #Scaling factor
  seek(f, where = 0x127C, origin="start")
  scaling_value <- readBin(f, "double", n=1, endian = "big")

  # BODY
  seek(f, where = 0x1800, origin="start")
  seek(f, where = 0x1800, origin="start")

  dat <- readBin(f, "raw", n=1000000)
  nums <- lapply(seq(1, length(dat),4), function(x) x:(x+3))
  en <- switch(endian, "little" = sort(seq_along(nums[[1]]), decreasing = TRUE),
         "big" = sort(seq_along(nums[[1]]), decreasing = FALSE))

  xx <- sapply(nums, function(n){
    as.numeric(paste0("0x", paste(dat[n][en], collapse = "")))
  })

  xx <- xx[seq(2,length(xx),2)]
  rts <- seq(start, end, (end-start)/length(xx))
  df <- data.frame(rt=rts[seq_along(xx)], value=xx*scaling_value)
  if (format_out == "matrix"){
    df <- as.matrix(df)
  }
  if (read_metadata){
    df <- structure(df, version=version, file.type=filetype,
              notebook = notebook, parent.directory = dir,
              run_date = date, instrument=instrument, method = acq_method,
              software=software, unit=unit, signal=signal, time_range = c(start,end),
              data_format = "long", parser="chromConverter")
  }
  df
}

#' @noRd
cc_collapse <- function(x){
  paste(x, collapse="")
}
#' @noRd
cc_trim_str <- function(x, len=2){
  substr(x, len, nchar(x))
}

#' @noRd
# check for .D folder
get_chemstation_dir_name <- function(path){
  dir <- gsub(basename(path), "", path)
  sp <- str_split_fixed(dir, "/", stringr::str_count(dir,"/")+1)[1,]
  grep("\\.D|\\.d$", sp, ignore.case = TRUE,value = TRUE)
}
