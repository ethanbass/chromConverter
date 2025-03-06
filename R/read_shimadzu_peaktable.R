#' Read Shimadzu Peak Tables
#' @author Ethan Bass
#' @noRd
read_sz_tables <- function(path, format_out = "data.frame"){
  existing_streams <- check_streams(path, what = "peaks")
  if (length(existing_streams) == 0){
    stop("Peak table streams could not be detected.")
  }
  pktab <- lapply(existing_streams, function(stream){
    tryCatch({read_sz_table(path, stream)}, error = function(e){
      message(sprintf("Unable to parse `%s`.", paste(stream,collapse=", ")))
      NA
    })
  })
  names(pktab) <- sapply(existing_streams, `[[`, 2)
  pktab
}

#' Read Shimadzu Peak Table
#'
#' There are at least two Shimadzu peak table formats. The first (\code{V0}),
#' does not begin with a magic number, whereas the second (\code{V1}) starts
#' with the magic number \code{x56/x45/x52/x31} (which spells out 'VER1').
#'
#' In V0, the first 4 bytes of the stream are a 4 byte integer specifying the
#' number of peaks in the peak table. In V1, the peak number directly follows
#' the magic number.
#'
#' @noRd

read_sz_table <- function(path, stream, format_out = "data.frame"){
  path_raw <- export_stream(path, stream)
  f <- file(path_raw, "rb")
  on.exit(close(f))
  magic <- readBin(f, "raw", n = 4)
  magic <- paste(paste0("x", as.character(magic)), collapse = "/")
  read_sz_table <- switch(magic,
                          "x56/x45/x52/x31" = read_sz_table_v1,
                          read_sz_table_v0)
  read_sz_table(f, format_out = format_out)
}

#' Read Shimadzu Peak Table 'VER1'
#' @noRd
read_sz_table_v1 <- function(f, format_out = "data.frame"){
  rows <- readBin(f, "integer", size = 4)
  seek(f,0,origin = "end")
  n_bytes <- seek(f, 0, origin = "end")
  block_len <- (n_bytes - 20)/rows
  seek(f, 20)
  tab <- do.call(rbind, lapply(seq_len(rows), function(i){
    read_sz_table_block_v1(f, block_len)
  }))
  tab$ID <- ifelse(tab$ID == 0, NA, tab$ID)
  if (format_out == "data.frame"){
    tab <- as.data.frame(tab)
  }
  tab
}

#' Read Shimadzu Peak Table 'VER0'
#' @noRd
read_sz_table_v0 <- function(f, format_out = "data.frame"){
  seek(f,0)
  rows <- readBin(f, "integer", size = 4, endian = "little")
  readBin(f, "integer", size = 4) #skip
  tab <- do.call(rbind, lapply(seq_len(rows), function(i){
    read_sz_table_block_v0(f)
  }))
  if (format_out == "data.frame"){
    tab <- as.data.frame(tab)
  }
  tab
}

#' Read Shimadzu Table Block 'VER0'
#' @author Ethan Bass
#' @noRd
read_sz_table_block_v0 <- function(f){
  readBin(f, "integer", size = 4, endian = "little")
  R.time <- readBin(f, "integer", size = 4, endian = "little")/60000
  Area <- readBin(f, "numeric", size = 8, endian = "little")
  readBin(f, "numeric", size = 8, endian = "little")
  Height <- readBin(f, "numeric", size = 8, endian = "little")
  readBin(f, "numeric", size = 8, endian = "little")
  unknown_ints <- readBin(f, "integer", size = 4, n=4, endian = "little")
  I.time <- readBin(f, "integer", size = 4, endian = "little")/60000
  F.time <- readBin(f, "integer", size = 4, endian = "little")/60000
  AH <- readBin(f, "integer", size = 4, endian = "little")/1000
  seek(f, 148, "current") #skip 148 bytes
  Plate.no <- readBin(f, "numeric", size = 8, endian = "little")
  Plate.ht <- readBin(f, "numeric", size = 8, endian = "little")
  Tailing <- readBin(f, "numeric", size = 8, endian = "little")
  Resolution <- readBin(f, "numeric", size = 8, endian = "little")
  Sep.factor <- readBin(f, "numeric", size = 8, endian = "little")
  Conc.percent <- readBin(f, "numeric", size = 8, endian = "little")
  Conc.norm <- readBin(f, "numeric", size = 8, endian = "little")
  unknown_ints <- readBin(f, "integer", size = 4, n=2, endian = "little")
  data.frame(R.time, Area, Height, I.time, F.time, AH, Plate.no, Plate.ht,
             Tailing, Resolution, Sep.factor)
}

#' Read Shimadzu Table Block 'VER1'
#' @author Ethan Bass
#' @noRd
read_sz_table_block_v1 <- function(f, block_len){
  readBin(f, "integer", size = 4, endian = "little")
  R.time <- readBin(f, "integer", size = 4, endian = "little")/60000
  Area <- readBin(f, "numeric", size = 8, endian = "little")
  readBin(f, "numeric", size = 8, endian = "little")
  Height <- readBin(f, "numeric", size = 8, endian = "little")
  readBin(f, "numeric", size = 8, endian = "little")
  unknown_ints <- readBin(f, "integer", size = 4, n=4, endian = "little")
  I.time <- readBin(f, "integer", size = 4, endian = "little")/60000
  F.time <- readBin(f, "integer", size = 4, endian = "little")/60000
  AH <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 104, "current")
  Conc <- readBin(f, "numeric", size = 8, endian = "little")
  ID <- readBin(f, "integer", size = 4, endian = "little")
  seek(f, 52, "current")
  k <- readBin(f, "numeric", size = 8, endian = "little")
  Plate.no <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 56, "current")
  unk1 <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 56, "current")
  Plate.ht <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 56, "current")
  unk2 <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 56, "current")
  Tailing <- readBin(f, "numeric", size = 8, endian = "little")
  Resolution <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 48, "current")
  unk3 <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 56, "current")
  unk4 <- readBin(f, "numeric", size = 8, endian = "little") #Area ratio?
  Sep.factor <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, 64, "current")
  Percent.conc <- readBin(f, "numeric", size = 8, endian = "little")
  Norm.conc <- readBin(f, "numeric", size = 8, endian = "little")
  seek(f, (block_len - 728), "current")
  data.frame(R.time, Area, Height, I.time, F.time, AH, Conc, ID, k, Plate.no,
             Plate.ht, Tailing, Resolution, Sep.factor, Percent.conc, Norm.conc)
}
