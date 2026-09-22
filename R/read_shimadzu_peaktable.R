#' Read 'Shimadzu' peak tables
#'
#' Read the integration results 'LabSolutions' stored alongside the raw data
#' in a 'Shimadzu' OLE container (`.lcd` or `.gcd`), one table per channel.
#'
#' Each table lives in its own stream, a short header followed by one
#' fixed-length record per peak. A chromatographic table is named for the
#' channel it belongs to (`PT-LC.1.1.DET.1.CH#1`, matching that channel's
#' `channel_id` metadata), while the tables a mass spectrometry run produces
#' are named for what they hold (`Mass Peak Table`, `Compound Peak Table`).
#'
#' There are two layouts. **V0** has no magic number and opens with an 8-byte
#' header: the peak count as a `uint32`, then four unparsed bytes. **V1** opens
#' with the magic number `56 45 52 31` (`VER1`), the peak count as a `uint32`,
#' and twelve further unparsed bytes, for a 20-byte header.
#'
#' Despite the `VER1` magic number, the two are not successive versions of one
#' format: a single file can hold both. `shimadzu_qtof_neg.lcd` carries a V1
#' `PT-PDA.1.1.PDA.1.1` alongside a V0 `Mass Peak Table`, written by the same
#' software at the same time. What the layouts track is the kind of table --- a
#' `PT-` channel table is V1 in every file seen so far, and a mass spectrometry
#' table is V0 --- which also explains why V0 has no identification fields
#' (`Conc`, `ID`, `k`). The parser still dispatches on the magic number rather
#' than on the name of the stream, since the name is the weaker signal.
#'
#' Retention, initial and final times are stored in milliseconds and converted
#' to minutes, as elsewhere in the package. All values are little-endian.
#'
#' A V0 record is 280 bytes:
#'
#' | **Offset** | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3     | `uint32` | Unparsed (peak number?) |
#' | 4–7     | `uint32` | Retention time (`R.time`) |
#' | 8–15    | `double` | Area |
#' | 16–23   | `double` | Unparsed |
#' | 24–31   | `double` | Height |
#' | 32–39   | `double` | Unparsed |
#' | 40–55   | `uint32` x 4 | Unparsed |
#' | 56–59   | `uint32` | Start of the peak (`I.time`) |
#' | 60–63   | `uint32` | End of the peak (`F.time`) |
#' | 64–67   | `uint32` | Area/height ratio x 1000 (`AH`) |
#' | 68–215  |          | Unparsed |
#' | 216–223 | `double` | Theoretical plates (`Plate.no`) |
#' | 224–231 | `double` | Plate height (`Plate.ht`) |
#' | 232–239 | `double` | Tailing factor |
#' | 240–247 | `double` | Resolution |
#' | 248–255 | `double` | Separation factor |
#' | 256–263 | `double` | Concentration, percent |
#' | 264–271 | `double` | Concentration, normalized |
#' | 272–279 | `uint32` x 2 | Unparsed |
#'
#' A V1 record is longer and its length is not fixed by the format: it is
#' derived as `(stream size - 20) / peak count`. The first 728 bytes are the
#' part this parser reads, and any remainder is skipped.
#'
#' | **Offset** | **Type** | **Field** |
#' | ---------- | -------- | --------- |
#' | 0–3     | `uint32` | Unparsed (peak number?) |
#' | 4–7     | `uint32` | Retention time (`R.time`) |
#' | 8–15    | `double` | Area |
#' | 16–23   | `double` | Unparsed |
#' | 24–31   | `double` | Height |
#' | 32–39   | `double` | Unparsed |
#' | 40–55   | `uint32` x 4 | Unparsed |
#' | 56–59   | `uint32` | Start of the peak (`I.time`) |
#' | 60–63   | `uint32` | End of the peak (`F.time`) |
#' | 64–71   | `double` | Area/height ratio (`AH`) |
#' | 72–175  |          | Unparsed |
#' | 176–183 | `double` | Concentration (`Conc`) |
#' | 184–187 | `int32`  | Compound identifier (`ID`), `0` for an unidentified peak |
#' | 188–239 |          | Unparsed |
#' | 240–247 | `double` | Retention factor (`k`) |
#' | 248–255 | `double` | Theoretical plates (`Plate.no`) |
#' | 256–375 |          | Unparsed, with an unidentified `double` at 312 |
#' | 376–383 | `double` | Plate height (`Plate.ht`) |
#' | 384–503 |          | Unparsed, with an unidentified `double` at 440 |
#' | 504–511 | `double` | Tailing factor |
#' | 512–519 | `double` | Resolution |
#' | 520–639 |          | Unparsed, with unidentified `double`s at 568 and 632 |
#' | 640–647 | `double` | Separation factor |
#' | 648–711 |          | Unparsed |
#' | 712–719 | `double` | Concentration, percent |
#' | 720–727 | `double` | Concentration, normalized |
#'
#' Note that `AH` is a scaled integer in V0 but a `double` in V1, and that the
#' V1 values sit mostly on a 64-byte stride, which suggests each derived
#' quantity occupies a slot of its own rather than being packed. The field
#' names are this package's reading of the format rather than the vendor's own.
#'
#' A V0 record length is not recorded anywhere in the stream, so unlike V1 it
#' cannot be checked against the file. A V0 variant with a different record
#' size would be misread rather than rejected.
#'
#' @param path Path to a 'Shimadzu' OLE file (`.lcd` or `.gcd`).
#' @param format_out Class of output. Either `data.frame` or `data.table`. A
#' peak table is heterogeneous and has no useful matrix representation, so
#' `matrix` resolves to `data.table`, as it does for mass spectra.
#' @return A named list with one peak table per channel, named for the stream
#' it was read from. A table that cannot be parsed is returned as `NA` with a
#' message, so one bad channel does not lose the others.
#' @author Ethan Bass
#' @family 'Shimadzu' parsers
#' @keywords internal
read_sz_tables <- function(path, format_out = "data.frame"){
  existing_streams <- check_streams(path, what = "peaks")
  if (length(existing_streams) == 0){
    stop("Peak table streams could not be detected.")
  }
  format_out <- check_format_out_table(format_out)
  pktab <- lapply(existing_streams, function(stream){
    tryCatch({
      tab <- read_sz_table(path, stream)
      if (format_out == "data.table") data.table::as.data.table(tab) else tab
    }, error = function(e){
      message(sprintf("Unable to parse `%s`.", paste(stream,collapse=", ")))
      NA
    })
  })
  names(pktab) <- sapply(existing_streams, `[[`, 2)
  pktab
}

#' Read Shimadzu Peak Table
#'
#' There are at least two Shimadzu peak table formats. The first (`V0`),
#' does not begin with a magic number, whereas the second (`V1`) starts
#' with the magic number `x56/x45/x52/x31` (which spells out 'VER1').
#'
#' In V0, the first 4 bytes of the stream are a 4 byte integer specifying the
#' number of peaks in the peak table. In V1, the peak number directly follows
#' the magic number.
#'
#' @noRd

read_sz_table <- function(path, stream){
  path_raw <- export_stream(path, stream)
  on.exit(unlink_stream(path_raw), add = TRUE)
  f <- file(path_raw, "rb")
  on.exit(close(f), add = TRUE)
  magic <- readBin(f, "raw", n = 4)
  magic <- paste(paste0("x", as.character(magic)), collapse = "/")
  read_sz_table <- switch(magic,
                          "x56/x45/x52/x31" = read_sz_table_v1,
                          read_sz_table_v0)
  read_sz_table(f)
}


#' Read Shimadzu Peak Table 'VER1'
#' @noRd
read_sz_table_v1 <- function(f){
  rows <- readBin(f, "integer", size = 4)
  seek(f,0,origin = "end")
  n_bytes <- seek(f, 0, origin = "end")
  block_len <- (n_bytes - 20)/rows
  seek(f, 20)
  tab <- do.call(rbind, lapply(seq_len(rows), function(i){
    read_sz_table_block_v1(f, block_len)
  }))
  tab$ID <- ifelse(tab$ID == 0, NA, tab$ID)
  tab
}

#' Read Shimadzu Peak Table 'VER0'
#' @noRd
read_sz_table_v0 <- function(f){
  seek(f,0)
  rows <- readBin(f, "integer", size = 4, endian = "little")
  readBin(f, "integer", size = 4) #skip
  tab <- do.call(rbind, lapply(seq_len(rows), function(i){
    read_sz_table_block_v0(f)
  }))
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
