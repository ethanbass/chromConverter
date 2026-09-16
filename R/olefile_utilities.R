#' Export OLE stream
#' This function is called internally by shimadzu binary parsers.
#' Use olefile to export the specified stream.
#' @param path Path to ole file.
#' @author Ethan Bass
#' @noRd

export_stream <- function(path, stream, path_out, remove_null_bytes = FALSE,
                          verbose = FALSE){
  check_py_module("olefile")
  if (missing(path_out)){
    path_out <- fs::file_temp(pattern = gsub(" ", "_",
                                             paste(c(fs::path_ext_remove(
                                               basename(path)), stream),
                                                     collapse="_")))
    if (.Platform$OS.type == "windows") {
      path_dir <- fs::path_dir(path_out)
      path_file <- fs::path_file(path_out)

      path_dir <- fs::path_real(path_dir)
      path_out <- fs::path(path_dir, path_file)
    }
    path_out <- fs::path_expand(path_out)
  }
  found <- py_export_stream()(path, as.list(stream), path_out,
                              remove_null_bytes)
  if (!found){
    if (verbose){
      warning(paste0("The stream ", sQuote(paste(stream, collapse = "/")),
                     " could not be found."), immediate. = TRUE)
    }
    return(NA)
  }
  path_out
}

#' 'Python' implementation of `export_stream`
#'
#' The work is kept inside a 'Python' function so that the OLE handle, the
#' stream and its contents are function-local and released as soon as it
#' returns. Running the same statements through `py_run_string` would instead
#' bind them in `__main__`, where they survive until the next call rebinds
#' them, pinning an open handle and a full copy of the last stream read for the
#' rest of the session.
#'
#' Passing the paths as arguments (rather than pasting them into the 'Python'
#' source) also avoids having them interpreted as containing escape sequences,
#' which broke 'Windows' paths containing backslashes.
#' @noRd

py_export_stream <- function(){
  if (is.null(py_modules[["_cc_export_stream"]])){
    init_python()
    reticulate::py_run_string("
import olefile

def _cc_export_stream(path, stream, path_out, remove_null_bytes=False):
    with olefile.OleFileIO(path) as ole:
        if not ole.exists(stream):
            return False
        data = ole.openstream(stream).read()
    if remove_null_bytes:
        data = data.replace(b'\\x00', b'')
    with open(path_out, 'wb') as binary_file:
        binary_file.write(data)
    return True
")
    py_modules[["_cc_export_stream"]] <- reticulate::py$`_cc_export_stream`
  }
  py_modules[["_cc_export_stream"]]
}


#' Remove a stream exported by `export_stream`
#' `export_stream` writes each stream to a temporary file, which callers should
#' delete once they are done reading it. Returns silently when the stream was
#' not found (in which case `export_stream` returns `NA`).
#' @author Ethan Bass
#' @noRd

unlink_stream <- function(path){
  if (length(path) > 0 && !is.na(path[1]) && nzchar(path[1])){
    unlink(path)
  }
  invisible(NULL)
}

#' Check OLE stream size
#' @param min_size Minimum stream size in bytes. Defaults to 552.
#' @author Ethan Bass
#' @noRd

check_streams <- function(path, what = c("pda", "chroms", "tic", "peaks",
                                         "qtof", "tlm", ""),
                          stream = NULL,
                          boolean = FALSE,
                          min_size = 1200){
  what <- match.arg(what, c("pda", "chroms", "tic", "peaks", "qtof", "tlm", ""))
  olefile <- py_import("olefile")
  ole <- olefile$OleFileIO(path)
  on.exit(ole$close(), add = TRUE)
  if (what == "pda"){
    pda_exists <- ole$get_size("PDA 3D Raw Data/3D Raw Data") > min_size
    if (boolean){
      return(pda_exists)
    } else if (!pda_exists){
      stop("PDA stream could not be found.")
    }
  } else {
    streams <- ole$listdir()
    what <- switch(what, "chroms" = "Chromatogram Ch|Max Plot",
                   "tic" = "Centroid SumTIC",
                   "peaks" = "Peak Table|PT")
    selected_streams <- streams[grep(what, streams)]
    sizes <- sapply(selected_streams, function(x){
      ole_stream_size(ole, x)})
    if (boolean){
      return(any(sizes > min_size))
    } else{
      selected_streams <- selected_streams[which(sizes > min_size)]
      selected_streams[!duplicated(sapply(selected_streams, `[[`, 2))]
    }
  }
}

#' Size of an OLE stream, or 0 if it cannot be read
#' Takes an already-open `ole` handle so that callers checking several streams
#' do not have to re-open and re-parse the container for each one.
#' @noRd

ole_stream_size <- function(ole, stream){
  tryCatch(ole$get_size(paste0(stream, collapse = "/")),
           error = function(e) 0)
}

#' Check OLE stream by name
#' @noRd

check_stream <- function(path, stream = NULL,
                          boolean = FALSE, min_size = 552){
  olefile <- py_import("olefile")
  ole <- olefile$OleFileIO(path)
  on.exit(ole$close(), add = TRUE)
  ole_stream_size(ole, stream) > min_size
}


#' List OLE streams
#' @author Ethan Bass
#' @noRd

ole_list_streams <- function(path, pattern = NULL, ignore.case = FALSE,
                             min_size = 552){
  olefile <- py_import("olefile")
  ole <- olefile$OleFileIO(path)
  on.exit(ole$close(), add = TRUE)
  streams <- ole$listdir()
  if (!is.null(pattern)){
    idx <- grep(streams, pattern = pattern, ignore.case = ignore.case)
    if (length(idx)==0)
      return(message("No streams found matching the specified pattern."))
    streams <- streams[idx]
  }
  if (!is.null(min_size)){
    idx <- which(vapply(streams, function(stream){
      ole_stream_size(ole, stream) > min_size
    }, FUN.VALUE = logical(1)))
    if (length(idx)==0)
      return(message(sprintf("All streams matching the specified pattern are smaller than %g bytes.",
                             min_size)))
    streams <- streams[idx]
  }
  streams
}


#' Convert 'Shimadzu' time to Unix time
#' 'Shimadzu' files store times in the 'Windows' `FILETIME` structure, where the
#' "low" and "high" words must be combined into a 64-bit integer representing
#' the number of 100-nanosecond intervals since 1601-01-01. `FILETIME` is
#' always UTC, so the instant returned here does not depend on `tz`, which only
#' selects how it is displayed.
#'
#' Note that the offset recorded by these files is the standard offset of the
#' zone (the 'Windows' standard bias) rather than the offset that was in force,
#' so it does not account for daylight saving time. Times rendered with it are
#' an hour behind the local times reported by 'Lab Solutions' wherever daylight
#' saving applied.
#' @importFrom bit64 as.integer64
#' @noRd
sztime_to_unixtime <- function(low, high, tz = "UTC") {
  tz <- parse_shimadzu_tz(tz)
  if (low < 0) {
    low <- bit64::as.integer64(low) + 2^32
  }
  filetime <- bit64::as.integer64(high) * 2^32 + bit64::as.integer64(low)
  unix_time <- (filetime / 10000000) - 11644473600
  as.POSIXct(unix_time, origin = "1970-01-01", tz = tz)
}

parse_shimadzu_tz <- function(tz){
  if (tz != "UTC"){
    tz <- convert_fractional_timezone_offset(tz)
    if (!grepl("/",tz)){
      pattern <- "([+-])(\\d{2})'(\\d{2})"
      captures <- regmatches(tz, regexec(pattern, tz))[[1]]
      sign <- captures[2]
      hours <- as.numeric(captures[3])
      minutes <- as.numeric(captures[4])

      decimal_hours <- hours + minutes/60

      if (sign == "+") {
        tz <- paste0("Etc/GMT-", decimal_hours)
      } else {
        tz <- paste0("Etc/GMT+", decimal_hours)
      }
    }
  }
  tz
}

#' @author Ethan Bass
#' @noRd
convert_fractional_timezone_offset <- function(tz) {
  clean_offset <- gsub("'", "", tz)

  timezone <- switch(clean_offset,
                     # 30-minute offsets (positive)
                     "+0330" = "Asia/Tehran",
                     "+0430" = "Asia/Kabul",
                     "+0530" = "Asia/Kolkata",
                     "+0630" = "Asia/Yangon",
                     "+0930" = "Australia/Adelaide",
                     "+1030" = "Australia/Adelaide",
                     "+1230" = "Pacific/Auckland",
                     "+1330" = "Pacific/Chatham",

                     # 30-minute offsets (negative)
                     "-0330" = "America/St_Johns",
                     "-0430" = "America/Caracas",
                     "-0930" = "Pacific/Marquesas",

                     # 45-minute offsets (positive)
                     "+0545" = "Asia/Kathmandu",
                     "+0845" = "Australia/Eucla",
                     "+1245" = "Pacific/Chatham",

                     # Return NULL for unknown offsets
                     tz
  )

  return(timezone)
}
