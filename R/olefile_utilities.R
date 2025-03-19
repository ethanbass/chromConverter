#' Export OLE stream
#' This function is called internally by \code{read_shimadzu_lcd}.
#' Use olefile to export te specified stream.
#' @param path Path to ole file.
#' @author Ethan Bass
#' @noRd

export_stream <- function(path, stream, path_out, remove_null_bytes = FALSE,
                          verbose = FALSE){
  reticulate::py_run_string('import olefile')
  reticulate::py_run_string(paste0('ole = olefile.OleFileIO("', path, '")'))
  python_stream <- paste0("[", paste(paste0("'", stream, "'"), collapse = ', '),"]")
  stream_exists <- reticulate::py_eval(paste0("ole.exists(", python_stream, ")"))
  if (!stream_exists){
    if (verbose){
      warning(paste0("The stream ", sQuote(python_stream), " could not be found."),
              immediate. = TRUE)
    }
    return(NA)
  } else{
    reticulate::py_run_string(paste0("st = ole.openstream(", python_stream, ")"))
    reticulate::py_run_string('data = st.read()')

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
    }
    if (remove_null_bytes){
      reticulate::py_run_string("data = data.replace(b'\\x00', b'')")
    }
    reticulate::py_run_string(paste0('with open("', path_out ,'", "wb") as binary_file:
      binary_file.write(data)'))
    path_out
  }
}


#' Check OLE stream size
#' @param min_size Minimum stream size in bytes. Defaults to 552.
#' @author Ethan Bass
#' @noRd

check_streams <- function(path, what = c("pda", "chroms", "tic", "peaks", ""),
                          stream = NULL,
                          boolean = FALSE,
                          min_size = 1200){
  what <- match.arg(what, c("pda", "chroms", "tic", "peaks", ""))
  olefile <- reticulate::import("olefile")
  ole <- olefile$OleFileIO(path)
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
      ole$get_size(paste0(x, collapse = "/"))})
    if (boolean){
      return(any(sizes > min_size))
    } else{
      selected_streams <- selected_streams[which(sizes > min_size)]
      selected_streams[!duplicated(sapply(selected_streams, `[[`, 2))]
    }
  }
}

#' Check OLE stream by name
#' @noRd

check_stream <- function(path, stream = NULL,
                          boolean = FALSE, min_size = 552){
  olefile <- reticulate::import("olefile")
  ole <- olefile$OleFileIO(path)
  python_stream <- paste0(stream, collapse = "/")
  pda_exists <- tryCatch(ole$get_size(python_stream),
                         error=function(e) 0) > min_size
  pda_exists
}


#' List OLE streams
#' @author Ethan Bass
#' @noRd

ole_list_streams <- function(path, pattern = NULL, ignore.case = FALSE,
                             min_size = 552){
  olefile <- reticulate::import("olefile")
  ole <- olefile$OleFileIO(path)
  streams <- ole$listdir()
  if (!is.null(pattern)){
    idx <- grep(streams, pattern = pattern, ignore.case = ignore.case)
    if (length(idx)==0)
      return(message("No streams found matching the specified pattern."))
    streams <- streams[idx]
  }
  if (!is.null(min_size)){
    idx <- which(sapply(streams, function(stream){
      check_stream(path, stream, min_size=min_size)
    }))
    if (length(idx)==0)
      return(message(sprintf("All streams matching the specified pattern are smaller than %g bytes.",
                             min_size)))
    streams <- streams[idx]
  }
  streams
}


#' Convert 'Shimadzu' time to Unix time
#' 'Shimadzu' LCD files seem to store times in 'Windows FILETIME' structure,
#' where the "low" time and "high" times must be combined into a 64-bit integer
#' representing the number of 100-nanosecond intervals since 1601-01-01.
#' Assuming that this interpretation is correct, there seems to be something
#' wrong with my conversion, since the times don't quite match the ones from the
#' ASCII files exported from 'Lab Solutions'.
#' @importFrom bit64 as.integer64
#' @noRd

sztime_to_unixtime <- function(low, high, tz = "UTC") {
  if (tz!="UTC"){
    tz <- -as.numeric(gsub("'00'", "", tz))
    if (tz > 0){
      tz <- paste0("+",tz)
    }
    tz <- paste0("Etc/GMT", tz)
  }
  if (low < 0) {
    low <- bit64::as.integer64(low) + 2^32
  }
  filetime <- bit64::as.integer64(high) * 2^32 + bit64::as.integer64(low)
  unix_time <- (filetime / 10000000) - 11644473600
  as.POSIXct(unix_time, origin = "1970-01-01", tz = tz)
}
