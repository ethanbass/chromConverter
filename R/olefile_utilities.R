
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
      path_out <- fs::file_temp(pattern =
                                  gsub(" ", "_", paste(c(fs::path_ext_remove(basename(path)), stream),
                                                       collapse="_")))
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
#' @noRd
check_streams <- function(path, what = c("pda", "chromatogram", "tic", ""),
                          stream = NULL,
                          boolean = FALSE){
  what <- match.arg(what, c("pda", "chromatogram", "tic", ""))
  olefile <- reticulate::import("olefile")
  ole <- olefile$OleFileIO(path)
  if (what == "pda"){
    pda_exists <- ole$get_size("PDA 3D Raw Data/3D Raw Data") > 0
    if (boolean){
      return(pda_exists)
    } else if (!pda_exists){
      stop("PDA stream could not be found.")
    }
  } else {
    streams <- ole$listdir()
    what <- switch(what, "chromatogram" = "Chromatogram Ch",
                   "tic" = "Centroid SumTIC")
    selected_streams <- streams[grep(what, streams)]
    sizes <- sapply(selected_streams, function(x){
      ole$get_size(paste0(x, collapse = "/"))})
    if (boolean){
      return(any(sizes > 0))
    } else{
      selected_streams[which(sizes > 0)]
    }
  }
}
