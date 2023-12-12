utils::globalVariables(names = c('.'))
# Globals <- list()


#' Get filetype
#' @noRd
get_filetype <- function(path, out = c("format_in", "filetype")){
  out <- match.arg(out, c("format_in", "filetype"))
  f <- file(path, "rb")
  on.exit(close(f))

  magic <- readBin(f, what = "raw", n = 4)
  magic <- paste(paste0("x",as.character(magic)),collapse="/")
  # magic
  filetype <- switch(magic,
                     "x01/x32/x00/x00" = "AgilentChemstationMS",
                     "x02/x02/x00/x00" = "AgilentMasshunterDADHeader",
                     # "x02/x33/x30/x00" = "AgilentChemstationMWD",
                     "x03/x02/x00/x00" = "AgilentMasshunterDAD",
                     "x02/x33/x30/x00" = "chemstation_30",
                     "x02/x33/x31/x00" = "chemstation_31",
                     "x03/x31/x33/x30" = "chemstation_130", #130
                     "x03/x31/x33/x31" = "chemstation_131", #131
                     "x03/x31/x37/x39" = "chemstation_179", #179
                     "x02/x38/x31/x00" = "chemstation_81", #81
                     "x03/x31/x38/x31" = "chemstation_181", #181
                     "x01/xa1/x46/x00" = "ThermoRAW",
                     "xd0/xcf/x11/xe0" = "ShimadzuLCD",
                     "x80/x00/x01/x00" = "WatersRAW",
                     "x43/x44/x46/x01" = "cdf"
  )
  if (is.null(filetype)){
    stop("File type not recognized. Please specify a filetype by providing an argument to `format_in`
          or file an issue at `https://github.com/ethanbass/chromConverter/issues`.")
  }
  if (filetype == "chemstation_131"){
    seek(f, 348)
    magic2 <- readBin(f, what="character", n = 2)
    magic2 <- paste(magic2, collapse="")
    filetype <- switch(magic2, "OL" = "openlab_131",
                   "LC" = "chemstation_131")
  }
  format_in <- switch(filetype,
                      "AgilentChemstationMS" = "chemstation",
                      "AgilentChemstationCH" = "chemstation_ch",
                      "AgilentChemstationFID" = "chemstation_ch",
                      # "chemstation_31" = "chemstation_uv",
                      # "chemstation_131" = "chemstation_uv",
                      # "openlab_131" = "chemstation_uv",
                      "ThermoRAW" = "thermoraw",
                      "ShimadzuLCD" = "shimadzu_lcd",
                      "WatersRAW" = "waters_raw",
                      filetype
  )

  switch(out, "filetype" = filetype, "format_in" = format_in)
}

#' Check parser
#' @noRd
check_parser <- function(format_in, parser=NULL, find = FALSE){
  allowed_formats <- list(openchrom = c("msd","csd","wsd"),
                          chromconverter = c("agilent_dx", "cdf", "chemstation_csv",
                                             "chemstation_ch", "chemstation_fid",
                                             "chemstation_uv", "chromeleon_uv",
                                             "chemstation_30", "chemstation_31",
                                             "chemstation_130", "chemstation_131",
                                             "openlab_131",
                                             "chemstation_179", "chemstation_81",
                                             "chemstation_181", "mzml", "mzxml",
                                             "mdf", "shimadzu_fid", "shimadzu_dad",
                                             "shimadzu_lcd", "waters_arw"),
                          aston = c("chemstation", "chemstation_uv",
                                    "chemstation_131",
                                    "masshunter_dad", "other"),
                          entab = c("chemstation", "chemstation_ch",
                                    "chemstation_30", "chemstation_31",
                                    "chemstation_131", "chemstation_fid",
                                    "chemstation_uv", "masshunter_dad",
                                    "thermoraw", "other"),
                          rainbow = c("chemstation", "chemstation_ch",
                                      "chemstation_130","chemstation_131",
                                      "chemstation_fid", "chemstation_179",
                                      "chemstation_uv", "waters_raw",
                                      "agilent_d"),
                          thermoraw = c("thermoraw")
  )
  if (find){
    if (!reticulate::py_module_available("aston")){
      allowed_formats <- allowed_formats[-which(names(allowed_formats) == "aston")]
    }
    if (!reticulate::py_module_available("rainbow")){
      allowed_formats <- allowed_formats[-which(names(allowed_formats) == "rainbow")]
    }
    if (!requireNamespace("entab", quietly = TRUE)){
      allowed_formats <- allowed_formats[-which(names(allowed_formats) == "entab")]
    }
    possible_parsers <- names(allowed_formats)[grep(format_in, allowed_formats)]
    if (length(possible_parsers) > 1){
      possible_parsers <- possible_parsers[match(
        c("thermoraw", "entab", "chromconverter", "rainbow", "aston"), possible_parsers)]
      if (any(is.na(possible_parsers))){
        possible_parsers <- possible_parsers[-which(is.na(possible_parsers))]
      }
    }
    possible_parsers[1]
  } else{
    if (!(format_in %in% allowed_formats[[parser]])){
      stop("Mismatched arguments!", "\n\n", "The ", paste0(sQuote(format_in), " format can be converted using the following parsers: ",
        paste(sQuote(names(allowed_formats)[grep(format_in, allowed_formats)]), collapse = ", "), ". \n \n",
        "The ", sQuote(parser), " parser can take the following formats as inputs: \n",
                                    paste(sQuote(allowed_formats[[parser]]), collapse=", "), ". \n \n",
        "Please double check your arguments and try again."))
    }
  }
}

#' Remove unicode characters
#' @noRd
remove_unicode_chars <- function(x){
  stringr::str_replace_all(x, "\xb5", "micro")
}

#' Extract file names
#' @noRd
extract_filenames <- function(files){
  if (all(grepl("\\.[Dd]$|\\.[Dd]?[/\\\\]", files))){
    file_names <- strsplit(files, "/")
    file_names <- gsub("\\.[Dd]", "",
                       sapply(file_names, function(n){
                         ifelse(any(grepl("\\.[Dd]", n)),
                                grep("\\.[Dd]", n, value = TRUE), tail(n,1))
                       }))
  } else {
    file_names <- sapply(strsplit(basename(files),"\\."), function(x) x[1])
  }
  file_names
}

#' Format extension
#' @noRd
format_to_extension <- function(format_in){
  switch(format_in,
         "agilent_d" = ".d|.D",
         "chemstation_uv" = ".uv|.UV",
         "chemstation_ch" = ".ch|.CH",
         "chemstation_fid" = ".ch|.CH",
         "chemstation_csv" = ".csv|.CSV",
         "masshunter_dad" = ".sp|.SP",
         "shimadzu_fid" = ".txt",
         "shimadzu_dad" = ".txt",
         "chromeleon_uv" = ".txt",
         "thermoraw" = ".raw",
         "mzml" = ".mzml", "mzxml" = ".mzxml",
         "waters_arw" = ".arw",
         "waters_raw" = ".raw",
         "msd" = ".", "csd" =".",
         "wsd" =".", "mdf" = ".mdf|.MDF",
         "other"=".")
}

#' @noRd
find_files <- function(paths, pattern){
  files <- unlist(lapply(paths, function(path){
    files <- list.files(path = path, pattern = pattern,
                        full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(files)==0){
      if (!dir.exists(path)){
        warning(paste0("The directory '", basename(path), "' does not exist."))
      } else{
        warning(paste0("No files matching the pattern '", pattern,
                       "' were found in '", basename(path), "'"))
      }
    }
    files
  }))
}

#' @noRd
set_temp_directory <- function(){
  ans <- readline("Export directory not specified! Export files to `temp` directory (y/n)?")
  if (ans %in% c("y","Y")){
    fs::dir_create("temp")
    path_out <- fs::path(getwd(),"temp")
    path_out
  } else{
    stop("Must specify directory to export files.")
  }
}

#' Check for suggested package
#' @noRd
#' @keywords internal
check_for_pkg <- function(pkg, return_boolean = FALSE){
  pkg_exists <- requireNamespace(pkg, quietly = TRUE)
  if (return_boolean){
    return(pkg_exists)
  } else if (!pkg_exists) {
    stop(paste(
      "Package", sQuote(pkg), "must be installed to perform this action:
          try", paste0("`install.packages('", pkg, "')`.")),
      call. = FALSE
    )
  }
}

#' Choose apply function
#' @return Returns \code{\link[pbapply]{pblapply}} if \code{progress_bar == TRUE},
#' otherwise returns \code{\link{lapply}}.
#' @noRd
choose_apply_fnc <- function(progress_bar, parallel = FALSE, cl = NULL){
  if (progress_bar){
    check_for_pkg("pbapply")
    fn <- pbapply::pblapply
    if (!is.null(cl)){
      fn <- purrr::partial(fn, cl = cl)
    }
  } else{
    fn <- lapply
  }
  fn
}

#' Rename list
#' @author Ethan Bass
#' @noRd
rename_list <- function(x, new_names){
  old_names <- names(x)
  names.idx <- match(names(x), new_names)
  new_names <- names(new_names)[names.idx]
  not_found <- which(is.na(new_names))
  if (any(not_found)){
    new_names[not_found] <- old_names[not_found]
  }
  names(x) <- new_names
  x
}
