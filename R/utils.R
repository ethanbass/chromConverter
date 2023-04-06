utils::globalVariables(names = c('.'))
# Globals <- list()

#' @noRd
check_parser <- function(format_in, parser=NULL, find = FALSE){
  allowed_formats <- list(openchrom = c("msd","csd","wsd"),
                          chromconverter = c("chemstation_csv", "chemstation_ch",
                                             "chemstation_fid", "chemstation_uv",
                                             "chromeleon_uv", "mzml",
                                             "shimadzu_fid", "shimadzu_dad",
                                             "waters_arw"),
                          aston = c("chemstation_uv", "masshunter_dad", "other"),
                          entab = c("chemstation_uv", "chemstation_fid", "masshunter_dad", "thermoraw", "other"),
                          rainbow = c("chemstation_uv", "waters_raw",
                                      "agilent_d", "chemstation", "chemstation_fid"),
                          thermoraw = c("thermoraw")
  )
  if (find){
    possible_parsers <- names(allowed_formats)[grep(format_in, allowed_formats)]
    if (all(c("aston","entab") %in% possible_parsers)){
      if (any(format_in == c("chemstation_uv", "masshunter_dad"))){
        possible_parsers <- ifelse(!requireNamespace("entab", quietly = TRUE), "aston", "entab")
      }
    }
    if (all(c("rainbow","aston") %in% possible_parsers)){
      possible_parsers <- "rainbow"
    }
    if (all(c("entab","thermoraw") %in% possible_parsers)){
      possible_parsers <- "thermoraw"
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
export_csvs <- function(data, path.out){
  sapply(seq_along(data), function(i){
    write.csv(data[[i]], file = paste0(paste0(path.out, names(data)[i]),".CSV"))
  })
}

#' @noRd
set_temp_directory <- function(){
  ans <- readline("Export directory not specified! Export files to `temp` directory (y/n)?")
  if (ans %in% c("y","Y")){
    if (!dir.exists("temp"))
      dir.create("temp")
    path_out <- paste0(getwd(),'/temp/')
    path_out
  } else{
    stop("Must specify directory to export files.")
  }
}

#' Check path
#' Check that path is properly formatted.
#' @param path path as character string
#' @noRd
check_path <- function(path){
  # check for leading slash
  if (.Platform$OS.type %in% c("unix","linux")){
    if (!(substr(path,1,1) %in% c("/", "~"))){
      path <- paste0("/", path)
    }
  }

  # check for trailing slash
  n <- nchar(path)
  if (substr(path, n, n) != "/"){
    path <- paste0(path, "/")
  }
  if (.Platform$OS.type == "windows"){
    path <- gsub("/", "\\\\", path)
  }
  path
}

#' Extract header from Shimadzu ascii files
#' @noRd
extract_header <- function(x, chrom.idx, sep){
  index <- chrom.idx+1
  line <- x[index]
  l <- length(strsplit(x = line, split = sep)[[1]])
  header <- strsplit(x = line, split = sep)[[1]]
  while (l > 1) {
    index <- index+1
    line <- strsplit(x = x[index], split = sep)[[1]]
    l <- length(line)
    if (l == 1 | suppressWarnings(!is.na(as.numeric(line[1]))))
      break
    header <- rbind(header, line)
  }
  list(header,index)
}

#' Check for suggested package
#' @noRd
check_for_pkg <- function(pkg, return_boolean = FALSE){
  pkg_exists <- requireNamespace(pkg, quietly = TRUE)
  if (!pkg_exists) {
    stop(paste(
      "Package", sQuote(pkg), "must be installed to perform this action:
          try", paste0("`install.packages('", pkg, "')`.")),
      call. = FALSE
    )
  }
  if (return_boolean){
    pkg_exists
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
  } else{
    fn <- lapply
  }
  fn
}

#'@noRd
transfer_metadata <- function (new_object, old_object, exclude = c("names", "row.names",
                                              "class", "dim", "dimnames"))
{
  a <- attributes(old_object)
  a[exclude] <- NULL
  attributes(new_object) <- c(attributes(new_object), a)
  new_object
}

#' @noRd
get_filetype <- function(file, out = c("format_in", "filetype")){
  out <- match.arg(out, c("format_in", "filetype"))
  magic <- readBin(file, what = "raw", n = 4)
  magic <- paste(paste0("x",as.character(magic)),collapse="/")
  # magic
  filetype <- switch(magic,
                     "x01/x32/x00/x00" = "AgilentChemstationMS",
                     "x02/x02/x00/x00" = "AgilentMasshunterDADHeader",
                     "x02/x33/x30/x00" = "AgilentChemstationMWD",
                     "x02/x33/x31/x00" = "AgilentChemstationDAD",
                     "x02/x38/x31/x00" = "AgilentChemstationFID", #81
                     "x03/x02/x00/x00" = "AgilentMasshunterDAD",
                     "x03/x31/x33/x30" = "AgilentChemstationCH", #131
                     "x03/x31/x33/x31" = "AgilentChemstationDAD", #131 rainbow
                     "x03/x31/x37/x39" = "AgilentChemstationFID", #179
                     "x03/x31/x38/x31" = "AgilentChemstationFID", #181
                     "x02/x33/x30/x00" = "AgilentChemstationCH", #31/30
                     "x01/xa1/x46/x00" = "ThermoRAW",
                     "xd0/xcf/x11/xe0" = "ShimadzuLCD",
                     "x80/x00/x01/x00" = "WatersRAW"
  )
  if (is.null(filetype)){
    stop("File type not recognized. Please specify a filetype by providing an argument to `format_in`
          or file an issue at `https://github.com/ethanbass/chromConverter/issues`.")
  }
   format_in <- switch(filetype,
          "AgilentChemstationMS" = "chemstation",
          "AgilentChemstationCH" = "chemstation_ch",
          "AgilentChemstationFID" = "chemstation_ch",
          "AgilentChemstationDAD" = "chemstation_uv",
          "ThermoRAW" = "thermoraw",
          "ShimadzuLCD" = "shimadzu_lcd",
          "WatersRAW" = "waters_raw"
          )

  switch(out, "filetype" = filetype, "format_in" = format_in)
}
