
#' @name import_chroms
#' @title import_chroms
#' @param paths paths to folders containing files
#' @param format.in Format of files to be imported/converted.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format.in}.
#' @param R.format R object format (i.e. data.frame or matrix).
#' @param export Logical. If true, will export files as csvs.
#' @param path.out Path for export.
#' @param format.out Output format. Currently only .csv.
#' @param dat Existing list of chromatographs to append results.
#' (Defaults to NULL).
#' @return A list of chromatograms in matrix or data.frame format.
#' @import reticulate
#' @importFrom utils write.csv
#' @examples \dontrun{
#' data <- import_chrom(paths)
#' }
#' @author Ethan Bass
#' @export import_chroms

import_chroms <- function(paths, pattern=NULL,
                         format.in=c("chemstation.uv", "masshunter.dad"),
                      R.format=c("matrix","data.frame"), export=FALSE,
                      path.out=NULL, format.out="csv", dat=NULL){
  R.format <- match.arg(R.format, c("matrix", "data.frame"))
  format.in <- match.arg(format.in, c("chemstation.uv", "masshunter.dad"))
  exists <- dir.exists(paths)
  if (mean(exists) == 0){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  if (export){
    if (is.null(path.out)){
      path.out <- getwd()
      warning("Export directory not specified. Files will be exported to
              current working directory.", immediate. = TRUE)
    }
    path.out <- gsub("/$","", path.out)
    if (!dir.exists(path.out)){
      stop(paste0("The export directory '", path.out, "' does not exist."))
    }
    csv <- reticulate::import("csv")
  }
  # import python modules
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  if (is.null(dat)){
    dat<-list()}
  # choose converter
  if (format.in == "masshunter.dad"){
    pattern <- ifelse(is.null(pattern),".sp", pattern)
    converter <- sp_converter
  } else if (format.in=="chemstation.uv"){
    pattern <- ifelse(is.null(pattern),".uv", pattern)
    converter <- trace_converter
  } else{
    converter <- trace_converter
  }
  for (path in paths){
    files <- list.files(path = path, pattern = pattern,
                        full.names = TRUE,recursive = TRUE)
    if (length(files)==0){
      if (!dir.exists(path)){
        warning(paste0("The directory '", basename(path), "' does not exist."))
      } else{
        warning(paste0("No files matching the pattern '", pattern,
                       "' were found in '", basename(path), "'"))
      }
    }
    file_names <- gsub("\\.[Dd]","", list.files(path=path, pattern = "\\.[Dd]$",
                                             full.names = FALSE))
    data <- lapply(X=files, function(f){
      df <- converter(f)
    })
    if (R.format == "matrix"){
      data <- lapply(data, FUN=as.matrix)}
    names(data) <- file_names
    if (export){
      sapply(seq_along(data), function(i){
        write.csv(data[[i]], file = paste0(paste(path.out,names(data)[i], sep="/"),".CSV"))
      })
    }
    dat <- append(dat,data)
  }
  dat
}

# # Export to csv file
# pd$DataFrame$to_csv(df_Data, "Data.csv")
# csv$writer(df_Data,"Data.csv")
# df_Data.to_csv("Data.csv")

#' @name sp_converter
#' @title converter for Agilent MassHunter UV file
#' @param file path to file
#' @return A data.frame object (retention time x wavelength).
#' @import reticulate
#' @noRd
sp_converter <- function(file){
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  df <- trace_file$agilent_uv$AgilentDAD(file)
  pd$DataFrame(df$data$values, columns=df$data$columns,
                    index=df$data$index)
}

#' @name trace_converter
#' @title converter for Agilent UV file and others
#' @param file path to file
#' @return A data.frame object (retention time x trace).
#' @import reticulate
#' @noRd
trace_converter <- function(file){
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  df <- trace_file$TraceFile(file)
  pd$DataFrame(df$data$values, columns=df$data$columns,
               index=df$data$index)
}
