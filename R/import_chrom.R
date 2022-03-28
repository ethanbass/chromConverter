
#' @name import_chrom
#' @title import_chrom
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

#' @import reticulate
#' @importFrom utils write.csv
#' @examples \dontrun{
#' data <- import_chrom(paths)
#' }
#' @export import_chrom

import_chrom <- function(paths, pattern=NULL,
                         format.in=c("chemstation.uv", "masshunter.DAD"),
                      R.format=c("matrix","data.frame"), export=FALSE,
                      path.out=NULL, format.out="csv", dat=NULL){
  R.format <- match.arg(R.format, c("matrix", "data.frame"))
  format.in <- match.arg(format.in, c("chemstation.uv", "masshunter.DAD"))
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
  if (format.in == "chemstation.uv"){
    pattern <- ifelse(is.null(pattern),".uv", pattern)
    converter <- trace_file$TraceFile
  } else if (format.in=="masshunter.DAD"){
    pattern <- ifelse(is.null(pattern),".sp", pattern)
    converter <- trace_file$agilent_uv$AgilentDAD
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
      df = pd$DataFrame(df$data$values, columns=df$data$columns,
                        index=df$data$index)
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

