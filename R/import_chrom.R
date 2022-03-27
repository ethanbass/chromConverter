
#' @name import_chrom
#' @title import_chrom
#' @param paths paths to folders containing files
#' @param pattern pattern (e.g. a file extension)
#' @param dat existing list of chromatographs to append results.
#' (Defaults to NULL).
#' @param format data.frame or matrix
#' @param export Logical. If true, will export files as csvs.
#' @import reticulate
#' @examples \dontrun{
#' data <- import_UV(paths)}
#' @export import_chrom

import_chrom <- function(paths, pattern=".uv", dat=NULL,
                      format=c("matrix","data.frame"), export=FALSE){
  format <- match.arg(format, c("matrix", "data.frame"))
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  # import python modules
  if (export){
    csv <- import("csv")}
  if (is.null(dat)){
    dat<-list()}
  for (path in paths){
    files <- list.files(path=path, pattern = pattern,full.names = TRUE,recursive = T)
    file_names <- gsub("\\.D","",list.files(path=path, pattern = "\\.D",full.names = F))
    data <- lapply(X=files, function(f){
      df <- trace_file$TraceFile(f)
      df = pd$DataFrame(df$data$values, columns=df$data$columns, index=df$data$index)
    })
    if (format=="matrix"){
      data <- lapply(data, FUN=as.matrix)}
    names(data) <- file_names
    dat <- append(dat,data)
  }
  dat
}

# # Export to csv file
# pd$DataFrame$to_csv(df_Data, "Data.csv")
# csv$writer(df_Data,"Data.csv")
# df_Data.to_csv("Data.csv")

