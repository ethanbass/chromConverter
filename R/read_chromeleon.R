#' Read Chromeleon txt files into R
#' @name read_chromeleon
#' @param paths path to files (or vector of paths)
#' @param suffix string specifying file suffix (e.g. 'txt')
#' @param dat files optional list of chromatograms. If provided, newly imported
#' chromatograms will be appended to the existing list.
#' @param R.format R object format (i.e. matrix, data.frame, or tibble).
#' @return A list of chromatograms in \code{matrix}, \code{data.frame}, or
#' \code{tibble} format, according to the value of 'R.format'.
#' @importFrom readr read_lines read_tsv
#' @importFrom utils tail
#' @author Ethan Bass
#' @examples \dontrun{
#' path <- system.file("extdata", ".", package = "chromConverter")
#' read_chromeleon(path)
#' }
#' @export read_chromeleon

read_chromeleon <- function(paths, suffix="txt", dat=NULL, R.format = c("matrix","data.frame","tibble")){

  R.format <- match.arg(R.format, c("matrix","data.frame","tibble"))

  dne <- which(!sapply(paths, dir.exists))
  if (length(dne)>0){
    for (i in 1:length(dne)){
      warning(paste0("path not found: '", paths[dne[i]], "'"))
    }}
  if (is.null(dat)){
    dat<-list()
  }
  for (path in paths){
    files <- list.files(path=path, pattern = suffix, full.names = T)
    file_names <- gsub(pattern = paste0(".",suffix), x = basename(files), replacement = "")
    mydata <- lapply(X=files, function(f){
      x<-read_lines(f)
      start <- tail(grep("Chromatogram Data:",x),1)
      # start<-tail(grep("\\[(.*?)\\]",x),1)
      x <- read_tsv(f, skip = start, show_col_types = F)
      x<-sapply(x, function(x) as.numeric(gsub(",",".",x)))
    })
    mydata <- lapply(mydata, FUN=as.matrix)
    names(mydata) <- file_names
    dat <- append(dat,mydata)
  }
  rm <- which(sapply(dat,dim)[1,]==0)
  if (length(rm) > 0){
    dat <- dat[-rm]
    warning(paste("The following chromatograms were found to be empty and automatically removed:", toString(rm,sep=",")))
  }
  if (R.format == "tibble"){
    dat
  } else if (R.format == "data.frame"){
    lapply(dat, function(x){
      x <- as.data.frame(x)
      rownames(x) <- x[,1]
      x[,3, drop=F]
    })
  } else if (R.format == "matrix"){
    lapply(dat, function(x){
      x <- as.matrix(x)
      rownames(x) <- x[,1]
      x[,3, drop=F]
    })
  }
}
