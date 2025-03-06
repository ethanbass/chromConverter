#' Read 'Varian' peak list.
#'
#' Read peak list(s) from 'Varian MS Workstation'.
#'
#' @param path Path to 'Varian' peak list file.
#' @importFrom utils read.csv
#' @author Ethan Bass
#' @return A data.frame containing the information from the specified report.
#' @examples \dontrun{
#' read_varian_peaklist(path)
#' }
#' @family 'Varian' parsers
#' @export

read_varian_peaklist <- function(path){
  x <- read.csv(path, skip = 5, header = FALSE)
  x$V1[x$V1 == ""] <- NA
  x <- tidyr::fill(data = x, "V1", .direction = "down")

  column_names <- x[1,]
  column_names[1] <- "compound"
  colnames(x) <- column_names

  x <- x[-which(x$`Line#` == ""),]
  x <- x[-which(x$`Line#` == "Line#"),]

  x$Area <- as.numeric(x$Area)
  x$Height <- as.numeric(x$Height)
  x <- x[, -16]
  x
}
