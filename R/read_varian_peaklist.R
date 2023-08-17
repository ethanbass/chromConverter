#' Read 'Varian' peaklist.
#' Read peak list(s) from 'Varian MS Workstation'.
#' @param file Path to Varian peak list file.
#' @importFrom utils read.csv
#' @author Ethan Bass
#' @export

read_varian_peaklist <- function(file){
  x <- read.csv(file, skip = 5, header = FALSE)
  x$V1[x$V1 == ""] <- NA
  x <- tidyr::fill(data = x, "V1", .direction = "down")

  column_names <- x[2,]
  column_names[1] <- "compound"
  colnames(x) <- column_names

  x <- x[-which(x$`Line#` == "Line#"),]
  x <- x[-which(x$`Line#` == ""), ]

  x$Area <- as.numeric(x$Area)
  x$Height <- as.numeric(x$Height)
  x <- x[,-16]
  x
}
