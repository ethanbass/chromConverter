#' Read Chemstation CSV
#'
#' Reads 'Agilent Chemstation' CSV files into R.
#'
#' 'Agilent Chemstation' CSV files are encoded in UTF-16.
#'
#' @name read_chemstation_csv
#' @importFrom utils tail read.csv
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_chemstation_csv <- function(file, format_out = c("matrix","data.frame")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  x <- read.csv(file, row.names = 1, header = TRUE,
                fileEncoding = "utf-16LE", check.names = FALSE)
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  x
}

