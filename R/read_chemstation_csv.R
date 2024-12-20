#' Read Chemstation CSV
#'
#' Reads 'Agilent Chemstation' CSV files into R.
#'
#' 'Agilent Chemstation' CSV files are encoded in UTF-16.
#'
#' @name read_chemstation_csv
#' @importFrom utils tail read.csv
#' @param path Path to file
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_chemstation_csv <- function(path, format_out = c("matrix", "data.frame", "data.table")){
  format_out <- check_format_out(format_out)
  x <- read.csv(path, row.names = 1, header = TRUE,
                fileEncoding = "utf-16LE", check.names = FALSE)
  convert_chrom_format(x, format_out = format_out)
}

