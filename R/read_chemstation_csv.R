#' Read 'Agilent ChemStation' CSV files
#'
#' Reads 'Agilent Chemstation' \code{.csv} files.
#'
#' 'Agilent Chemstation' CSV files are encoded in UTF-16.
#'
#' @name read_chemstation_csv
#' @importFrom utils tail read.csv
#' @param path Path to 'Agilent' \code{.csv} file.
#' @param format_out Class of output. Either \code{matrix}, \code{data.frame},
#' or \code{data.table}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @examplesIf interactive()
#' read_chemstation_csv("tests/testthat/testdata/dad1.csv")
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_chemstation_csv <- function(path, format_out = c("matrix", "data.frame", "data.table")){
  format_out <- check_format_out(format_out)
  x <- read.csv(path, row.names = 1, header = TRUE,
                fileEncoding = "utf-16LE", check.names = FALSE)
  convert_chrom_format(x, format_out = format_out)
}

