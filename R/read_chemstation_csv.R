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
#' @param data_format Either \code{wide} (default) or \code{long}.
#' @param read_metadata Logical. Whether to attach metadata. Defaults to
#' \code{TRUE}. There is no instrumental metadata saved in the CSV files so this
#' will only attach metadata about the settings used by chromConverter to parse
#' the file.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength) and \code{data_format}.
#' @examplesIf interactive()
#' read_chemstation_csv("tests/testthat/testdata/dad1.csv")
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_chemstation_csv <- function(path, format_out = c("matrix", "data.frame",
                                                      "data.table"),
                                 data_format = c("wide", "long"),
                                 read_metadata = TRUE){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out = format_out)
  data <- read.csv(path, row.names = 1, header = TRUE,
                fileEncoding = "utf-16LE", check.names = FALSE)

  if (data_format == "long"){
    data <- reshape_chrom_long(data)
  }
  data <- convert_chrom_format(data, format_out = format_out,
                               data_format = data_format)
  if (read_metadata){
    data <- attach_metadata_minimal(data, data_format = data_format,
                                    format_out = format_out,
                                    parser = "chromconverter", source_file = path,
                                    source_file_format = "chemstation_csv")
  }
}

