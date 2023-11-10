#' Waters ascii (.arw) reader
#'
#' Reads 'Waters ARW' files.
#'
#' For help exporting files from Empower, you can consult the official
#' documentation: [How_to_export_3D_raw_data_from_Empower](https://support.waters.com/KB_Inf/Empower_Breeze/WKB77571_How_to_export_3D_raw_data_from_Empower_to_a_Microsoft_Excel_spreadsheet).
#'
#' @name read_waters_arw
#' @importFrom utils tail read.csv
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_waters_arw <- function(file, format_out = c("matrix", "data.frame"),
                            data_format = c("wide", "long"),
                            read_metadata = TRUE,
                            metadata_format = c("chromconverter", "raw")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "waters_arw", raw = "raw")
  x <- read.csv(file, sep = "\t", skip = 2, header = FALSE, row.names = 1)
  # PDA (3D)
  if (rownames(x)[1] == "Wavelength"){
    colnames(x) <- x[1,]
    rm <- 1
    if (rownames(x)[2] == "Time"){
      rm <- c(rm, 2)
    }
    x <- x[-rm,]
    if (data_format == "long"){
      x <- as.data.frame(reshape_chrom(x, data_format = "long"))
    }
  } else if (ncol(x) == 1){
    colnames(x) <- "Intensity"
    if (data_format == "long"){
      x <- data.frame(RT = rownames(x), Intensity = x[,1])
    }
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (read_metadata){
    meta <- try(read_waters_metadata(file))
    if (!inherits(meta, "try-error")){
      x <- attach_metadata(x, meta, format_in = metadata_format,
                           format_out = format_out,
                           data_format = data_format,
                           parser = "chromConverter",
                           source_file = file)
    }
  }
  x
}
