#' @name call_entab
#' @title Entab parsers
#' @param file path to file
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param format_in Format of input.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param read_metadata Whether to read metadata from file.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @importFrom tidyr pivot_wider
#' @export

call_entab <- function(file, data_format = c("wide","long"),
                       format_in = "",
                       format_out = c("matrix", "data.frame"),
                       read_metadata = TRUE){
  if (!requireNamespace("entab", quietly = TRUE)){
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
         call. = FALSE)
  }
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))
  r <- entab::Reader(file)
  x <- entab::as.data.frame(r)
  if (data_format == "wide"){
    times <- unique(x$time)
    id <- names(x)[2]
    x <- as.data.frame(pivot_wider(x, id_cols = "time",
                                   names_from = {{id}},
                                   values_from = "intensity"),
                       row.names = "time")
    rownames(x) <- times
    x <- x[,-1]
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (read_metadata){
    meta <- r$metadata()
    if (format_in == "chemstation_uv"){
      metadata_from_file <- try(read_chemstation_metadata(file), silent = TRUE)
      meta <- c(meta, metadata_from_file)
    }
    if (format_in == "masshunter_dad"){
      metadata_from_file <- try(read_masshunter_metadata(file), silent = TRUE)
      meta <- c(meta, metadata_from_file)
    }
    x <- attach_metadata(x, meta, format_in = format_in, format_out = format_out,
                         data_format = data_format, parser = "entab",
                         source_file = file)
  }
  x
}
