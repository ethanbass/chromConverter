#' Read 'Allotrope Simple Model' (ASM) 2D chromatograms
#'
#' Reads ['Allotrope Simple Model'](https://www.allotrope.org/asm) files into R.
#'
#' @param path Path to ASM \code{.json} file.
#' @param format_out Matrix or data.frame.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element. Defaults to \code{TRUE}.
#' @return A 2D chromatogram in the format specified by \code{data_format} and
#' \code{format_out}. If \code{data_format} is \code{wide}, the chromatogram will
#' be returned with retention times as rows and a single column for the intensity.
#' If \code{long} format is requested, two columns will be returned: one for the
#' retention time and one for the intensity. The \code{format_out} argument
#' determines whether the chromatogram is returned as a \code{matrix} or
#' \code{data.frame}. Metadata can be attached to the chromatogram as
#' \code{\link{attributes}} if \code{read_metadata} is \code{TRUE}.
#' @author Ethan Bass
#' @export

read_asm <- function(path, data_format = c("wide", "long"),
                     format_out = c("matrix", "data.frame", "data.table"),
                     read_metadata = TRUE,
                     metadata_format = c("chromconverter", "raw"),
                     collapse = TRUE){
  data_format <- match.arg(data_format, c("wide", "long"))
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, "chromconverter" = "asm", "raw")

  xx <- jsonlite::fromJSON(path, flatten = FALSE)

  msd <- xx[[2]][[1]][["measurement aggregate document"]][["measurement document"]][[1]]
  detectors <- msd$`detection type`
  dat <- lapply(seq_along(detectors), function(i){
         xx <-  read_asm_2d_data(msd, idx = i, data_format = data_format,
                           format_out = format_out)
    if (length(detectors) > 1 && format_out %in% c("data.frame", "data.table"))
      xx$detector <- detectors[i]
    xx
  })
  names(dat) <- detectors
  if (read_metadata){
    device_meta <- xx[[2]][["device system document"]]
    msm_meta <- extract_asm_mdm(msd)
    meta <- c(device_meta, msm_meta)
    meta$file_version <- xx$`$asm.manifest`
    meta$operator <- xx[[2]][[1]][["analyst"]]
    meta$time_unit <- msd[["chromatogram data cube"]][["cube-structure"]][["dimensions"]][[1]][["unit"]]
    meta$detector_unit <- msd[["chromatogram data cube"]][["cube-structure"]][["measures"]][[1]][["unit"]]
    dat <- lapply(dat, function(x){
      attach_metadata(x, meta, format_in = metadata_format,
                      format_out = format_out, data_format = data_format,
                      parser = "chromconverter", source_file = path,
                      source_file_format = "allotrope_simple_model",
                      scale = FALSE)
    })
  }
  if (collapse) dat <- collapse_list(dat)
  dat
}

#' Read ASM 2D data
#' @author Ethan Bass
#' @noRd
read_asm_2d_data <- function(msd, idx = 1, data_format, format_out){
  cdc <- msd[["chromatogram data cube"]][["data"]]
  times <- cdc[["dimensions"]][[idx]][1,]
  int <- cdc[["measures"]][[idx]][1,]
  format_2d_chromatogram(rt = times, int = int, data_format = data_format,
                         format_out = format_out)
}

#' Extract ASM measurement document metadata
#' @author Ethan Bass
#' @noRd
extract_asm_mdm <- function(msd){
  idx <- names(msd) %in% c("sample document", "injection document",
                           "device control aggregate document",
                           "chromatography column document")
  selected_msd <- msd[idx]
  selected_msd$`device control aggregate document` <-
    selected_msd$`device control aggregate document`[[1]]
  lapply(selected_msd, function(df){
    if (inherits(df, "list")){
      names(df) <- sapply(df, function(x)x[[1]])
      df <- unlist(df, recursive = FALSE)
    }
    df <- do.call(data.frame, df)
    colnames(df) <- gsub("\\.+", "_", colnames(df))
    df[!duplicated(df),]
  })
}

#' Search ASM metadata list for string
#' @noRd
search_metadata <- function(x, str){
  x[grep(str, names(x))]
}
