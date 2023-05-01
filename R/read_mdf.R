#' Read MDF files into R
#' @param file Path to a 'Lumex' \code{.mdf} file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @return A chromatogram in the format specified by the \code{format_out} and
#' \code{data_format} arguments (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_mdf <- function(file, format_out = c("matrix","data.frame"),
                     data_format = c("wide","long"), read_metadata = TRUE){
  data_format <- match.arg(data_format, c("wide","long"))
  format_out <- match.arg(format_out, c("matrix","data.frame"))

  f <- file(file, "rb")

  # extract metadata
  metadata <- readBin(f, "character", n = 1)
  meta <- extract_mdf_metadata(metadata)
  # meta <- rbind(meta, c(Property = "source_file", file, Group = "Conversion"))

  array1_len <- as.numeric(meta[which(meta$Group == "Array photometric" & meta$Property == "Size"),"Value"])
  array2_len <- as.numeric(meta[which(meta$Group == "Array current" & meta$Property == "Size"),"Value"])

  # read array 1
  end_metadata <- seek(f,NA,"current") - 1
  seek(f, end_metadata, "start")
  seek(f, end_metadata, "start")
  photo_array <- readBin(f, "double", size=8, n=array1_len)

  # read array 2
  current_array <-readBin(f, "integer", size=4, n=array2_len)

  # close file
  close(f)

  t1 <- as.numeric(meta[which(meta$Group == "Interval Time" & meta$Property == "From"), "Value"])
  t2 <- as.numeric(meta[which(meta$Group == "Interval Time" & meta$Property == "To"), "Value"])
  t_step <- as.numeric(meta[which(meta$Group == "Interval Time" & meta$Property == "Step"), "Value"])

  # create time array
  time_array <- seq(t1, t2, by = t_step)

  # construct data.frame
  if (data_format == "wide"){
    data <- data.frame(Intensity = photo_array, Current = current_array, row.names=time_array)
  } else if (data_format == "long"){
    data <- data.frame(RT = time_array, Intensity = photo_array, Current = current_array)
  }

  if (data_format == "long"){
    data <- reshape_chrom(data)
  }
  if (format_out == "matrix"){
    as.matrix(format_out)
  }
  if (read_metadata){
    data <- attach_metadata(x = data, meta = meta, format_in = "mdf",
                            format_out = format_out, data_format = data_format,
                            parser = "chromconverter", source_file = file)
  }
  data
}

#' Extract MDF metadata
#' @noRd
extract_mdf_metadata <- function(x){
  ma <- strsplit(x, "\n")[[1]]
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  x2 <- splitAt(ma, grep("\\[*\\]", ma))
  x2 <- x2[-length(x2)]
  names(x2) <- gsub("\\[|\\]","",sapply(x2, function(xx) xx[[1]]))
  x3 <- lapply(seq_along(x2), function(i){
    xx <- x2[[i]][-1]
    xx <- as.data.frame(do.call(rbind,lapply(xx, function(xxx){
      stringr::str_split_fixed(xxx, "=", 2)
    })))
    xx[,3] <- names(x2)[i]
    colnames(xx) <- c("Property","Value","Group")
    xx
  })
  names(x3) <- names(x2)
  meta <- do.call(rbind, x3)
  meta <- meta[-which(meta$Property == ""),]
  meta
}

