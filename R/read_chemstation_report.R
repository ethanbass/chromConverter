#' Read Agilent Chemstation Report
#' @param file Path to file
#' @param data_format Format to output data. Either \code{chromatographr} or
#' \code{chemstation}.
#' @param metadata_format Format to output metadata. Either \code{list} or
#' \code{data.frame}.
#' @author Ethan Bass
#' @noRd

read_chemstation_report <- function(file, data_format = c("chromatographr", "chemstation"),
                                    metadata_format = c("list","data.frame")){
  data_format <- match.arg(tolower(data_format), c("chromatographr", "chemstation"))
  metadata_format = match.arg(metadata_format, c("list","data.frame"))
  x <- readLines(file, encoding = "UTF-16LE", skipNul = TRUE)
  x[1] <- gsub("\xff\xfe", "", x[1], useBytes = TRUE)
  x <- gsub("\xb5", "<b5>", x, useBytes = TRUE)

  sections <- grep("=====================================================================", x)
  metadata <- x[(sections[1]+1):(sections[2]-1)]
  metadata <- remove_blank_lines(metadata)
  merge_lines <- function(xx){
    idx <- grep(":", xx, invert = TRUE)
    xx[idx-1] <- paste0(xx[idx-1], xx[idx])
    xx <- xx[-idx]
    xx
  }
  metadata <- gsub("^\\s+","", metadata)
  metadata <- gsub("\\s+\\:\\s+", " : ", metadata)
  metadata <- merge_lines(metadata)
  metadata <- unlist(strsplit(metadata, "(?<!\\s:\\s)\\s{2,}(?!\\s)", perl = TRUE))

  sample_info <- x[1:(sections[1]-1)]
  sample_info <- remove_blank_lines(sample_info)
  sample_info[1] <- gsub("Data File", "Data File:", sample_info[1])

  metadata <- c(sample_info[1:2], metadata)

  metadata <- strsplit(metadata, " ?: ")

  if (metadata_format == "data.frame"){
    # return metadata as data.frame
    df <- as.data.frame(do.call(rbind,metadata))
    colnames(df) <- c("Field","Value")
  } else if (metadata_format == "list"){
    # return metadata as list
    names(metadata) <- sapply(metadata, function(x)x[1])
    metadata <- lapply(metadata, function(x) x[2])
  }
  signals <- grep("Signal [0-9]:", x)
  signals <- c(signals, (grep("End of Report", x)-2))

  peak_lists <- lapply(seq_along(signals[-length(signals)]),function(i){
    table <- x[signals[i]:(signals[i+1]-1)]
    convert_chemstation_peaklist(table, data_format = data_format)
  })
  names(peak_lists) <- x[signals[-length(signals)]]
  peak_lists
}

#' Convert 'Chemstation' REPORT peak list to data.frame.
#' @param table The table to convert.
#' @param data_format Format to output data. Either \code{chromatographr} or
#' \code{chemstation}.
#' @author Ethan Bass
#' @noRd
convert_chemstation_peaklist <- function(table, data_format =
                                           c("chemstation","chromatographr")){
  markdown_table <- table[-which(table == "")]
  split.pos <- c(1,gregexpr("\\|",markdown_table[4])[[1]])
  header1 <- sapply(seq_len(length(split.pos)-1), function(i){
    substr(markdown_table[2], split.pos[i], split.pos[i+1]-1)
  })
  header2 <- sapply(seq_len(length(split.pos)-1), function(i){
    substr(markdown_table[3], split.pos[i], split.pos[i+1]-1)
  })
  header <- paste(header1, header2)
  header <- gsub("\\s+$|^\\s+", "", header)
  header <- gsub("\\s+", " ", header)
  # line <- markdown_table[10]
  rows <- lapply(markdown_table[5:(length(markdown_table)-1)], function(line){
    gsub("\\s+", "", sapply(seq_len(length(split.pos)-1), function(i){
      substr(line, split.pos[i], split.pos[i+1]-1)
    }))
  })

  # Create the dataframe
  df <- data.frame(matrix(unlist(rows), nrow = length(rows), byrow = TRUE),
                   stringsAsFactors = FALSE)
  colnames(df) <- header
  df2 <- as.data.frame(purrr::map_df(df[,-c(which(colnames(df)=="Type"))],
                                     as.numeric))
  df2 <- cbind(df2, df[, "Type", drop = FALSE])
  if (data_format == "chromatographr"){
    df2 <- df2[, -c(1,6)]
    colnames(df2) <- c("rt", "width", "area", "height", "type")
    df2
  }
}

#' Remove blank lines
#' @noRd
remove_blank_lines <- function(x){
  x[which(x != "")]
}

#' Read Agilent Chemstation Reports
#' @param files Paths to Chemstation report files.
#' @param data_format Format to output data. Either \code{chromatographr} or
#' \code{chemstation}.
#' @param metadata_format Format to output metadata. Either \code{list} or
#' \code{data.frame}.
#' @author Ethan Bass
#' @export

read_chemstation_reports <- function(files, data_format = c("chromatographr", "chemstation"),
                                     metadata_format = c("list","data.frame")){
  data_format <- match.arg(tolower(data_format), c("chromatographr", "chemstation"))
  metadata_format = match.arg(metadata_format, c("list","data.frame"))
  names(files) <- sub(".*/([^/]+)\\.D/.*$", "\\1", files)
  pks <- lapply(seq_along(files), function(i){
    xx <- read_chemstation_report(files[i], data_format = data_format,
                                  metadata_format = metadata_format)
    dat <- lapply(seq_along(xx), function(ii){
      lambda <- sub(".*Sig=([0-9]+).*", "\\1", names(xx)[ii])
      cbind(sample = names(files)[i], lambda = lambda, xx[[ii]])
    })
    names(dat) <- sub(".*Sig=([0-9]+).*", "\\1", names(xx))
    dat
  })
  names(pks) <- names(files)
  structure(pks,
            chrom_list = NA,
            lambdas = names(pks[[1]]), fit = "chemstation", sd.max = NA,
            max.iter = NA,
            time.units = "min",
            class = "peak_list")
}
