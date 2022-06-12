utils::globalVariables(names = c('.'))

check_parser_match <- function(format_in, parser){
  allowed_formats <- list(openchrom = c("msd","csd","wsd"),
                          chromconverter = c("chemstation_csv", "shimadzu_fid", "shimadzu_dad",
                                             "chromeleon_uv", "waters_arw", "mzml"),
                          aston = c("chemstation_uv", "masshunter_dad", "other"),
                          entab = c("chemstation_uv", "masshunter_dad", "other"),
                          thermoraw = c("thermoraw")
  )
  if (!(format_in %in% allowed_formats[[parser]])){
    # message("The ", paste0(sQuote(parser), " parser must take one of the following formats: \n",
    #             paste(sQuote(allowed_formats[[parser]]), collapse=", "), ". \n"))
    stop("Mismatched arguments! \n \n The ", paste0(sQuote(format_in), " format can be converted using the following parsers: ",
      paste(sQuote(names(allowed_formats)[grep("chemstation_uv", allowed_formats)]), collapse = ", "), ". \n \n",
      "Please double check your arguments and try again."))
  }
}

find_files <- function(paths, pattern){
  files <- unlist(lapply(paths, function(path){
    files <- list.files(path = path, pattern = pattern,
                        full.names = TRUE, recursive = TRUE)
    if (length(files)==0){
      if (!dir.exists(path)){
        warning(paste0("The directory '", basename(path), "' does not exist."))
      } else{
        warning(paste0("No files matching the pattern '", pattern,
                       "' were found in '", basename(path), "'"))
      }
    }
    files
  }))
}

export_csvs <- function(data, path.out){
  sapply(seq_along(data), function(i){
    write.csv(data[[i]], file = paste0(paste0(path.out, names(data)[i]),".CSV"))
  })
}

set_temp_directory <- function(){
  ans <- readline("Export directory not specified! Export files to `temp` directory (y/n)?")
  if (ans %in% c("y","Y")){
    if (!dir.exists("temp"))
      dir.create("temp")
    path_out <- paste0(getwd(),'/temp/')
    path_out
  } else{
    stop("Must specify directory to export files.")
  }
}


extract_header <- function(x, chrom.idx){
  index <- chrom.idx+1
  line <- x[index]
  l <- length(strsplit(line,"\t")[[1]])
  header <- strsplit(line,"\t")[[1]]
  while (l>1) {
    index <- index+1
    line <- strsplit(x[index], "\t")[[1]]
    l <- length(line)
    if (l == 1 | suppressWarnings(!is.na(as.numeric(line[1]))))
      break
    header <- rbind(header, line)
  }
  list(header,index)
}
