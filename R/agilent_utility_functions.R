#' Get Agilent offsets
#' @noRd
get_agilent_offsets <- function(version){
  if (version == "131_LC"){
    offsets <- list(version = 326,
                    file_type = 347,
                    sample_name = 858,
                    operator = 1880,
                    date = 2391,
                    detector_model = 2492,
                    method = 2574,
                    software = 3089,
                    units = 3093,
                    vial = 4055,
                    num_times = 278, #big-endian
                    rt_first = 282,
                    rt_last = 286,
                    scaling_factor = 3085,
                    data_start = 4096
    )
  } else if (version == "131_OL"){
    offsets <- list(version = 326,
                    file_type = 347,
                    sample_name = 858,
                    operator = 1880,
                    date = 2391,
                    # detector = 2492,
                    method = 2574,
                    # software = 3089,
                    units = 3093,
                    vial = 4055,
                    num_times = 278, #big-endian
                    rt_first = 282,
                    rt_last = 286,
                    scaling_factor = 3085,
                    data_start = 4096
    )
  } else if (version == "31"){
    offsets <- list(version = 0,
                    file_type = 4,
                    sample_name = 24,
                    operator = 148,
                    date = 178,
                    detector_model = 208,
                    instrument = 218,
                    method = 228,
                    # unknown = 260,
                    num_times = 278, # big-endian
                    scaling_factor = 318,
                    units = 326,
                    data_start = 512
    )
  } else if (version == "2"){
    offsets <- list(version = 0,
                    file_type = 4,
                    sample_name = 40,
                    operator = 148,
                    date = 178,
                    detector_model = 208,
                    instrument = 218,
                    method = 228,
                    # unknown = 260,
                    signal = 326,
                    header_length = 266,
                    num_times = 280, # big-endian
                    start_time = 282, #big-endian, 4 bytes
                    end_time = 286 #big-endian, 4 bytes
                    # scaling_factor = 318,
    )
  } else if (version %in% c("179","179_4b", "179_8b", "181")){
    offsets <- list(
      version = 326,
      file_type = 347, #0x15B
      sample_name = 858, #0x35A
      operator = 1880, #0x758
      date = 2391, # 0x957
      instrument = 2492, # 0x9BC
      method = 2574, # 0xA0E
      software = 3089, # 0xC11
      units = 4172, # 0x104C
      signal = 4213, # 0x1075
      num_times = 278, # 0x116
      rt_first = 282, # 0x11A
      rt_last = 286, # 0x11E
      scaling_factor = 4732, # 0x127C
      intercept = 4724,
      data_start = 4096 # 0x1000
    )
  } else if (version == "130"){
    offsets <- list(
      # sequence_line_or_injection = 252, #UINT16
      # injection_or_sequence_line = 256, #UINT16
      # data_offset = 264, # UINT32
      # start_time = 282,
      # end_time = 286,
      version = 326, # utf16
      file_type = 347, # utf16
      sample_name = 858, # utf16
      operator = 1880, # utf16
      date = 2391, # utf16
      inlet = 2492, # utf16
      instrument = 2533, # utf16'
      method = 2574, # utf16
      software = 3089, # 'utf16'
      software_version = 3601, #utf16'
      software_revision = 3802, #'utf16'
      vial = 4054,
      units = 4172, # 'utf16'
      signal = 4213, # 'utf16'
      intercept = 4110, # INT32
      scaling_factor = 4732) #ENDIAN + 'd'
  } else if (version == 30){
    offsets <- list(
      version = 0,
      file_type = 4, # utf16
      sample_name = 24, # utf16
      operator = 148, # utf16
      date = 178, # utf16
      detector_model = 208, # utf16'
      instrument = 218,
      method = 228, # utf16
      software = 322, # 'utf16'
      software_version = 355, #utf16'
      software_revision = 405, #'utf16'
      units = 580, # 'utf16'
      signal = 596, # 'utf16'
      intercept = 636, # INT32
      scaling_factor = 644,
      data_start = 1024 #ENDIAN + 'd'
    )
  } else if (version %in% c("8", "81")){
    offsets <- list(version = 0,
                    file_type = 4,
                    sample_name = 24,
                    description = 86,
                    operator = 148,
                    date = 178,
                    detector_model = 208,
                    instrument = 218,
                    method = 228,
                    units = 580,
                    num_times = 278,
                    rt_first = 282,
                    rt_last = 286,
                    scaling_toggle = 542,
                    scaling_factor = 644,
                    intercept = 636,
                    data_start = switch(version, "8" = 1024, "81" = 4096)
    )
  }
  offsets
}

#' Read ChemStation string
#' @noRd
read_cs_string <- function(f, type = 1, pos = NULL){
  if (!is.null(pos)){
    seek(f, where = pos, origin = "start")
  }
  n <- get_nchar(f)
  if (type == 1){
    tryCatch(rawToChar(readBin(f, what = "raw", n = n)), error = function(e) NA)
  } else if (type == 2){
    tryCatch(rawToChar(readBin(f, what = "raw", n = n*2)[c(TRUE, FALSE)]),
             error = function(e) NA)
  }
}

#' @noRd
cc_collapse <- function(x){
  paste(x, collapse="")
}

#' @noRd
cc_trim_str <- function(x, len = 2){
  substr(x, len, nchar(x))
}

#' Find .D folder
#' @noRd
get_chemstation_dir_name <- function(path){
  dir <- gsub(basename(path), "", path)
  sp <- str_split_fixed(dir, "/", stringr::str_count(dir, "/") + 1)[1,]
  grep("\\.D|\\.d$", sp, ignore.case = TRUE, value = TRUE)
}

#' Get number of characters for Agilent segment
#' @noRd
get_nchar <- function(f){
  as.numeric(readBin(f, what = "raw", n = 1))
}