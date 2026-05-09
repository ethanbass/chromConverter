#' Read 'Agilent' ACAML files from directory.
#'
#' Extracts injection metadata from 'Agilent Common Analytical Markup Language'
#' (ACAML) files into an R object.
#'
#' ACAML is an XML-based format used by Agilent OpenLab to store sequence and
#' sample metadata. This function extracts information from the
#' `InjectionMetaData` nodes embedded in the `InjectionMetaDataItems`
#' custom field files, which do not seem to be readily accessible through other
#' means.
#'
#' @inheritParams shared_params
#' @param path Path(s) to ACAML files or to folders that contain the files.
#' @param find_files Logical. Set to `TRUE` (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to `FALSE`.
#' @return A `data.frame`, `data.table` or `tibble` (according to the value of
#' `format_out`) containing sample metadata derived from the supplied ACAML
#' files.
#' @examples \dontrun{
#' read_acaml(path)
#' }
#' @export
read_acaml <- function(path, find_files,
                       format_out = c("data.frame", "data.table", "tibble"),
                       progress_bar = TRUE, cl = 1){
  format_out <- match.arg(format_out, c("data.frame", "data.table", "tibble"))
  laplee <- choose_apply_fnc(progress_bar, cl = cl)
  if (missing(find_files)){
    find_files <- !all(file_test("-f", path))
  }
  exists <- dir.exists(path) | file.exists(path)
  if (all(!exists)){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  if (find_files){
    files <- find_files(path, "acaml")
  } else {
    files <- path
  }
  acaml_list <- laplee(files, function(f) {
    df <- read_acaml_single(f, format_out = format_out)
    df$SourceFile <- basename(f)
    df
  })
  do.call(rbind, acaml_list)
}

#' Read 'Agilent' ACAML
#'
#' Extracts injection metadata from a single 'Agilent Common Analytical Markup
#' Language' (ACAML) file into a `data.frame`.
#'
#' ACAML is an XML-based format used by Agilent OpenLab to store sequence and
#' sample metadata. This function extracts information from the
#' `InjectionMetaData` nodes embedded in the `InjectionMetaDataItems`
#' custom field files, which do not seem to be readily accessible through other
#' means.
#'
#' @param path Path to an `ACAML` file.
#' @noRd

read_acaml_single <- function(path, format_out = c("data.frame","data.table",
                                                   "tibble")) {
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)

  injections <- xml2::xml_find_all(doc,
                                   ".//*[local-name()='InjectionMetaData']")

  df <- purrr::map(injections, function(node) {
    attrs <- xml2::xml_attrs(node)

    val_nodes <- c("ReplicateNumber", "SampleAmount",
                   "SampleInjectionsCount", "SampleOrderNumber")
    val_data <- purrr::map(val_nodes, function(tag) {
      child <- xml2::xml_find_first(node, paste0("*[local-name()='", tag, "']"))
      if (is.na(child)) return(stats::setNames(list(NA), tag))
      v <- xml2::xml_attr(child, "val")
      u <- xml2::xml_attr(child, "unit")
      out <- list(v)
      names(out) <- tag
      if (!is.na(u) && nzchar(u)) out[[paste0(tag, "_unit")]] <- u
      out
    }) |> purrr::list_flatten()

    text_nodes <- c("Dil", "Mult", "Locked")
    text_data <- purrr::map(text_nodes, function(tag) {
      child <- xml2::xml_find_first(node, paste0("*[local-name()='", tag, "']"))
      v <- if (is.na(child)) NA else xml2::xml_text(child)
      stats::setNames(list(v), tag)
    }) |> purrr::list_flatten()

    tibble::as_tibble_row(c(as.list(attrs), val_data, text_data))
  }) |>
    purrr::list_rbind()
  int_cols <- c("SampleOrderNumber", "SampleInjectionsCount", "ReplicateNumber")
  df[int_cols] <- lapply(df[int_cols], as.integer)
  df$SampleAmount <- as.numeric(df$SampleAmount)
  df$LastModifiedDateTime <- as.POSIXct(
    sub("([+-][0-9]{2}:[0-9]{2})$", "", df$LastModifiedDateTime),
    format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
  )
  df$InjectionAcqDateTime  <- as.POSIXct(df$InjectionAcqDateTime,
                                         format = "%Y-%m-%dT%H:%M:%OSZ",
                                         tz = "UTC")
  if (format_out == "data.frame"){
    df <- as.data.frame(df)
  } else if (format_out == "data.table"){
    df <- data.table::as.data.table(df)
  }
  df
}
