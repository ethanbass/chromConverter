#' Write Agilent AMX method file
#'
#' Writes a copy of an 'OpenLab CDS' acquisition method (`.amx`) with a new
#' pump gradient, flow, stop time or post time. All other settings are copied
#' unchanged from the template.
#'
#' @details
#' `gradient` takes the table that [read_agilent_amx()] returns as
#' `pump$gradient`, in wide or long format. It needs a percentage for every
#' solvent channel the template uses, and each row must add up to 100. The row
#' at time zero sets the starting composition, and the other rows replace the
#' composition steps in the pump timetable. Flow steps in the timetable are
#' kept, and `gradient` cannot change them.
#'
#' Setting `stop_time` also sets the run time of the method. The checksum of
#' each edited part is recalculated. The `Agilent/ReportableInformation` part,
#' which repeats the settings as formatted text, is not rewritten and still
#' describes the template.
#'
#' The writer has only been tested on methods for a G7104C quaternary pump.
#'
#' @param path Path to the template `.amx` file.
#' @param path_out Path to write the new method.
#' @param gradient A table of solvent composition over time (see Details).
#' `NULL` (default) keeps the template's gradient.
#' @param flow Flow rate in mL/min. `NULL` (default) keeps the template's
#' value.
#' @param stop_time Pump stop time in minutes. `NULL` (default) keeps the
#' template's value.
#' @param post_time Pump post time in minutes. `NULL` (default) keeps the
#' template's value.
#' @param force Whether to overwrite `path_out` if it exists. Defaults to
#' `FALSE`.
#' @return Invisibly returns `path_out`.
#' @examples \dontrun{
#' gradient <- data.frame(time_min = c(0, 2, 8),
#'                        pct_A = c(70, 70, 5),
#'                        pct_B = c(30, 30, 95))
#' write_agilent_amx("template.amx", "new.amx", gradient = gradient,
#'                   stop_time = 10)
#' }
#' @seealso [read_agilent_amx()]
#' @family write functions
#' @export
write_agilent_amx <- function(path, path_out, gradient = NULL, flow = NULL,
                              stop_time = NULL, post_time = NULL,
                              force = FALSE){
  path <- fs::path_expand(path)
  path_out <- fs::path_expand(path_out)
  if (fs::file_exists(path_out) && !force){
    stop("`path_out` already exists. Use `force = TRUE` to overwrite it.",
         call. = FALSE)
  }
  check_amx_scalar(flow, "flow")
  check_amx_scalar(stop_time, "stop_time")
  check_amx_scalar(post_time, "post_time")

  files <- unzip(path, list = TRUE)$Name
  pump_part <- grep("PumpDriver(?!.*\\.chk$)", files, perl = TRUE,
                    value = TRUE)
  if (length(pump_part) != 1){
    stop("Expected one pump driver in the method, found ", length(pump_part),
         ".", call. = FALSE)
  }
  props_part <- "MethodProperties/MethodProperties"

  tmp <- temp_directory(path)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  unzip(path, files = c(pump_part, props_part), exdir = tmp)
  pump <- xml2::read_xml(fs::path(tmp, pump_part))
  props <- xml2::read_xml(fs::path(tmp, props_part))

  if (!is.null(gradient)){
    set_amx_gradient(pump, gradient)
  }
  if (!is.null(flow)){
    set_amx_value(pump, "/PumpMethod/Flow", flow)
  }
  if (!is.null(stop_time)){
    set_amx_time(pump, "StopTime", stop_time)
    set_amx_value(props, "//*[local-name()='MethodRuntimeInSeconds']",
                  round(stop_time * 60))
  }
  if (!is.null(post_time)){
    set_amx_time(pump, "PostTime", post_time)
  }

  stop_min <- get_amx_num(pump, "/PumpMethod/StopTime/StopTimeValue")
  times <- as.numeric(xml2::xml_text(
    xml2::xml_find_all(pump, "/PumpMethod/Timetable/TimetableEntry/Time")
  ))
  if (!is.na(stop_min) && any(times > stop_min)){
    warning("The timetable runs past the stop time of ", stop_min, " min.",
            call. = FALSE)
  }

  write_amx_part(pump, fs::path(tmp, pump_part))
  write_amx_part(props, fs::path(tmp, props_part))

  out <- fs::path(tmp, "out.amx")
  fs::file_copy(path, out)
  owd <- setwd(tmp)
  on.exit(setwd(owd), add = TRUE, after = FALSE)
  changed <- c(pump_part, props_part)
  status <- utils::zip(out, files = c(changed, paste0(changed, ".chk")),
                       flags = "-X -D -q")
  if (status != 0){
    stop("The archive could not be written. `utils::zip` needs a zip ",
         "program: see `?utils::zip`.", call. = FALSE)
  }
  fs::file_copy(out, path_out, overwrite = TRUE)
  invisible(path_out)
}

#' Replace the solvent composition steps of an AMX pump method
#' @noRd
set_amx_gradient <- function(doc, gradient){
  gradient <- as.data.frame(gradient)
  if ("channel" %in% names(gradient)){
    gradient <- as.data.frame(tidyr::pivot_wider(gradient,
                                                 names_from = "channel",
                                                 values_from = "percent",
                                                 names_prefix = "pct_"))
  }
  if (any(c("flow_mL_min", "pct_flow") %in% names(gradient))){
    stop("`gradient` cannot change the flow. Drop its flow column to keep ",
         "the flow steps in the template.", call. = FALSE)
  }

  solvents <- xml2::xml_find_all(doc,
                                 "/PumpMethod/SolventComposition/SolventElement")
  channels <- sub("^Channel_", "",
                  xml2::xml_text(xml2::xml_find_first(solvents, "./Channel")))
  used <- as.logical(xml2::xml_text(xml2::xml_find_first(solvents, "./Used")))
  pct_cols <- paste0("pct_", channels[used])
  if (!"time_min" %in% names(gradient) ||
      !setequal(grep("^pct_", names(gradient), value = TRUE), pct_cols)){
    stop("`gradient` needs a `time_min` column and one column for each ",
         "channel the method uses: ",
         paste(sQuote(pct_cols, q = FALSE), collapse = ", "), ".",
         call. = FALSE)
  }

  time <- gradient$time_min
  pct <- as.matrix(gradient[pct_cols])
  if (!is.numeric(time) || anyNA(time) || any(time < 0) ||
      anyDuplicated(time)){
    stop("`time_min` must hold distinct, non-negative times.", call. = FALSE)
  }
  if (!is.numeric(pct) || anyNA(pct) || any(pct < 0 | pct > 100)){
    stop("Percentages must be between 0 and 100.", call. = FALSE)
  }
  if (any(abs(rowSums(pct) - 100) > 1e-6)){
    stop("Each row of `gradient` must add up to 100%.", call. = FALSE)
  }
  ord <- order(time)
  time <- time[ord]
  pct <- pct[ord, , drop = FALSE]

  if (length(time) > 0 && time[1] == 0){
    for (i in which(used)){
      node <- xml2::xml_find_first(solvents[[i]], "./Percentage")
      xml2::xml_text(node) <- format_amx_num(pct[1, paste0("pct_",
                                                           channels[i])])
    }
    time <- time[-1]
    pct <- pct[-1, , drop = FALSE]
  }

  timetable <- xml2::xml_find_first(doc, "/PumpMethod/Timetable")
  if (inherits(timetable, "xml_missing")){
    stop("The pump method has no timetable.", call. = FALSE)
  }
  entries <- xml2::xml_find_all(timetable, "./TimetableEntry")
  xml2::xml_remove(
    entries[xml2::xml_attr(entries, "type") == "ChangeSolventCompositionType"]
  )
  for (i in seq_along(time)){
    entry <- xml2::xml_add_child(timetable, "TimetableEntry")
    xml2::xml_set_attr(entry, "xsi:type", "ChangeSolventCompositionType",
                       ns = xml2::xml_ns(doc))
    xml2::xml_set_attr(entry, "ObjectID", random_guid())
    xml2::xml_add_child(entry, "Time", format_amx_num(time[i]))
    for (j in seq_along(channels)){
      value <- if (used[j]) pct[i, paste0("pct_", channels[j])] else 0
      xml2::xml_add_child(entry, paste0("Percent", channels[j]),
                          format_amx_num(value))
    }
  }
}

#' Set a pump stop or post time
#' @noRd
set_amx_time <- function(doc, what, value){
  section <- xml2::xml_find_first(doc, paste0("/PumpMethod/", what))
  if (inherits(section, "xml_missing")){
    stop("The pump method has no ", what, " setting.", call. = FALSE)
  }
  set_amx_value(section, paste0("./", what, "Mode"), paste0(what, "_Set"))
  node <- xml2::xml_find_first(section, paste0("./", what, "Value"))
  if (inherits(node, "xml_missing")){
    node <- xml2::xml_add_child(section, paste0(what, "Value"))
  }
  xml2::xml_text(node) <- format_amx_num(value)
}

#' Set the text of an existing AMX node
#' @noRd
set_amx_value <- function(doc, xpath, value){
  node <- xml2::xml_find_first(doc, xpath)
  if (inherits(node, "xml_missing")){
    stop("Could not find `", xpath, "` in the method.", call. = FALSE)
  }
  xml2::xml_text(node) <- if (is.numeric(value)) format_amx_num(value) else value
}

#' Write an AMX part and its checksum
#'
#' Matches the .NET serializer, which writes no declaration or trailing
#' newline and puts a space before `/>`, so that unedited parts keep their
#' checksums.
#' @noRd
write_amx_part <- function(doc, file){
  x <- sub("\n$", "", as.character(doc, options = "no_declaration"))
  x <- gsub("([^ ])/>", "\\1 />", x)
  writeBin(charToRaw(enc2utf8(x)), file)
  writeBin(digest::digest(file = file, algo = "sha1", raw = TRUE),
           paste0(file, ".chk"))
}

#' Format a number for an AMX part
#' @noRd
format_amx_num <- function(x){
  format(x, digits = 15, scientific = FALSE, trim = TRUE)
}

#' Check a numeric AMX setting
#' @noRd
check_amx_scalar <- function(x, arg){
  if (!is.null(x) && (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0)){
    stop("`", arg, "` must be a single non-negative number.", call. = FALSE)
  }
}

#' Generate a random version 4 GUID
#' @noRd
random_guid <- function(){
  hex <- sample(c(0:9, letters[1:6]), 32, replace = TRUE)
  hex[13] <- "4"
  hex[17] <- sample(c("8", "9", "a", "b"), 1)
  hex <- paste(hex, collapse = "")
  paste(substring(hex, c(1, 9, 13, 17, 21), c(8, 12, 16, 20, 32)),
        collapse = "-")
}
