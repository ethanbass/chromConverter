#' Write Agilent AMX method file
#'
#' Writes a copy of an 'OpenLab CDS' acquisition method (`.amx`) with a new
#' pump gradient, flow, stop time, post time or column temperature. All other
#' settings are copied unchanged from the template.
#'
#' @details
#' `gradient` takes the table that [read_agilent_amx()] returns as
#' `pump$gradient`, in wide or long format. It needs a percentage for every
#' solvent channel the template uses, and each row must add up to 100. It
#' must have a row at time zero, which sets the starting composition. The
#' other rows replace the composition steps in the pump timetable. Flow steps
#' in the timetable are kept, and `gradient` cannot change them. If the
#' template has flow steps, drop the `flow_mL_min` column (or the `"flow"` rows
#' in long format) from the table [read_agilent_amx()] returns.
#'
#' Setting `flow` gives a constant flow rate and removes the template's flow
#' steps.
#'
#' `temperature` sets both sides of the column compartment, and the template
#' must already control the temperature. A template whose compartment is set
#' to "Not Controlled" gives an error.
#'
#' Setting `stop_time` also sets the run time of the method. The checksum of
#' each edited part is recalculated. The `Agilent/ReportableInformation` part,
#' which repeats the settings as formatted text, is not rewritten and still
#' describes the template.
#'
#' The writer needs the 'zip' package, and has only been tested on
#' methods for a G7104C quaternary pump and a G7116A column compartment.
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
#' @param temperature Column temperature in degrees Celsius. `NULL` (default)
#' keeps the template's value.
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
                              temperature = NULL, force = FALSE){
  path <- fs::path_expand(path)
  path_out <- fs::path_expand(path_out)
  if (fs::file_exists(path_out) && !force){
    stop("`path_out` already exists. Use `force = TRUE` to overwrite it.",
         call. = FALSE)
  }
  check_amx_scalar(flow, "flow")
  check_amx_scalar(stop_time, "stop_time")
  check_amx_scalar(post_time, "post_time")
  check_amx_scalar(temperature, "temperature")
  check_for_pkg("zip")

  entries <- zip::zip_list(path)
  files <- entries$filename
  pump_part <- grep("PumpDriver(?!.*\\.chk$)", files, perl = TRUE,
                    value = TRUE)
  if (length(pump_part) != 1){
    stop("Expected one pump driver in the method, found ", length(pump_part),
         ".", call. = FALSE)
  }
  comp_part <- grep("ColumnCompDriver(?!.*\\.chk$)", files, perl = TRUE,
                    value = TRUE)
  if (!is.null(temperature) && length(comp_part) != 1){
    stop("Expected one column compartment in the method, found ",
         length(comp_part), ".", call. = FALSE)
  }
  props_part <- "MethodProperties/MethodProperties"
  if (!props_part %in% files){
    stop("The method has no `", props_part, "` part.", call. = FALSE)
  }

  tmp <- temp_directory(path)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  parts <- fs::path(tmp, "parts")
  unzip(path, exdir = parts, setTimes = TRUE)
  pump <- xml2::read_xml(fs::path(parts, pump_part))
  props <- xml2::read_xml(fs::path(parts, props_part))

  if (!is.null(gradient)){
    set_amx_gradient(pump, gradient)
  }
  if (!is.null(flow)){
    set_amx_value(pump, "/PumpMethod/Flow", flow)
    steps <- xml2::xml_find_all(pump, "/PumpMethod/Timetable/TimetableEntry")
    xml2::xml_remove(steps[xml2::xml_attr(steps, "type") == "ChangeFlowType"])
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

  write_amx_part(pump, fs::path(parts, pump_part))
  write_amx_part(props, fs::path(parts, props_part))
  if (!is.null(temperature)){
    comp <- xml2::read_xml(fs::path(parts, comp_part))
    set_amx_temperature(comp, temperature)
    write_amx_part(comp, fs::path(parts, comp_part))
  }

  out <- fs::path(tmp, "out.amx")
  zip_amx(entries, parts, out)
  fs::file_copy(out, path_out, overwrite = TRUE)
  invisible(path_out)
}

#' Replace the solvent composition steps of an AMX pump method
#' @noRd
set_amx_gradient <- function(doc, gradient){
  gradient <- as.data.frame(gradient)
  if (nrow(gradient) == 0){
    stop("`gradient` has no rows.", call. = FALSE)
  }
  if ("channel" %in% names(gradient)){
    if ("time_min" %in% names(gradient) &&
        anyDuplicated(gradient[c("time_min", "channel")])){
      stop("`gradient` has more than one percentage for a channel at the ",
           "same time.", call. = FALSE)
    }
    gradient <- as.data.frame(tidyr::pivot_wider(gradient,
                                                 names_from = "channel",
                                                 values_from = "percent",
                                                 names_prefix = "pct_"))
  }
  if (any(c("flow_mL_min", "pct_flow") %in% names(gradient))){
    stop("`gradient` cannot change the flow. Drop its flow column to keep ",
         "the flow steps in the template, or set `flow` for a constant flow.",
         call. = FALSE)
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
  if (!is.numeric(time) || !all(is.finite(time)) || any(time < 0) ||
      anyDuplicated(time)){
    stop("`time_min` must hold distinct, non-negative times.", call. = FALSE)
  }
  if (!is.numeric(pct)){
    stop("Percentages must be numbers.", call. = FALSE)
  }
  if (anyNA(pct)){
    stop("`gradient` needs a percentage for every channel at every time.",
         call. = FALSE)
  }
  if (any(pct < 0 | pct > 100)){
    stop("Percentages must be between 0 and 100.", call. = FALSE)
  }
  if (any(abs(rowSums(pct) - 100) > 1e-6)){
    stop("Each row of `gradient` must add up to 100%.", call. = FALSE)
  }
  ord <- order(time)
  time <- time[ord]
  pct <- pct[ord, , drop = FALSE]
  if (time[1] != 0){
    stop("`gradient` needs a row at time 0 for the starting composition.",
         call. = FALSE)
  }

  for (i in which(used)){
    node <- xml2::xml_find_first(solvents[[i]], "./Percentage")
    xml2::xml_text(node) <- format_amx_num(pct[1, paste0("pct_", channels[i])])
  }
  time <- time[-1]
  pct <- pct[-1, , drop = FALSE]

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

#' Set the temperature of both sides of an AMX column compartment
#' @noRd
set_amx_temperature <- function(doc, temperature){
  nodes <- xml2::xml_find_all(
    doc, paste0("/ColumnCompMethod/*[self::LeftTemperatureControl or ",
                "self::RightTemperatureControl]",
                "[TemperatureControlMode = 'TemperatureSet']/Temperature")
  )
  if (length(nodes) != 2){
    stop("The template's column compartment does not control the ",
         "temperature of both sides. Set a temperature in the template.",
         call. = FALSE)
  }
  xml2::xml_text(nodes) <- format_amx_num(temperature)
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
  format(x, digits = 15, scientific = FALSE, trim = TRUE, decimal.mark = ".")
}

#' Zip AMX parts in the template's order, storing the entries it stored
#' @noRd
zip_amx <- function(entries, root, zipfile){
  stored <- entries$compressed_size == entries$uncompressed_size
  runs <- cumsum(c(TRUE, diff(stored) != 0))
  for (run in unique(runs)){
    i <- runs == run
    add <- if (run == 1) zip::zip else zip::zip_append
    add(zipfile, entries$filename[i], root = root, include_directories = FALSE,
        compression_level = if (stored[i][1]) 0 else 9)
  }
  set_amx_zip_headers(zipfile)
}

#' Rewrite the zip header fields that OpenLab's package reader checks
#'
#' Writes them as OpenLab does: made by DOS, version 4.5; version needed 1.0
#' for stored entries and 2.0 for deflated ones; no UTF-8 flag; no file
#' attributes.
#' @noRd
set_amx_zip_headers <- function(zipfile){
  x <- readBin(zipfile, "raw", file.size(zipfile))
  u16 <- function(i){
    readBin(x[i + 0:1], "integer", size = 2, signed = FALSE, endian = "little")
  }
  u32 <- function(i) readBin(x[i + 0:3], "integer", size = 4, endian = "little")
  put <- function(i, value, size = 2){
    x[i + seq_len(size) - 1] <<- writeBin(as.integer(value), raw(), size = size,
                                          endian = "little")
  }
  eocd <- utils::tail(grepRaw(as.raw(c(0x50, 0x4b, 0x05, 0x06)), x,
                              all = TRUE), 1)
  p <- u32(eocd + 16) + 1
  for (k in seq_len(u16(eocd + 10))){
    needed <- if (u16(p + 10) == 8) 20 else 10
    flags <- bitwAnd(u16(p + 8), 0xF7FF)
    put(p + 4, 45)
    put(p + 6, needed)
    put(p + 8, flags)
    put(p + 38, 0, size = 4)
    local <- u32(p + 42) + 1
    put(local + 4, needed)
    put(local + 6, flags)
    p <- p + 46 + u16(p + 28) + u16(p + 30) + u16(p + 32)
  }
  writeBin(x, zipfile)
}

#' Check a numeric AMX setting
#' @noRd
check_amx_scalar <- function(x, arg){
  if (!is.null(x) && (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 0)){
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
