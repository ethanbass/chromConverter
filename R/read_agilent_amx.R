#' Read Agilent AMX method file
#'
#' Parses an Agilent `.amx` method archive, extracting instrument parameters
#' from one or more of its driver sub-files.
#'
#' @importFrom utils unzip
#' @param path Path to the `.amx` file.
#' @param what One or more instrument modules to parse. Any
#'   combination of `"dad"`, `"pump"`, `"comp"`, and `"sampler"`. Defaults to
#'   all four.
#' @param path_out Directory into which the archive
#'   is extracted. If `NULL` (default), a temporary directory is used and
#'   cleaned up on exit.
#' @param format_out Class of output (for tables). Either `"data.frame"`,
#' `"tibble"` or `"data.table"`.
#' @param gradient_format Whether to return the gradient in `"wide"` (default)
#' or `"long"` format.
#'
#' @return A named list with one element per parsed module, plus `"metadata"`.
#'   Elements present depend on `what`; see below for the structure of each.
#'
#'   **`metadata`** — a list with scalar elements:
#'   \describe{
#'     \item{`method_name`}{Original method name.}
#'     \item{`version`}{Method version string.}
#'     \item{`status`}{Approval state.}
#'     \item{`created`}{Creation timestamp (`POSIXct`, UTC).}
#'     \item{`created_by`}{Username of creator.}
#'     \item{`modified`}{Last-modified timestamp (`POSIXct`, UTC).}
#'     \item{`modified_by`}{Username of last modifier.}
#'   }
#'
#'   **`pump`** — a list with scalar elements `flow_mL_min`, `stop_time_min`,
#'   `post_time_min`, `pressure_low_bar`, `pressure_high_bar`, plus:
#'   \describe{
#'     \item{`solvents`}{A data.frame of active solvent channels: `channel`,
#'       `percentage`, `solvent`.}
#'     \item{`gradient`}{A data.frame of timetable entries. Wide format (default):
#'       `time_min` plus one `pct_<channel>` column per active channel. Long
#'       format: `time_min`, `channel`, `percent`.}
#'   }
#'
#'   **`dad`** — a list with scalar elements `peakwidth_nm`, `slitwidth_nm`,
#'   `uv_lamp_required`, `vis_lamp_required`, `spectra_from_nm`,
#'   `spectra_to_nm`, `spectra_step_nm`, plus:
#'   \describe{
#'     \item{`signals`}{A data.frame of active signals: `id`, `wavelength_nm`,
#'       `bandwidth_nm`.}
#'   }
#'
#'   **`comp`** — a list with scalar element `post_time_min`, plus:
#'   \describe{
#'     \item{`temp_controls`}{Two-row data.frame (Left/Right): `side`,
#'       `temperature_C`, `not_ready_limit_C`, `equilibration_time_min`.}
#'   }
#'
#'   **`sampler`** — a list with scalar elements: `thermostat_installed`,
#'   `draw_speed_uL_min`, `eject_speed_uL_min`,
#'   `wait_after_draw_min`, `injection_volume_uL`,  `wash_time_s`.
#'
#' @examples \dontrun{
#' read_agilent_amx(path)
#' }
#' @export
read_agilent_amx <- function(path, what = c("dad", "pump", "comp", "sampler"),
                             path_out = NULL,
                             format_out = c("data.frame", "tibble",
                                            "data.table"),
                             gradient_format = c("wide", "long")){
  what <- match.arg(what, c("dad", "pump", "comp", "sampler"),
                    several.ok = TRUE)
  format_out <- match.arg(format_out, c("data.frame", "tibble", "data.table"))
  gradient_format <- match.arg(gradient_format, c("wide", "long"))

  files <- unzip(path, list = TRUE)
  files_sel <- lapply(what, function(w){
    rgx <- sprintf("%sDriver(?!.*\\.chk$)", simple_cap(w))
    grep(rgx, files$Name, perl = TRUE, value = TRUE)
  })
  names(files_sel) <- what

  # grep("CollectorDriver(?!.*\\.chk$)", files$Name, perl = TRUE)

  if (length(files_sel) > 1){
    what <- what[vapply(files_sel, length, FUN.VALUE = numeric(1)) > 0]
  }
  files_sel <- c(metadata=grep("ReportableInformation",
                               files$Name, value = TRUE), files_sel)
  what <- c("metadata", what)
  if (is.null(path_out)) {
    tmp <- tempdir()
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  }
  path_out <- fs::path(tmp, basename(path))
  fs::dir_create(path_out, force = TRUE)
  unzip(path, files = unlist(files_sel), exdir = path_out)
  files.path <- lapply(files_sel, function(fl){
    fs::path(path_out, fl)
  })
  xx <- purrr::imap(files.path, function(w, name){
    fn <- switch(name, metadata = parse_reportable_information,
                 dad = purrr::partial(parse_dad_method,
                                      format_out = format_out),
                 pump = purrr::partial(parse_pump_method,
                                       format_out = format_out,
                                       gradient_format = gradient_format),
                 comp = purrr::partial(parse_column_method,
                                       format_out = format_out),
                 sampler = parse_multisampler_method)
    fn(w)
  })
  renamer <- c(comp = "column", sampler = "autosampler")
  idx <- match(names(xx), names(renamer))
  names(xx)[!is.na(idx)] <- renamer[idx[!is.na(idx)]]
  xx
}

#' Parse reportable information
#' @noRd
parse_reportable_information <- function(file_path) {

  doc <- xml2::read_xml(file_path)

  get_value <- function(id) {
    xpath <- sprintf(".//*[local-name()='Parameter'][*[local-name()='ID']='%s']/*[local-name()='Value']", id)
    get_amx_val(doc, xpath)
  }

  list(
    method_name  = get_value("ID_Method_Original"),
    version      = get_value("ID_Method_Version"),
    status       = get_value("ID_Method_ApprovalState"),
    created      = as.POSIXct(sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2",
                                  get_value("ID_Method_Created")),
                              format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC"),
    created_by   = get_value("ID_Method_CreatedBy"),
    modified     = as.POSIXct(sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2",
                                  get_value("ID_Method_Modified")),
                              format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC"),
    modified_by  = get_value("ID_Method_ModifiedBy")
  )
}

#' Parse pump method
#' @noRd
parse_pump_method <- function(file_path, format_out = "data.frame",
                              gradient_format = c("wide", "long")) {

  gradient_format <- match.arg(gradient_format, c("wide", "long"))
  doc <- xml2::read_xml(file_path)

  solvent_nodes <- xml2::xml_find_all(doc, ".//SolventElement")

  solvents <- purrr::map_dfr(solvent_nodes, function(node) {
    data.frame(
      channel    = get_amx_val(node, ".//Channel"),
      in_use     = get_amx_bool(node, ".//Used"),
      percentage = get_amx_num(node, ".//Percentage"),
      solvent    = get_amx_val(node, ".//Name")
    )
  })
  solvents <- solvents[solvents$in_use, setdiff(names(solvents), "in_use")]

  active_channels <- gsub("Channel_", "", solvents$channel)

  timetable_nodes <- xml2::xml_find_all(doc, ".//TimetableEntry")

  composition_nodes <- timetable_nodes[
    xml2::xml_attr(timetable_nodes, "type") == "ChangeSolventCompositionType"
  ]
  flow_nodes <- timetable_nodes[
    xml2::xml_attr(timetable_nodes, "type") == "ChangeFlowType"
  ]

  gradient <- purrr::map_dfr(composition_nodes, function(node) {
    data.frame(
      time_min = get_amx_num(node, ".//Time"),
      channel  = c("A", "B", "C", "D"),
      percent  = c(
        get_amx_num(node, ".//PercentA"),
        get_amx_num(node, ".//PercentB"),
        get_amx_num(node, ".//PercentC"),
        get_amx_num(node, ".//PercentD")
      )
    )
  })

  flow_gradient <- purrr::map_dfr(flow_nodes, function(node) {
    data.frame(
      time_min = get_amx_num(node, ".//Time"),
      channel  = "flow",
      percent  = get_amx_num(node, ".//Flow")
    )
  })

  if (nrow(gradient) != 0){
    gradient <- gradient[gradient$channel %in% active_channels, ]

    zero_row <- data.frame(
      time_min = 0,
      channel  = active_channels,
      percent  = solvents$percentage
    )

    if (!any(gradient$time_min == 0)) {
      gradient <- rbind(zero_row, gradient)
    }
  }
  if (nrow(flow_gradient) != 0) {
    flow_zero <- data.frame(
      time_min = 0,
      channel  = "flow",
      percent  = get_amx_num(doc, "/PumpMethod/Flow")
    )
    if (!any(flow_gradient$time_min == 0)) {
      flow_gradient <- rbind(flow_zero, flow_gradient)
    }
  }
  if (nrow(gradient) != 0 || nrow(flow_gradient) != 0){
    gradient <- rbind(gradient, flow_gradient)
    gradient <- gradient[order(gradient$time_min), ]
  }
  if (nrow(gradient) != 0 && gradient_format == "wide") {
    gradient <- tidyr::pivot_wider(gradient, names_from = "channel",
                                   values_from = "percent",
                                   names_prefix = "pct_")
    if ("pct_flow" %in% names(gradient)) {
      names(gradient)[names(gradient) == "pct_flow"] <- "flow_mL_min"
      gradient <- tidyr::fill(gradient, "flow_mL_min", .direction = "down")
    }
  }

  gradient <- convert_format_out(gradient, format_out = format_out)
  list(
    flow_mL_min       = get_amx_num(doc, "/PumpMethod/Flow"),
    stop_time_min     = get_amx_num(doc, "/PumpMethod/StopTime/StopTimeValue"),
    post_time_min     = get_amx_num(doc, "/PumpMethod/PostTime/PostTimeValue"),
    pressure_low_bar  = get_amx_num(doc, "/PumpMethod/LowPressureLimit"),
    pressure_high_bar = get_amx_num(doc, "/PumpMethod/HighPressureLimit"),
    solvents          = solvents,
    gradient          = gradient
  )
}

#' Parse DAD method
#' @noRd
parse_dad_method <- function(file_path, format_out = "data.frame") {

  doc <- xml2::read_xml(file_path)

  signal_nodes <- xml2::xml_find_all(doc, ".//Signal")

  signals <- purrr::map_dfr(signal_nodes, function(node) {
    data.frame(
      id            = get_amx_val(node, ".//ID"),
      in_use        = get_amx_bool(node, ".//UseSignal"),
      wavelength_nm = suppressWarnings(
        get_amx_num(node, ".//Wavelength")
      ),
      bandwidth_nm  = suppressWarnings(
        get_amx_num(node, ".//Bandwidth")
      )
    )
  })
  signals <- signals[signals$in_use, setdiff(names(signals), "in_use")]
  signals <- convert_format_out(signals, format_out = format_out)
  list(
    peakwidth_nm      = get_amx_num(doc, ".//Peakwidth"),
    slitwidth_nm      = get_amx_num(doc, ".//Slitwidth"),
    uv_lamp_required  = get_amx_bool(doc, ".//UVLampRequired"),
    vis_lamp_required = get_amx_bool(doc, ".//VISLampRequired"),
    spectra_from_nm   = get_amx_num(doc, ".//SpectraRangeFrom"),
    spectra_to_nm     = get_amx_num(doc, ".//SpectraRangeTo"),
    spectra_step_nm   = get_amx_num(doc, ".//SpectraStep"),
    signals           = signals
  )
}

#' Parse multisampler method
#' @noRd
parse_multisampler_method <- function(file_path) {

  doc <- xml2::read_xml(file_path)

  config_raw <- get_amx_val(doc, ".//ConfigurationXml")
  config     <- xml2::read_xml(config_raw)

  list(
    thermostat_installed    = get_amx_bool(config, ".//IsThermostatInstalled"),
    draw_speed_uL_min       = get_amx_num(doc, ".//DrawSpeed"),
    eject_speed_uL_min      = get_amx_num(doc, ".//EjectSpeed"),
    wait_after_draw_min     = get_amx_num(doc, ".//WaitTimeAfterDrawing"),
    injection_volume_uL     = get_amx_num(doc, ".//InjectionVolume"),
    wash_time_s             = get_amx_num(doc, ".//WashTime")
  )
}

#' Parse column compartment method
#' @noRd
parse_column_method <- function(file_path, format_out = "data.frame") {

  doc <- xml2::read_xml(file_path)

  temp_controls <- purrr::map_dfr(c("Left", "Right"), function(side) {
    node <- xml2::xml_find_first(doc, paste0(".//", side, "TemperatureControl"))
    data.frame(
      side                   = side,
      temperature_C          = get_amx_num(node, ".//Temperature"),
      not_ready_limit_C      = get_amx_num(node, ".//NotReadyLimitValue"),
      equilibration_time_min = get_amx_num(node, ".//EquilibrationTime")
    )
  })
  temp_controls <- convert_format_out(temp_controls, format_out = format_out)
  list(
    post_time_min = get_amx_num(doc, ".//PostTimeValue"),
    temp_controls = temp_controls
  )
}

#' Get AMX value
#' @noRd
get_amx_val <- function(doc, path, fun = identity) {
  node <- xml2::xml_find_first(doc, path)
  if (is.na(node)) return(NA)
  fun(xml2::xml_text(node))
}

#' Get AMX number
#' @noRd
get_amx_num <- purrr::partial(get_amx_val, fun = as.numeric)

#' Get AMX bool
#' @noRd
get_amx_bool <- purrr::partial(get_amx_val, fun = as.logical)

#' Convert format
#' @noRd
convert_format_out <- function(x, format_out = c("data.frame", "data.table",
                                                 "tibble")) {
  format_out <- match.arg(format_out, c("data.frame", "data.table", "tibble"))
  switch(format_out,
         data.frame  = as.data.frame(x),
         data.table  = data.table::as.data.table(x),
         tibble = tibble::tibble(x)
  )
}
