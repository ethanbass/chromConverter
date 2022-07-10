.onLoad <- function(libname, pkgname){
  env <- reticulate::configure_environment("chromConverter")
  trace_file <<- reticulate::import("aston.tracefile", delay_load = TRUE)
  pd <<- reticulate::import("pandas", delay_load = TRUE)
  csv <<- reticulate::import("csv", delay_load = TRUE)
}
