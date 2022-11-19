.onLoad <- function(libname, pkgname){
  env <- reticulate::configure_environment("chromConverter")
  trace_file <<- reticulate::import("aston.tracefile", delay_load = TRUE)
  pd <<- reticulate::import("pandas", delay_load = TRUE)
  csv <<- reticulate::import("csv", delay_load = TRUE)
  rb_read <<- reticulate::import("rainbow.__init__", delay_load=TRUE)
  rb_parse_agilent <<- reticulate::import("rainbow.agilent", delay_load=TRUE)
}
