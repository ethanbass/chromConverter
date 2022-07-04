.onLoad <- function(libname, pkgname){
  env <- reticulate::configure_environment("chromConverter")
  if (!env){
  reqs <- c("pandas","scipy","numpy","aston")
  reqs_available <- sapply(reqs, py_module_available)
  if (!all(reqs_available)){
    py_install(reqs[which(!reqs_available)], pip = TRUE)
  }
  }
  trace_file <<- reticulate::import("aston.tracefile", delay_load = TRUE)
  pd <<- reticulate::import("pandas", delay_load = TRUE)
  csv <<- reticulate::import("csv", delay_load = TRUE)
}
