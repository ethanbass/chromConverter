.onLoad <- function(libname, pkgname){
  try(env <- reticulate::configure_environment("chromConverter"))
  try({trace_file <<- reticulate::import("aston.tracefile", delay_load = TRUE)}, silent=TRUE)
  try({pd <<- reticulate::import("pandas", delay_load = TRUE)}, silent = TRUE)
  try({csv <<- reticulate::import("csv", delay_load = TRUE)}, silent = TRUE)
  try({rb_read <<- reticulate::import("rainbow.__init__", delay_load=TRUE)}, silent = TRUE)
  try({rb_parse_agilent <<- reticulate::import("rainbow.agilent", delay_load=TRUE)}, silent = TRUE)
  try({olefile <<- reticulate::import("olefile", delay_load=TRUE)}, silent = TRUE)
}
