.onLoad <- function(libname, pkgname){
  reticulate::py_require(packages = get_parser_reqs("default"))
}
