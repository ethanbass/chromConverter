#' @name install_python_dependencies
#' @title Basic function to install python dependencies.
#' @import reticulate
#' @export
install_python_dependencies <- function(){
  py_install(c("scipy","numpy", "pandas"))
  py_install("aston", pip = T)
}
