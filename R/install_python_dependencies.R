#' @name install_python_dependencies
#' @title Basic function to install python dependencies.
#' @import reticulate
#' @param ... Additional commands to \code{\link[reticulate]{py_install}}
#' @export
install_python_dependencies <- function(...){
  py_install(c("scipy","numpy", "pandas"), ...)
  py_install("aston", pip = T)
}
