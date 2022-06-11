
# helper function to test equality
elementwise.all.equal <- Vectorize(function(x, y, ...) {isTRUE(all.equal(x, y, ...))})

# helper function to skip tests if we don't have the right python dependencies
skip_if_missing_dependecies <- function() {
  reqs <- c("scipy","numpy", "aston", "pandas")
  have_reqs <- sapply(reqs, py_module_available)
  if (mean(have_reqs) < 1)
    skip(paste("required packages", reqs[!have_reqs],
               "not available for testing"))
}

skip_if_missing_thermorawfileparser <- function() {
  reqs <- c("scipy","numpy", "aston", "pandas")
  have_reqs <- sapply(reqs, py_module_available)
  if (mean(have_reqs) < 1)
    skip(paste("required packages", reqs[!have_reqs],
               "not available for testing"))
}
