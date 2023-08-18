
# helper function to test equality
elementwise.all.equal <- Vectorize(function(x, y, ...) {isTRUE(all.equal(x, y, ...))})

# helper function to skip tests if we don't have the right python dependencies
skip_if_missing_dependecies <- function() {
  reqs <- c("scipy","numpy", "aston", "pandas")
  have_reqs <- sapply(reqs, reticulate::py_module_available)
  if (mean(have_reqs) < 1)
    skip(paste("required packages", reqs[!have_reqs],
               "not available for testing"))
}

skip_if_missing_thermorawfileparser <- function() {
  if (.Platform$OS.type != "windows"){
    path <- readLines(system.file("shell/thermofileparser.sh", package = "chromConverter"))[2]
    path <- strsplit(path," ")[[1]][2]
  } else {
    path <- readLines(system.file("shell/path_parser.txt", package = "chromConverter"))
    path <- gsub("\\\\", "/", path)
  }
  have_reqs <- fs::file_exists(path)
  if (mean(have_reqs) < 1)
    skip(paste("required packages", reqs[!have_reqs],
               "not available for testing"))
}

skip_if_missing_openchrom <- function() {
  have_openchrom_cli <- switch(configure_openchrom(cli="status"), true = TRUE,
         false = FALSE)
  if (!have_openchrom_cli){
    skip(paste("required packages", reqs[!have_reqs],
               "not available for testing"))
  }
}
