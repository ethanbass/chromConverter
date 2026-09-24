if (requireNamespace("pbapply", quietly = TRUE)){
  old_pboptions <- pbapply::pboptions(type = "none")
  withr::defer(pbapply::pboptions(old_pboptions), testthat::teardown_env())
}
