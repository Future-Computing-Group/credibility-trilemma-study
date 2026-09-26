# Study test entry point. Run from the repository root:  Rscript tests/testthat.R
# The study is a project-style codebase, so the suite runs through test_dir();
# setup.R sources the companion's modules and the study's own.
suppressPackageStartupMessages(library(testthat))
testthat::test_dir(file.path("tests", "testthat"), reporter = "summary",
                   stop_on_failure = TRUE)
