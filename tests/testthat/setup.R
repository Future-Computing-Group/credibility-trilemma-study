# Sourced by testthat before any test file: the repository root is two levels
# above this directory, and R/p1.R knows how to load everything else.
study_root_dir <- normalizePath(file.path("..", ".."))
if (file.exists(file.path(study_root_dir, "R", "p1.R"))) {
  source(file.path(study_root_dir, "R", "p1.R"))
  study_load(study_root_dir)
}
