# Loading the companion platform and the study.
#
# The study sources five files of the companion's R/ directory from the git
# submodule `p1/` and adds only its own objects on top. The blobs of those five
# files are recorded here: every number the study computes is valid for
# exactly these blobs, and tests/testthat/test-p1-pin.R compares them.

p1_release_tag <- "v2.1.0"
p1_version_doi <- "10.5281/zenodo.22876728"

# The commit development ran against before the release tag existed. Its
# sim_nodelevel.R blob (d3e831c) differs from the tag's only in the
# demand-responsive posted-price tuning helpers, which the study does not call.
p1_dev_sha <- "ca989fb6d6b55ec6a5265d83853c4097a376bb46"

# Blob ids of the sourced files at the release tag, in load order.
p1_sourced_blobs <- c(
  structure_instances.R = "65d44b143d964c10babc906da66f93cc95e7d0c5",
  sim_helpers.R         = "e33a2be3ccd85097565e04c1a53182c7d0cffb54",
  sim_market.R          = "1f670f0fca5a57eef8a8458672fff954013d6475",
  sim_nodelevel.R       = "ce5f6e58c6e6bd1f35c263a1b39dba1dec5a0bb4",
  sim_exp7.R            = "7a1182e370ae4dc046417f2cf696b457a9fc46d5"
)

# The study's own modules, in load order.
study_modules <- c("study_tasks.R", "rank.R", "mechanisms.R", "deviations.R",
                   "verify.R", "ledger.R", "programme.R", "tables.R")

#' Run git inside the submodule and return its one line of output.
p1_git <- function(root, ...) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(file.path(root, "p1")),
                                           ...), stdout = TRUE, stderr = FALSE))
  if (length(out) == 0L) NA_character_ else out[[1]]
}

#' The commit the release tag names in the submodule, or NA if it is not cut.
p1_tag_commit <- function(root) {
  p1_git(root, "rev-parse", "--verify", "--quiet",
         paste0(p1_release_tag, "^{commit}"))
}

#' Source the companion's five modules and every study module present.
study_load <- function(root) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(purrr)
    library(tidyr)
  })
  for (f in names(p1_sourced_blobs)) {
    source(file.path(root, "p1", "R", f), local = globalenv())
  }
  for (f in study_modules) {
    p <- file.path(root, "R", f)
    if (file.exists(p)) source(p, local = globalenv())
  }
  invisible(TRUE)
}
