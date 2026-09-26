# make compute: run every block at the study's operating points and write the
# raw tables under results/<label>/, where the label names the companion tree
# the numbers come from: release-<tag> at the release pin, dev-<commit> before.
source(file.path("R", "p1.R"))
study_load(".")
sha <- p1_git(".", "rev-parse", "HEAD")
label <- if (identical(sha, p1_tag_commit("."))) paste0("release-", p1_release_tag) else
  paste0("dev-", substr(sha, 1, 7))
out <- file.path("results", label)
workers <- as.integer(Sys.getenv("STUDY_WORKERS",
                                 max(1L, parallel::detectCores() - 1L)))
res <- run_programme(study_defaults(), workers = workers, out_dir = out,
                     p1_commit = sha)
saveRDS(res, file.path(out, "results.rds"))
writeLines(label, file.path("results", "latest"))
cat(sprintf("%s: %d blocks on %d workers in %.0f s\n", out, res$timing$blocks,
            res$timing$workers, res$timing$elapsed_s))
