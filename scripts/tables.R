# make tables: the four table bodies and the numbers file for each operating
# point, from the latest `make compute` output.
source(file.path("R", "p1.R"))
study_load(".")
out <- file.path("results", readLines(file.path("results", "latest")))
res <- readRDS(file.path(out, "results.rds"))
for (pt in unique(res$round$point)) {
  write_tables(res, file.path(out, pt), point = pt)
  cat("tables written to", file.path(out, pt, "tables"), "\n")
}
