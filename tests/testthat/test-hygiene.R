# Release guard: no tracked file and no commit message may carry
# authoring-environment or process text into a public repository.
#
# The file scan runs over `git ls-files`, which is exactly what a release
# publishes. This file is skipped by the scan because it has to spell every
# needle out. Commit messages are read from `git log`, so an attribution
# trailer is caught even though no tracked file carries it.

leak_patterns <- c(
  "absolute workspace path" = "/Users/|Dropbox|5 Support|Groups/FCG",
  "internal campaign log"   = "OPSLOG",
  "lesson number"           = "\\bL[0-9]{2,3}\\b",
  "review-round code"       = "\\bR[123]\\.[0-9]\\b|R&R",
  "review vocabulary"       = "(?i)\\b(reviewers?|referees?|revisions?|rebuttals?)\\b",
  "ladder label"            = "(?i)\\brungs?\\b",
  "internal directory"      = "\\bReview/|\\bPlans?/|planning/",
  "internal task id"        = "\\bT[0-9]\\.[0-9]\\b|\\bD1[0-4]\\b|\\bNA-[0-9]",
  "process vocabulary"      = "(?i)\\b(amendments?|registered|frozen|pre-registration|protocol deviations?)\\b",
  "internal host"           = "(?i)\\b(nrouter|puhti|mahti|roihu|5gtn)\\b|csc\\.fi",
  "development date"        = "\\b20(2[5-9]|[3-9][0-9])-[01][0-9]-[0-3][0-9]\\b",
  "AI co-author trailer"    = "Co-Authored-By"
)

binary_pattern <- "\\.(rds|png|jpe?g|gif|pdf|zip|gz|ico|woff2?)$"
guard_file <- "tests/testthat/test-hygiene.R"

scan_lines <- function(label, lines) {
  hits <- character()
  for (nm in names(leak_patterns)) {
    m <- grepl(leak_patterns[[nm]], lines, perl = TRUE)
    if (any(m)) hits <- c(hits, sprintf("%s:%d [%s] %s", label, which(m), nm,
                                        substr(lines[m], 1, 100)))
  }
  hits
}

# The release archive is a tarball with no .git: there nothing is tracked and
# there is no history, and the archive was built from a tree this guard passed.
skip_outside_checkout <- function(root) {
  skip_if_not(file.exists(file.path(root, ".git")),
              "not a git checkout: nothing tracked to scan")
}

test_that("no tracked file carries internal or process text", {
  root  <- study_root_dir
  skip_outside_checkout(root)
  files <- system2("git", c("-C", shQuote(root), "ls-files"), stdout = TRUE)
  files <- setdiff(files[!grepl(binary_pattern, files)], c(guard_file, "p1"))
  hits  <- unlist(lapply(files, function(f) {
    p <- file.path(root, f)
    if (!file.exists(p) || dir.exists(p)) return(character())
    scan_lines(f, readLines(p, warn = FALSE))
  }))
  expect_identical(hits, character(0), info = paste(hits, collapse = "\n"))
})

test_that("no commit message carries internal text or an attribution trailer", {
  root <- study_root_dir
  skip_outside_checkout(root)
  msgs <- tryCatch(system2("git", c("-C", shQuote(root), "log", "--format=%B"),
                           stdout = TRUE, stderr = FALSE),
                   warning = function(w) character(0))
  hits <- scan_lines("commit message", msgs)
  expect_identical(hits, character(0), info = paste(hits, collapse = "\n"))
})
