# The study sources five files of the companion platform and nothing else. The
# numbers it reports are valid for exactly those blobs, so the blobs are
# recorded and compared, and the release pin is its own test.

test_that("the companion's five modules are loaded from the submodule", {
  expect_setequal(names(p1_sourced_blobs),
                  c("structure_instances.R", "sim_helpers.R", "sim_market.R",
                    "sim_nodelevel.R", "sim_exp7.R"))
  for (fn in c("leaf_rank", "lb_optimum", "polymatroid_certificate", "flow_rank",
               ".greedy_pack_by", "vcg_allocate", "dsic_certificate",
               "exact_pack_by_value", "node_env", "node_exact_pair",
               "exp7_environment", "exp7_certify", "misreport_strategy_set",
               "exp7_agent_gains", "task_expected_value", ".ratio_interval")) {
    expect_true(exists(fn, mode = "function"), info = fn)
  }
})

test_that("the sourced blobs at the submodule's commit are the recorded ones", {
  head <- p1_git(study_root_dir, "rev-parse", "HEAD")
  for (f in names(p1_sourced_blobs)) {
    expect_identical(p1_git(study_root_dir, "rev-parse", paste0("HEAD:R/", f)),
                     p1_sourced_blobs[[f]], info = f)
  }
  expect_true(head %in% c(p1_dev_sha, p1_tag_commit(study_root_dir)))
})

test_that("the submodule sits at the release tag and its version DOI is published", {
  tag_commit <- p1_tag_commit(study_root_dir)
  if (is.na(tag_commit)) {
    skip(paste0("companion release ", p1_release_tag, " is not cut yet: ",
                "development runs against commit ", substr(p1_dev_sha, 1, 7),
                " and no number from this tree is a reported number"))
  }
  expect_identical(p1_git(study_root_dir, "rev-parse", "HEAD"), tag_commit)
  for (f in names(p1_sourced_blobs)) {
    expect_identical(p1_git(study_root_dir, "rev-parse",
                            paste0(p1_release_tag, ":R/", f)),
                     p1_sourced_blobs[[f]], info = f)
  }
  # Base R's libcurl, not the curl binary, which the job's container lacks.
  status <- attr(curlGetHeaders(paste0("https://doi.org/", p1_version_doi),
                                redirect = TRUE), "status")
  expect_identical(status, 200L)
})
