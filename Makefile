# Every target runs from the repository root. `make reproduce` is the whole
# chain: the companion platform's suite at the pinned tree, this study's suite,
# the computation, and the table bodies the manuscript inputs.
R ?= Rscript

.PHONY: test test-p1 test-study compute tables reproduce

test: test-p1 test-study

# The companion's own suite, at the submodule's tree, with failures propagated.
test-p1:
	cd p1 && $(R) -e 'suppressPackageStartupMessages(library(testthat)); testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)'

test-study:
	$(R) tests/testthat.R

compute:
	$(R) scripts/compute.R

tables:
	$(R) scripts/tables.R

reproduce: test compute tables
