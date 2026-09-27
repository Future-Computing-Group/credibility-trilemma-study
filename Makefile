# Every target runs from the repository root. `make reproduce` is the whole
# chain: the companion platform's suite at the pinned tree, this study's suite,
# the computation, and the table bodies the manuscript inputs.
R ?= Rscript

.PHONY: test test-p1 test-study compute tables reproduce

test: test-p1 test-study

# The companion's own suite, at the submodule's commit, with failures
# propagated. In a git checkout it runs in a scratch clone of that commit:
# inside a submodule `.git` is a file holding a relative gitdir, which one of
# the companion's tests reads from its own working directory and cannot
# resolve. From the release archive (no .git) it runs in a scratch copy of p1/,
# so the suite writes nothing into the extracted tree.
test-p1:
	tmp=$$(mktemp -d) && \
	if [ -e p1/.git ]; then \
	  git clone -q p1 "$$tmp/p1" && \
	  git -C "$$tmp/p1" checkout -q $$(git -C p1 rev-parse HEAD); \
	else cp -R p1 "$$tmp/p1"; fi && \
	cd "$$tmp/p1" && $(R) -e 'suppressPackageStartupMessages(library(testthat)); testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)'

test-study:
	$(R) tests/testthat.R

compute:
	$(R) scripts/compute.R

tables:
	$(R) scripts/tables.R

reproduce: test compute tables
