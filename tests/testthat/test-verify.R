# Broadcast verification on the clinching transcript: honest transcripts pass,
# each mutation of the commitment proof's cases is rejected by the verifier
# that case names, and the phantom passes once the roster clause is removed.

verify_fixture <- function(inst, prior, t) {
  r    <- fixture_round(inst, N = 40L, cap_scale = 0.3, t = t, prior = prior)
  vcg  <- mechanism("vcg")
  ap   <- arm_p_round(r$b, vcg, r$model, r$pr, 0.5)
  g    <- arm_g_round(r$b, vcg, r$model, r$pr, prior, r$k)
  sel  <- if (any(ap$realised)) ap[ap$realised, ][which.max(ap$dp[ap$realised]), ] else NULL
  best <- g[which.max(g$rev_gain), ]
  list(r = r, sel = sel,
       muts = clinch_mutations(r$b, r$model,
                               phantom = list(type = best$leaf, exit = best$bid_ph),
                               forged  = if (!is.null(sel)) list(j = sel$j, delta = sel$delta)))
}

test_that("honest transcripts pass every verifier", {
  for (inst in c("T", "S")) for (prior in c("a", "b")) for (t in 1:2) {
    f <- verify_fixture(inst, prior, t)
    v <- verify_transcript(f$muts$honest$tr, f$r$b, f$r$model)
    expect_false(any(v), info = paste(inst, prior, t, names(v)[v]))
  }
})

test_that("each mutation is rejected by the verifier its proof case names", {
  seen <- character()
  for (inst in c("T", "S")) for (t in 1:3) {
    f <- verify_fixture(inst, "a", t)
    for (m in setdiff(names(f$muts), "honest")) {
      mu <- f$muts[[m]]
      if (!mu$realised) next
      seen <- c(seen, m)
      v <- verify_transcript(mu$tr, f$r$b, f$r$model)
      expect_true(v[[mutation_verifier[[m]]]], info = paste(inst, t, m))
      if (m %in% c("price", "active", "s", "rank")) expect_gt(mu$revenue, 0)
    }
  }
  expect_setequal(unique(seen), names(mutation_verifier))
})

test_that("with the roster clause removed the phantom passes every verifier", {
  f <- verify_fixture("S", "a", 1L)
  v <- verify_transcript(f$muts$phantom$tr, f$r$b, f$r$model, roster_check = FALSE)
  expect_false(any(v))
  expect_true(verify_transcript(f$muts$phantom$tr, f$r$b, f$r$model)[["roster"]])
})

test_that("the forged-message run is clinching at the perturbed profile", {
  f <- verify_fixture("T", "a", 1L)
  skip_if(is.null(f$sel), "no realised triple in this fixture round")
  bh <- f$r$b
  bh[f$sel$j] <- bh[f$sel$j] + f$sel$delta
  ref <- mech_run(mechanism("vcg"), bh, f$r$model)
  expect_equal(f$muts$forged$tr$pay, ref$p, tolerance = 1e-12)
  # On a public transcript the raised demand moves every payment it blocks,
  # the realising agent's by the lemma's increment among them.
  expect_equal(f$muts$forged$revenue,
               sum(ref$p) - sum(mech_run(mechanism("vcg"), f$r$b, f$r$model)$p),
               tolerance = 1e-12)
  expect_gte(f$muts$forged$revenue, f$sel$dp - 1e-12)
})

test_that("static: no verifier reads the operator's surplus", {
  s <- readLines(file.path(study_root_dir, "R", "verify.R"))
  expect_false(any(grepl("surplus\\s*>", s)))
})
