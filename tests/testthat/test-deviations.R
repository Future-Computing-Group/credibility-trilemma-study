# The operator's deviations: Arm P, the perturbation of the payment
# perturbation lemma inside the exact contested window, re-run through the
# mechanism; Arm G, a phantom row re-run through the mechanism; and the
# per-agent rationalisation check of the deviation definition.

# The reserve halves the active population, so the tree needs a tighter
# capacity than the fan for the revenue-optimal arm to be contended. A round
# realises at most one triple per saturated block, so the fixture pools rounds.
armp_fixture <- function(inst, prior, mech_name, N = 40L,
                         cap = if (inst == "T") 0.15 else 0.3, rounds = 1:6) {
  bind_rows(lapply(rounds, function(t) {
    r <- fixture_round(inst, N = N, cap_scale = cap, t = t, prior = prior)
    mech <- mechanism(mech_name, if (mech_name == "myerson") r$pr)
    ap <- arm_p_round(r$b, mech, r$model, r$pr, study_defaults()$thetas)
    if (!"j" %in% names(ap)) ap$j <- NA_integer_
    ap$cross <- r$pr$fam[ap$i] != r$pr$fam[ifelse(is.na(ap$j), ap$i, ap$j)]
    ap
  }))
}

test_that("Arm P: inside the window the allocation stays and the increment is the lemma's", {
  for (inst in c("T", "S")) for (prior in c("a", "b")) for (mn in c("vcg", "myerson")) {
    ap <- armp_fixture(inst, prior, mn)
    lab <- paste(inst, prior, mn)
    expect_gt(sum(ap$realised), 0, label = lab)
    rz <- ap[ap$realised, ]
    expect_true(all(rz$x_unchanged), info = lab)
    expect_lt(max(abs(rz$dp - rz$dp_formula)), 1e-12)
    expect_true(all(rz$gamma == 1), info = lab)
    expect_true(all(rz$rationalised), info = lab)
    expect_true(all(rz$dp > 0), info = lab)
    # Beyond the first window entry the pair's order flips and the lemma stops.
    nc <- rz[!is.na(rz$nc_departs), ]
    expect_gt(nrow(nc), 0)
    expect_true(all(nc$nc_departs), info = lab)
  }
})

test_that("Arm P on the worked example raises agent 1's payment by delta", {
  m  <- fixture_worked_example()
  pr <- data.frame(fam = "unif", lo = 0, hi = 20)[c(1, 1), ]
  tr <- contested_triple(1L, c(10, 5), mechanism("vcg"), m, pr)
  expect_identical(tr$j, 2L)
  expect_equal(tr$gamma, 1)
  expect_equal(tr$dbar, 5)             # min(10 - 5, +inf, 20 - 5)
  ap <- arm_p_round(c(10, 5), mechanism("vcg"), m, pr, thetas = 0.2)
  row <- ap[ap$i == 1 & ap$theta == 0.2, ]
  expect_equal(row$dp, 1)
  expect_equal(row$dp_formula, 1)
})

test_that("Arm P on branch (V) with heterogeneous priors follows the pullback", {
  ap <- armp_fixture("T", "b", "myerson", N = 60L)
  rz <- ap[ap$realised, ]
  crossed <- rz$cross
  expect_gt(sum(crossed), 0)
  expect_true(any(abs(rz$dp[crossed] - rz$delta[crossed]) > 1e-6))
  expect_lt(max(abs(rz$dp - rz$dp_formula)), 1e-12)
})

test_that("Arm G: the phantom is re-run through the mechanism and passes each agent's check", {
  for (mn in c("vcg", "myerson")) {
    r  <- fixture_round("T", N = 40L, cap_scale = 0.3, prior = "b")
    mech <- mechanism(mn, if (mn == "myerson") r$pr)
    g  <- arm_g_round(r$b, mech, r$model, r$pr, prior = "b", k = r$k,
                       uniform_leaves = study_defaults()$prior_b_uniform_leaves)
    expect_gt(nrow(g), 0)
    expect_true(all(g$rationalised))
    expect_true(all(g$phantom_in_support))
    expect_true(all(g$class %in% c("nonwin", "win_nodisp", "win_disp")))
    expect_true(all(c("nonwin", "win_disp") %in% g$class))
    expect_true(all(g$conc_w_num[g$class == "nonwin"] == 0))
    expect_true(all(g$conc_w_num[g$class == "win_disp"] > 0))
    expect_true(any(g$rev_gain > 0))
  }
})

test_that("the rationalisation check refuses a payment set outside a mechanism", {
  r    <- fixture_round("S", N = 40L)
  mech <- mechanism("vcg")
  out  <- mech_run(mech, r$b, r$model)
  i    <- which(out$x > 0 & out$p > 0)[1]
  expect_true(rationalises(i, out$x[i], out$p[i], r$b, r$b[i], mech, r$model, r$pr))
  expect_false(rationalises(i, out$x[i], out$p[i] * 1.1, r$b, r$b[i], mech, r$model, r$pr))
  wit <- r$b
  wit[setdiff(seq_along(wit), i)[1]] <- 2 * r$k        # outside the support
  expect_false(rationalises(i, out$x[i], out$p[i], wit, r$b[i], mech, r$model, r$pr))
})

test_that("pay-as-bid: no operator gain from either arm at any round", {
  for (inst in c("T", "S")) for (t in 1:2) {
    r  <- fixture_round(inst, t = t, prior = "a")
    fp <- mechanism("fp")
    ap <- arm_p_round(r$b, fp, r$model, r$pr, 0.5, triples_from = mechanism("vcg"))
    expect_true(all(ap$dp[ap$realised] <= 1e-12))
    g  <- arm_g_round(r$b, fp, r$model, r$pr, prior = "a", k = r$k)
    expect_true(all(g$rev_gain <= 1e-12))
  }
})

test_that("static: every deviation runs a mechanism and nothing sets a payment by fiat", {
  src <- readLines(file.path(study_root_dir, "R", "deviations.R"))
  for (fn in c("arm_p_agent", "arm_g_eval", "rationalises")) {
    body <- paste(deparse(get(fn)), collapse = "\n")
    expect_match(body, "mech_(run|agent)\\(", info = fn)
  }
  for (f in list.files(file.path(study_root_dir, "R"), full.names = TRUE)) {
    s <- readLines(f)
    expect_false(any(grepl("detectable\\s*(<-|=)\\s*(TRUE|FALSE)", s)), info = f)
    expect_false(any(grepl("(\\bp\\b|p_i|pay|payment)[^#]*\\*\\s*\\(\\s*1\\s*\\+", s)), info = f)
  }
  # No payment variable is assigned in the deviation or ledger code: payments
  # there are only read off mechanism outputs.
  for (f in c("deviations.R", "ledger.R")) {
    s <- sub("#.*$", "", readLines(file.path(study_root_dir, "R", f)))
    expect_false(any(grepl("(^|[^.$\\w])(p|p_i|pay|payment|payments)(\\[[^]]*\\])?\\s*(<-|=[^=])", s, perl = TRUE)),
                 info = f)
  }
})
