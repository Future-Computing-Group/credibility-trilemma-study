# The mechanisms on the companion's kernel: VCG (the companion's), the
# revenue-optimal virtual-value greedy with Archer-Tardos payments, pay-as-bid,
# and the ascending clinching auction on the rank oracle.

vcg_p1 <- function(r) {
  d <- study_defaults()
  out <- vcg_allocate(r$tasks, r$env, d$util_hat, base_latency_for_bids(r$env),
                      init_success_model(), lambda_l_default = d$lambda_l)
  x <- as.numeric(r$tasks$task_id %in% out$task_id)
  p <- numeric(nrow(r$tasks))
  p[match(out$task_id, r$tasks$task_id)] <- out$vcg_payment
  list(x = x, p = p)
}

test_that("value-greedy is the exact optimum on T and S study rounds", {
  for (inst in c("T", "S")) for (t in 1:3) {
    r    <- fixture_round(inst, t = t, prior = "b")
    pair <- node_exact_pair(r$env, r$tasks, r$b)
    expect_equal(pair[["greedy"]], pair[["exact"]], tolerance = 1e-12)
  }
})

test_that("the rank greedy and the companion's kernel allocate alike", {
  for (inst in c("T", "S")) {
    r <- fixture_round(inst, N = 30L)
    expect_identical(greedy_alloc(r$b, r$model), r$model$pack(r$b))
  }
})

test_that("Clarke payments equal the Archer-Tardos critical bids", {
  for (inst in c("T", "S")) for (prior in c("a", "b")) for (t in 1:2) {
    r   <- fixture_round(inst, t = t, prior = prior)
    ref <- vcg_p1(r)
    out <- mech_run(mechanism("vcg"), r$b, r$model)
    expect_identical(out$x, ref$x)
    expect_equal(out$p, ref$p, tolerance = 1e-12)
    expect_true(sum(ref$p) > 0)     # the capacity binds, so the check is not vacuous
  }
})

test_that("the revenue-optimal mechanism: reserve, threshold, and VCG at a zero reserve", {
  r  <- fixture_round("T", prior = "a")
  my <- mech_run(mechanism("myerson", r$pr), r$b, r$model)
  expect_true(all(my$x[r$b < r$k / 2] == 0))
  expect_true(all(my$p[my$x > 0] >= r$k / 2 - 1e-12))
  # Each winner's payment is its threshold: just above it wins, just below loses.
  for (i in which(my$x > 0)[1:5]) {
    up <- r$b; up[i] <- my$p[i] + 1e-9
    dn <- r$b; dn[i] <- my$p[i] - 1e-9
    expect_identical(mech_run(mechanism("myerson", r$pr), up, r$model)$x[i], 1)
    expect_identical(mech_run(mechanism("myerson", r$pr), dn, r$model)$x[i], 0)
  }
  # A common key with reserve zero reproduces VCG exactly.
  id <- study_prior_table(r$tasks$recipe, "id", r$k)
  expect_equal(mech_run(mechanism("myerson", id), r$b, r$model),
               mech_run(mechanism("vcg"), r$b, r$model))
  # Both priors are regular, so the constructor's ironing check passes, and a
  # non-monotone virtual value is refused.
  expect_error(mechanism("myerson", data.frame(fam = "bad", lo = 0, hi = 1)))
})

test_that("pay-as-bid charges each winner its bid on VCG's allocation", {
  r  <- fixture_round("S")
  fp <- mech_run(mechanism("fp"), r$b, r$model)
  expect_identical(fp$x, mech_run(mechanism("vcg"), r$b, r$model)$x)
  expect_equal(fp$p, r$b * fp$x)
})

test_that("clinching reproduces VCG's allocation and payments", {
  for (inst in c("T", "S")) for (prior in c("a", "b")) {
    r  <- fixture_round(inst, prior = prior)
    cl <- clinch_run(r$b, r$model)
    ref <- vcg_p1(r)
    expect_identical(cl$x[seq_along(r$b)], ref$x)
    expect_equal(cl$pay[seq_along(r$b)], ref$p, tolerance = 1e-12)
  }
  r  <- fixture_round("T", size = "small", N = 9L, cap_scale = 1)
  cl <- clinch_run(r$b, r$model)
  expect_equal(cl$pay, vcg_p1(r)$p, tolerance = 1e-12)
})

test_that("worked example: x = (2, 1), p = (5, 0), and p_1 = 6 after delta = 1", {
  m  <- fixture_worked_example()
  vc <- mech_run(mechanism("vcg"), c(10, 5), m)
  expect_equal(vc$x, c(2, 1))
  expect_equal(vc$p, c(5, 0))
  expect_equal(mech_run(mechanism("vcg"), c(10, 6), m)$p[1], 6)
  cl <- clinch_run(c(10, 5), m)
  expect_equal(cl$x, c(2, 1))
  expect_equal(cl$pay, c(5, 0))
})

test_that("the competitiveness boundary: Myerson charges (20, 1) and no in-support deviation pays", {
  m  <- fixture_worked_example()
  pr <- data.frame(fam = "unif", lo = c(10, 1), hi = c(11, 2))
  my <- mechanism("myerson", pr)
  for (b1 in seq(10, 11, by = 0.25)) for (b2 in seq(1, 2, by = 0.25)) {
    out <- mech_run(my, c(b1, b2), m)
    expect_equal(out$x, c(2, 1))
    expect_equal(out$p, c(20, 1))
    # Every in-support counterfactual for the other agent leaves each payment put.
    for (h in seq(1, 2, by = 0.1))   expect_equal(mech_run(my, c(b1, h), m)$p[1], 20)
    for (h in seq(10, 11, by = 0.1)) expect_equal(mech_run(my, c(h, b2), m)$p[2], 1)
  }
})

test_that("the clinching transcript records the price path and the on-path ranks", {
  r  <- fixture_round("S", N = 30L)
  cl <- clinch_run(r$b, r$model)
  ps <- vapply(cl$events, `[[`, numeric(1), "p")
  expect_identical(ps[1], 0)
  expect_true(all(diff(ps) > 0))
  expect_setequal(ps[-1], sort(unique(r$b)))
  e <- cl$events[[length(cl$events) %/% 2]]
  expect_equal(e$fA, task_rank(r$tasks[as.integer(e$A), ], r$env))
  expect_identical(sum(cl$x), set_rank(r$model, seq_along(r$b)))
})
