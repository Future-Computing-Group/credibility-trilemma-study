# The study generator: one task per agent per round, drawn on the companion's
# seeded round stream and in the companion's task tibble shape, with the two
# value priors whose Myerson reserve lies inside the support.

test_that("the generator's defaults are the values the study reports", {
  d <- study_defaults()
  expect_identical(d$rounds, 30L)            # the companion's joint-misreport block
  expect_identical(d$seeds, 1:10)            # the companion's seed count
  expect_identical(d$util_hat, 0.5)
  expect_identical(d$deadlines, c(500L, 750L, 1000L))
  expect_identical(d$lambda_l, node_lambda_l())
  expect_null(d$load_level)
  expect_identical(d$prior_b_uniform_leaves, c("l1", "l3"))
  expect_identical(d$thetas, c(0.5, 1 - 1e-9))
  pts <- d$points
  expect_identical(pts$N[pts$point == "population"], c(90L, 45L))
  expect_identical(pts$N[pts$point == "load" & pts$graph_type != "entangled"],
                   c(135L, 68L))
  expect_identical(pts$cap_scale[pts$graph_type == "entangled"], 0.1)
})

test_that("one task per agent, in the companion's tibble shape, seeded per round", {
  env <- fixture_env("T", "scale", N = 50L)
  a   <- study_round_tasks(env, 50L, t = 3L, seed = 2L, prior = "a")
  expect_identical(names(a), c("task_id", "agent_id", "deadline", "value_base",
                               "recipe"))
  expect_identical(nrow(a), 50L)
  expect_identical(a$agent_id, 1:50)
  expect_identical(anyDuplicated(a$task_id), 0L)
  expect_identical(study_round_tasks(env, 50L, 3L, 2L, "a"), a)
  expect_false(identical(study_round_tasks(env, 50L, 4L, 2L, "a")$value_base,
                         a$value_base))
  # The recipe is drawn by the companion's rule on the round's own seed.
  set.seed(2L * 1009L + 3L)
  expect_identical(a$recipe, sample(names(env$leaf_shares), 50L, replace = TRUE,
                                    prob = env$leaf_shares))
})

test_that("prior (b) transforms the same uniforms on its squared-law leaves", {
  env <- fixture_env("S", "scale", N = 40L)
  a <- study_round_tasks(env, 40L, 1L, 1L, "a")
  b <- study_round_tasks(env, 40L, 1L, 1L, "b", uniform_leaves = study_defaults()$prior_b_uniform_leaves)
  expect_identical(a$recipe, b$recipe)
  sq <- !(b$recipe %in% study_defaults()$prior_b_uniform_leaves)
  expect_true(any(sq) && any(!sq))
  expect_equal(b$value_base[!sq], a$value_base[!sq])
  expect_equal(b$value_base[sq]^2, a$value_base[sq])
  expect_true(all(a$value_base > 0 & a$value_base < 1))
})

test_that("bids are the companion's value model, linear in the drawn value", {
  env   <- fixture_env("T", "scale", N = 30L)
  tasks <- study_round_tasks(env, 30L, 1L, 1L, "a")
  k     <- study_bid_scale(env)
  expect_equal(k, 0.1607795, tolerance = 1e-6)
  expect_equal(study_bids(tasks, env), k * tasks$value_base, tolerance = 1e-12)
})

test_that("virtual values: closed forms, reserves, inverses, monotonicity", {
  k  <- 0.2
  pr <- study_prior_table(c("l1", "l2"), "b", k, c("l1", "l3"))   # l1 uniform, l2 squared law
  expect_identical(pr$fam, c("unif", "sq"))
  expect_equal(prior_phi(c(k / 2, k / sqrt(3)), pr), c(0, 0))
  expect_equal(prior_phi(c(k, k), pr), c(k, k))
  z <- seq(0.01, 1, by = 0.01) * k
  for (f in c("unif", "sq")) {
    p1 <- data.frame(fam = f, lo = 0, hi = k)[rep(1, length(z)), ]
    ph <- prior_phi(z, p1)
    expect_true(all(diff(ph) > 0))
    expect_equal(prior_phiinv(ph, p1), z, tolerance = 1e-12)
  }
  # The squared law's virtual value in value units is (3v^2 - 1) / (2v).
  v <- 0.7
  expect_equal(prior_phi(k * v, pr[2, ]) / k, (3 * v^2 - 1) / (2 * v))
})

test_that("the companion's load level reaches nothing the study reads", {
  # The constructor's load level sets only the companion's task-arrival rate,
  # which the study's own generator does not use: two environments built at
  # different levels differ in that rate and the level's own label alone.
  for (gt in c("tree", "sp")) {
    lo <- exp7_environment(gt, "low", 40L, NA, "node", 0.5)
    hi <- exp7_environment(gt, "high", 40L, NA, "node", 0.5)
    expect_false(identical(lo$load_factor, hi$load_factor))
    lo$load_factor <- hi$load_factor <- lo$load_level <- hi$load_level <- NULL
    expect_identical(lo, hi)
  }
  expect_false("load_level" %in% config_rows(study_defaults(), NA)$key)
})
