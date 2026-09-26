# The region and its rank: the companion's certificate and rank functions on
# its instances, the agent-level rank the market clears over, and the
# conditional non-modularity gap.

gap_at <- function(r, L, i, j, S = character(0)) {
  at <- function(s) r[[subset_name(s, L)]]
  at(c(S, i)) + at(c(S, j)) - at(S) - at(c(S, i, j))
}

pair_gaps <- function(r, L) {
  pr <- utils::combn(L, 2, simplify = FALSE)
  setNames(vapply(pr, function(p) gap_at(r, L, p[1], p[2]), numeric(1)),
           vapply(pr, paste, character(1), collapse = ""))
}

instance_ranks <- function(size, inst) {
  spec <- leaf_instance_specs(size)[[inst]]
  anc  <- ancestor_matrix(spec)
  list(L = rownames(anc), and = leaf_rank(anc, token_capacity(spec)),
       or = flow_rank(spec, anc)$value, anc = anc, C = token_capacity(spec))
}

test_that("region: certificates, rank cross-checks and the leaf-level gaps", {
  for (size in c("small", "scale")) for (inst in c("T", "S", "X")) {
    x <- instance_ranks(size, inst)
    expect_equal(x$and, lb_optimum_rank(x$anc, x$C), info = paste(size, inst))
    cert <- polymatroid_certificate(x$and, x$L)
    if (inst == "X") {
      expect_identical(unname(cert), c(TRUE, TRUE, FALSE))
    } else {
      expect_true(all(cert), info = paste(size, inst))
    }
  }
  T <- instance_ranks("scale", "T")
  expect_equal(T$or, T$and)
  expect_equal(unname(pair_gaps(T$and, T$L)), c(50, 0, 0, 0, 0, 0))
  cond <- leaf_gap_table(T$and, T$L)
  expect_identical(nrow(cond), 24L)
  expect_identical(sum(cond$gap > 0), 10L)
  expect_true(all(cond$gap[cond$gap > 0] == 50))

  S <- instance_ranks("scale", "S")
  expect_equal(unname(pair_gaps(S$and, S$L)), rep(50, 6))
  expect_equal(unname(pair_gaps(S$or, S$L)), c(25, 50, 25, 25, 0, 25))
  expect_equal(S$and[["l1,l2,l3,l4"]], 50)
  expect_equal(S$or[["l1,l2,l3,l4"]], 100)
  expect_true(all(polymatroid_certificate(S$or, S$L)))

  X <- instance_ranks("scale", "X")
  expect_true(all(polymatroid_certificate(X$or, X$L)))
  expect_equal(unname(pair_gaps(X$or, X$L)), rep(0, 6))
  expect_identical(sum(leaf_gap_table(X$or, X$L)$gap > 0), 12L)

  expect_equal(pair_gaps(instance_ranks("small", "T")$and, c("l1", "l2", "l3"))[["l1l2"]], 4)
  expect_equal(unname(pair_gaps(instance_ranks("small", "S")$and, c("l1", "l2", "l3"))), rep(4, 3))
  expect_equal(unname(pair_gaps(instance_ranks("small", "S")$or, c("l1", "l2", "l3"))), rep(2, 3))
  expect_equal(unname(pair_gaps(instance_ranks("small", "X")$or, c("l1", "l2", "l3"))), c(1, 0, 1))
})

test_that("the certificate holds on T and S study rounds and fails on X", {
  for (inst in c("T", "S", "X")) {
    env   <- fixture_env(inst, "scale", N = 40L)
    tasks <- study_round_tasks(env, 40L, 1L, 1L, "a")
    expect_identical(dsic_certificate(env, tasks), inst != "X", info = inst)
  }
})

test_that("task_rank is the exact unit-value packing optimum on every subset", {
  for (inst in c("T", "S")) {
    env   <- fixture_env(inst, "small", N = 10L)
    tasks <- study_round_tasks(env, 10L, 1L, 3L, "a")
    A     <- task_recipes(tasks, env)
    C     <- tier_capacities(env)$capacity[match(colnames(A), tier_capacities(env)$tier)]
    for (m in 1:(2^10 - 1)) {
      s <- which(bitwAnd(m, bitwShiftL(1L, 0:9)) > 0)
      expect_identical(task_rank(tasks[s, ], env),
                       exact_pack_by_value(rep(1, length(s)), A[s, , drop = FALSE], C)$value,
                       info = paste(inst, m))
    }
  }
  # A 14-task round at a contended scale point, on a sample of its subsets.
  env   <- fixture_env("T", "scale", N = 14L, cap_scale = 0.06)
  tasks <- study_round_tasks(env, 14L, 1L, 1L, "a")
  A     <- task_recipes(tasks, env)
  C     <- tier_capacities(env)$capacity[match(colnames(A), tier_capacities(env)$tier)]
  set.seed(99L)
  for (r in 1:150) {
    s <- sort(sample.int(14L, sample.int(14L, 1)))
    expect_identical(task_rank(tasks[s, ], env),
                     exact_pack_by_value(rep(1, length(s)), A[s, , drop = FALSE], C)$value)
  }
})

test_that("conditional gaps are 0 or 1 on the unit-task market and never negative", {
  for (inst in c("T", "S")) {
    env   <- fixture_env(inst, "small", N = 7L)
    tasks <- study_round_tasks(env, 7L, 1L, 5L, "a")
    m     <- market_model(tasks, env)
    g <- c()
    for (i in 1:6) for (j in (i + 1):7) {
      rest <- setdiff(1:7, c(i, j))
      for (mm in 0:(2^5 - 1)) {
        S <- rest[bitwAnd(mm, bitwShiftL(1L, 0:4)) > 0]
        g <- c(g, conditional_gap(i, j, S, m))
      }
    }
    expect_true(all(g %in% c(0, 1)), info = inst)
  }
})

test_that("U(2,3): every pairwise gap is 0 and every conditional gap is 1", {
  m <- fixture_toy_model(3, function(s) min(length(s), 2))
  for (p in list(c(1, 2), c(1, 3), c(2, 3))) {
    expect_equal(conditional_gap(p[1], p[2], integer(0), m), 0)
    expect_equal(conditional_gap(p[1], p[2], setdiff(1:3, p), m), 1)
  }
})

test_that("unconditional task-pair gaps vanish at the evaluation capacities", {
  env   <- fixture_env("T", "scale", N = 135L)
  tasks <- study_round_tasks(env, 135L, 1L, 1L, "a")
  expect_identical(task_pair_zero_gap_share(market_model(tasks, env)), 1)
  env   <- fixture_env("T", "small", N = 12L, cap_scale = 0.25)   # e1 holds one token
  tasks <- study_round_tasks(env, 12L, 1L, 1L, "a")
  expect_lt(task_pair_zero_gap_share(market_model(tasks, env)), 1)
})
