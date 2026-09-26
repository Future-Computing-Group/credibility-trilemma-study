# Fixtures shared by the study's tests: the companion's small and scale
# instances as certified environments, and toy polymatroids given as rank
# tables over agent sets.

#' A certified node environment on one of the companion's instances.
fixture_env <- function(inst = c("T", "S", "X"), size = c("small", "scale"),
                        N = 10L, cap_scale = 1.0) {
  inst <- match.arg(inst)
  size <- match.arg(size)
  gt   <- c(T = "tree", S = "sp", X = "entangled")[[inst]]
  env  <- node_env(gt, "high", N, spec = leaf_instance_specs(size)[[inst]])
  scale_capacities(env, cap_scale)
}

#' A toy polymatroid over agents: each agent is its own type and the rank is
#' read off a table keyed by the sorted agent set ("" for the empty set).
fixture_toy_model <- function(n, rank_of) {
  tab <- list()
  for (m in 0:(2^n - 1)) {
    s <- which(bitwAnd(m, bitwShiftL(1L, seq_len(n) - 1L)) > 0)
    tab[[paste0("s", paste(s, collapse = ","))]] <- rank_of(s)
  }
  toy_model(n, function(cnt) tab[[paste0("s", paste(which(cnt > 0), collapse = ","))]])
}

#' A study round on an instance: tasks, bids, priors and the market model.
fixture_round <- function(inst = "T", size = "scale", N = 40L, cap_scale = 0.3,
                          t = 1L, seed = 1L, prior = "a") {
  env   <- fixture_env(inst, size, N, cap_scale)
  tasks <- study_round_tasks(env, N, t, seed, prior,
                             uniform_leaves = study_defaults()$prior_b_uniform_leaves)
  env   <- exp7_certify(env, tasks, "node")
  k     <- study_bid_scale(env)
  list(env = env, tasks = tasks, b = study_bids(tasks, env), k = k,
       pr = study_prior_table(tasks$recipe, prior, k, study_defaults()$prior_b_uniform_leaves),
       model = market_model(tasks, env))
}

#' The worked example of the payment perturbation lemma:
#' f({1}) = f({2}) = 2, f({1,2}) = 3.
fixture_worked_example <- function() {
  fixture_toy_model(2, function(s) c(0, 2, 2, 3)[1 + length(s) + (length(s) == 2)])
}
