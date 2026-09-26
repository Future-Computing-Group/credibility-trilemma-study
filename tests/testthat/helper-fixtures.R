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
    tab[[paste(s, collapse = ",")]] <- rank_of(s)
  }
  toy_model(n, function(cnt) tab[[paste(which(cnt > 0), collapse = ",")]])
}
