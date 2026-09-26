# The study's agents: one task per agent per round, on the companion's seeded
# round stream, valued by the companion's bid model, with value priors whose
# Myerson reserve lies inside the support.
#
# This file and nothing else draws random numbers.

#' Every constant a computation block reads, in one place.
#'
#' `rounds` and `seeds` are the companion's joint-misreport block (30 rounds,
#' ten seeds); `util_hat`, `deadlines` and `lambda_l` are the bid-model
#' settings that block runs at. The operating points: `population` keeps the
#' companion's agent counts (90 on the tree, 45 on the fan), which with one
#' task per agent offers two thirds of the companion's task volume; `load`
#' keeps the companion's offered load instead (its agents carry 1.5 tasks per
#' round at its high load level, hence 135 and 68 single-task agents), and carries the
#' crossing instance at the companion's enumerable point as a boundary row.
#' `fee` is the operator's per-unit fee and `stake` the ownership share of the
#' gross payments used when the no-stake condition is removed, both in bid
#' units and shares respectively.
study_defaults <- function() {
  list(
    rounds     = 30L,
    seeds      = 1:10,
    util_hat   = 0.5,
    deadlines  = c(500L, 750L, 1000L),
    lambda_l   = node_lambda_l(),
    priors     = c("a", "b"),
    prior_b_uniform_leaves = c("l1", "l3"),
    thetas     = c(0.5, 1 - 1e-9),
    fee        = 0.01,
    stake      = 0.1,
    points = tibble::tribble(
      ~point,       ~graph_type, ~N,   ~cap_scale,
      "population", "tree",      90L,  1.0,
      "population", "sp",        45L,  1.0,
      "load",       "tree",      135L, 1.0,
      "load",       "sp",        68L,  1.0,
      "load",       "entangled", 12L,  0.1
    )
  )
}

#' One round of single-task agents, in the companion's task tibble shape.
#'
#' The round's stream is seeded as the companion seeds it (`seed * 1009 + t`),
#' the leaf of each task is drawn by the companion's rule for a round's
#' recipes, and the value is drawn from the prior: (a) uniform on (0, 1) at
#' every leaf; (b) uniform on the leaves `prior_b_uniform_leaves` and
#' F(v) = v^2 on (0, 1) elsewhere, the latter as the square root of the same
#' uniform draw, so the two priors share their random numbers.
#'
#' @param env   Environment from the companion's node constructors.
#' @param N     Agents, one task each.
#' @param t     Round.
#' @param seed  Seed of the run.
#' @param prior "a" or "b".
#' @param deadlines Deadlines a task draws from (ms).
#' @param uniform_leaves Leaves carrying U(0, 1) under prior (b).
#' @return Tibble of task_id, agent_id, deadline, value_base, recipe.
study_round_tasks <- function(env, N, t, seed, prior = c("a", "b"),
                              deadlines = study_defaults()$deadlines,
                              uniform_leaves) {
  prior <- match.arg(prior)
  set.seed(seed * 1009L + t)
  recipe   <- sample(names(env$leaf_shares), N, replace = TRUE,
                     prob = env$leaf_shares)
  deadline <- sample(deadlines, N, replace = TRUE)
  u        <- runif(N)
  sq       <- study_prior_table(recipe, prior, 1, uniform_leaves)$fam == "sq"
  tibble(task_id    = paste0("a", seq_len(N), "_t", t, "_1"),
         agent_id   = seq_len(N),
         deadline   = deadline,
         value_base = ifelse(sq, sqrt(u), u),
         recipe     = recipe)
}

#' The companion's bid for a task of unit value: the common multiplier.
study_bid_scale <- function(env, d = study_defaults()) {
  unit <- tibble(task_id = "unit", agent_id = 0L, deadline = max(d$deadlines),
                 value_base = 1, recipe = rownames(env$anc)[1])
  task_expected_value(unit, d$util_hat, base_latency_for_bids(env),
                      init_success_model(), lambda_l_default = d$lambda_l)
}

#' Bids: the companion's latency-discounted expected value of each task.
#'
#' At a fixed exogenous utilisation every task's multiplier is the same, so a
#' bid is the drawn value times `study_bid_scale()`; asserted, since the
#' priors are stated in bid units through that one number.
study_bids <- function(tasks, env, d = study_defaults()) {
  b <- task_expected_value(tasks, d$util_hat, base_latency_for_bids(env),
                           init_success_model(), lambda_l_default = d$lambda_l)
  stopifnot("bids are not linear in the drawn value" =
              isTRUE(all.equal(b, study_bid_scale(env, d) * tasks$value_base,
                               tolerance = 1e-12)))
  b
}

#' Each agent's prior in bid units: family, support bottom and top.
#'
#' @param leaves Leaf of each agent's task.
#' @param prior  "a", "b", or "id" (a reserve-free common key, tests only).
#' @param k      Bid multiplier: the support is (0, k).
#' @param uniform_leaves Leaves carrying U(0, 1) under prior (b); read only
#'   under prior (b).
study_prior_table <- function(leaves, prior, k, uniform_leaves) {
  fam <- switch(prior,
                a  = rep("unif", length(leaves)),
                b  = ifelse(leaves %in% uniform_leaves, "unif", "sq"),
                id = rep("id", length(leaves)))
  data.frame(fam = fam, lo = 0, hi = if (prior == "id") Inf else k)
}

#' Virtual value in bid units, per agent.
#'
#' unif on (lo, hi): 2b - hi. sq, F(b) = (b / hi)^2 on (0, hi):
#' (3 b^2 - hi^2) / (2 b). id: b.
prior_phi <- function(b, pr) {
  ifelse(pr$fam == "unif", 2 * b - pr$hi,
         ifelse(pr$fam == "sq", (3 * b^2 - pr$hi^2) / (2 * b), b))
}

#' Inverse virtual value in bid units, per agent.
prior_phiinv <- function(y, pr) {
  ifelse(pr$fam == "unif", (y + pr$hi) / 2,
         ifelse(pr$fam == "sq", (y + sqrt(y^2 + 3 * pr$hi^2)) / 3, y))
}
