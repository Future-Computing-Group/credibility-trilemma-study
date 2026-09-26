# The rank the market clears over, at the level of agents.
#
# One task per agent is one token at one leaf, so the agents' feasible sets
# are the matroid the leaf-block polymatroid induces, and the rank of a set of
# agents depends only on how many of them sit at each leaf. A "model" is that
# region seen from the agents: each agent's type (its leaf), the rank as a
# function of the type-count vector, and the packer the mechanisms allocate
# with. Toy polymatroids use the same shape with one type per agent.

.rank_memo <- new.env(hash = TRUE)

#' The agent-level rank of the environment's region, memoised by counts.
#'
#' The rank of a count vector is the companion's exact leaf-block optimum at
#' unit marginals, `lb_optimum`, which is exact on laminar and crossing
#' families alike. Memoised per region, since a round asks for the same count
#' vectors many times.
#'
#' @param env Environment from the companion's node constructors.
#' @return Function of an integer count vector (one entry per leaf, in the
#'   ancestor matrix's row order) returning the rank.
rank_oracle <- function(env) {
  anc <- env$anc
  C   <- node_token_capacity(env)
  L   <- rownames(anc)
  sig <- paste(c(L, colnames(anc), anc, C), collapse = "|")
  if (is.null(.rank_memo[[sig]])) assign(sig, new.env(hash = TRUE), envir = .rank_memo)
  memo <- .rank_memo[[sig]]
  function(cnt) {
    key <- paste(cnt, collapse = ",")
    v <- memo[[key]]
    if (is.null(v)) {
      marg <- setNames(lapply(cnt, function(k) rep(1, k)), L)
      v <- lb_optimum(marg, anc, C)
      assign(key, v, envir = memo)
    }
    v
  }
}

#' The market's model of one round: types, rank, and the companion's packer.
#'
#' `pack` allocates with the companion's value-greedy kernel on any priority
#' vector, which skips non-positive keys: under bids it is the welfare greedy,
#' under virtual values the Myerson greedy with its reserve.
market_model <- function(tasks, env) {
  L <- rownames(env$anc)
  list(n_types = length(L), type = match(tasks$recipe, L), f = rank_oracle(env),
       unit = TRUE, tasks = tasks, env = env,
       pack = function(keys) {
         x <- numeric(length(keys))
         x[.greedy_pack_by(keys, tasks, env)] <- 1
         x
       })
}

#' A toy polymatroid over agents, each its own type, with rank `f(counts)`.
toy_model <- function(n, f) {
  list(n_types = n, type = seq_len(n), f = f,
       unit = all(vapply(seq_len(n), function(i) f(tabulate(i, n)), numeric(1)) <= 1),
       pack = NULL)
}

#' Type-count vector of a set of agents.
counts_of <- function(model, idx) tabulate(model$type[idx], model$n_types)

#' Rank of a set of agents.
set_rank <- function(model, idx) model$f(counts_of(model, idx))

#' f(E): the rank of the region over its whole ground set, identities that did
#' not bid in the round included.
#'
#' On the market every leaf may carry any number of identities, so f(E) is the
#' rank with every leaf's count at least the region's total capacity: the
#' region's full capacity f(L). On a toy model the ground set is its agents.
ground_rank <- function(model) {
  if (is.null(model$env)) return(set_rank(model, seq_along(model$type)))
  model$f(rep(ceiling(sum(node_token_capacity(model$env))), model$n_types))
}

#' Rank of a set of tasks on the environment's region.
task_rank <- function(tasks, env) {
  rank_oracle(env)(tabulate(match(tasks$recipe, rownames(env$anc)), nrow(env$anc)))
}

#' The conditional non-modularity gap of agents i and j at prefix S.
#'
#' f(S + i) + f(S + j) - f(S) - f(S + i + j); S = empty gives the pairwise gap.
conditional_gap <- function(i, j, S, model) {
  set_rank(model, c(S, i)) + set_rank(model, c(S, j)) - set_rank(model, S) -
    set_rank(model, c(S, i, j))
}

#' Every conditional gap at leaf level, from a rank vector named by subset.
#'
#' @return Tibble: i, j, S (subset name), gap; one row per pair and prefix.
leaf_gap_table <- function(r, L) {
  at <- function(s) r[[subset_name(s, L)]]
  bind_rows(lapply(utils::combn(L, 2, simplify = FALSE), function(p) {
    rest <- setdiff(L, p)
    bind_rows(lapply(leaf_subsets(rest), function(S) {
      g <- at(c(S, p[1])) + at(c(S, p[2])) - at(S) - at(c(S, p))
      tibble(i = p[1], j = p[2], S = subset_name(S, L), gap = g)
    }))
  }))
}

#' Share of the round's task pairs whose pairwise gap is zero.
#'
#' The pairwise gap of two tasks depends only on their leaves, so pairs are
#' counted by leaf pair.
task_pair_zero_gap_share <- function(model) {
  n <- tabulate(model$type, model$n_types)
  e <- function(...) tabulate(c(...), model$n_types)
  zero <- 0
  tot  <- 0
  for (a in seq_along(n)) for (b in a:length(n)) {
    np  <- if (a == b) choose(n[a], 2) else n[a] * n[b]
    if (np == 0) next
    gap <- model$f(e(a)) + model$f(e(b)) - model$f(e(a, b))
    tot  <- tot + np
    zero <- zero + np * (gap == 0)
  }
  zero / tot
}
