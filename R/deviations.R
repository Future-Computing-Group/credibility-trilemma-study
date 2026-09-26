# The operator's deviations, each executed by running the prescribed mechanism
# on a counterfactual (Arm P) or augmented (Arm G) profile, and the per-agent
# check that decides whether an outcome is rationalised by some in-support
# profile of the others.

#' Agent j's priority key at bid z, on the mechanism's branch.
key_of <- function(mech, j, z) {
  if (mech$branch == "W") z else prior_phi(z, mech$pr[j, ])
}

#' Pullback of j's key into agent i's bid scale.
pullback <- function(mech, i, j, z) mech$keyinv(i, key_of(mech, j, z))

#' The contested triple of agent i: S the agents processed before i, j the
#' next agent in priority, the conditional gap, and the contested window.
#'
#' The window is the definition's, on the mechanism's branch: the first entry
#' keeps j below i, the second keeps j's pullback above every other active
#' agent, and the support entry keeps j's bid in its support; an empty maximum
#' reads as +inf, a pullback beyond the range of j's virtual value as the
#' corresponding support endpoint. Clause (B) of the competitive event is
#' checked on branch (V).
#'
#' @return A list, or NULL when agent i is inactive or last in priority.
contested_triple <- function(i, b, mech, model, pr) {
  kap <- mech$keys(b)
  act <- which(is.finite(kap) & kap > 0)
  if (!(i %in% act)) return(NULL)
  S     <- act[kap[act] > kap[i]]
  below <- act[kap[act] < kap[i]]
  if (length(below) == 0L) return(NULL)
  j     <- below[which.max(kap[below])]
  out   <- below[below != j]
  lo_j  <- pr$lo[j]
  hi_j  <- pr$hi[j]
  inv_j <- function(y) {
    if (mech$branch == "W") return(y)
    y_hi <- prior_phi(hi_j, pr[j, ])
    y_lo <- if (lo_j > 0) prior_phi(lo_j, pr[j, ]) else -Inf
    if (y >= y_hi) hi_j else if (y <= y_lo) lo_j else prior_phiinv(y, pr[j, ])
  }
  e1 <- inv_j(kap[i]) - b[j]
  e2 <- if (length(out) == 0L) Inf else b[j] - inv_j(max(kap[out]))
  e3 <- hi_j - b[j]
  B_ok <- mech$branch == "W" ||
    kap[j] > max(0, if (pr$lo[i] > 0 || pr$fam[i] == "unif")
                      prior_phi(pr$lo[i], pr[i, ]) else -Inf)
  gamma <- conditional_gap(i, j, S, model)
  list(i = i, j = j, S = S, gamma = gamma, e1 = e1, e2 = e2, e3 = e3,
       dbar = min(e1, e2, e3), B_ok = B_ok,
       realised = gamma > 0 && min(e1, e2, e3) > 0 && B_ok)
}

#' Is agent i's observed outcome rationalised by an honest run on `witness`?
#'
#' The deviation definition's support criterion: the witness keeps agent i's
#' own bid, every other entry lies in its prior's support, and the prescribed
#' mechanism run on it returns the observed allocation and payment.
rationalises <- function(i, x_obs, p_obs, witness, b_i, mech, model, pr,
                         tol = 1e-9) {
  others <- setdiff(seq_along(witness), i)
  if (witness[i] != b_i) return(FALSE)
  if (any(witness[others] < pr$lo[others] | witness[others] > pr$hi[others])) {
    return(FALSE)
  }
  run <- mech_agent(mech, witness, model, i)
  isTRUE(run$x[i] == x_obs) && abs(run$p_i - p_obs) <= tol * max(1, abs(p_obs))
}

#' Arm P for agent i: raise j's bid inside the window and re-run the mechanism.
#'
#' The operator reports the true allocation and the payment the re-run
#' returns; each amplitude theta times the window's supremum is checked
#' against the lemma's closed form, the supremum's closed form is recorded
#' beside it, and a negative control pushes j just past the first window entry.
#'
#' @param base Honest run, `mech_run(mech, b, model)`.
#' @param tri  `contested_triple()` for agent i, possibly from another
#'   mechanism (pay-as-bid reuses VCG's triples).
#' @return Tibble, one row per theta.
arm_p_agent <- function(tri, b, mech, model, pr, base, thetas) {
  i <- tri$i
  j <- tri$j
  rows <- lapply(thetas, function(th) {
    delta <- th * tri$dbar
    bh    <- b
    bh[j] <- b[j] + delta
    run   <- mech_agent(mech, bh, model, i)
    formula <- if (mech$pay == "bid") 0 else
      tri$gamma * (pullback(mech, i, j, bh[j]) - pullback(mech, i, j, b[j]))
    tibble(i = i, j = j, theta = th, gamma = tri$gamma, dbar = tri$dbar,
           delta = delta, dp = run$p_i - base$p[i], dp_formula = formula,
           x_unchanged = run$x[i] == base$x[i],
           rationalised = rationalises(i, base$x[i], run$p_i, bh, b[i], mech,
                                       model, pr))
  })
  out <- bind_rows(rows)
  out$sup_formula <- if (mech$pay == "bid") 0 else
    tri$gamma * (pullback(mech, i, j, b[j] + tri$dbar) - pullback(mech, i, j, b[j]))
  # Negative control: j just past the first entry, where that stays in support.
  nc <- NA
  d_nc <- tri$e1 * (1 + 1e-6)
  if (mech$pay == "at" && is.finite(d_nc) && b[j] + d_nc < pr$hi[j]) {
    bh <- b
    bh[j] <- b[j] + d_nc
    run <- mech_agent(mech, bh, model, i)
    lemma <- tri$gamma * (pullback(mech, i, j, bh[j]) - pullback(mech, i, j, b[j]))
    nc <- run$x[i] != base$x[i] || abs((run$p_i - base$p[i]) - lemma) > 1e-9
  }
  out$nc_departs <- nc
  out
}

#' Arm P over a round: every allocated agent's contested triple, and the
#' perturbation where the triple is realised.
#'
#' An agent that is not allocated has a zero gap with its successor (its own
#' marginal at S is zero), so only allocated agents can realise a triple.
#'
#' @param triples_from Mechanism whose priority order defines the triples;
#'   the mechanism itself by default.
#' @return Tibble, one row per realising agent and theta, plus the agents
#'   whose triple is not realised as one row each with `realised = FALSE`.
arm_p_round <- function(b, mech, model, pr, thetas, triples_from = mech) {
  base <- mech_run(mech, b, model)
  win  <- which(base$x > 0)
  bind_rows(lapply(win, function(i) {
    tri <- contested_triple(i, b, triples_from, model, pr)
    if (is.null(tri) || !tri$realised) {
      return(tibble(i = i, realised = FALSE,
                    gamma = if (is.null(tri)) NA_real_ else tri$gamma))
    }
    mutate(arm_p_agent(tri, b, mech, model, pr, base, thetas), realised = TRUE)
  }))
}

#' A model and prior table with one phantom task appended at a leaf.
augment <- function(model, pr, leaf_type, pr_ph) {
  m <- model
  m$type <- c(model$type, leaf_type)
  if (!is.null(model$tasks)) {
    ph <- model$tasks[1, ]
    ph$task_id  <- "phantom"
    ph$agent_id <- max(model$tasks$agent_id) + 1L
    ph$recipe   <- rownames(model$env$anc)[leaf_type]
    m$tasks <- bind_rows(model$tasks, ph)
    m <- market_model(m$tasks, model$env)
  }
  list(model = m, pr = rbind(pr, pr_ph))
}

#' Arm G for one phantom: the mechanism re-run on the augmented profile.
#'
#' The phantom carries an identity outside the round's agents and an
#' in-support bid; its units are withheld from delivery and its payment is
#' not revenue. Extraction is the payment increment of the agents still
#' allocated; the welfare term is the value of the genuine units lost.
arm_g_eval <- function(b, mech, model, pr, leaf_type, pr_ph, bid_ph, base) {
  n   <- length(b)
  aug <- augment(model, pr, leaf_type, pr_ph)
  mech_aug <- if (mech$branch == "V") mechanism(mech$name, aug$pr) else mech
  run <- mech_run(mech_aug, c(b, bid_ph), aug$model)
  xg  <- run$x[seq_len(n)]
  pg  <- run$p[seq_len(n)]
  stay <- xg > 0
  displaced <- base$x > 0 & !stay
  ok <- vapply(seq_len(n), function(i)
    rationalises(i, xg[i], pg[i], c(b, bid_ph), b[i], mech_aug, aug$model,
                 aug$pr), logical(1))
  tibble(leaf = leaf_type, bid_ph = bid_ph, x_ph = run$x[n + 1],
         class = if (run$x[n + 1] == 0) "nonwin" else
                   if (any(displaced)) "win_disp" else "win_nodisp",
         n_displaced = sum(displaced),
         rev_gain = sum(pg) - sum(base$p),
         extraction = sum(pg[stay] - base$p[stay]),
         conc_w_num = sum(b * base$x) - sum(b * xg),
         p_ph = run$p[n + 1],
         phantom_in_support = bid_ph >= pr_ph$lo & bid_ph <= pr_ph$hi,
         rationalised = all(ok))
}

#' Arm G over a round: per leaf, the named finite candidate set.
#'
#' A phantom at the support top shows which genuine winner the leaf's block
#' gives up, the block's marginal winner; the candidates are the support top,
#' just above that winner's key (displacing it) and just below it (winning
#' nothing and raising the winners' thresholds). The best over this set is a
#' lower bound on the best phantom.
arm_g_round <- function(b, mech, model, pr, prior, k, eps = 1e-9) {
  base <- mech_run(mech, b, model)
  L <- if (is.null(model$env)) seq_len(model$n_types) else rownames(model$env$anc)
  bind_rows(lapply(seq_along(L), function(l) {
    pr_ph <- study_prior_table(L[l], prior, k)
    top   <- arm_g_eval(b, mech, model, pr, l, pr_ph, pr_ph$hi, base)
    cands <- top
    if (top$n_displaced > 0) {
      aug <- augment(model, pr, l, pr_ph)
      mech_aug <- if (mech$branch == "V") mechanism(mech$name, aug$pr) else mech
      run <- mech_run(mech_aug, c(b, pr_ph$hi), aug$model)
      f   <- which(base$x > 0 & run$x[seq_along(b)] == 0)[1]
      kf  <- mech$keys(b)[f]
      inv <- function(y) if (mech$branch == "W") y else prior_phiinv(y, pr_ph)
      for (y in c(kf + eps * k, kf - eps * k)) {
        z <- inv(y)
        if (y > 0 && z >= pr_ph$lo && z <= pr_ph$hi) {
          cands <- bind_rows(cands, arm_g_eval(b, mech, model, pr, l, pr_ph, z, base))
        }
      }
    }
    cands
  }))
}
