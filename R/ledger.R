# Domain separation: the operator's books per proof case, under the side
# conditions of the proposition and with each removed in turn.
#
# The operator earns a fee on delivered units. (C0) routes agent-to-owner
# payments through escrow, so without it the gross payments enter the
# operator's books; (C1) forbids an ownership stake, so without it a share
# `stake` of the gross payments accrues; (C2) forbids affiliation with a
# phantom's owner, so without it the phantom's payment accrues; (C4) charges
# the fee on delivered units only, so without it the phantom's undelivered
# units carry the fee. Every deviation outcome comes from a mechanism run.

#' The condition sets: all in force, then each of (C0), (C1), (C2), (C4) off.
ledger_conditions <- function() {
  on <- c(C0 = TRUE, C1 = TRUE, C2 = TRUE, C4 = TRUE)
  list(all = on,
       no_C0 = replace(on, "C0", FALSE), no_C1 = replace(on, "C1", FALSE),
       no_C2 = replace(on, "C2", FALSE), no_C4 = replace(on, "C4", FALSE))
}

#' The operator's payoff from one outcome under one condition set.
#'
#' @param units_gen Delivered genuine units.
#' @param gross_gen Gross genuine agent-to-owner transfers.
#' @param units_ph  Units allocated to a phantom (undelivered).
#' @param gross_ph  The phantom's transfer to its owner.
operator_payoff <- function(units_gen, gross_gen, units_ph, gross_ph, cond,
                            fee, stake) {
  fee * (units_gen + (!cond[["C4"]]) * units_ph) +
    (if (!cond[["C0"]]) gross_gen else if (!cond[["C1"]]) stake * gross_gen else 0) +
    (!cond[["C2"]]) * gross_ph
}

#' Every proof case on one round of VCG, under every condition set.
#'
#' Case 1 raises the highest losing bid to its support top (bid substitution)
#' and re-runs VCG; case 2 conceals the top winner and every losing bid and
#' re-runs; case 3 proposes the allocation plus the top loser and tests it
#' against the region; case 4 is Arm P's best single perturbation and the
#' best non-winning phantom; cases 5a and 5b are the best winning phantom that
#' displaces no genuine winner and the best that displaces one. (C3) is read
#' off the round against f(E), the region's rank over its whole ground set:
#' more active agents than f(E), and an allocation that fills it.
#'
#' @return Tibble: case, conditions, realised, delta (payoff change), and the
#'   case's inputs (dp, p_ph, units_ph), rejected, c3.
ledger_round <- function(b, model, pr, prior, k, d,
                         ap = arm_p_round(b, mechanism("vcg"), model, pr, d$thetas[1]),
                         g = arm_g_round(b, mechanism("vcg"), model, pr, prior, k,
                                         d$prior_b_uniform_leaves)) {
  fee   <- d$fee
  stake <- d$stake
  vcg  <- mechanism("vcg")
  base <- mech_run(vcg, b, model)
  n    <- length(b)
  fE   <- ground_rank(model)
  c3   <- sum(b > 0) > fE && sum(base$x) == fE
  win  <- which(base$x > 0)
  lose <- setdiff(which(b > 0), win)
  g0   <- sum(base$p)
  u0   <- sum(base$x)

  outcome <- function(units_gen, gross_gen, units_ph = 0, gross_ph = 0)
    list(units_gen = units_gen, gross_gen = gross_gen, units_ph = units_ph,
         gross_ph = gross_ph)
  cases <- list()
  top_l <- if (length(lose)) lose[which.max(b[lose])] else NA
  cases[["1_reallocation"]] <- if (is.na(top_l)) NULL else {
    bh <- b
    bh[top_l] <- pr$hi[top_l]
    run <- mech_run(vcg, bh, model)
    outcome(sum(run$x), sum(run$p))
  }
  cases[["2_total_reduction"]] <- if (!length(win)) NULL else {
    bh <- b
    bh[c(win[which.max(b[win])], lose)] <- 0
    run <- mech_run(vcg, bh, model)
    outcome(sum(run$x), sum(run$p))
  }
  rejected_3 <- if (is.na(top_l)) NA else
    set_rank(model, c(win, top_l)) < length(win) + 1
  rz <- if ("dp" %in% names(ap)) ap[ap$realised & ap$theta == d$thetas[1], ] else ap[0, ]
  dp_sel <- if (nrow(rz)) max(rz$dp) else NA
  cases[["4_perturbation"]] <- if (is.na(dp_sel)) NULL else outcome(u0, g0 + dp_sel)
  best <- function(cls) {
    x <- g[g$class == cls, ]
    if (nrow(x) == 0L) NULL else x[which.max(x$rev_gain), ]
  }
  ng <- best("nonwin")
  cases[["4_phantom_nonwinning"]] <- if (is.null(ng)) NULL else
    outcome(u0, g0 + ng$rev_gain)
  for (cs in c("5a_phantom_no_displacement", "5b_phantom_displacement")) {
    x <- best(if (startsWith(cs, "5a")) "win_nodisp" else "win_disp")
    cases[[cs]] <- if (is.null(x)) NULL else
      outcome(u0 - x$n_displaced, g0 + x$rev_gain, x$x_ph, x$p_ph)
  }

  all_cases <- c("1_reallocation", "2_total_reduction", "3_capacity",
                 "4_perturbation", "4_phantom_nonwinning",
                 "5a_phantom_no_displacement", "5b_phantom_displacement")
  conds <- ledger_conditions()
  bind_rows(lapply(all_cases, function(cs) {
    bind_rows(lapply(names(conds), function(cn) {
      cond <- conds[[cn]]
      o <- cases[[cs]]
      realised <- !is.null(o) || (cs == "3_capacity" && !is.na(rejected_3))
      delta <- if (is.null(o)) if (cs == "3_capacity" && realised) 0 else NA_real_ else
        operator_payoff(o$units_gen, o$gross_gen, o$units_ph, o$gross_ph, cond, fee, stake) -
        operator_payoff(u0, g0, 0, 0, cond, fee, stake)
      tibble(case = cs, conditions = cn, realised = realised, delta = delta,
             dp = if (cs == "4_perturbation") dp_sel else NA_real_,
             p_ph = if (!is.null(o)) o$gross_ph else NA_real_,
             units_ph = if (!is.null(o)) o$units_ph else NA_real_,
             rejected = if (cs == "3_capacity") rejected_3 else NA,
             c3 = c3)
    }))
  }))
}
