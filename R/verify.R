# Broadcast verification of the clinching transcript.
#
# The transcript carries, per price event, the announced active-or-holding set
# A(p), the on-path ranks f(A(p)) and f(A(p) - i), the residual supplies, the
# clinches and the price delivered to each roster member, beside the log of
# demand and exit messages each bound to a signer. The verifiers are the ones
# the commitment proof names: every participant compares the price sequence it
# received; each agent checks its own status in A(p), and anyone checks A(p)
# against the messages; each agent recomputes its residual supply from the
# broadcast ranks; every rank value is checked against the authenticated
# oracle; every message's signer must be the identity it claims; and every
# identity must be on the roster committed before bidding.

#' The verifier each mutation's proof case names.
mutation_verifier <- c(price   = "price_agreement",
                       active  = "active_set",
                       s       = "clinch_recompute",
                       rank    = "rank_auth",
                       phantom = "roster",
                       forged  = "identity_binding")

#' Run every verifier on a transcript.
#'
#' @param tr   A `clinch_run()` result.
#' @param bids The roster members' own bids, each known to its own agent.
#' @param model The round's model: the rank oracle every rank value is
#'   authenticated against.
#' @param roster_check The roster clause of participant authenticity.
#' @return Named logical: TRUE where that verifier rejects.
verify_transcript <- function(tr, bids, model, roster_check = TRUE) {
  roster <- tr$roster
  oracle <- function(S) model$f(tabulate(tr$types[S], model$n_types))
  iv     <- active_intervals(tr$msgs)
  held   <- character(0)
  price_bad <- FALSE
  set_bad   <- FALSE
  clinch_bad <- FALSE
  rank_bad  <- FALSE
  for (ev in tr$events) {
    if (length(unique(ev$price_to)) > 1L) price_bad <- TRUE
    derived <- union(iv$id[iv$start <= ev$p & ev$p < iv$end], held)
    own_ok  <- vapply(roster, function(a) {
      (a %in% ev$A) == (bids[as.integer(a)] > ev$p || a %in% held)
    }, logical(1))
    if (!setequal(derived, ev$A) || !all(own_ok)) set_bad <- TRUE
    cand <- names(ev$s)
    if (length(cand) > 0L) {
      if (any(abs(ev$s - (ev$fA - ev$fAi[cand])) > 1e-9)) clinch_bad <- TRUE
      if (!setequal(ev$clinch, cand[ev$s >= 1 - 1e-9])) clinch_bad <- TRUE
    }
    if (abs(ev$fA - oracle(ev$A)) > 1e-9) rank_bad <- TRUE
    for (i in names(ev$fAi)) {
      if (abs(ev$fAi[[i]] - oracle(setdiff(ev$A, i))) > 1e-9) rank_bad <- TRUE
    }
    held <- union(held, ev$clinch)
  }
  c(price_agreement  = price_bad,
    active_set       = set_bad,
    clinch_recompute = clinch_bad,
    rank_auth        = rank_bad,
    identity_binding = any(tr$msgs$signer != tr$msgs$id),
    roster           = roster_check && any(!(tr$msgs$id %in% roster)))
}

#' The honest clinching transcript and one mutation per proof case.
#'
#' The four announcement mutations share one target: the first event whose
#' clinching agents are all still active at the next price, with one of its
#' clinches deferred to that price. `price`
#' delivers the next price to that agent alone; `active` keeps the agent whose
#' exit triggered the clinch in the announced set, with ranks authentic for
#' that set; `s` announces a zero residual supply against honest ranks; `rank`
#' announces f(A - i) = f(A), so the residual supply is consistent with a
#' forged rank. `phantom` adds a demand message under an identity outside the
#' roster, signed with its own key; `forged` extends a rostered agent's demand
#' past its exit with messages the operator signs. Each mutated transcript is
#' produced by the clinching engine under the operator's announcements, so it
#' is consistent everywhere else; the revenue it would carry, its extraction
#' (the payment increment of the genuine agents it still allocates) and its
#' welfare loss (the value of the genuine units it takes away) are read off it.
#'
#' @param phantom list(type, exit) or NULL.
#' @param forged  list(j, delta) or NULL.
#' @return Named list of list(realised, revenue, extraction, conc_w_num, tr).
clinch_mutations <- function(b, model, phantom = NULL, forged = NULL) {
  honest <- clinch_run(b, model)
  rev0   <- sum(honest$pay)
  wrap   <- function(tr) {
    stay <- tr$x > 0
    list(realised = TRUE, revenue = sum(tr$pay) - rev0,
         extraction = sum(tr$pay[stay] - honest$pay[stay]),
         conc_w_num = sum(b * honest$x) - sum(b * tr$x), tr = tr)
  }
  none   <- list(realised = FALSE, revenue = NA_real_, extraction = NA_real_,
                 conc_w_num = NA_real_, tr = NULL)
  out    <- list(honest = wrap(honest))

  ps <- vapply(honest$events, `[[`, numeric(1), "p")
  target <- NULL
  for (e in seq_len(length(ps) - 1L)) {
    ok <- intersect(honest$events[[e]]$clinch, honest$roster)
    # Every agent clinching at the target event is still active at the next
    # price, so a deferred clinch is never a lost one.
    if (length(ok) > 0L && all(b[as.integer(ok)] > ps[e + 1])) {
      target <- list(e = e, i = ok[1], p_next = ps[e + 1],
                     j = as.character(which(b == ps[e])[1]))
      break
    }
  }
  if (is.null(target)) {
    out[c("price", "active", "s", "rank")] <- list(none, none, none, none)
  } else {
    at <- function(e) e == target$e
    out$price <- wrap(clinch_run(b, model, hooks = list(
      price = function(e, v, clinch) { if (at(e)) v[target$i] <- target$p_next; v })))
    out$active <- if (is.na(target$j)) none else wrap(clinch_run(b, model, hooks = list(
      A = function(e, A) if (at(e)) union(A, target$j) else A)))
    out$s <- wrap(clinch_run(b, model, hooks = list(
      s = function(e, v) { if (at(e) && target$i %in% names(v)) v[target$i] <- 0; v })))
    out$rank <- wrap(clinch_run(b, model, hooks = list(
      f = function(e, v, fA) { if (at(e) && target$i %in% names(v)) v[target$i] <- fA; v })))
  }

  msgs0 <- honest_messages(b, model)
  out$phantom <- if (is.null(phantom)) none else {
    m <- bind_rows(msgs0, tibble(id = "ph", signer = "ph", kind = c("demand", "exit"),
                                 price = c(0, phantom$exit), type = phantom$type))
    wrap(clinch_run(b, model, msgs = m))
  }
  out$forged <- if (is.null(forged)) none else {
    j <- as.character(forged$j)
    m <- bind_rows(msgs0, tibble(id = j, signer = "op", kind = c("demand", "exit"),
                                 price = c(b[forged$j], b[forged$j] + forged$delta),
                                 type = model$type[forged$j]))
    wrap(clinch_run(b, model, msgs = m))
  }
  out
}
