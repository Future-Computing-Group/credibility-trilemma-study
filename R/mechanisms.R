# The mechanisms, on the companion's packing kernel and the rank oracle.
#
# Every mechanism here is an Edmonds greedy in a priority order with
# Archer-Tardos payments, or its pay-as-bid variant, or the ascending clinching
# auction. Allocation on the market goes through the companion's kernel
# `.greedy_pack_by`, which skips non-positive keys; payments are read off the
# step function of each agent's counterfactual allocation, whose breakpoints
# are the other agents' keys, so they are exact rather than searched on a grid.

#' A mechanism: priority keys, their inverse, the activity threshold, the rule.
#'
#' "vcg": keys are bids (branch W), Archer-Tardos payments, which are Clarke's.
#' "myerson": keys are virtual values (branch V); a bid below its support
#' bottom is rejected, and the kernel's non-positive skip is the reserve.
#' Ironing is the identity when every virtual value is strictly increasing on
#' its support, which the constructor checks. "fp": VCG's allocation, each
#' winner pays its bid.
#'
#' @param name "vcg", "myerson" or "fp".
#' @param pr   Prior table (fam, lo, hi), one row per agent; "myerson" only.
mechanism <- function(name = c("vcg", "myerson", "fp"), pr = NULL) {
  name <- match.arg(name)
  if (name != "myerson") {
    return(list(name = name, branch = "W", pay = if (name == "fp") "bid" else "at",
                keys = function(b) b, keyinv = function(i, y) y,
                thresh = function(i) 0))
  }
  stopifnot("a prior family has no closed-form virtual value" =
              all(pr$fam %in% c("unif", "sq", "id")))
  for (r in which(!duplicated(pr))) {
    top <- if (is.finite(pr$hi[r])) pr$hi[r] else pr$lo[r] + 1
    z   <- pr$lo[r] + (top - pr$lo[r]) * seq(0.001, 1, length.out = 1000)
    stopifnot("a virtual value is not strictly increasing: ironing needed" =
                all(diff(prior_phi(z, pr[rep(r, length(z)), ])) > 0))
  }
  list(name = name, branch = "V", pay = "at", pr = pr,
       keys   = function(b) ifelse(b >= pr$lo, prior_phi(b, pr), -Inf),
       keyinv = function(i, y) prior_phiinv(y, pr[i, ]),
       thresh = function(i) max(pr$lo[i], prior_phiinv(0, pr[i, ])))
}

#' Edmonds greedy on the rank oracle: x_i = f(B + i) - f(B), B the agents
#' processed before i, skipping non-positive keys as the companion's kernel does.
greedy_alloc <- function(keys, model) {
  x   <- numeric(length(keys))
  cnt <- integer(model$n_types)
  fB  <- 0
  for (i in order(keys, decreasing = TRUE)) {
    if (!is.finite(keys[i]) || keys[i] <= 0) next
    cnt[model$type[i]] <- cnt[model$type[i]] + 1L
    fBi  <- model$f(cnt)
    x[i] <- fBi - fB
    fB   <- fBi
  }
  x
}

#' Archer-Tardos payment of agent i: b_i x_i(b_i) - integral of x_i(z) from 0.
#'
#' As agent i's own bid z falls, the set processed before it grows by one agent
#' each time i's key passes another active agent's key, at z = keyinv(key_k),
#' and x_i(z) = f(B(z) + i) - f(B(z)). Below the activity threshold x_i is 0.
#' Submodularity makes x_i non-increasing as B grows, so the sweep stops at
#' the first zero.
at_payment <- function(i, b, kap, mech, model) {
  a <- mech$thresh(i)
  act   <- which(is.finite(kap) & kap > 0)
  act   <- act[act != i]
  above <- act[kap[act] > kap[i]]
  below <- act[kap[act] < kap[i]]
  below <- below[order(kap[below], decreasing = TRUE)]
  ti  <- model$type[i]
  cnt <- counts_of(model, above)
  fB  <- model$f(cnt)
  x_at <- function(cnt, fB) {
    cnt[ti] <- cnt[ti] + 1L
    model$f(cnt) - fB
  }
  x_top <- x_at(cnt, fB)
  cur   <- x_top
  upper <- b[i]
  integral <- 0
  for (k in below) {
    if (cur == 0 || upper <= a) break
    zk <- max(mech$keyinv(i, kap[k]), a)
    if (zk < upper) {
      integral <- integral + cur * (upper - zk)
      upper    <- zk
    }
    if (upper <= a) break
    cnt[model$type[k]] <- cnt[model$type[k]] + 1L
    fB  <- model$f(cnt)
    cur <- x_at(cnt, fB)
  }
  if (cur > 0 && upper > a) integral <- integral + cur * (upper - a)
  b[i] * x_top - integral
}

#' Run a mechanism on a bid profile: allocation and every payment.
#'
#' @return list(x, p), one entry per agent.
mech_run <- function(mech, b, model) {
  kap <- mech$keys(b)
  x   <- if (is.null(model$pack)) greedy_alloc(kap, model) else model$pack(kap)
  p   <- numeric(length(b))
  for (i in which(x > 0)) {
    p[i] <- if (mech$pay == "bid") b[i] * x[i] else at_payment(i, b, kap, mech, model)
  }
  list(x = x, p = p)
}

#' Run a mechanism and read one agent's outcome: the whole allocation, and
#' agent i's payment only.
mech_agent <- function(mech, b, model, i) {
  kap <- mech$keys(b)
  x   <- if (is.null(model$pack)) greedy_alloc(kap, model) else model$pack(kap)
  p_i <- if (x[i] == 0) 0 else if (mech$pay == "bid") b[i] * x[i] else
    at_payment(i, b, kap, mech, model)
  list(x = x, p_i = p_i)
}

# ---------------------------------------------------------------------------
# Ascending clinching
# ---------------------------------------------------------------------------

#' The honest message log: each agent broadcasts its own demand at price 0 and
#' its exit at its value.
honest_messages <- function(b, model) {
  ids <- as.character(seq_along(b))
  tibble(id = rep(ids, each = 2), signer = rep(ids, each = 2),
         kind = rep(c("demand", "exit"), length(b)),
         price = as.vector(rbind(0, b)), type = rep(model$type, each = 2))
}

#' Each identity's active interval [start, end) derived from a message log.
#'
#' Messages are read in price order (log order on ties); an identity is active
#' between a demand and the next exit. Adjacent intervals merge; an identity
#' with two separated intervals is refused.
active_intervals <- function(msgs) {
  ids <- unique(msgs$id)
  out <- lapply(ids, function(id) {
    m <- msgs[msgs$id == id, ]
    m <- m[order(m$price, seq_len(nrow(m))), ]
    start <- NA_real_
    iv <- matrix(numeric(0), ncol = 2)
    for (r in seq_len(nrow(m))) {
      if (m$kind[r] == "demand" && is.na(start)) start <- m$price[r]
      if (m$kind[r] == "exit" && !is.na(start)) {
        iv <- rbind(iv, c(start, m$price[r]))
        start <- NA_real_
      }
    }
    if (!is.na(start)) iv <- rbind(iv, c(start, Inf))
    if (nrow(iv) > 1L) {
      stopifnot("an identity has two separated active intervals" =
                  all(iv[-1, 1] == iv[-nrow(iv), 2]))
      iv <- matrix(c(iv[1, 1], iv[nrow(iv), 2]), ncol = 2)
    }
    c(iv[1, 1], iv[1, 2])
  })
  tibble(id = ids, start = vapply(out, `[`, numeric(1), 1),
         end = vapply(out, `[`, numeric(1), 2))
}

#' The ascending clinching auction, event-driven on the rank oracle.
#'
#' The price rises from 0; the active set changes only when an identity's
#' demand message starts or its exit message arrives, so the price path is the
#' sorted exit prices. On the unit-demand market an agent that has clinched
#' holds its unit, the set the rank is read on is A(p) = D(p) + clinched, and
#' agent i's residual supply is s_i(p) = f(A(p)) - f(A(p) - i): it clinches
#' its unit at the first price where no one else still active or holding can
#' take it. On a general polymatroid (toy models) the clinched vector c is
#' committed against the remaining polymatroid, whose rank is
#' g(S) = min over T containing S of f(T) - c(T), and s_i(p) = g(D) - g(D - i).
#'
#' The operator's announcements pass through `hooks` (identity by default):
#' `A(e, A)`, the announced set at event e; `f(e, v)`, the announced ranks
#' f(A - i) of the candidates (named by id); `s(e, v)`, the announced residual
#' supplies; `price(e, v, clinch)`, the price delivered to each roster member.
#'
#' @param b      Genuine agents' bids, which are their exit prices.
#' @param model  Model of the round (types and rank oracle).
#' @param msgs   Message log; the honest one by default.
#' @param types  Type of every identity in the log, named by id.
#' @param hooks  Operator announcement hooks.
#' @return list(x, pay) over the genuine agents (unnamed, in agent order), and
#'   the transcript: roster, msgs, types, events.
clinch_run <- function(b, model, msgs = honest_messages(b, model),
                       types = NULL, hooks = list()) {
  roster <- as.character(seq_along(b))
  iv     <- active_intervals(msgs)
  ids    <- iv$id
  if (is.null(types)) types <- setNames(msgs$type[match(ids, msgs$id)], ids)
  hk <- function(nm, ...) if (is.null(hooks[[nm]])) list(...)[[2]] else hooks[[nm]](...)
  prices <- sort(unique(c(0, msgs$price[msgs$kind == "exit"])))
  prices <- prices[is.finite(prices)]
  x   <- setNames(numeric(length(ids)), ids)
  pay <- setNames(numeric(length(ids)), ids)
  cnt_of <- function(S) tabulate(types[S], model$n_types)
  events <- vector("list", length(prices))

  for (e in seq_along(prices)) {
    p   <- prices[e]
    act <- ids[iv$start <= p & p < iv$end]
    if (isTRUE(model$unit)) {
      held  <- ids[x > 0]
      A     <- hk("A", e, union(act, held))
      cntA  <- cnt_of(A)
      fA    <- model$f(cntA)
      cands <- intersect(setdiff(act, held), A)
      fm    <- vapply(seq_len(model$n_types), function(t) {
        if (cntA[t] == 0L) return(NA_real_)
        c2 <- cntA
        c2[t] <- c2[t] - 1L
        model$f(c2)
      }, numeric(1))
      fAi   <- hk("f", e, setNames(fm[types[cands]], cands))
      s     <- hk("s", e, fA - fAi)
      clinch <- cands[s >= 1 - 1e-9]
      price_to <- hk("price", e, setNames(rep(p, length(roster)), roster), clinch)
      for (i in clinch) {
        x[i]   <- 1
        pay[i] <- pay[i] + if (i %in% roster) price_to[[i]] else p
      }
      events[[e]] <- list(p = p, A = A, fA = fA, fAi = fAi, s = s,
                          clinch = clinch, price_to = price_to)
    } else {
      g <- function(S) {
        rest <- setdiff(ids, S)
        best <- Inf
        for (m in 0:(2^length(rest) - 1)) {
          T <- c(S, rest[bitwAnd(m, bitwShiftL(1L, seq_along(rest) - 1L)) > 0])
          best <- min(best, model$f(cnt_of(T)) - sum(x[T]))
        }
        best
      }
      s_rec <- c()
      repeat {
        moved <- FALSE
        for (i in act) {
          si <- g(act) - g(setdiff(act, i))
          if (si > 1e-12) {
            x[i]   <- x[i] + si
            pay[i] <- pay[i] + p * si
            s_rec[i] <- si
            moved  <- TRUE
          }
        }
        if (!moved) break
      }
      events[[e]] <- list(p = p, A = act, s = s_rec)
    }
  }
  list(x = unname(x[roster]), pay = unname(pay[roster]), x_all = x,
       pay_all = pay, roster = roster, msgs = msgs, types = types,
       events = events)
}
