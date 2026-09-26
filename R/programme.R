# The programme: every computation of the evaluation, round by round, on the
# companion's certified instances at the study's operating points.

#' One round: region checks, the honest mechanisms and their identities, both
#' arms under every mechanism, the clinching transcript and its mutations, the
#' domain-separation ledger, and (first round of a block) the agent-side leg.
#'
#' A round on an uncertified region (the crossing instance) records the
#' certificate and the greedy-versus-exact comparison only: it lies outside
#' the hypotheses of every result the other computations check.
#'
#' @param pt    One row of the operating points.
#' @param prior "a" or "b".
#' @param d     The constants, `study_defaults()` unless a test hands others.
run_round <- function(pt, prior, seed, t, d) {
  env   <- exp7_environment(pt$graph_type, d$load_level, pt$N, NA, "node",
                            pt$cap_scale)
  tasks <- study_round_tasks(env, pt$N, t, seed, prior, d$deadlines)
  cert  <- dsic_certificate(env, tasks)
  env$dsic_status <- if (cert) "certified" else "uncertified"
  b     <- study_bids(tasks, env, d)
  pair  <- node_exact_pair(env, tasks, b)
  key   <- tibble(point = pt$point, graph_type = pt$graph_type, N = pt$N,
                  cap_scale = pt$cap_scale, prior = prior, seed = seed, t = t)
  round <- mutate(key, cert = cert,
                  greedy_exact = pair[["greedy"]] >= pair[["exact"]] * (1 - 1e-12))
  if (!cert) return(list(round = round))

  k     <- study_bid_scale(env, d)
  pr    <- study_prior_table(tasks$recipe, prior, k)
  model <- market_model(tasks, env)
  mechs <- list(vcg = mechanism("vcg"), myerson = mechanism("myerson", pr),
                fp = mechanism("fp"))
  honest <- lapply(mechs, function(m) mech_run(m, b, model))

  # The companion's VCG on the same round, and clinching against it.
  blb <- base_latency_for_bids(env)
  ref <- vcg_allocate(tasks, env, d$util_hat, blb, init_success_model(),
                      lambda_l_default = d$lambda_l)
  ref_p <- numeric(length(b))
  ref_p[match(ref$task_id, tasks$task_id)] <- ref$vcg_payment
  vcg_eq_p1 <- identical(as.numeric(tasks$task_id %in% ref$task_id), honest$vcg$x) &&
    max(abs(ref_p - honest$vcg$p)) <= 1e-12
  cl <- clinch_run(b, model)
  clinch_eq_vcg <- identical(cl$x, honest$vcg$x) &&
    max(abs(cl$pay - honest$vcg$p)) <= 1e-12

  # Arm P under each mechanism; pay-as-bid on VCG's triples.
  ap_raw <- list(
    vcg     = arm_p_round(b, mechs$vcg, model, pr, d$thetas),
    myerson = arm_p_round(b, mechs$myerson, model, pr, d$thetas),
    fp      = arm_p_round(b, mechs$fp, model, pr, 0.5, triples_from = mechs$vcg))
  armp <- bind_rows(lapply(names(ap_raw), function(m) {
    x <- ap_raw[[m]]
    if (!"dp" %in% names(x)) return(NULL)
    mutate(x[x$realised, ], mech = m)
  }))
  n_real <- vapply(ap_raw, function(x) length(unique(x$i[x$realised])), integer(1))

  # Arm G under each mechanism.
  g_raw <- lapply(names(mechs), function(m)
    arm_g_round(b, mechs[[m]], model, pr, prior, k))
  names(g_raw) <- names(mechs)
  armg <- bind_rows(lapply(names(g_raw), function(m) mutate(g_raw[[m]], mech = m)))

  # The companion's VCG on the round augmented by the best phantom.
  gb  <- g_raw$vcg[which.max(g_raw$vcg$rev_gain), ]
  L   <- rownames(env$anc)
  ph  <- tasks[1, ]
  ph$task_id    <- "phantom"
  ph$agent_id   <- max(tasks$agent_id) + 1L
  ph$recipe     <- L[gb$leaf]
  ph$deadline   <- max(d$deadlines)
  ph$value_base <- gb$bid_ph / k
  aug_tasks <- bind_rows(tasks, ph)
  ref_aug <- vcg_allocate(aug_tasks, env, d$util_hat, blb, init_success_model(),
                          lambda_l_default = d$lambda_l)
  aug <- augment(model, pr, gb$leaf, study_prior_table(L[gb$leaf], prior, k))
  run_aug <- mech_run(mechs$vcg, c(b, gb$bid_ph), aug$model)
  ref_aug_p <- numeric(nrow(aug_tasks))
  ref_aug_p[match(ref_aug$task_id, aug_tasks$task_id)] <- ref_aug$vcg_payment
  vcg_aug_eq_p1 <- identical(as.numeric(aug_tasks$task_id %in% ref_aug$task_id),
                             run_aug$x) &&
    max(abs(ref_aug_p - run_aug$p)) <= 1e-9 * max(1, abs(ref_aug_p))

  # Clinching: transcript mutations, the verifiers, and the two arms there.
  rz  <- if (NROW(armp) && "mech" %in% names(armp))
    armp[armp$mech == "vcg" & armp$theta == 0.5, ] else NULL
  sel <- if (!is.null(rz) && nrow(rz)) rz[which.max(rz$dp), ] else NULL
  muts <- clinch_mutations(b, model,
                           phantom = list(type = gb$leaf, exit = gb$bid_ph),
                           forged  = if (!is.null(sel)) list(j = sel$j, delta = sel$delta))
  verify <- bind_rows(lapply(names(muts), function(m) {
    mu <- muts[[m]]
    v  <- if (mu$realised) verify_transcript(mu$tr, b, model) else NULL
    tibble(mutation = m, realised = mu$realised, revenue = mu$revenue,
           rejected_named = if (m == "honest" || is.null(v)) NA else v[[mutation_verifier[[m]]]],
           rejected_any = if (is.null(v)) NA else any(v),
           rejected_by = if (is.null(v)) NA_character_ else paste(names(v)[v], collapse = "+"))
  }))
  v_off <- verify_transcript(muts$phantom$tr, b, model, roster_check = FALSE)
  verify <- bind_rows(verify, tibble(mutation = "phantom_roster_off", realised = TRUE,
                                     revenue = muts$phantom$revenue,
                                     rejected_named = NA, rejected_any = any(v_off),
                                     rejected_by = paste(names(v_off)[v_off], collapse = "+")))
  clinch_dev <- tibble(
    arm = c("P", "G"),
    realised = c(muts$forged$realised, muts$phantom$realised),
    gain = c(muts$forged$revenue, muts$phantom$revenue),
    rejected = c(verify$rejected_any[verify$mutation == "forged"],
                 verify$rejected_any[verify$mutation == "phantom"]))

  led <- ledger_round(b, model, pr, prior, k, d$fee, d$stake,
                      ap = ap_raw$vcg, g = g_raw$vcg)

  agent <- NULL
  if (t == 1L) {
    agent <- agent_side_round(b, model, pr)
    p1g <- max(vapply(1:3, function(a)
      max(exp7_agent_gains("vcg", tasks, env, a, b, d$util_hat, blb,
                           init_success_model(), d$lambda_l)), numeric(1)))
    agent <- bind_rows(agent, tibble(mech = "vcg_p1_path", max_gain = p1g,
                                     n_members = NA_integer_))
  }

  rank_E <- set_rank(model, seq_along(b))
  round <- mutate(round,
    n_active = sum(b > 0), rank_E = rank_E, c3 = led$c3[1],
    zero_gap_share = task_pair_zero_gap_share(model),
    vcg_eq_p1 = vcg_eq_p1, clinch_eq_vcg = clinch_eq_vcg, vcg_aug_eq_p1 = vcg_aug_eq_p1,
    W_vcg = sum(b * honest$vcg$x), W_myerson = sum(b * honest$myerson$x),
    W_fp = sum(b * honest$fp$x),
    rev_vcg = sum(honest$vcg$p), rev_myerson = sum(honest$myerson$p),
    rev_fp = sum(honest$fp$p),
    realised_vcg = n_real[["vcg"]], realised_myerson = n_real[["myerson"]])
  tag <- function(x) if (is.null(x) || !nrow(x)) NULL else bind_cols(key[rep(1, nrow(x)), ], x)
  list(round = round, armp = tag(armp), armg = tag(armg), verify = tag(verify),
       clinch_dev = tag(clinch_dev), ledger = tag(led), agent = tag(agent))
}

#' Agent-side leg: the companion's misreport set against VCG and Myerson.
#'
#' Each agent in turn scales its own bid by every member of the companion's
#' deviation set, the others truthful; its gain is its true value of what it
#' wins minus what it pays, against truthful reporting.
agent_side_round <- function(b, model, pr) {
  mechs <- list(vcg = mechanism("vcg"), myerson = mechanism("myerson", pr))
  bind_rows(lapply(names(mechs), function(m) {
    mech <- mechs[[m]]
    base <- mech_run(mech, b, model)
    gains <- unlist(lapply(seq_along(b), function(i) {
      u0 <- b[i] * base$x[i] - base$p[i]
      vapply(misreport_strategy_set(b[i]), function(mult) {
        bh <- b
        bh[i] <- b[i] * mult
        run <- mech_agent(mech, bh, model, i)
        b[i] * run$x[i] - run$p_i - u0
      }, numeric(1))
    }))
    tibble(mech = m, max_gain = max(gains),
           n_members = length(misreport_strategy_set(1)))
  }))
}

#' Every round of one block (operating point, prior, seed).
run_block <- function(pt, prior, seed, d) {
  rr <- lapply(seq_len(d$rounds), function(t) run_round(pt, prior, seed, t, d))
  parts <- unique(unlist(lapply(rr, names)))
  setNames(lapply(parts, function(p) bind_rows(lapply(rr, `[[`, p))), parts)
}

#' The constants of a run as key-value rows.
config_rows <- function(d, p1_commit) {
  pts <- d$points
  tibble(key = c("rounds", "seeds", "util_hat", "deadlines", "lambda_l",
                 "load_level", "priors", "prior_b_uniform_leaves", "thetas",
                 "fee", "stake", "points", "p1_commit"),
         value = c(d$rounds, paste(d$seeds, collapse = " "), d$util_hat,
                   paste(d$deadlines, collapse = " "), format(d$lambda_l, digits = 15),
                   d$load_level, paste(d$priors, collapse = " "),
                   paste(d$prior_b_uniform_leaves, collapse = " "),
                   paste(format(d$thetas, digits = 15), collapse = " "), d$fee, d$stake,
                   paste(pts$point, pts$graph_type, pts$N, pts$cap_scale, sep = ":",
                         collapse = " "), p1_commit))
}

#' Run every block and write the raw tables.
#'
#' Blocks run in parallel over forked workers; each block is deterministic
#' given its seed, so the result does not depend on the worker count. The
#' crossing instance runs under prior (a) only: it is a boundary row.
#'
#' @return The combined tables, as a named list.
run_programme <- function(d = study_defaults(), workers = 1L, out_dir,
                          p1_commit = NA_character_) {
  grid <- tidyr::expand_grid(pi = seq_len(nrow(d$points)), prior = d$priors,
                             seed = d$seeds)
  grid <- grid[!(d$points$graph_type[grid$pi] == "entangled" & grid$prior != "a"), ]
  run1 <- function(r) run_block(d$points[grid$pi[r], ], grid$prior[r], grid$seed[r], d)
  t0 <- proc.time()[["elapsed"]]
  blocks <- if (workers > 1L) parallel::mclapply(seq_len(nrow(grid)), run1,
                                                 mc.cores = workers,
                                                 mc.preschedule = FALSE)
            else lapply(seq_len(nrow(grid)), run1)
  bad <- vapply(blocks, inherits, logical(1), "try-error")
  if (any(bad)) stop("a block failed: ", as.character(blocks[[which(bad)[1]]]))
  parts <- unique(unlist(lapply(blocks, names)))
  res <- setNames(lapply(parts, function(p) bind_rows(lapply(blocks, `[[`, p))), parts)
  res$config <- config_rows(d, p1_commit)
  res$timing <- tibble(blocks = nrow(grid), workers = workers,
                       elapsed_s = proc.time()[["elapsed"]] - t0)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (p in names(res)) readr::write_csv(res[[p]], file.path(out_dir, paste0(p, ".csv")))
  res
}

#' The constant c of the conditional CoNC bound, in selection form.
#'
#' Per round the operator perturbs the one realising agent with the largest
#' increment (a measurable selection of one triple per profile); with every
#' realised gap equal to one, c at amplitude theta is the mean of that
#' increment over all rounds, zero where nothing is realised, which factors as
#' the realisation frequency times the mean increment given realisation.
#' `c_sup` is the same with each increment at the window's supremum.
c_selection <- function(armp, rounds, by = c("point", "graph_type", "prior")) {
  stopifnot("a realised gap is not one" = all(armp$gamma == 1))
  per <- armp %>%
    group_by(across(all_of(c(by, "mech", "theta", "seed", "t")))) %>%
    summarise(h = max(dp), hs = max(sup_formula), .groups = "drop")
  n_rounds <- rounds %>% count(across(all_of(by)), name = "n_rounds")
  per %>%
    group_by(across(all_of(c(by, "mech", "theta")))) %>%
    summarise(sum_h = sum(h), sum_hs = sum(hs), n_event = n(), .groups = "drop") %>%
    left_join(n_rounds, by = by) %>%
    mutate(c = sum_h / n_rounds, p_event = n_event / n_rounds,
           h_given_event = sum_h / n_event, c_sup = sum_hs / n_rounds)
}
