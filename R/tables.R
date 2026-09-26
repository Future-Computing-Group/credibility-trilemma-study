# The table bodies the Evaluation section inputs, and the numbers its prose
# quotes, each written with the function it comes from.

#' Significant-figure formatting for table cells; exact zero prints as 0.
fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "---", ifelse(x == 0, "0",
                                 formatC(signif(x, digits), digits = digits, format = "g")))
}

#' Mean with its Student t 95 per cent interval over seeds.
fmt_ci <- function(v, scale = 1, digits = 3) {
  iv <- .ratio_interval(v * scale)
  if (is.na(iv$lo)) fmt(iv$mean, digits) else
    sprintf("%s [%s, %s]", fmt(iv$mean, digits), fmt(iv$lo, digits), fmt(iv$hi, digits))
}

tabular <- function(spec, header, rows) {
  c(sprintf("\\begin{tabular}{%s}", spec), "\\toprule",
    paste(header, collapse = " & ") |> paste0(" \\\\"), "\\midrule",
    vapply(rows, function(r) paste0(paste(r, collapse = " & "), " \\\\"), character(1)),
    "\\bottomrule", "\\end{tabular}")
}

leaf_tex <- function(pair) sprintf("$\\ell_%s\\ell_%s$", substr(pair[1], 2, 2), substr(pair[2], 2, 2))

#' The instance table: inputs, computed from the companion's rank functions.
instance_rows <- function() {
  specs <- leaf_instance_specs("scale")
  bind_rows(lapply(c("T", "S", "X"), function(inst) {
    spec <- specs[[inst]]
    anc  <- ancestor_matrix(spec)
    L    <- rownames(anc)
    full <- subset_name(L, L)
    one  <- function(r) {
      cert <- all(polymatroid_certificate(r, L))
      gt   <- leaf_gap_table(r, L)
      pw   <- gt[gt$S == "{}" & gt$gap > 0, ]
      list(f = r[[full]], cert = cert,
           pairs = if (!cert) "---" else if (!nrow(pw)) "none" else
             paste(sprintf("%s: %s", vapply(seq_len(nrow(pw)), function(k)
               leaf_tex(c(pw$i[k], pw$j[k])), character(1)), fmt(pw$gap)), collapse = ", "),
           cond = if (!cert) "---" else as.character(sum(gt$gap > 0)))
    }
    a <- one(leaf_rank(anc, token_capacity(spec)))
    o <- one(flow_rank(spec, anc)$value)
    tibble(inst = inst, f_and = a$f, f_or = o$f, cert_and = a$cert, cert_or = o$cert,
           pairs_and = a$pairs, pairs_or = o$pairs, cond_and = a$cond, cond_or = o$cond)
  }))
}

#' Per-round gains of each mechanism and arm, one row per round.
deviation_rounds <- function(res, point) {
  rd <- res$round[res$round$point == point & res$round$cert, ]
  keys <- c("graph_type", "prior", "seed", "t")
  ap <- if (NROW(res$armp)) res$armp[res$armp$point == point & res$armp$theta == 0.5, ] else NULL
  sum_p <- function(m) {
    x <- if (is.null(ap)) NULL else ap[ap$mech == m, ]
    s <- if (NROW(x)) x %>% group_by(across(all_of(keys))) %>%
      summarise(gain = sum(dp), pass = mean(rationalised), n_pass = n(), .groups = "drop")
    else tibble(graph_type = character(), prior = character(), seed = integer(),
                t = integer(), gain = numeric(), pass = numeric(), n_pass = integer())
    rd[, keys] %>% left_join(s, by = keys) %>%
      mutate(gain = coalesce(gain, 0), extraction = gain, conc_w_num = 0)
  }
  g <- res$armg[res$armg$point == point, ]
  best_g <- function(m) {
    g[g$mech == m, ] %>% group_by(across(all_of(keys))) %>%
      summarise(pass = mean(rationalised), n_pass = n(),
                disp = class[which.max(rev_gain)] == "win_disp",
                extraction = extraction[which.max(rev_gain)],
                conc_w_num = conc_w_num[which.max(rev_gain)],
                gain = max(rev_gain), .groups = "drop") %>%
      right_join(rd[, keys], by = keys)
  }
  cd <- res$clinch_dev[res$clinch_dev$point == point, ]
  cl <- function(a, like) {
    x <- cd[cd$arm == a, ] %>% transmute(graph_type, prior, seed, t,
                                         gain_c = ifelse(realised, gain, 0),
                                         pass = ifelse(realised, as.numeric(!rejected), NA),
                                         n_pass = as.integer(realised))
    like %>% select(all_of(keys), extraction, conc_w_num) %>% left_join(x, by = keys) %>%
      mutate(gain = gain_c, extraction = if (a == "P") gain_c else extraction) %>%
      select(-gain_c)
  }
  W <- rd %>% transmute(graph_type, prior, seed, t, vcg = W_vcg, myerson = W_myerson,
                        fp = W_fp, clinching = W_vcg)
  pv <- sum_p("vcg")
  gv <- best_g("vcg")
  out <- list(
    tibble(mech = "vcg", arm = "P", pv), tibble(mech = "myerson", arm = "P", sum_p("myerson")),
    tibble(mech = "clinching", arm = "P", cl("P", pv)), tibble(mech = "fp", arm = "P", sum_p("fp")),
    tibble(mech = "vcg", arm = "G", gv), tibble(mech = "myerson", arm = "G", best_g("myerson")),
    tibble(mech = "clinching", arm = "G", cl("G", gv)), tibble(mech = "fp", arm = "G", best_g("fp")))
  bind_rows(out) %>%
    left_join(tidyr::pivot_longer(W, c(vcg, myerson, fp, clinching),
                                  names_to = "mech", values_to = "W"),
              by = c(keys, "mech"))
}

#' Seed-level means of the per-round deviation quantities.
deviation_seeds <- function(dr) {
  dr %>% group_by(graph_type, prior, mech, arm, seed) %>%
    summarise(gain = mean(gain), extraction = mean(extraction),
              conc_op = mean(gain) / mean(W), conc_w = mean(conc_w_num) / mean(W),
              pass_n = sum(n_pass * pass, na.rm = TRUE), pass_d = sum(n_pass, na.rm = TRUE),
              .groups = "drop")
}

mech_label <- c(vcg = "VCG", myerson = "Revenue-optimal", clinching = "Clinching",
                fp = "Pay-as-bid")
case_label <- c("1_reallocation" = "1, reallocation", "2_total_reduction" = "2, total reduction",
                "3_capacity" = "3, capacity violation", "4_perturbation" = "4, perturbation (Arm P)",
                "4_phantom_nonwinning" = "4, non-winning phantom",
                "5a_phantom_no_displacement" = "5(a), phantom, no displacement",
                "5b_phantom_displacement" = "5(b), phantom, displacement")
mutation_label <- c(price = "Price equivocation", active = "Misreported $D(p)$",
                    s = "Miscomputed $s_i(p)$", rank = "Inauthentic rank value",
                    phantom = "Off-roster demand message",
                    forged = "Message forged under a rostered identity")
mutation_case <- c(price = "1", active = "2", s = "3", rank = "4", phantom = "5",
                   forged = "5")

#' Write the four table bodies and the numbers file for one operating point.
write_tables <- function(res, out_dir, point = "load", d = study_defaults()) {
  tdir <- file.path(out_dir, "tables")
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  inst_name <- c(tree = "T", sp = "S", entangled = "X")

  # Instances.
  ir <- instance_rows()
  yn <- function(x) ifelse(x, "yes", "no")
  writeLines(tabular("lrrllll",
    c("Instance", "$f(L)$ fork-join", "$f(L)$ routed", "$\\gamma_{ij}>0$, fork-join",
      "$\\gamma_{ij}>0$, routed", "$\\gamma_{ij|S}>0$ of 24", "Polymatroid"),
    lapply(seq_len(nrow(ir)), function(k) with(ir[k, ], c(
      inst, fmt(f_and), fmt(f_or), pairs_and, pairs_or,
      sprintf("%s / %s", cond_and, cond_or), sprintf("%s / %s", yn(cert_and), yn(cert_or)))))),
    file.path(tdir, "eval-instances.tex"))

  # Deviations.
  dr <- deviation_rounds(res, point)
  ds <- deviation_seeds(dr)
  cells <- ds %>% distinct(graph_type, prior, mech, arm) %>%
    mutate(mech = factor(mech, names(mech_label)), arm = factor(arm, c("P", "G"))) %>%
    arrange(factor(graph_type, c("tree", "sp")), prior, arm, mech)
  rows <- lapply(seq_len(nrow(cells)), function(k) {
    s <- ds %>% filter(graph_type == cells$graph_type[k], prior == cells$prior[k],
                       mech == as.character(cells$mech[k]), arm == as.character(cells$arm[k]))
    pass <- if (sum(s$pass_d) == 0) "---" else fmt(sum(s$pass_n) / sum(s$pass_d))
    c(inst_name[[cells$graph_type[k]]], sprintf("(%s)", cells$prior[k]),
      mech_label[[as.character(cells$mech[k])]], as.character(cells$arm[k]),
      fmt_ci(s$gain, 1e3), fmt_ci(s$extraction, 1e3), fmt_ci(s$conc_op, 100),
      fmt_ci(s$conc_w, 100), pass)
  })
  writeLines(tabular("llllrrrrr",
    c("Inst.", "Prior", "Mechanism", "Arm", "Gain ($10^{-3}$)", "Extraction ($10^{-3}$)",
      "$\\mathrm{CoNC}^{\\mathrm{op}}$ (\\%)", "$\\mathrm{CoNC}^{\\mathrm{W}}$ (\\%)", "Passes"),
    rows), file.path(tdir, "eval-deviations.tex"))

  # Broadcast.
  v <- res$verify[res$verify$point == point, ]
  hon <- v[v$mutation == "honest", ]
  brow <- list(c("Honest transcript", "---", "none",
                 sprintf("%d of %d", sum(hon$rejected_any), nrow(hon)), "0"))
  for (m in names(mutation_verifier)) {
    x <- v[v$mutation == m & v$realised, ]
    brow[[length(brow) + 1]] <- c(mutation_label[[m]], mutation_case[[m]],
      gsub("_", " ", mutation_verifier[[m]]),
      sprintf("%d of %d", sum(x$rejected_named), nrow(x)),
      fmt(mean(x$revenue) * 1e3))
  }
  x <- v[v$mutation == "phantom_roster_off", ]
  brow[[length(brow) + 1]] <- c("Off-roster message, roster clause removed", "5", "none",
                                sprintf("%d of %d", sum(x$rejected_any), nrow(x)),
                                fmt(mean(x$revenue) * 1e3))
  writeLines(tabular("llllr", c("Mutation", "Case", "Rejecting verifier", "Rejected (rounds)",
                                "Revenue ($10^{-3}$)"), brow),
             file.path(tdir, "eval-broadcast.tex"))

  # Separation.
  lg <- res$ledger[res$ledger$point == point & res$ledger$realised, ]
  srow <- lapply(names(case_label), function(cs) {
    x <- lg[lg$case == cs, ]
    n <- length(unique(paste(x$graph_type, x$prior, x$seed, x$t)))
    vals <- vapply(names(ledger_conditions()), function(cn) {
      y <- x[x$conditions == cn, ]
      if (!nrow(y)) "---" else if (cs == "3_capacity")
        sprintf("rejected, %d of %d", sum(y$rejected), nrow(y)) else fmt(max(y$delta) * 1e3)
    }, character(1))
    c(case_label[[cs]], as.character(n), vals)
  })
  writeLines(tabular("lrrrrrr", c("Proof case (largest payoff change, $10^{-3}$)", "Rounds",
                                   "All in force", "No (C0)",
                                   sprintf("No (C1), $\\lambda=%s$", d$stake), "No (C2)", "No (C4)"),
                     srow), file.path(tdir, "eval-separation.tex"))

  readr::write_csv(evaluation_numbers(res, point, d, ds), file.path(out_dir, "numbers.csv"))
  invisible(tdir)
}

#' The numbers the Evaluation prose quotes, each with the function it comes from.
evaluation_numbers <- function(res, point, d, ds) {
  num <- list()
  add <- function(ph, group, value, source)
    num[[length(num) + 1]] <<- tibble(placeholder = ph, group = group,
                                      value = as.character(value), source = source)
  rd <- res$round[res$round$point == point, ]
  rc <- rd[rd$cert, ]
  add("leaves carrying prior U(0,1) in profile (b)", "", paste(d$prior_b_uniform_leaves, collapse = " and "), "study_defaults")
  add("rounds per seed", "", d$rounds, "study_defaults")
  add("seeds", "", length(d$seeds), "study_defaults")
  ap <- res$armp[res$armp$point == point & res$armp$mech %in% c("vcg", "myerson"), ]
  add("max |dp computed - dp formula|", "all realisations, both branches",
      if (NROW(ap)) fmt(max(abs(ap$dp - ap$dp_formula))) else "no realisation", "arm_p_agent")
  add("allocation unchanged inside the window", "all realisations",
      if (NROW(ap)) all(ap$x_unchanged) else NA, "arm_p_agent")
  add("negative control departs beyond the first window entry", "share of controls run",
      if (NROW(ap)) fmt(mean(ap$nc_departs, na.rm = TRUE)) else NA, "arm_p_agent")
  cs <- if (NROW(res$armp)) c_selection(res$armp[res$armp$point == point, ], rc,
                                        by = c("graph_type", "prior")) else NULL
  for (g in unique(rc$graph_type)) for (p in unique(rc$prior)) {
    grp <- sprintf("%s, prior (%s)", g, p)
    r <- rc[rc$graph_type == g & rc$prior == p, ]
    for (m in c("vcg", "myerson")) {
      cc <- if (is.null(cs)) NULL else cs[cs$graph_type == g & cs$prior == p & cs$mech == m, ]
      c05 <- if (NROW(cc)) cc[cc$theta == 0.5, ] else NULL
      c1  <- if (NROW(cc)) cc[cc$theta == max(d$thetas), ] else NULL
      br  <- if (m == "vcg") "branch (W)" else "branch (V)"
      add("share of rounds with a realising triple", paste(grp, m, br),
          fmt(mean(r[[paste0("realised_", m)]] > 0)), "contested_triple via arm_p_round")
      add("c at theta = 1/2 (selection form)", paste(grp, m),
          if (NROW(c05)) fmt(c05$c) else "0", "c_selection")
      add("c at theta = 1 - 1e-9 (attained)", paste(grp, m),
          if (NROW(c1)) fmt(c1$c) else "0", "c_selection")
      add("c, theta -> 1 supremum", paste(grp, m),
          if (NROW(c05)) fmt(c05$c_sup) else "0", "c_selection")
      add("P(E) and E[h | E] at theta = 1/2", paste(grp, m),
          if (NROW(c05)) sprintf("%s; %s", fmt(c05$p_event), fmt(c05$h_given_event)) else "0; ---",
          "c_selection")
      s <- ds[ds$graph_type == g & ds$prior == p & ds$mech == m & ds$arm == "P", ]
      add("all-agent Arm P gain per round (1e-3), mean [95% CI]", paste(grp, m),
          fmt_ci(s$gain, 1e3), "arm_p_round, deviation_seeds")
      add("CoNC^op, Arm P (%)", paste(grp, m), fmt_ci(s$conc_op, 100), "deviation_seeds")
    }
    add("share of task pairs with zero unconditional gap", g,
        fmt(mean(rc$zero_gap_share[rc$graph_type == g])), "task_pair_zero_gap_share")
  }
  # One-agent gains per round: the selection's increment, with seed intervals.
  if (NROW(res$armp)) {
    one <- res$armp[res$armp$point == point & res$armp$theta == 0.5, ] %>%
      group_by(graph_type, prior, mech, seed, t) %>% summarise(h = max(dp), .groups = "drop")
    for (g in unique(rc$graph_type)) for (p in unique(rc$prior)) for (m in c("vcg", "myerson", "fp")) {
      r <- rc[rc$graph_type == g & rc$prior == p, c("graph_type", "prior", "seed", "t")]
      h <- r %>% left_join(one[one$mech == m, ], by = c("graph_type", "prior", "seed", "t")) %>%
        mutate(h = coalesce(h, 0)) %>% group_by(seed) %>% summarise(h = mean(h), .groups = "drop")
      add("one-agent Arm P gain per round (1e-3), mean [95% CI]",
          sprintf("%s, prior (%s), %s", g, p, m), fmt_ci(h$h, 1e3), "arm_p_round")
    }
  }
  ag <- res$agent[res$agent$point == point, ]
  for (m in unique(ag$mech)) add("max agent gain over the misreport set", m,
                                 fmt(max(ag$max_gain[ag$mech == m])), "agent_side_round")
  g <- res$armg[res$armg$point == point & res$armg$mech == "vcg", ] %>%
    group_by(graph_type, prior, seed, t) %>%
    summarise(disp = class[which.max(rev_gain)] == "win_disp", .groups = "drop")
  add("share of rounds the best VCG phantom displaces an allocated agent", "",
      fmt(mean(g$disp)), "arm_g_round")
  add("Arm G outcomes passing the per-agent check", "",
      fmt(mean(res$armg$rationalised[res$armg$point == point])), "rationalises_all")
  add("first-price: max operator gain over Arms P and G", "",
      fmt(max(c(res$armp$dp[res$armp$mech == "fp" & res$armp$point == point],
                res$armg$rev_gain[res$armg$mech == "fp" & res$armg$point == point]))),
      "arm_p_round, arm_g_round")
  xr <- rd[rd$graph_type == "entangled", ]
  if (nrow(xr)) {
    add("X boundary: rounds certified", "", sprintf("%d of %d", sum(xr$cert), nrow(xr)), "dsic_certificate")
    add("X boundary: rounds where value-greedy falls short of the exact optimum", "",
        fmt(mean(!xr$greedy_exact)), "node_exact_pair")
  }
  v <- res$verify[res$verify$point == point, ]
  add("honest transcripts checked (rounds)", "", sum(v$mutation == "honest"), "verify_transcript")
  add("honest transcripts rejected", "", sum(v$rejected_any[v$mutation == "honest"]), "verify_transcript")
  for (m in names(mutation_verifier)) {
    x <- v[v$mutation == m & v$realised, ]
    add("revenue the mutation would carry (1e-3), mean", m, fmt(mean(x$revenue) * 1e3), "clinch_mutations")
  }
  add("share of rounds the phantom is accepted with the roster clause removed", "",
      fmt(mean(!v$rejected_any[v$mutation == "phantom_roster_off"])), "verify_transcript")
  add("share of rounds with binding capacity, (C3)", "", fmt(mean(rc$c3)), "ledger_round")
  lg <- res$ledger[res$ledger$point == point & res$ledger$realised, ]
  ke <- lg[lg$case == "4_perturbation" & lg$conditions == "no_C1", ]
  add("knife-edge: max |gain - lambda dp|", "", if (nrow(ke)) fmt(max(abs(ke$delta - d$stake * ke$dp))) else "---",
      "ledger_round")
  add("identities at every certified round: certificate, greedy exact, VCG = companion, clinching = VCG, augmented VCG = companion",
      "", paste(all(rc$cert), all(rc$greedy_exact), all(rc$vcg_eq_p1), all(rc$clinch_eq_vcg),
                all(rc$vcg_aug_eq_p1), sep = "/"), "run_round")
  bind_rows(num)
}
