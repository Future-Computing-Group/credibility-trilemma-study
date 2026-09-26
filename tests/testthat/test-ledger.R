# Domain separation: the operator's payoff ledger per proof case, with every
# side condition in force and with (C0), (C1), (C2), (C4) removed one at a time.

ledger_fixture <- function(inst = "T", t = 1L) {
  r <- fixture_round(inst, N = 40L, cap_scale = 0.3, t = t, prior = "a")
  d <- study_defaults()
  list(r = r, led = ledger_round(r$b, r$model, r$pr, "a", r$k, d$fee, d$stake))
}

test_that("(C3) is checked on the round, not assumed", {
  f <- ledger_fixture()
  expect_true(f$led$c3[1])
  big <- fixture_round("S", N = 40L, cap_scale = 1)       # 40 tasks, 50 units
  expect_false(ledger_round(big$b, big$model, big$pr, "a", big$k, 0.01, 0.1)$c3[1])
})

test_that("(C3) is read against the region's rank over the whole ground set", {
  # A round at the population point's size on the tree: its agents leave
  # part of the region unfilled, so the capacity does not bind even though
  # more agents are active than their own set's rank.
  r  <- fixture_round("T", N = 90L, cap_scale = 1, t = 9L, seed = 1L, prior = "a")
  rk <- set_rank(r$model, seq_along(r$b))
  expect_gt(sum(r$b > 0), rk)
  expect_lt(rk, 100)
  d  <- study_defaults()
  expect_false(ledger_round(r$b, r$model, r$pr, "a", r$k, d$fee, d$stake)$c3[1])
  expect_identical(ground_rank(r$model), 100)
})

test_that("with (C0)-(C4) in force no case raises the operator's payoff", {
  for (inst in c("T", "S")) for (t in 1:3) {
    led <- ledger_fixture(inst, t)$led
    on  <- led[led$conditions == "all" & led$realised, ]
    expect_true(all(on$delta <= 1e-12), info = paste(inst, t))
    expect_true(all(led$rejected[led$case == "3_capacity" & led$realised]))
  }
})

test_that("each removed condition opens the case the proof predicts", {
  d <- study_defaults()
  seen <- character()
  for (inst in c("T", "S")) for (t in 1:3) {
    led <- ledger_fixture(inst, t)$led
    at  <- function(cs, cond) led[led$case == cs & led$conditions == cond & led$realised, ]
    x <- at("4_perturbation", "no_C0")
    if (nrow(x)) { seen <- c(seen, "C0"); expect_true(all(x$delta > 0)) }
    x <- at("4_perturbation", "no_C1")
    if (nrow(x)) {
      seen <- c(seen, "C1")
      expect_lt(max(abs(x$delta - d$stake * x$dp)), 1e-12)
    }
    for (cs in c("5a_phantom_no_displacement", "5b_phantom_displacement")) {
      x <- at(cs, "no_C2")
      if (nrow(x)) {
        seen <- c(seen, "C2")
        on <- at(cs, "all")
        expect_equal(x$delta - on$delta, x$p_ph)
        expect_true(all(x$p_ph > 0))
      }
    }
    x <- at("5b_phantom_displacement", "no_C4")
    if (nrow(x)) {
      seen <- c(seen, "C4")
      on <- at("5b_phantom_displacement", "all")
      expect_equal(x$delta - on$delta, d$fee * x$units_ph)
    }
  }
  expect_setequal(unique(seen), c("C0", "C1", "C2", "C4"))
})

test_that("static: the ledger runs every operator strategy through the arms", {
  body <- paste(deparse(ledger_round), collapse = "\n")
  for (fn in c("mech_run\\(", "arm_p_round\\(", "arm_g_round\\(", "set_rank\\(")) {
    expect_match(body, fn)
  }
})

test_that("the separation table counts only rounds where (C3) holds", {
  row <- function(t, c3, delta) tibble(point = "p", graph_type = "tree", prior = "a",
                                       seed = 1L, t = t, case = "5a_phantom_no_displacement",
                                       conditions = names(ledger_conditions()),
                                       realised = TRUE, delta = delta, rejected = NA, c3 = c3)
  lg <- bind_rows(row(1L, TRUE, -0.01), row(2L, FALSE, 0.01))
  sr <- separation_rows(lg)
  x  <- sr[[which(names(case_label) == "5a_phantom_no_displacement")]]
  expect_identical(x[2], "1")
  expect_identical(x[3], fmt(-0.01 * 1e3))
})
