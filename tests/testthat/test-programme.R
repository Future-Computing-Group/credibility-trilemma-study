# The programme: one round end to end, the agent-side leg, the constants of a
# block, the selection-form constant c, and the table writers.

small_defaults <- function() {
  d <- study_defaults()
  d$rounds <- 2L
  d$seeds  <- 1L
  d$points <- tibble::tribble(
    ~point,  ~graph_type, ~N,  ~cap_scale,
    "probe", "tree",      40L, 0.15,
    "probe", "sp",        40L, 0.3,
    "probe", "entangled", 12L, 0.1)
  d
}

test_that("one round end to end: the identities hold and every block is filled", {
  d  <- small_defaults()
  rr <- run_round(d$points[1, ], "b", seed = 1L, t = 1L, d = d)
  expect_true(rr$round$cert)
  expect_true(rr$round$greedy_exact)
  expect_true(rr$round$vcg_eq_p1)
  expect_true(rr$round$clinch_eq_vcg)
  expect_true(rr$round$vcg_aug_eq_p1)
  expect_setequal(unique(rr$armp$mech), c("vcg", "myerson", "fp"))
  expect_setequal(unique(rr$armg$mech), c("vcg", "myerson", "fp"))
  expect_setequal(rr$verify$mutation, c("honest", names(mutation_verifier), "phantom_roster_off"))
  expect_setequal(unique(rr$clinch_dev$arm), c("P", "G"))
  expect_true(all(rr$clinch_dev$rejected[rr$clinch_dev$realised]))
  expect_gt(nrow(rr$ledger), 0)
  x <- run_round(d$points[3, ], "a", seed = 1L, t = 1L, d = d)
  expect_false(x$round$cert)
  expect_null(x$armp)
})

test_that("agent-side leg: the companion's misreport set finds no profitable report", {
  r <- fixture_round("T", N = 30L, cap_scale = 0.15, prior = "b")
  a <- agent_side_round(r$b, r$model, r$pr)
  expect_setequal(a$mech, c("vcg", "myerson"))
  expect_true(all(a$max_gain <= 1e-9))
  expect_true(any(a$n_members > 1))
  d <- study_defaults()
  g <- exp7_agent_gains("vcg", r$tasks, r$env, 1L, r$b, d$util_hat,
                        base_latency_for_bids(r$env), init_success_model(),
                        d$lambda_l)
  expect_lte(max(g), 1e-9)
})

test_that("a block reads every constant from the defaults it is handed", {
  d <- small_defaults()
  b <- run_block(d$points[2, ], "a", seed = 1L, d = d)
  expect_identical(nrow(b$round), d$rounds)
  expect_identical(unique(b$round$N), 40L)
  expect_identical(unique(b$round$cap_scale), 0.3)
})

test_that("static: random numbers are drawn by the study generator only", {
  for (f in list.files(file.path(study_root_dir, "R"), full.names = TRUE)) {
    if (basename(f) %in% c("study_tasks.R")) next
    s <- sub("#.*$", "", readLines(f))
    expect_false(any(grepl("runif|rnorm|sample\\(|sample\\.int|set\\.seed", s)),
                 info = basename(f))
  }
})

test_that("the constant c in selection form is the mean one-agent gain per round", {
  armp <- tibble(point = "p", graph_type = "tree", prior = "a", mech = "vcg",
                 seed = 1L, t = c(1L, 1L, 2L), theta = 0.5, gamma = 1,
                 dp = c(0.2, 0.5, 0.1), sup_formula = c(0.4, 1, 0.2))
  rounds <- tibble(point = "p", graph_type = "tree", prior = "a", seed = 1L,
                   t = 1:4)
  cc <- c_selection(armp, rounds)
  expect_equal(cc$c, (0.5 + 0.1) / 4)
  expect_equal(cc$p_event, 2 / 4)
  expect_equal(cc$h_given_event, (0.5 + 0.1) / 2)
  expect_equal(cc$c_sup, (1 + 0.2) / 4)
})

test_that("the table writers emit the four bodies with no placeholder left", {
  d   <- small_defaults()
  out <- tempfile("tables")
  res <- run_programme(d, workers = 1L, out_dir = out)
  write_tables(res, out, point = "probe")
  for (f in c("eval-instances.tex", "eval-deviations.tex", "eval-broadcast.tex",
              "eval-separation.tex")) {
    s <- readLines(file.path(out, "tables", f))
    expect_match(paste(s, collapse = "\n"), "\\\\begin\\{tabular\\}")
    expect_false(any(grepl("\\[\\[|NA\\b", s)), info = f)
  }
  nums <- read.csv(file.path(out, "numbers.csv"))
  expect_true(all(c("placeholder", "value", "source") %in% names(nums)))
  expect_gt(nrow(nums), 10)
  cfg <- read.csv(file.path(out, "config.csv"))
  expect_identical(cfg$value[cfg$key == "rounds"], "2")
})

test_that("the clinching rows read the clinching transcript, not the VCG rows", {
  key <- tibble(point = "p", graph_type = "tree", prior = "a", seed = 1L, t = 1L)
  round <- mutate(key, cert = TRUE, W_vcg = 10, W_myerson = 8, W_fp = 10)
  armg <- bind_rows(lapply(c("vcg", "myerson", "fp"), function(m)
    mutate(key, mech = m, class = "nonwin", rev_gain = 0.2, extraction = 0.2,
           conc_w_num = 0, rationalised = TRUE)))
  clinch_dev <- bind_rows(
    mutate(key, arm = "P", realised = TRUE, gain = 0.3, extraction = 0.25,
           conc_w_num = 0.5, rejected = TRUE),
    mutate(key, arm = "G", realised = TRUE, gain = 0.4, extraction = 0.1,
           conc_w_num = 1.5, rejected = TRUE))
  res <- list(round = round, armp = NULL, armg = armg, clinch_dev = clinch_dev)
  dr <- deviation_rounds(res, "p")
  cl <- dr[dr$mech == "clinching", ]
  expect_equal(cl$gain[cl$arm == "G"], 0.4)
  expect_equal(cl$extraction[cl$arm == "G"], 0.1)
  expect_equal(cl$conc_w_num[cl$arm == "G"], 1.5)
  expect_equal(cl$extraction[cl$arm == "P"], 0.25)
  expect_equal(cl$conc_w_num[cl$arm == "P"], 0.5)
})
