# Credibility study: operator deviations on certified polymatroidal markets

Computation code for the evaluation of the paper

> **Credibility Trilemma in Polymatroidal Service Markets**
> Preprint: arXiv:2605.26604.

The study runs the objects of the paper's credibility analysis on the certified instances of the companion platform, the simulation code of *Agentic Service Markets Across the Computing Continuum: A Polymatroidal Architecture* (arXiv:2603.05614), which this repository carries as the git submodule `p1/`. It sources five files of the platform's `R/` directory and adds only its own objects on top; nothing in the platform is modified.

## What is computed

- **Agents.** One task per agent per round (a single-parameter agent with allocation in {0, 1}), drawn on the platform's seeded round stream and valued by its bid model at a fixed exogenous utilisation, so a bid is the drawn value times one common multiplier. Values follow prior (a), U(0, 1) at every leaf, or prior (b), U(0, 1) on leaves l1 and l3 and F(v) = v^2 on the others. `R/study_tasks.R`.
- **Region.** The agent-level rank is the platform's exact leaf-block optimum at unit marginals (`task_rank`, `market_model`), and the conditional non-modularity gap is four rank calls (`conditional_gap`). `R/rank.R`.
- **Mechanisms.** VCG is the platform's `vcg_allocate`; the study re-runs it as the welfare greedy on the platform's kernel `.greedy_pack_by` with Archer-Tardos payments read off each agent's counterfactual step function, and checks the two agree at every round. The revenue-optimal mechanism is the virtual-value greedy on the same kernel, whose non-positive skip is the reserve. Pay-as-bid is VCG's allocation at the bids. The ascending clinching auction is event-driven on the rank oracle, with residual supply s_i(p) = f(A(p)) - f(A(p) - i), and emits the broadcast transcript. `R/mechanisms.R`.
- **Deviations.** Arm P raises the next-in-priority agent's bid inside the contested window of the payment perturbation lemma and re-runs the mechanism; Arm G appends a phantom task under an identity outside the round's agents and re-runs the mechanism. Each observed outcome is checked against a re-run of the prescribed mechanism on the rationalising profile, with support membership. `R/deviations.R`.
- **Broadcast verification.** Verifiers over the clinching transcript, one transcript mutation per case of the commitment proof, and the roster clause as a removable condition. `R/verify.R`.
- **Domain separation.** The operator's ledger per proof case, with the side conditions in force and removed one at a time. `R/ledger.R`.
- **Programme and tables.** `R/programme.R` runs every block; `R/tables.R` writes the four LaTeX table bodies and `numbers.csv`, which names the function each quoted number comes from.

## Requirements

- R (>= 4.1) with `dplyr`, `tibble`, `purrr`, `tidyr`, `readr` and `testthat`.
- The platform's own suite additionally needs the packages its README lists (`here`, `lpSolve`, `withr`, `digest`, `yaml` among them).

## Running

```sh
git clone --recurse-submodules <this repository>
make test        # the platform's suite at the pinned tree, then this study's suite
make compute     # every block; raw tables under results/<label>/
make tables      # table bodies and numbers.csv per operating point
make reproduce   # the three above, in order
```

`make compute` runs blocks on `STUDY_WORKERS` forked workers (default: all cores but one); each block is deterministic given its seed, so the result does not depend on the worker count. The output directory is labelled by the platform tree: `results/release-v2.1.0/` at the release pin, `results/dev-<commit>/` otherwise. Numbers under a `dev-` label are development numbers.

Install the hygiene hook once per clone: `git config core.hooksPath scripts/git-hooks`.

## The platform pin

The submodule `p1/` is added from a relative local path and developed against platform commit `ca989fb`, whose five sourced blobs are recorded in `R/p1.R` and compared by `tests/testthat/test-p1-pin.R`. At release the submodule URL is switched to the platform's public remote and pinned to its release tag:

```sh
git submodule set-url p1 https://github.com/Future-Computing-Group/agentic-economy-sim
git -C p1 fetch --tags && git -C p1 checkout v2.1.0
git add .gitmodules p1
```

The pin test is skipped until the tag exists; from then on it requires the submodule to sit at the tag, the tag's five blobs to equal the recorded ones, and the platform's version DOI (10.5281/zenodo.22876728) to resolve.

## Operating points and wall time

Thirty rounds per seed and ten seeds, the platform's joint-misreport block. Three points per instance: `population`, the platform's agent counts (90 on the tree, 45 on the fan), and `load`, the platform's offered load at its high load level (1.5 tasks per agent, so 135 and 68 single-task agents), which also carries the crossing instance at the platform's enumerable point (12 agents, a tenth of the capacity) as a boundary row. The platform's load level itself is not a study constant: in the platform's environment constructor it sets only the task-arrival rate of the platform's own generator, which the study does not call, and a test checks that environments built at two levels differ in nothing else. A third point, `contention`, keeps the load point's agent counts at half the capacity (scale 0.5), the tighter region on which the revenue-optimal mechanism's allocation, thinned by its reserve, meets competition. The platform's own contention sweep is not run.

Measured on a 10-core laptop: `make test-study` about 50 s; `make compute` 1436 s (24 min) for 90 blocks on 9 workers; `make tables` about 2 s. The platform's own suite (`make test-p1`) runs about twenty minutes, as its README states.

## Licence

MIT; see `LICENSE`. Cite the paper as given in `CITATION.cff`.
