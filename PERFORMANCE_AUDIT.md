# WBES Dashboard — Performance & Rhino-Compliance Audit

_Audited: 2026-06-24. Scope: startup/load time, query/reaction time, and conformance to Appsilon's Rhino framework._

---

## TL;DR — Root Cause

The dashboard runs a **674 MB raw Stata `.dta` ETL pipeline — including live World Bank API calls — inside the Shiny `server()` function, i.e. once per user session, on the critical startup path.** Nothing heavy is precomputed offline, and the one cache that would help (`data/wbes_processed_cache/`) is `.gitignore`d and lives on an ephemeral container filesystem, so the full pipeline re-runs on most cold starts. The `future()`/async wrapper is a no-op on Posit Cloud (it forces `plan("sequential")`), so the whole thing blocks.

**The fix is architectural, not incremental: move the ETL offline, ship small precomputed artifacts (a few MB), and load them once at app scope.** This alone takes cold start from minutes-plus-network to a few seconds, and removes the per-session recompute and per-session 250k-row memory footprint.

---

## 1. Why startup is slow (ranked by impact)

### 1.1 — The whole ETL runs per-session, inside `server()` 🔴 critical
[`app/main.R:631-743`](app/main.R:631) — `shiny::observe({ future({ load_wbes_data(...) }) ... })` lives **inside `server()`**. There is no app-scope / global data load. Every new session (and every container cold-start) triggers the entire pipeline.

### 1.2 — The data artifact is a 674 MB raw `.dta`, parsed live 🔴 critical
`data/assets.zip` contains a single **674 MB** uncompressed Stata file (`ES-Indicators-Database-Global-Methodology_December_15_2025.dta`). The "zero-extraction Parquet" branch in [`app/logic/wbes_data.R:144`](app/logic/wbes_data.R:144) is **never taken** (there is no `.parquet` in the zip). So every cold load:
1. unzips 674 MB to `tempdir()` ([`wbes_data.R:210-213`](app/logic/wbes_data.R:210));
2. `haven::read_dta(..., encoding = "latin1")` parses the entire Stata file into RAM ([`wbes_data.R:246`](app/logic/wbes_data.R:246)) — tens of seconds and multiple GB on its own;
3. runs `process_microdata()` — one giant `mutate()` over every row with ~60 `get0()`/`as_factor()`/`coalesce_num()` derivations ([`wbes_data.R:527-661`](app/logic/wbes_data.R:527));
4. runs `enrich_wbes_with_income()` — **live World Bank API calls** on the load path ([`wb_api.R:252`](app/logic/wb_integration/wb_api.R:252), called from [`wbes_data.R:183`](app/logic/wbes_data.R:183));
5. runs **6 separate `group_by |> summarise(across(...))`** passes over the full microdata ([`wbes_data.R:357-437`](app/logic/wbes_data.R:357));
6. writes 8 Parquet files + an `xz`-compressed RDS ([`parquet_cache.R:59`](app/logic/parquet_cache.R:59)).

### 1.3 — A second blocking network call: WB macro prefetch (5-min timeout) 🔴 critical
After data loads, [`app/main.R:701`](app/main.R:701) calls `prefetch_wb_data_for_countries(timeout_seconds = 300)` — a batch World Bank API pull for ~150 countries × ~25 indicators × ~20 years — **still on the startup path**, before `waiter_hide()`. On a slow or rate-limited API this stalls the loading screen for up to 5 minutes.

### 1.4 — The cache that would save loads #2…n doesn't persist 🔴 critical
`data/wbes_processed_cache/` and `data/processed_wbes_metadata.rds` are **`.gitignore`d** (see `.gitignore`) and written into the app dir at runtime. On Posit Connect Cloud the deployed bundle omits them, and the container FS is ephemeral / resets on scale-to-zero — so the "fresh cache → fast path" at [`wbes_data.R:44`](app/logic/wbes_data.R:44) rarely hits in production. Cold = full 674 MB path, far more often than "once per 24h."

### 1.5 — `future()` async is a no-op on Posit 🟠 major
[`app/main.R:609-613`](app/main.R:609): when `SHINY_PORT != ""` (true on Posit) it sets `plan("sequential")`, so `future({...})` runs **synchronously in the session's R process** — no async benefit, the session blocks. Locally, `plan("multisession", workers = 2)` instead pays to spin up workers, re-load packages, and serialize the ~GB result back.

### 1.6 — Per-session memory bloat 🟠 major
The returned list keeps `raw` (full 250k×N microdata) **and** `processed` (microdata + ~60 derived cols) **and** all aggregates, held in a `reactiveVal` and passed to all 25 modules ([`wbes_data.R:488-515`](app/logic/wbes_data.R:488), [`main.R:1013-1041`](app/main.R:1013)). Each concurrent session holds the entire microdata → GC churn, memory pressure, and on a small instance, swap/OOM that further degrades start time.

### 1.7 — Brittle hard-coded filename 🟡 minor
[`wbes_data.R:23`](app/logic/wbes_data.R:23) hardcodes `..._November_24_2025.dta`, but the shipped file is `..._December_15_2025.dta`. The preferred-match at [`wbes_data.R:199`](app/logic/wbes_data.R:199) silently fails and falls back to "any `.dta`" — works today only because there's exactly one file.

---

## 2. Why interactions (reaction time) are slow

- **Full microdata filtered on every input change.** When a year filter is active, several modules switch from small aggregates to the **250k-row `processed` frame** and re-run `apply_common_filters()` — including `group_by(country) |> filter(year == max(year))` ([`shared_filters.R:144-151`](app/logic/shared_filters.R:144)) — on each interaction (e.g. `mod_benchmark_sector.R`, `mod_overview.R`).
- **Monolithic per-filter aggregation.** Benchmark modules compute *all indicators × all groups* in one reactive that invalidates on any filter change; KPI tiles each recompute their own `mean()`; heatmaps rebuild matrices with nested `sapply`; maps fully re-render on every indicator switch.
- **No caching anywhere.** `grep` finds **zero** uses of `bindCache` / `memoise` / `reactiveFileReader`, despite `digest` being a declared dependency. Identical recomputations are never reused.
- **Live WB API calls inside a reactive.** `mod_country_profile` calls `get_wb_country_context()` per country switch instead of reading the prefetched cache — network latency in the hot path.
- **Repeated coordinate `merge()`** on every `filtered_data()` access, even though lat/lng could live in the precomputed aggregate.

---

## 3. Rhino-compliance gaps

The skeleton **is** genuine Rhino (good): `app/main.R` with `ui`/`server`, `app/logic` + `app/view` box modules, `rhino.yml`, `config.yml`, `app.R = rhino::app()`, `tests/testthat`, `renv`, Sass via Node, `app/static`. Deviations from Rhino / Appsilon best practice:

1. **ETL inside `logic` called from `server`.** Rhino wants `logic/` to be pure, testable functions and heavy data prep to be an **offline build step** producing an artifact the app reads. Today the app *is* the ETL.
2. **`legacy_entrypoint: box_top_level`** in `rhino.yml` — the legacy entrypoint. `main.R` already exports `ui`/`server`, so this flag is likely unnecessary and a smell.
3. **Dead / duplicate code:**
   - `app/logic/data_loader.R` (451 lines, incl. a fake sample-data generator) is **never imported** — `main.R` uses `app/logic/wbes_data`.
   - `app/view/mod_benchmark_size.R.backup` is committed.
   - `app/view/` has both wired-in `mod_*.R` and orphaned plain views (`overview.R`, `benchmark.R`, `country_profile.R`, `infrastructure.R`, `finance_access.R`, `data_quality.R`, `about.R`, `comp_region_filter.R`) not referenced by `main.R`.
4. **`box::use(dplyr[...])` wildcard imports** in `wbes_data.R` and `wb_api.R` — discouraged by Rhino's linter and Appsilon style; import named functions instead.
5. **`box::use(...)` inside a function body** ([`wb_api.R:664`](app/logic/wb_integration/wb_api.R:664), `690`) instead of at module top.
6. **Repo clutter:** ~15 root-level status/markdown reports (`AUDIT_REPORT.md`, `BATCH_0A_*`, `SESSION_SUMMARY.md`, …) not covered by `.Rbuildignore`.

---

## 4. Target architecture

```
  ┌──────────────────────── OFFLINE (build time, run once / on data refresh) ────────────────────────┐
  │  scripts/build_data.R                                                                             │
  │    read_dta(674MB)  →  process_microdata()  →  enrich_with_income (WB API)  →  6× aggregates      │
  │    →  prefetch WB macro indicators  →  write SMALL artifacts:                                     │
  │         data/processed/{latest,country_panel,country_sector,country_size,country_region,          │
  │                         regional}.parquet   +  meta.rds  +  wb_macro.parquet                      │
  │         (+ optional microdata.parquet, partitioned, for row-level drilldowns only)                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                              │  commit / deploy  (a few MB, not 674 MB)
                                              ▼
  ┌──────────────────────────── RUNTIME (app) ────────────────────────────┐
  │  app/main.R  — load small artifacts ONCE at APP SCOPE (outside server) │
  │    app_data <- load_precomputed()         # shared across ALL sessions │
  │  server(): wbes_data <- reactiveVal(app_data)   # instant, no ETL      │
  │    • no read_dta, no WB API on the load path                           │
  │    • modules read small aggregates; render outputs use bindCache()     │
  │    • row-level needs → Arrow/DuckDB lazy query (filter/col pushdown)   │
  └───────────────────────────────────────────────────────────────────────┘
```

**Key principles**
- **Separate ETL from app.** Heavy `.dta → process → enrich → aggregate → WB-prefetch` happens **once, offline**, in `scripts/build_data.R`. Output: small Parquet/RDS artifacts (a few MB) that get committed/deployed.
- **Load once at app scope, share across sessions.** Read the small artifacts at the top of `main.R` (outside `server`), so sessions get instant data. No per-session ETL, no per-session 250k-row copy.
- **No network on the startup path.** WB income enrichment and WB macro prefetch are baked into the artifact at build time. The app reads `wb_macro.parquet`; live API becomes a rare, cached fallback only.
- **Drop `raw`/`processed` from the in-memory payload.** Ship only the aggregates the UI needs. If genuine row-level drilldown is required (e.g. distribution fitting in Overview), store microdata as a **partitioned Parquet / DuckDB** file and query it lazily (Arrow `dplyr` with predicate + column pushdown, `collect()` only the small result) instead of holding 250k rows in RAM.
- **Cache reactions.** Add `bindCache()` (keyed on filter state) to expensive `renderPlotly`/`renderLeaflet`/`renderDT`; split monolithic per-filter aggregations into cached/domain-scoped reactives; read WB macro from the prefetched artifact, not the API.

---

## 5. Prioritized remediation plan

| # | Change | Effort | Expected impact |
|---|--------|--------|-----------------|
| **P1** | `scripts/build_data.R`: run the existing ETL once offline; emit small Parquet aggregates + `wb_macro.parquet` + meta. Stop shipping the 674 MB `.dta` in the deploy bundle. | M | **Cold start: minutes → seconds.** Removes 674 MB read + live WB calls from runtime. |
| **P2** | `main.R`: load artifacts **once at app scope**; `server()` just wraps them in `reactiveVal`. Delete the `future()`/`observe` ETL block and the startup `prefetch_wb_data_for_countries`. | S | Removes per-session recompute and the 5-min WB prefetch stall. |
| **P3** | Drop `raw`/`processed` from the runtime payload; serve aggregates only. Route any row-level need through Arrow/DuckDB lazy queries. | M | Cuts per-session memory from ~GB to ~tens of MB; enables concurrency. |
| **P4** | Add `bindCache()` to heavy render outputs; consolidate per-KPI `mean()`s into one cached reactive per module; split monolithic benchmark aggregations by domain. | M–L | **Reaction time: ~8–12 s → <2 s** on filter changes. |
| **P5** | `mod_country_profile`: read WB macro from the prefetched artifact; live API only as a cached fallback. | S | Removes network latency on country switches. |
| **P6** | Rhino cleanup: delete `data_loader.R`, `*.R.backup`, orphan views; replace `dplyr[...]` with named imports; move in-function `box::use` to top; re-evaluate `legacy_entrypoint`; `.Rbuildignore` the status `.md`s; run `rhino::lint_r()`. | S–M | Passes Rhino lint; removes dead-code confusion; smaller bundle. |
| **P7** | Make the data filename non-brittle (glob `*.dta` / config key instead of hardcoded date). | XS | Avoids silent breakage on data refresh. |

P1 + P2 are the 80/20 — together they fix the startup complaint. P3–P5 fix reaction time and scale. P6–P7 bring it to clean Rhino conformance.

---

## 6. Verification

- Time cold start before/after (target: <5 s to interactive on Posit).
- `profvis` a filter-change interaction before/after P4 (target: <2 s).
- Peak RSS per session before/after P3.
- `rhino::lint_r()` clean; `testthat` green (update/remove tests that cover deleted `data_loader.R`).

---

## 7. Implementation status (completed 2026-06-24)

**Done and verified:**

- **P1 — Offline ETL** (`scripts/build_data.R`): runs the full pipeline once and
  writes `data/processed/` artifacts. Measured: 258,638 firms read in ~7 s, full
  build ~20 s, **artifacts total 6.9 MB** (vs the 674 MB `.dta`). Income
  enrichment 99.6%.
- **P2 — App-scope load** (`app/logic/data_artifacts.R` + `app/main.R`): artifacts
  load **once per process, shared across sessions** (measured **0.13 s**). The
  per-session `future()`/`observe()` ETL and the 5-minute startup WB prefetch are
  removed. One-time ETL fallback remains if artifacts are absent.
- **P3 — Memory**: `raw` microdata dropped from the runtime payload (only dead
  code referenced it); `processed` (258k×58) loaded once and shared, not per
  session.
- **P5 — WB prefetch**: country profile already preferred the cache; the build now
  produces `wb_macro.rds` (resilient per-category fetch — a dead WGI/governance
  indicator no longer fails the whole batch), so country switches hit the cache.
- **P6 — Rhino cleanup**: deleted `data_loader.R`, `mod_benchmark_size.R.backup`,
  8 orphan view files, and 4 broken test files (all referenced a non-existent
  `load_sample_data`). Added a box-aware `.lintr`, cleared the stale
  `app/view/__init__.R`, extended `.Rbuildignore`, dropped unused `future`/
  `future.apply` deps, added `cachem`. New tests (`test-data_artifacts.R`,
  `test-shared_filters.R`) — **38 pass, 0 fail**.
- **P7 — Filename**: `RAW_DTA_FILENAME` (hard-coded date) → `RAW_DTA_PATTERN`
  (stable-prefix glob).
- **P4 — Caching (partial)**: shared cross-session `cachem` cache wired at app
  scope; `bindCache` on the country-profile WB reactive (keyed on country +
  survey year — a complete, verified key).

**Verified:** app boots and serves HTTP 200 with full UI; `box::use(app/main)`
loads in ~2.5 s; new files lint clean; test suite green.

**Recommended follow-up (P4, broader):** add `bindCache` to the benchmark
modules' heatmap/map/KPI renders, keyed on `global_filters()` plus the module's
own indicator/selection inputs. Deferred from this pass because a correct cache
key must include every input the render depends on, which needs interaction
testing (running the app and exercising each control) to avoid stale-output
bugs — safer to do with the app running than to apply blind across the
2,000+-line benchmark modules. Pattern to apply:

```r
output$x <- renderPlotly({ ... }) |>
  bindCache(global_filters(), input$module_indicator, input$selected_groups)
```

**Deployment / data hosting:**

- The small **aggregate** artifacts in `data/processed/` (`latest`, `country_panel`,
  `country_sector`, `country_size`, `country_region`, `regional`,
  `country_coordinates`, `meta.rds`, `wb_macro.rds`, `build_info.rds` — ~1.3 MB)
  are committed to the repo and are safe to publish (aggregated/derived from
  public World Bank data).
- The **firm-level** `data/processed/processed.parquet` (258k rows, 5.6 MB) is
  **git-ignored** — it is WBES microdata and the repo is public. At runtime the
  app reads it from a local file if present (dev), otherwise downloads it once
  from the URL in the `WBES_PROCESSED_URL` environment variable (e.g. a Firebase
  Storage / GCS / S3 tokenized download URL). If neither is available the app
  still runs on the aggregates, with reduced firm-level drill-down. See
  `app/logic/data_artifacts.R::resolve_processed_path()`.
- To finish the Firebase setup: upload `data/processed/processed.parquet` to a
  (private) Firebase Storage bucket, get its download URL, and set
  `WBES_PROCESSED_URL` on the Posit deployment (and in `.Renviron` for local
  parity if you don't keep the file locally).
- Never deploy/commit `data/assets.zip` (674 MB raw `.dta`); it is only needed to
  re-run `scripts/build_data.R` on a data refresh.
