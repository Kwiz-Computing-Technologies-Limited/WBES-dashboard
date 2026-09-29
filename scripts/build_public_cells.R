#!/usr/bin/env Rscript
# scripts/build_public_cells.R
# =============================================================================
# Builds the PUBLIC, aggregated stand-in for the firm-level table.
#
# processed.parquet holds one row per surveyed firm. That is World Bank
# microdata and is never published or bundled into a public deploy. This script
# collapses it into cells of at least MIN_CELL firms and writes
# data/public/processed_cells.parquet, which IS safe to ship.
#
# Each cell carries, for every indicator, its mean over the firms that answered
# (`<col>`) and how many answered (`<col>__n`). data_artifacts.R expands a cell
# back into n_firms rows with exactly `<col>__n` non-missing values, so any mean
# the app takes at cell granularity or coarser (country, region, sector, size,
# year, female ownership) equals the firm-level mean. Firm-level spread is gone,
# so the app switches off its significance tests in this mode.
#
# Disclosure control: a cell below MIN_CELL firms is pooled with sector left
# blank; if still too small, with size blank too; then with ownership blank
# too; anything still below MIN_CELL is dropped. `pooled` records the stage
# (0 = full detail, 1 = sector withheld, 2 = + size, 3 = + ownership), so the
# app can say how many firms a sector, size or ownership breakdown leaves out.
#
# Every other table published with the cells (latest, country_panel,
# country_sector, ...) is rebuilt from the cells alone and written with them
# to data/public/, so the public bundle is internally consistent: no table can
# be differenced against the cells to recover a withheld firm. See
# app/logic/public_data.R.
#
# Usage (from project root, needs data/processed/processed.parquet):
#   Rscript scripts/build_public_cells.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
})

MIN_CELL <- as.integer(Sys.getenv("WBES_MIN_CELL", "5"))
SRC <- "data/processed/processed.parquet"
PUBLIC_DIR <- "data/public"
OUT <- file.path(PUBLIC_DIR, "processed_cells.parquet")

# The published tables and the columns each one is grouped by (see
# wbes_data.R). Each is rebuilt from the cells so none can be differenced
# against them.
PUBLIC_TABLES <- list(
  latest = c("country", "country_code"),
  country_panel = c("country", "year"),
  country_sector = c("country", "country_code", "sector"),
  country_size = c("country", "country_code", "firm_size"),
  country_region = c("country", "country_code", "region"),
  regional = "region"
)

options(box.path = getwd())
box::use(app/logic/public_data[suppress_small_items, public_table])

stopifnot(file.exists(SRC))
firms <- read_parquet(SRC)

keys <- c("country", "country_code", "year", "region", "income",
          "sector", "firm_size", "female_ownership")
indicators <- setdiff(names(firms)[vapply(firms, is.numeric, logical(1))], "year")

collapse <- function(d) {
  d |>
    group_by(across(all_of(c(keys, "pooled")))) |>
    summarise(
      n_firms = n(),
      across(all_of(indicators),
             list(mean = ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE),
                  n = ~ sum(!is.na(.x))),
             .names = "{.col}__{.fn}"),
      .groups = "drop"
    )
}

# Pool small cells in stages, withholding the finest attribute first. Each stage
# keeps the cells that reach MIN_CELL and passes the rest on.
withhold <- list(
  character(0),                                  # full detail
  "sector",                                      # sector withheld
  c("sector", "firm_size"),                      # + size withheld
  c("sector", "firm_size", "female_ownership")   # + ownership withheld
)
kept <- list()
rest <- firms
for (i in seq_along(withhold)) {
  for (col in withhold[[i]]) rest[[col]] <- rest[[col]][NA_integer_]
  sizes <- rest |> count(across(all_of(keys)), name = "n_firms")
  rest <- rest |> left_join(sizes, by = keys)
  kept[[i]] <- rest |> filter(n_firms >= MIN_CELL) |> select(-n_firms) |>
    mutate(pooled = i - 1L)
  rest <- rest |> filter(n_firms < MIN_CELL) |> select(-n_firms)
}
dropped <- rest

out <- collapse(bind_rows(kept)) |>
  rename_with(~ sub("__mean$", "", .x), ends_with("__mean"))

stopifnot(min(out$n_firms) >= MIN_CELL)

# Item-level control: a question answered by fewer than MIN_CELL firms in a
# cell is blanked, or its mean would be those few firms' own answers.
n_before <- sum(as.matrix(out[grep("__n$", names(out))]) > 0)
out <- suppress_small_items(out, MIN_CELL)
counts <- as.matrix(out[grep("__n$", names(out))])
stopifnot(all(counts == 0 | counts >= MIN_CELL))
n_blanked <- n_before - sum(counts > 0)

dir.create(PUBLIC_DIR, showWarnings = FALSE)
write_parquet(out, OUT)

# Differencing control: every table shipped with the cells is rebuilt from them.
for (tbl in names(PUBLIC_TABLES)) {
  template <- read_parquet(file.path("data/processed", paste0(tbl, ".parquet")))
  pub <- public_table(template, out, PUBLIC_TABLES[[tbl]])
  write_parquet(pub, file.path(PUBLIC_DIR, paste0(tbl, ".parquet")))
  cat(sprintf("  %-15s %4d of %4d rows rebuilt from cells\n", tbl, nrow(pub), nrow(template)))
}

cat(sprintf(
  "cells: %d (min %d firms)\nfirms kept by stage (full, -sector, -size, -ownership): %s; dropped %d of %d\nindicator values blanked (fewer than %d respondents): %d\nwrote %s (%.1f KB)\n",
  nrow(out), min(out$n_firms), paste(vapply(kept, nrow, integer(1)), collapse = " / "),
  nrow(dropped), sum(vapply(kept, nrow, integer(1))) + nrow(dropped), MIN_CELL, n_blanked,
  OUT, file.size(OUT) / 1024
))
