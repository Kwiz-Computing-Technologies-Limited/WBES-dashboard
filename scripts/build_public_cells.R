#!/usr/bin/env Rscript
# scripts/build_public_cells.R
# =============================================================================
# Builds the PUBLIC, aggregated stand-in for the firm-level table.
#
# processed.parquet holds one row per surveyed firm. That is World Bank
# microdata and is never published or bundled into a public deploy. This script
# collapses it into cells of at least MIN_CELL firms and writes
# data/processed/processed_cells.parquet, which IS safe to ship.
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
# Usage (from project root, needs data/processed/processed.parquet):
#   Rscript scripts/build_public_cells.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
})

MIN_CELL <- as.integer(Sys.getenv("WBES_MIN_CELL", "5"))
SRC <- "data/processed/processed.parquet"
OUT <- "data/processed/processed_cells.parquet"

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
write_parquet(out, OUT)

cat(sprintf(
  "cells: %d (min %d firms)\nfirms kept by stage (full, -sector, -size, -ownership): %s; dropped %d of %d\nwrote %s (%.1f KB)\n",
  nrow(out), min(out$n_firms), paste(vapply(kept, nrow, integer(1)), collapse = " / "),
  nrow(dropped), sum(vapply(kept, nrow, integer(1))) + nrow(dropped), OUT, file.size(OUT) / 1024
))
