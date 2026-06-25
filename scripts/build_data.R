#!/usr/bin/env Rscript
# scripts/build_data.R
# =============================================================================
# OFFLINE DATA BUILD — run once per data refresh, NOT at app startup.
#
# Reads the raw 674 MB WBES Stata microdata, runs the full ETL ONCE
# (process -> World Bank income enrichment -> 6 aggregates -> WB macro prefetch)
# and writes SMALL precomputed artifacts to data/processed/ that the app loads
# directly at app scope (no .dta read, no live API on the startup path).
#
# Usage (from project root):
#   Rscript scripts/build_data.R
#
# Outputs (data/processed/):
#   latest.parquet, country_panel.parquet, country_sector.parquet,
#   country_size.parquet, country_region.parquet, regional.parquet,
#   country_coordinates.parquet   — small aggregates
#   processed.parquet             — firm-level, lean column set (6 modules need it)
#   meta.rds                      — labels, quality, dimension vectors, metadata
#   wb_macro.rds                  — prefetched World Bank macro indicators
#   build_info.rds                — provenance (source file, timestamp, row counts)
# =============================================================================

suppressWarnings(suppressMessages({
  library(here)
}))

# Resolve project root so box can find app/logic/* and here() anchors to data/.
setwd(here::here())
options(box.path = getwd())

box::use(
  app/logic/wbes_data[load_wbes_data],
  app/logic/shared_filters[remove_na_columns],
  app/logic/wb_integration[prefetch_wb_data_for_countries],
  arrow[write_parquet],
  logger[log_info, log_warn]
)

t_start <- Sys.time()
out_dir <- here::here("data", "processed")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

log_info("=== WBES offline data build started ===")

# -----------------------------------------------------------------------------
# 1. Run the full ETL once (force .dta read; do NOT touch the runtime cache).
# -----------------------------------------------------------------------------
log_info("Step 1/4: Loading + processing microdata (this is the heavy step)...")
data <- load_wbes_data(use_cache = FALSE)

# -----------------------------------------------------------------------------
# 2. Bake in remove_na_columns (previously done per-session in main.R).
# -----------------------------------------------------------------------------
log_info("Step 2/4: Pruning all-NA columns from aggregates...")
for (tbl in c("latest", "processed", "country_panel", "country_sector",
              "country_size", "country_region")) {
  if (!is.null(data[[tbl]])) {
    data[[tbl]] <- remove_na_columns(data[[tbl]])
  }
}

# -----------------------------------------------------------------------------
# 3. Prefetch World Bank macro indicators (previously done per-session at start).
# -----------------------------------------------------------------------------
log_info("Step 3/4: Prefetching World Bank macro indicators...")
wb_macro <- tryCatch(
  prefetch_wb_data_for_countries(
    wbes_countries = data$countries,
    timeout_seconds = 600
  ),
  error = function(e) {
    log_warn(paste("WB macro prefetch failed (app will fall back to live API):", e$message))
    NULL
  }
)

# -----------------------------------------------------------------------------
# 4. Write artifacts.
# -----------------------------------------------------------------------------
log_info("Step 4/4: Writing artifacts to data/processed/ ...")

write_pq <- function(df, name) {
  if (is.null(df)) {
    log_warn(sprintf("  %s: NULL, skipped", name))
    return(invisible())
  }
  path <- file.path(out_dir, paste0(name, ".parquet"))
  write_parquet(df, path, compression = "zstd")
  log_info(sprintf("  %-22s %6.2f MB  (%d rows x %d cols)",
                   paste0(name, ".parquet"),
                   file.size(path) / 1024^2, nrow(df), ncol(df)))
}

# 4a. Small aggregate tables (app reads these as-is).
write_pq(data$latest,               "latest")
write_pq(data$country_panel,        "country_panel")
write_pq(data$country_sector,       "country_sector")
write_pq(data$country_size,         "country_size")
write_pq(data$country_region,       "country_region")
write_pq(data$regional,             "regional")
write_pq(data$country_coordinates,  "country_coordinates")

# 4b. Firm-level processed — keep ONLY the dimension + derived metric columns the
#     6 consuming modules use. Drops the 295 raw Stata columns (which are
#     haven_labelled and unused downstream), so the file is small + plain-typed.
#     Metric list mirrors metric_cols in app/logic/wbes_data.R.
dimension_cols <- c(
  "country", "country_code", "year", "region", "income",
  "sector", "firm_size", "sample_weight", "female_ownership"
)
metric_cols <- c(
  "power_outages_per_month", "avg_outage_duration_hrs", "firms_with_generator_pct",
  "firms_with_credit_line_pct", "firms_with_bank_account_pct", "loan_rejection_rate_pct",
  "collateral_required_pct", "bribery_incidence_pct", "corruption_obstacle_pct",
  "capacity_utilization_pct", "export_share_pct", "export_firms_pct",
  "female_ownership_pct", "female_workers_pct", "crime_obstacle_pct", "security_costs_pct",
  "workforce_obstacle_pct", "annual_sales_growth_pct",
  "electricity_obstacle", "water_obstacle", "transport_obstacle", "generator_share_pct",
  "water_insufficiency_pct", "water_shortages_per_month", "water_shortage_duration_hrs",
  "days_to_water_connection", "transport_perception_index", "transport_biggest_obstacle",
  "internet_disruptions_pct", "days_to_internet_connection", "website_pct",
  "crime_losses_pct", "loan_application_pct", "overdraft_facility_pct",
  "bribe_for_permit", "bribe_for_import", "bribe_for_utilities", "bribe_for_tax",
  "bribe_for_contract", "mgmt_time_regulations_pct",
  "no_need_for_loan", "loan_procedures_complex", "loan_interest_high",
  "insufficient_collateral", "loan_size_inadequate", "days_to_get_loan"
)
if (!is.null(data$processed)) {
  pcols <- names(data$processed)
  ic_cols <- pcols[grepl("^IC[.]FRM", pcols)]
  keep <- intersect(unique(c(dimension_cols, metric_cols, ic_cols)), pcols)
  processed_lean <- data$processed[, keep, drop = FALSE]
  # Coerce any lingering haven_labelled to plain numeric so Parquet is portable.
  processed_lean[] <- lapply(processed_lean, function(col) {
    if (inherits(col, "haven_labelled")) as.numeric(col) else col
  })
  write_pq(processed_lean, "processed")
}

# 4c. Metadata + label dictionaries + quality docs (small; RDS keeps R types).
meta <- list(
  countries           = data$countries,
  country_codes       = data$country_codes,
  years               = data$years,
  regions             = data$regions,
  sectors             = data$sectors,
  column_labels       = data$column_labels,
  label_mapping       = data$label_mapping,
  metadata            = data$metadata,
  quality             = data$quality
)
saveRDS(meta, file.path(out_dir, "meta.rds"), compress = "xz")
log_info(sprintf("  %-22s %6.2f MB", "meta.rds",
                 file.size(file.path(out_dir, "meta.rds")) / 1024^2))

# 4d. World Bank macro prefetch.
if (!is.null(wb_macro)) {
  saveRDS(wb_macro, file.path(out_dir, "wb_macro.rds"), compress = "xz")
  log_info(sprintf("  %-22s %6.2f MB", "wb_macro.rds",
                   file.size(file.path(out_dir, "wb_macro.rds")) / 1024^2))
}

# 4e. Build provenance.
build_info <- list(
  built_at      = Sys.time(),
  source_file   = data$metadata$files,
  n_firms       = if (!is.null(data$processed)) nrow(data$processed) else NA_integer_,
  n_countries   = length(data$countries),
  n_years       = length(data$years),
  builder       = "scripts/build_data.R"
)
saveRDS(build_info, file.path(out_dir, "build_info.rds"))

elapsed <- as.numeric(Sys.time() - t_start, units = "mins")
total_mb <- sum(file.size(list.files(out_dir, full.names = TRUE)), na.rm = TRUE) / 1024^2
log_info(sprintf("=== Build complete in %.1f min | %d artifacts | %.1f MB total ===",
                 elapsed, length(list.files(out_dir)), total_mb))
log_info("Next: the app loads these at app scope via app/logic/data_artifacts.R")
