#!/usr/bin/env Rscript
# scripts/deploy_shinyapps.R
# =============================================================================
# Deploy the dashboard to shinyapps.io in the public aggregated data mode.
#
# The bundle is assembled in a clean staging directory from an explicit list,
# so firm-level data cannot be uploaded by accident: the raw survey file
# (data/assets.zip, *.dta) and the firm table (data/processed/processed.parquet)
# are never copied, and the script refuses to deploy if either is present.
# The app then runs on the data/public/ set: cells of at least 5 firms with no
# indicator resting on fewer than 5 respondents, and every table rebuilt from
# those cells (app/logic/public_data.R).
#
# shinyapps.io does not take environment variables from deployApp(), so the
# mode is set by a .Renviron in the bundle (read by R at app start-up).
#
# Prerequisites:
#   Rscript scripts/build_public_cells.R      # writes the data/public/ set
#   rsconnect account configured for the target (rsconnect::accounts())
#
# Usage (from project root):
#   Rscript scripts/deploy_shinyapps.R [account] [app_name]
#   defaults: kwizresearchservices  wbes-dashboard
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
account <- if (length(args) >= 1) args[[1]] else "kwizresearchservices"
app_name <- if (length(args) >= 2) args[[2]] else "wbes-dashboard"

# The disclosure-controlled set from build_public_cells.R: the cells and every
# table rebuilt from them (none built from all firms, so none can be differenced
# against the cells).
PUBLIC_FILES <- c(
  "processed_cells.parquet", "latest.parquet", "country_panel.parquet",
  "country_sector.parquet", "country_size.parquet", "country_region.parquet",
  "regional.parquet"
)
# Non-firm artifacts: labels and dimensions, map coordinates, World Bank API data.
SHARED_FILES <- c("meta.rds", "wb_macro.rds", "country_coordinates.parquet")
APP_FILES <- c("app.R", "config.yml", "rhino.yml", "dependencies.R")

stopifnot(file.exists("rhino.yml"))
missing <- c(PUBLIC_FILES[!file.exists(file.path("data/public", PUBLIC_FILES))],
             SHARED_FILES[!file.exists(file.path("data/processed", SHARED_FILES))])
if (length(missing) > 0) {
  stop("Missing data artifacts: ", paste(missing, collapse = ", "),
       " (run scripts/build_public_cells.R for data/public/)")
}

stage <- file.path(tempfile("wbes-shinyapps-"), app_name)
dir.create(file.path(stage, "data", "processed"), recursive = TRUE)
file.copy(APP_FILES, stage)
file.copy("app", stage, recursive = TRUE)
# The public set goes where the app looks for its tables.
file.copy(file.path("data/public", PUBLIC_FILES), file.path(stage, "data", "processed"))
file.copy(file.path("data/processed", SHARED_FILES), file.path(stage, "data", "processed"))
writeLines("WBES_DATA_MODE=aggregated", file.path(stage, ".Renviron"))

# Refuse to publish anything firm-level.
bundle <- list.files(stage, recursive = TRUE, all.files = TRUE)
forbidden <- grep("(^|/)(processed\\.parquet|assets\\.zip)$|\\.dta$", bundle, value = TRUE)
if (length(forbidden) > 0) {
  stop("Refusing to deploy firm-level data: ", paste(forbidden, collapse = ", "))
}
size_mb <- sum(file.size(file.path(stage, bundle))) / 1024^2
cat(sprintf("Bundle: %d files, %.1f MB -> %s/%s\n", length(bundle), size_mb, account, app_name))

rsconnect::deployApp(
  appDir = stage,
  appName = app_name,
  appTitle = "Business Environment Benchmarking",
  account = account,
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  lint = FALSE
)
