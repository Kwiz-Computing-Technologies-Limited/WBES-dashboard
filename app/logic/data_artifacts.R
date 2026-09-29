# app/logic/data_artifacts.R
# Loads the small precomputed artifacts produced by scripts/build_data.R.
#
# This is the RUNTIME data entrypoint. It is called ONCE at app scope
# (app/main.R, outside the session server) so every session shares one copy and
# no .dta read / World Bank API call happens on the startup path.
#
# If the artifacts are absent (e.g. a fresh checkout that hasn't been built),
# load_app_data() falls back to the full ETL ONCE (still app scope, not per
# session) and logs a prominent warning telling you to run the build script.

box::use(
  arrow[read_parquet],
  here[here],
  logger[log_info, log_warn],
  stats[setNames],
  utils[download.file, URLencode],
  httr[GET, add_headers, write_disk, timeout, status_code, content]
)

ARTIFACT_SUBDIR <- "processed"

# Small AGGREGATE tables committed to the repo and read as plain data frames.
PARQUET_TABLES <- c(
  "latest", "country_panel", "country_sector", "country_size",
  "country_region", "regional", "country_coordinates"
)

# Firm-level microdata is kept OUT of the (public) repo. At runtime it is read
# from a local file if present (dev), otherwise downloaded once from this env var
# (e.g. a Firebase Storage / GCS / S3 tokenized download URL). If neither is
# available the app still runs on the aggregate tables, with reduced firm-level
# drill-down in the few modules that use it.
PROCESSED_URL_ENV <- "WBES_PROCESSED_URL"

# "aggregated" makes the app run on processed_cells.parquet only (public deploys).
DATA_MODE_ENV <- "WBES_DATA_MODE"

#' Expand aggregated cells back into one row per firm
#'
#' Cells come from scripts/build_public_cells.R: `<col>` is the mean over the
#' firms that answered and `<col>__n` how many did. Each cell becomes n_firms
#' rows in which the first `<col>__n` hold the mean and the rest are NA, so
#' unweighted means at cell granularity or coarser reproduce the firm-level
#' means exactly. Firm-level spread is not recoverable from this table.
#'
#' Every row keeps its cell's `cell_id`. That column is what marks the rows as
#' stand-ins rather than firms: it survives filtering and disappears as soon as
#' the rows are summarised, which is exactly when inference becomes legitimate
#' again (see stat_utils::is_cell_expanded()).
#'
#' @param cells Data frame read from processed_cells.parquet
#' @return Data frame with one row per (retained) firm
#' @export
expand_cells <- function(cells) {
  cells$cell_id <- seq_len(nrow(cells))
  count_cols <- grep("__n$", names(cells), value = TRUE)
  row_cell <- rep(seq_len(nrow(cells)), cells$n_firms)
  pos <- sequence(cells$n_firms)
  out <- cells[row_cell, setdiff(names(cells), c(count_cols, "n_firms")), drop = FALSE]
  for (cc in count_cols) {
    col <- sub("__n$", "", cc)
    out[[col]][pos > cells[[cc]][row_cell]] <- NA
  }
  rownames(out) <- NULL
  out
}

#' Download a gs://bucket/object using the runtime service-account identity
#'
#' Uses Application Default Credentials via the GCP metadata server (available on
#' Cloud Run / GCE). No key files, no public bucket. Returns TRUE on success.
#'
#' @param gs_url A gs://bucket/path/to/object URL
#' @param dest Local destination path
#' @return TRUE if the object was downloaded to `dest`
download_gcs_object <- function(gs_url, dest) {
  m <- regmatches(gs_url, regexec("^gs://([^/]+)/(.+)$", gs_url))[[1]]
  if (length(m) != 3) {
    log_warn("Malformed gs:// URL for processed.parquet")
    return(FALSE)
  }
  bucket <- m[2]
  object <- URLencode(m[3], reserved = TRUE)

  token <- tryCatch({
    resp <- GET(
      "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token",
      add_headers(`Metadata-Flavor` = "Google"), timeout(5)
    )
    if (status_code(resp) != 200) NULL else content(resp)$access_token
  }, error = function(e) NULL)
  if (is.null(token)) {
    log_warn("Could not obtain a GCS access token from the metadata server")
    return(FALSE)
  }

  api <- sprintf("https://storage.googleapis.com/storage/v1/b/%s/o/%s?alt=media", bucket, object)
  ok <- tryCatch({
    resp <- GET(api, add_headers(Authorization = paste("Bearer", token)),
                write_disk(dest, overwrite = TRUE), timeout(120))
    status_code(resp) == 200
  }, error = function(e) {
    log_warn(sprintf("GCS download failed: %s", e$message))
    FALSE
  })
  isTRUE(ok) && file.exists(dest) && file.size(dest) > 0
}

#' Resolve the firm-level processed.parquet to a local path (download if needed)
#'
#' Order: local file (dev / image-baked) -> WBES_PROCESSED_URL (gs:// via the
#' service account, or an https download URL) -> NULL.
#'
#' @param data_path Base data directory
#' @return Path to a readable processed.parquet, or NULL if unavailable
resolve_processed_path <- function(data_path = here("data")) {
  local_path <- file.path(artifact_dir(data_path), "processed.parquet")
  if (file.exists(local_path)) {
    return(local_path)
  }

  url <- Sys.getenv(PROCESSED_URL_ENV, unset = "")
  if (nzchar(url)) {
    dest <- file.path(tempdir(), "wbes_processed.parquet")
    if (file.exists(dest)) {
      return(dest)
    }
    log_info(sprintf("Fetching firm-level microdata from %s ...", PROCESSED_URL_ENV))
    ok <- if (grepl("^gs://", url)) {
      download_gcs_object(url, dest)
    } else {
      tryCatch({
        download.file(url, dest, mode = "wb", quiet = TRUE)
        file.exists(dest)
      }, error = function(e) {
        log_warn(sprintf("Download of processed.parquet failed: %s", e$message))
        FALSE
      })
    }
    if (isTRUE(ok) && file.exists(dest)) {
      return(dest)
    }
    log_warn("Remote fetch of processed.parquet failed; firm-level drill-downs unavailable.")
    return(NULL)
  }

  log_warn(paste0(
    "processed.parquet not found locally and ", PROCESSED_URL_ENV, " is unset; ",
    "firm-level drill-downs will be unavailable. Set ", PROCESSED_URL_ENV,
    " to a Firebase Storage / object-store download URL."
  ))
  NULL
}

#' Directory holding build artifacts
artifact_dir <- function(data_path = here("data")) {
  file.path(data_path, ARTIFACT_SUBDIR)
}

#' Are the precomputed artifacts present?
#'
#' @param data_path Base data directory
#' @return TRUE if the minimum artifact set exists
#' @export
artifacts_available <- function(data_path = here("data")) {
  dir <- artifact_dir(data_path)
  file.exists(file.path(dir, "meta.rds")) &&
    file.exists(file.path(dir, "latest.parquet"))
}

#' Is this app data the public aggregated stand-in for the firm table?
#'
#' Only an explicit "aggregated" counts. The full-ETL fallback and older data
#' lists carry real firm rows and may have no mode set at all.
#'
#' @param data The app data list
#' @return TRUE when `data$processed` is expanded cells, not firms
#' @export
is_aggregated <- function(data) {
  identical(data$firm_data_mode, "aggregated")
}

# The pooling stage from which each breakdown is withheld (build_public_cells.R).
WITHHELD_FROM_STAGE <- c(sector = 1L, firm_size = 2L, female_ownership = 3L)

#' How many firms a breakdown by `dimension` leaves out
#'
#' In the aggregated data, firms from groups too small to publish keep their
#' country and year but have their sector (then size, then ownership) blanked.
#' Filtering or grouping on that dimension drops them, so the figure shown is
#' for the firms that could be attributed. This counts the ones that could not.
#'
#' @param rows Expanded cell rows (the app's `processed` in aggregated mode)
#' @param dimension One of "sector", "firm_size", "female_ownership"
#' @return Number of rows withheld from that breakdown (0 for real firm rows)
#' @export
withheld_firms <- function(rows, dimension) {
  stage <- WITHHELD_FROM_STAGE[[dimension]]
  if (is.null(rows) || !"pooled" %in% names(rows)) return(0L)
  sum(rows$pooled >= stage, na.rm = TRUE)
}

#' A sentence saying how many firms a breakdown leaves out, or NULL if none
#'
#' @param rows Expanded cell rows in scope, before the breakdown's own filter
#' @param dimensions Breakdown dimensions in play ("sector", "firm_size",
#'   "female_ownership"); a firm is left out if any of them is withheld for it
#' @return A character scalar, or NULL when nothing is withheld
#' @export
withheld_note <- function(rows, dimensions) {
  dimensions <- intersect(dimensions, names(WITHHELD_FROM_STAGE))
  if (length(dimensions) == 0 || is.null(rows) || nrow(rows) == 0) return(NULL)
  # A lower stage withholds fewer attributes, so the most-detailed dimension decides.
  lowest <- names(which.min(WITHHELD_FROM_STAGE[dimensions]))
  n_out <- withheld_firms(rows, lowest)
  if (n_out == 0) return(NULL)
  labels <- c(sector = "sector", firm_size = "firm size", female_ownership = "ownership")
  by <- paste(labels[dimensions], collapse = " or ")
  sprintf(paste(
    "Public data: %s of %s firms here (%.1f%%) are in groups too small to publish",
    "by %s, so figures broken down by %s leave them out."),
    format(n_out, big.mark = ","), format(nrow(rows), big.mark = ","),
    100 * n_out / nrow(rows), by, by)
}

#' Load precomputed artifacts into the app's data structure
#'
#' Reconstructs the same list shape as wbes_data::load_wbes_data() (minus the
#' raw microdata, which no live module uses) plus a `wb_macro` element holding
#' the prefetched World Bank indicators.
#'
#' @param data_path Base data directory
#' @return List with WBES data components, or NULL if artifacts are missing
#' @export
load_precomputed <- function(data_path = here("data")) {
  dir <- artifact_dir(data_path)

  if (!artifacts_available(data_path)) {
    log_warn("Precomputed artifacts not found in data/processed/")
    return(NULL)
  }

  log_info("Loading precomputed artifacts from data/processed/ ...")

  result <- readRDS(file.path(dir, "meta.rds"))

  for (tbl in PARQUET_TABLES) {
    path <- file.path(dir, paste0(tbl, ".parquet"))
    if (file.exists(path)) {
      result[[tbl]] <- read_parquet(path, as_data_frame = TRUE)
      log_info(sprintf("  loaded %s (%d rows)", tbl, nrow(result[[tbl]])))
    } else {
      result[[tbl]] <- NULL
    }
  }

  # Firm-level microdata: local file (dev) or remote download (prod). A public
  # deploy (WBES_DATA_MODE=aggregated) never reads it and uses the aggregated
  # cells instead, which are also the fallback when the microdata is unavailable.
  result$firm_data_mode <- "none"
  aggregated_only <- identical(Sys.getenv(DATA_MODE_ENV), "aggregated")
  processed_path <- if (aggregated_only) NULL else resolve_processed_path(data_path)
  cells_path <- file.path(dir, "processed_cells.parquet")
  if (!is.null(processed_path)) {
    result$processed <- read_parquet(processed_path, as_data_frame = TRUE)
    result$firm_data_mode <- "firm"
    log_info(sprintf("  loaded processed (%d rows)", nrow(result$processed)))
  } else if (file.exists(cells_path)) {
    result$processed <- expand_cells(read_parquet(cells_path, as_data_frame = TRUE))
    result$firm_data_mode <- "aggregated"
    log_info(sprintf("  loaded processed from aggregated cells (%d rows)", nrow(result$processed)))
  } else {
    result$processed <- NULL
  }

  # World Bank macro prefetch (optional).
  wb_path <- file.path(dir, "wb_macro.rds")
  result$wb_macro <- if (file.exists(wb_path)) readRDS(wb_path) else NULL

  log_info("Precomputed artifacts loaded.")
  result
}

#' Load application data once at app scope
#'
#' Prefers precomputed artifacts; falls back to the full ETL exactly once if
#' they are missing. Either way returns the WBES data list plus `wb_macro`.
#'
#' @param data_path Base data directory
#' @return List with WBES data components and `wb_macro`
#' @export
load_app_data <- function(data_path = here("data")) {
  if (artifacts_available(data_path)) {
    return(load_precomputed(data_path))
  }

  log_warn(paste(
    "No precomputed artifacts found - falling back to the full ETL.",
    "Run `Rscript scripts/build_data.R` once to generate data/processed/",
    "for fast startup."
  ))

  # Lazy import so the heavy ETL chain is only pulled in on the fallback path.
  box::use(
    app/logic/wbes_data[load_wbes_data],
    app/logic/shared_filters[remove_na_columns]
  )

  data <- load_wbes_data(data_path = data_path, use_cache = TRUE)

  for (tbl in c("latest", "processed", "country_panel", "country_sector",
                "country_size", "country_region")) {
    if (!is.null(data[[tbl]])) data[[tbl]] <- remove_na_columns(data[[tbl]])
  }
  finish_etl_data(data)
}

#' Shape the full-ETL result into the runtime data contract
#'
#' @param data List returned by wbes_data::load_wbes_data()
#' @return The same list without runtime-irrelevant elements, and with
#'   `firm_data_mode` set: the ETL's processed table is real firm rows, so
#'   firm-level statistics stay available (it is not the aggregated stand-in)
#' @export
finish_etl_data <- function(data) {
  data$raw <- NULL          # never needed at runtime
  data$wb_macro <- NULL     # fallback path has no prefetch; live API is the backstop
  data$firm_data_mode <- if (is.null(data$processed)) "none" else "firm"
  data
}
