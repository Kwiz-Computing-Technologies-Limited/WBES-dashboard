# app/logic/parquet_cache.R
# Parquet Cache Management for WBES Data
# Provides efficient data storage using Apache Arrow/Parquet format

box::use(
  arrow[write_parquet, read_parquet, schema],
  logger[log_info, log_warn, log_error],
  here[here],
  utils[unzip, zip]
)

#' Get path to Parquet cache directory
#'
#' @param data_path Base data directory
#' @return Path to cache directory
get_parquet_cache_dir <- function(data_path = here("data")) {
  cache_dir <- file.path(data_path, "wbes_processed_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(cache_dir)
}

#' Check if Parquet cache exists and is fresh
#'
#' @param data_path Base data directory
#' @param cache_hours Maximum age in hours
#' @return TRUE if cache exists and is fresh
#' @export
is_parquet_cache_fresh <- function(data_path = here("data"), cache_hours = 24) {
  metadata_file <- file.path(data_path, "processed_wbes_metadata.rds")

  if (!file.exists(metadata_file)) {
    return(FALSE)
  }

  cache_age <- difftime(Sys.time(), file.mtime(metadata_file), units = "hours")

  if (cache_age > cache_hours) {
    log_info(sprintf("Parquet cache is stale (%.1f hours old, max: %d hours)", cache_age, cache_hours))
    return(FALSE)
  }

  log_info(sprintf("Parquet cache is fresh (%.1f hours old)", cache_age))
  return(TRUE)
}

#' Save WBES data to Parquet format
#'
#' Splits data into:
#' - Parquet files for tabular data (in wbes_processed_cache/)
#' - RDS file for metadata and non-tabular data
#'
#' @param wbes_data List containing WBES data
#' @param data_path Base data directory
#' @export
save_wbes_parquet <- function(wbes_data, data_path = here("data")) {
  log_info("Saving WBES data to Parquet format...")

  cache_dir <- get_parquet_cache_dir(data_path)

  # Tables to save as Parquet
  parquet_tables <- c(
    "raw",
    "processed",
    "latest",
    "country_panel",
    "country_sector",
    "country_size",
    "country_region",
    "country_coordinates"
  )

  # Save each table as Parquet
  for (table_name in parquet_tables) {
    if (!is.null(wbes_data[[table_name]])) {
      parquet_file <- file.path(cache_dir, paste0(table_name, ".parquet"))

      tryCatch({
        write_parquet(wbes_data[[table_name]], parquet_file, compression = "zstd")
        file_size_mb <- file.size(parquet_file) / (1024^2)
        log_info(sprintf("Saved %s.parquet (%.2f MB)", table_name, file_size_mb))
      }, error = function(e) {
        log_error(sprintf("Failed to save %s.parquet: %s", table_name, e$message))
      })
    }
  }

  # Metadata and non-tabular data
  metadata <- list(
    countries = wbes_data$countries,
    country_codes = wbes_data$country_codes,
    years = wbes_data$years,
    regions = wbes_data$regions,
    sectors = wbes_data$sectors,
    column_labels = wbes_data$column_labels,
    label_mapping = wbes_data$label_mapping,
    metadata = wbes_data$metadata,
    quality = wbes_data$quality,
    cached_at = Sys.time(),
    cache_version = "1.0.0"
  )

  # Save metadata as RDS
  metadata_file <- file.path(data_path, "processed_wbes_metadata.rds")
  tryCatch({
    saveRDS(metadata, metadata_file, compress = "xz")
    log_info("Saved processed_wbes_metadata.rds")
  }, error = function(e) {
    log_error(sprintf("Failed to save metadata: %s", e$message))
  })

  # Explicit memory cleanup
  gc()

  log_info("Parquet cache saved successfully")
}

#' Load WBES data from Parquet format
#'
#' @param data_path Base data directory
#' @return List containing WBES data with Arrow tables
#' @export
load_wbes_parquet <- function(data_path = here("data")) {
  log_info("Loading WBES data from Parquet cache...")

  cache_dir <- get_parquet_cache_dir(data_path)
  metadata_file <- file.path(data_path, "processed_wbes_metadata.rds")

  # Check if cache exists
  if (!file.exists(metadata_file)) {
    log_warn("Parquet cache metadata not found")
    return(NULL)
  }

  # Load metadata
  metadata <- tryCatch({
    readRDS(metadata_file)
  }, error = function(e) {
    log_error(sprintf("Failed to load metadata: %s", e$message))
    return(NULL)
  })

  if (is.null(metadata)) {
    return(NULL)
  }

  # Tables to load as Arrow tables
  parquet_tables <- c(
    "raw",
    "processed",
    "latest",
    "country_panel",
    "country_sector",
    "country_size",
    "country_region",
    "country_coordinates"
  )

  result <- metadata

  # Load each Parquet file as Arrow table
  for (table_name in parquet_tables) {
    parquet_file <- file.path(cache_dir, paste0(table_name, ".parquet"))

    if (file.exists(parquet_file)) {
      tryCatch({
        # Read as Arrow table (lazy, memory-efficient)
        result[[table_name]] <- read_parquet(parquet_file, as_data_frame = FALSE)
        log_info(sprintf("Loaded %s.parquet as Arrow table", table_name))
      }, error = function(e) {
        log_error(sprintf("Failed to load %s.parquet: %s", table_name, e$message))
        result[[table_name]] <- NULL
      })
    } else {
      log_warn(sprintf("Parquet file not found: %s.parquet", table_name))
      result[[table_name]] <- NULL
    }
  }

  # Explicit memory cleanup
  gc()

  log_info("Parquet cache loaded successfully")
  return(result)
}

#' Convert Arrow table to data frame when needed
#'
#' Helper function to convert Arrow tables to tibbles only when necessary
#'
#' @param arrow_table Arrow table or data frame
#' @return Data frame/tibble
#' @export
to_df <- function(arrow_table) {
  if (inherits(arrow_table, "ArrowObject")) {
    return(as.data.frame(arrow_table))
  }
  return(arrow_table)
}

#' Clear Parquet cache
#'
#' @param data_path Base data directory
#' @export
clear_parquet_cache <- function(data_path = here("data")) {
  cache_dir <- get_parquet_cache_dir(data_path)
  metadata_file <- file.path(data_path, "processed_wbes_metadata.rds")

  # Remove all Parquet files
  parquet_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(parquet_files) > 0) {
    file.remove(parquet_files)
    log_info(sprintf("Removed %d Parquet files", length(parquet_files)))
  }

  # Remove metadata
  if (file.exists(metadata_file)) {
    file.remove(metadata_file)
    log_info("Removed metadata file")
  }

  # Remove cache directory
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    log_info("Removed cache directory")
  }

  gc()
}

#' Get cache status information
#'
#' @param data_path Base data directory
#' @return Data frame with cache file information
#' @export
get_parquet_cache_status <- function(data_path = here("data")) {
  cache_dir <- get_parquet_cache_dir(data_path)
  metadata_file <- file.path(data_path, "processed_wbes_metadata.rds")

  if (!file.exists(metadata_file)) {
    return(data.frame(
      file = character(),
      size_mb = numeric(),
      age_hours = numeric(),
      last_modified = character()
    ))
  }

  # Get all Parquet files
  parquet_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
  all_files <- c(metadata_file, parquet_files)

  cache_info <- data.frame(
    file = basename(all_files),
    size_mb = file.size(all_files) / (1024^2),
    age_hours = sapply(all_files, function(f) {
      as.numeric(difftime(Sys.time(), file.mtime(f), units = "hours"))
    }),
    last_modified = sapply(all_files, function(f) {
      format(file.mtime(f), "%Y-%m-%d %H:%M:%S")
    })
  )

  return(cache_info)
}
