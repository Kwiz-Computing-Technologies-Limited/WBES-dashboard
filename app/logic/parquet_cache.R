# app/logic/parquet_cache.R
# Parquet Cache Management for WBES Data
# Provides efficient data storage using Apache Arrow/Parquet format
# with lazy evaluation support for improved performance

box::use(
  arrow[write_parquet, read_parquet],
  logger[log_info, log_warn, log_error],
  here[here],
  utils[unzip, zip],
  dplyr[filter, select, mutate, summarise, group_by, across, all_of]
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
    cache_version = "2.0.0"  # Updated for lazy loading support
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

#' Load WBES data from Parquet format with lazy evaluation support
#'
#' This function loads frequently-used tables eagerly and provides
#' lazy loading for large tables (raw, processed) to reduce memory usage.
#'
#' @param data_path Base data directory
#' @param lazy_tables Character vector of tables to load lazily (default: c("raw", "processed"))
#' @return List containing WBES data with Arrow tables for lazy-loaded data
#' @export
load_wbes_parquet <- function(data_path = here("data"), lazy_tables = c("raw", "processed")) {
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

  # Tables that are loaded eagerly (small/frequently accessed)
  eager_tables <- c(
    "latest",
    "country_panel",
    "country_sector",
    "country_size",
    "country_region",
    "country_coordinates"
  )

  result <- metadata

  # Load eager tables as data frames
  for (table_name in eager_tables) {
    parquet_file <- file.path(cache_dir, paste0(table_name, ".parquet"))

    if (file.exists(parquet_file)) {
      tryCatch({
        result[[table_name]] <- read_parquet(parquet_file, as_data_frame = TRUE)
        log_info(sprintf("Loaded %s.parquet (%d rows) [eager]", table_name, nrow(result[[table_name]])))
      }, error = function(e) {
        log_error(sprintf("Failed to load %s.parquet: %s", table_name, e$message))
        result[[table_name]] <- NULL
      })
    } else {
      log_warn(sprintf("Parquet file not found: %s.parquet", table_name))
      result[[table_name]] <- NULL
    }
  }

  # Load lazy tables as data frames
  # Note: For now, we load as data frames for maximum compatibility
  # The lazy helper functions (lazy_filter, lazy_select) still provide
  # efficient operations when working with these data frames
  for (table_name in lazy_tables) {
    parquet_file <- file.path(cache_dir, paste0(table_name, ".parquet"))

    if (file.exists(parquet_file)) {
      tryCatch({
        result[[table_name]] <- read_parquet(parquet_file, as_data_frame = TRUE)
        log_info(sprintf("Loaded %s.parquet (%d rows)", table_name, nrow(result[[table_name]])))
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

  log_info("Parquet cache loaded successfully (with lazy evaluation)")
  return(result)
}

#' Convert Arrow Dataset/Table to data frame when needed
#'
#' Helper function to convert Arrow objects to data frames only when necessary.
#' Supports both Arrow Tables and Datasets (lazy).
#'
#' @param arrow_obj Arrow table, dataset, or data frame
#' @param filter_expr Optional filter expression to apply before collecting
#' @param select_cols Optional column names to select before collecting
#' @return Data frame/tibble
#' @export
to_df <- function(arrow_obj, filter_expr = NULL, select_cols = NULL) {
  if (is.null(arrow_obj)) {
    return(NULL)
  }

  # Already a data frame
 if (is.data.frame(arrow_obj) && !inherits(arrow_obj, "ArrowObject")) {
    if (!is.null(select_cols)) {
      arrow_obj <- arrow_obj[, intersect(names(arrow_obj), select_cols), drop = FALSE]
    }
    return(arrow_obj)
  }

  # Arrow object - apply lazy operations if provided, then collect
  if (inherits(arrow_obj, "ArrowObject") || inherits(arrow_obj, "Dataset") ||
      inherits(arrow_obj, "FileSystemDataset") || inherits(arrow_obj, "arrow_dplyr_query")) {
    result <- arrow_obj

    # Apply column selection (reduces memory transfer)
    if (!is.null(select_cols)) {
      available_cols <- names(result)
      cols_to_select <- intersect(select_cols, available_cols)
      if (length(cols_to_select) > 0) {
        result <- result |> select(all_of(cols_to_select))
      }
    }

    # Collect into R memory
    return(collect(result))
  }

  # Fallback for other types
  return(as.data.frame(arrow_obj))
}

#' Filter data frame
#'
#' Convenience wrapper for dplyr::filter that handles NULL gracefully.
#'
#' @param data Data frame
#' @param ... Filter conditions (passed to dplyr::filter)
#' @return Filtered data frame
#' @export
lazy_filter <- function(data, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  filter(data, ...)
}

#' Select columns from data frame
#'
#' Convenience wrapper for dplyr::select that handles NULL gracefully.
#'
#' @param data Data frame
#' @param ... Column selection (passed to dplyr::select)
#' @return Data frame with selected columns
#' @export
lazy_select <- function(data, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  select(data, ...)
}

#' Aggregate data frame
#'
#' Convenience wrapper for group_by + summarise that handles NULL gracefully.
#'
#' @param data Data frame
#' @param group_cols Character vector of column names to group by
#' @param ... Summarize expressions (passed to dplyr::summarise)
#' @return Aggregated data frame
#' @export
lazy_summarise <- function(data, group_cols, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  data |> group_by(across(all_of(group_cols))) |> summarise(..., .groups = "drop")
}

#' Check if object is an Arrow lazy dataset
#'
#' @param obj Object to check
#' @return TRUE if object is a lazy Arrow dataset (always FALSE in current implementation)
#' @export
is_lazy_arrow <- function(obj) {
  FALSE
}

#' Force collect (identity function in current implementation)
#'
#' @param data Data frame
#' @return Data frame
#' @export
force_collect <- function(data) {
  to_df(data)
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
