# app/logic/wb_integration/wb_cache.R
# World Bank Data Caching System
# Caches WB API responses locally with configurable refresh intervals

box::use(
  logger[log_info, log_warn],
  here[here]
)

#' Get path to WB cache directory
#'
#' @return Path to cache directory
get_wb_cache_dir <- function() {
  cache_dir <- here("data", "wb_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(cache_dir)
}

#' Check if cached WB data exists and is fresh
#'
#' @param cache_name Name of the cached data (e.g., "income_classifications")
#' @param max_age_hours Maximum age in hours before cache is considered stale (default: 168 = 1 week)
#' @return TRUE if cache exists and is fresh, FALSE otherwise
#' @export
is_wb_cache_fresh <- function(cache_name, max_age_hours = 168) {
  cache_file <- file.path(get_wb_cache_dir(), paste0(cache_name, ".rds"))

  if (!file.exists(cache_file)) {
    return(FALSE)
  }

  cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")

  if (cache_age > max_age_hours) {
    log_info(sprintf("Cache '%s' is stale (%.1f hours old, max: %d hours)", cache_name, cache_age, max_age_hours))
    return(FALSE)
  }

  log_info(sprintf("Cache '%s' is fresh (%.1f hours old)", cache_name, cache_age))
  return(TRUE)
}

#' Read cached WB data
#'
#' @param cache_name Name of the cached data
#' @return Cached data or NULL if not found
#' @export
read_wb_cache <- function(cache_name) {
  cache_file <- file.path(get_wb_cache_dir(), paste0(cache_name, ".rds"))

  if (!file.exists(cache_file)) {
    log_warn(sprintf("Cache '%s' not found", cache_name))
    return(NULL)
  }

  tryCatch({
    data <- readRDS(cache_file)
    log_info(sprintf("Loaded WB data from cache: %s", cache_name))
    return(data)
  }, error = function(e) {
    log_warn(sprintf("Failed to read cache '%s': %s", cache_name, e$message))
    return(NULL)
  })
}

#' Write WB data to cache
#'
#' @param cache_name Name to give the cached data
#' @param data Data to cache
#' @export
write_wb_cache <- function(cache_name, data) {
  cache_file <- file.path(get_wb_cache_dir(), paste0(cache_name, ".rds"))

  tryCatch({
    saveRDS(data, cache_file)
    log_info(sprintf("Cached WB data: %s", cache_name))
  }, error = function(e) {
    log_warn(sprintf("Failed to write cache '%s': %s", cache_name, e$message))
  })
}

#' Get cached or fetch WB data with automatic caching
#'
#' Generic function that checks cache first, then fetches if needed
#'
#' @param cache_name Name for the cache
#' @param fetch_function Function to call if cache is stale/missing
#' @param max_age_hours Maximum cache age in hours (default: 168 = 1 week)
#' @return Data from cache or fresh fetch
#' @export
get_or_fetch_wb_data <- function(cache_name, fetch_function, max_age_hours = 168) {
  # Check if cache is fresh
  if (is_wb_cache_fresh(cache_name, max_age_hours)) {
    cached_data <- read_wb_cache(cache_name)
    if (!is.null(cached_data)) {
      return(cached_data)
    }
  }

  # Cache is stale or missing, fetch fresh data
  log_info(sprintf("Fetching fresh WB data: %s", cache_name))
  fresh_data <- fetch_function()

  if (!is.null(fresh_data)) {
    write_wb_cache(cache_name, fresh_data)
  }

  return(fresh_data)
}

#' Clear all WB cache files
#'
#' @export
clear_wb_cache <- function() {
  cache_dir <- get_wb_cache_dir()
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(cache_files) > 0) {
    file.remove(cache_files)
    log_info(sprintf("Cleared %d WB cache files", length(cache_files)))
  } else {
    log_info("No WB cache files to clear")
  }
}

#' Get cache status information
#'
#' @return Data frame with cache file information
#' @export
get_wb_cache_status <- function() {
  cache_dir <- get_wb_cache_dir()
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(cache_files) == 0) {
    return(data.frame(
      cache_name = character(),
      file_size_mb = numeric(),
      age_hours = numeric(),
      last_modified = character()
    ))
  }

  cache_info <- data.frame(
    cache_name = basename(cache_files) |> sub("\\.rds$", "", x = _),
    file_size_mb = file.size(cache_files) / (1024^2),
    age_hours = sapply(cache_files, function(f) {
      as.numeric(difftime(Sys.time(), file.mtime(f), units = "hours"))
    }),
    last_modified = sapply(cache_files, function(f) {
      format(file.mtime(f), "%Y-%m-%d %H:%M:%S")
    })
  )

  return(cache_info)
}
