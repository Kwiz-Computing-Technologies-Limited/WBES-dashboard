# app/logic/shared_filters.R
# Shared Filter State Management
# Provides unified filtering logic across all modules

box::use(
  shiny[reactive, reactiveVal, observeEvent],
  dplyr[filter, select, contains],
  stats[na.omit]
)

#' Remove columns that are entirely NA
#' @param data Data frame
#' @return Data frame with NA columns removed
#' @export
remove_na_columns <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)

  # Identify columns that are entirely NA or have no useful data
  cols_to_keep <- sapply(names(data), function(col) {
    values <- data[[col]]
    # Keep if at least 1% of values are non-NA
    sum(!is.na(values)) > (nrow(data) * 0.01)
  })

  data[, cols_to_keep, drop = FALSE]
}

#' Get valid choices for a filter dimension, excluding NA values
#' @param data Data frame
#' @param column Column name
#' @param add_all Whether to add "All" option
#' @param all_label Label for "All" option
#' @return Named character vector of choices
#' @export
get_filter_choices <- function(data, column, add_all = TRUE, all_label = "All") {
  if (is.null(data) || !column %in% names(data)) {
    if (add_all) {
      return(stats::setNames("all", all_label))
    } else {
      return(character(0))
    }
  }

  # Get unique non-NA values
  values <- data[[column]] |>
    unique() |>
    na.omit() |>
    as.character() |>
    sort()

  if (length(values) == 0) {
    if (add_all) {
      return(stats::setNames("all", all_label))
    } else {
      return(character(0))
    }
  }

  # Create named vector for selectInput
  choices <- stats::setNames(values, values)

  if (add_all) {
    choices <- c(stats::setNames("all", all_label), choices)
  }

  choices
}

#' Apply filters to data based on filter state
#' @param data Data frame
#' @param region_value Region filter value
#' @param sector_value Sector filter value
#' @param firm_size_value Firm size filter value
#' @param income_value Income filter value
#' @param year_value Year filter value (can be a vector)
#' @param custom_regions List of custom regions
#' @param filter_by_region_fn Function to apply region filtering (from custom_regions module)
#' @return Filtered data frame
#' @export
apply_common_filters <- function(data,
                                  region_value = "all",
                                  sector_value = "all",
                                  firm_size_value = "all",
                                  income_value = "all",
                                  year_value = "all",
                                  custom_regions = NULL,
                                  filter_by_region_fn = NULL) {
  if (is.null(data)) return(NULL)

 # Helper to check if filter should be applied (handles NULL, NA, vectors)
  should_filter <- function(value) {
    if (is.null(value)) return(FALSE)
    if (length(value) == 0) return(FALSE)
    if (length(value) == 1 && (is.na(value) || value == "all")) return(FALSE)
    # For vectors, check if it's not just "all"
    if (length(value) > 1) return(!all(value == "all"))
    return(TRUE)
  }

  # Apply region filter (using custom_regions if available)
  if (should_filter(region_value)) {
    if (!is.null(filter_by_region_fn)) {
      data <- filter_by_region_fn(data, region_value, custom_regions)
    } else if ("region" %in% names(data)) {
      data <- data |> filter(!is.na(region) & region %in% region_value)
    }
  }

  # Apply sector filter
  if (should_filter(sector_value) && "sector" %in% names(data)) {
    data <- data |> filter(!is.na(sector) & sector %in% sector_value)
  }

  # Apply firm size filter
  if (should_filter(firm_size_value) && "firm_size" %in% names(data)) {
    data <- data |> filter(!is.na(firm_size) & firm_size %in% firm_size_value)
  }

  # Apply income filter
  if (should_filter(income_value) && "income" %in% names(data)) {
    data <- data |> filter(!is.na(income) & income %in% income_value)
  }

  # Apply year filter
  if (should_filter(year_value) && "year" %in% names(data)) {
    # Convert to numeric for comparison
    year_nums <- as.numeric(year_value)
    year_nums <- year_nums[!is.na(year_nums)]
    if (length(year_nums) > 0) {
      data <- data |> filter(!is.na(year) & year %in% year_nums)
    }
  }

  data
}

#' Get available indicator columns from data
#' Excludes NA columns and returns only valid metric columns
#' @param data Data frame
#' @return Character vector of indicator column names
#' @export
get_available_indicators <- function(data) {
  if (is.null(data)) return(character(0))

  # Define metric columns to look for
  metric_patterns <- c(
    "power_outages", "outage_duration", "generator",
    "credit_line", "bank_account", "loan", "collateral",
    "bribery", "corruption", "capacity_utilization",
    "export", "female", "crime", "security", "workforce",
    "sales_growth", "obstacle", "IC\\.FRM\\."
  )

  # Get columns that match patterns and have data
  indicator_cols <- names(data)[grepl(paste(metric_patterns, collapse = "|"), names(data))]

  # Filter to only columns with sufficient non-NA data (>1%)
  valid_indicators <- indicator_cols[sapply(indicator_cols, function(col) {
    sum(!is.na(data[[col]])) > (nrow(data) * 0.01)
  })]

  valid_indicators
}

#' Create filter state reactive values
#' @return List of reactive values for filter state
#' @export
create_filter_state <- function() {
  list(
    region = reactiveVal("all"),
    sector = reactiveVal("all"),
    firm_size = reactiveVal("all"),
    income = reactiveVal("all"),
    year = reactiveVal("all"),
    custom_regions = reactiveVal(list()),
    active_tab = reactiveVal("overview")
  )
}
