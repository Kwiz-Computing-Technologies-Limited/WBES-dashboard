# app/logic/shared_filters.R
# Shared Filter State Management
# Provides unified filtering logic across all modules

box::use(
  shiny[reactive, reactiveVal, observeEvent],
  dplyr[filter, select, contains, group_by, ungroup],
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
#' @param custom_sectors List of custom sectors
#' @param filter_by_region_fn Function to apply region filtering (from custom_regions module)
#' @param filter_by_sector_fn Function to apply sector filtering (from custom_sectors module)
#' @return Filtered data frame
#' @export
apply_common_filters <- function(data,
                                  region_value = "all",
                                  sector_value = "all",
                                  firm_size_value = "all",
                                  income_value = "all",
                                  year_value = "all",
                                  custom_regions = NULL,
                                  custom_sectors = NULL,
                                  filter_by_region_fn = NULL,
                                  filter_by_sector_fn = NULL) {
  if (is.null(data)) return(NULL)

 # Helper to check if filter should be applied (handles NULL, NA, vectors)
  should_filter <- function(value) {
    if (is.null(value)) return(FALSE)
    if (length(value) == 0) return(FALSE)
    if (length(value) == 1 && (is.na(value) || value == "all" || value == "")) return(FALSE)
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

  # Apply sector filter (using custom_sectors if available)
  if (should_filter(sector_value) && "sector" %in% names(data)) {
    if (!is.null(filter_by_sector_fn)) {
      # Use custom sector filtering function (handles "custom:" prefix)
      data <- filter_by_sector_fn(data, sector_value, custom_sectors)
    } else if (grepl("^custom:", sector_value)) {
      # Handle custom sector inline if no function provided
      sector_name <- sub("^custom:", "", sector_value)
      if (!is.null(custom_sectors) && !is.null(custom_sectors[[sector_name]])) {
        custom_sector <- custom_sectors[[sector_name]]
        data <- data |> filter(!is.na(sector) & sector %in% custom_sector$sectors)
      }
    } else {
      # Standard sector filter
      data <- data |> filter(!is.na(sector) & sector %in% sector_value)
    }
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
    # Check if "latest" is selected (and ignore other values like "all")
    if ("latest" %in% year_value) {
      # Get latest year per country
      if ("country" %in% names(data)) {
        data <- data |>
          group_by(country) |>
          filter(year == max(year, na.rm = TRUE)) |>
          ungroup()
      }
    } else {
      # Filter out "all" from year_value before converting
      year_value_clean <- year_value[year_value != "all"]

      # Convert to numeric for comparison
      year_nums <- as.numeric(year_value_clean)
      year_nums <- year_nums[!is.na(year_nums)]

      if (length(year_nums) > 0) {
        data <- data |> filter(!is.na(year) & year %in% year_nums)
      }
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
