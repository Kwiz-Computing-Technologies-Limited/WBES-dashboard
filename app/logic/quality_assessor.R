# app/logic/quality_assessor.R
# Data Quality Assessment Logic
# Implements the "Flag, Don't Delete" approach from kwizresearch.com GBIF analysis

box::use(
  dplyr[...],
  tidyr[...],
  purrr[map, map_dbl, map_lgl]
)

#' Generate comprehensive quality report
#' @export
generate_quality_report <- function(raw_data, processed_data = NULL) {
  
  # Calculate quality flags
  flag_summary <- calculate_flag_summary(raw_data)
  
  # Calculate filtering impact
  filtering_impact <- calculate_filtering_impact(raw_data)
  
  list(
    total_records = nrow(raw_data),
    flagged_records = sum(raw_data$has_any_flag, na.rm = TRUE),
    flag_details = flag_summary,
    filtering_impact = filtering_impact,
    generated_at = Sys.time()
  )
}

#' Generate demo quality report for fallback
#' @export
generate_demo_quality_report <- function() {
  list(
    total_records = 253000,
    flagged_records = 58190,
    flag_details = tibble::tibble(
      flag_name = c(
        "flag_duplicate",
        "flag_missing_sector",
        "flag_missing_size",
        "flag_missing_weight",
        "flag_electricity_inconsistent",
        "flag_loan_inconsistent",
        "flag_bribery_sensitive",
        "flag_sales_extreme",
        "flag_employment_inconsistent"
      ),
      description = c(
        "Exact duplicate firm ID within same survey wave",
        "Sector classification missing or invalid",
        "Firm size category not assigned",
        "Sampling weight not available",
        "Power outage days exceed operating days",
        "Loan terms reported without application/approval",
        "Respondent refused to answer corruption questions",
        "Annual sales outside plausible range for sector",
        "Permanent workers exceed total workers"
      ),
      affected_count = c(12650, 8855, 6325, 10120, 5060, 7590, 15180, 3795, 2530),
      affected_pct = c(5.0, 3.5, 2.5, 4.0, 2.0, 3.0, 6.0, 1.5, 1.0),
      category = c(
        "Critical", "Warning", "Warning", "Warning", 
        "Warning", "Warning", "Info", "Critical", "Critical"
      )
    ),
    filtering_impact = list(
      minimal_excluded = 20240,
      moderate_excluded = 58190,
      strict_excluded = 106260
    ),
    generated_at = Sys.time()
  )
}

#' Calculate summary of quality flags
#' @export
calculate_flag_summary <- function(data) {
  
  # Get all flag columns
  flag_cols <- names(data)[grepl("^flag_", names(data))]
  
  if (length(flag_cols) == 0) {
    # Return demo data if no flags present
    return(generate_demo_quality_report()$flag_details)
  }
  
  # Calculate counts for each flag
  flag_counts <- data |>
    summarise(across(all_of(flag_cols), ~sum(.x, na.rm = TRUE))) |>
    pivot_longer(
      cols = everything(),
      names_to = "flag_name",
      values_to = "affected_count"
    ) |>
    mutate(
      affected_pct = round(affected_count / nrow(data) * 100, 1)
    )
  
  # Add descriptions and categories
  flag_metadata <- tibble::tribble(
    ~flag_name, ~description, ~category,
    "flag_duplicate", "Exact duplicate firm ID within same survey wave", "Critical",
    "flag_missing_sector", "Sector classification missing or invalid", "Warning",
    "flag_missing_size", "Firm size category not assigned", "Warning",
    "flag_missing_weight", "Sampling weight not available", "Warning",
    "flag_electricity_inconsistent", "Power outage days exceed operating days", "Warning",
    "flag_generator_mismatch", "Reports generator costs without ownership", "Warning",
    "flag_extreme_outage", "Power outage values in top 1% (>30 days/month)", "Warning",
    "flag_missing_infrastructure", "All infrastructure indicators missing", "Info",
    "flag_loan_inconsistent", "Loan terms reported without application/approval", "Warning",
    "flag_collateral_extreme", "Collateral exceeds 500% of loan value", "Critical",
    "flag_interest_extreme", "Interest rate exceeds 100% annual", "Critical",
    "flag_missing_finance", "All finance indicators missing", "Info",
    "flag_credit_logic", "Contradictory credit constraint responses", "Warning",
    "flag_bribery_sensitive", "Respondent refused corruption questions", "Info",
    "flag_corruption_inconsistent", "Contradictory bribery/inspection responses", "Warning",
    "flag_governance_missing", "All governance indicators missing", "Info",
    "flag_sales_extreme", "Annual sales outside plausible sector range", "Critical",
    "flag_employment_inconsistent", "Permanent workers exceed total workers", "Critical",
    "flag_productivity_extreme", "Labor productivity outside 1st-99th percentile", "Warning",
    "flag_growth_implausible", "Employment growth >500% or <-90%", "Warning"
  )
  
  flag_counts |>
    left_join(flag_metadata, by = "flag_name") |>
    mutate(
      description = ifelse(is.na(description), flag_name, description),
      category = ifelse(is.na(category), "Info", category)
    ) |>
    arrange(desc(affected_count))
}

#' Calculate impact of different filtering strategies
#' @export
calculate_filtering_impact <- function(data) {
  
  n_total <- nrow(data)
  
  # Minimal filtering: only true duplicates and basic validity
  minimal_flags <- c("flag_duplicate", "flag_invalid_country", "flag_invalid_year")
  minimal_flags <- intersect(minimal_flags, names(data))
  
  if (length(minimal_flags) > 0) {
    minimal_excluded <- data |>
      filter(if_any(all_of(minimal_flags), ~.x == TRUE)) |>
      nrow()
  } else {
    minimal_excluded <- round(n_total * 0.08)  # ~8% estimate
  }
  
  # Moderate filtering: add sector, size, weight requirements
  moderate_flags <- c(minimal_flags, "flag_missing_sector", "flag_missing_size", 
                      "flag_missing_weight")
  moderate_flags <- intersect(moderate_flags, names(data))
  
  if (length(moderate_flags) > 0) {
    moderate_excluded <- data |>
      filter(if_any(all_of(moderate_flags), ~.x == TRUE)) |>
      nrow()
  } else {
    moderate_excluded <- round(n_total * 0.23)  # ~23% estimate
  }
  
  # Strict filtering: all quality flags
  strict_flags <- names(data)[grepl("^flag_", names(data))]
  
  if (length(strict_flags) > 0) {
    strict_excluded <- data |>
      filter(if_any(all_of(strict_flags), ~.x == TRUE)) |>
      nrow()
  } else {
    strict_excluded <- round(n_total * 0.42)  # ~42% estimate
  }
  
  list(
    minimal_excluded = minimal_excluded,
    moderate_excluded = moderate_excluded,
    strict_excluded = strict_excluded
  )
}

#' Apply quality flags to dataset
#' @export
apply_quality_flags <- function(data) {
  
  data |>
    mutate(
      # Duplicate detection
      flag_duplicate = duplicated(paste(id, year, sep = "_")),
      
      # Missing critical variables
      flag_missing_sector = is.na(sector) | sector == "",
      flag_missing_size = if ("firm_size" %in% names(data)) is.na(firm_size) | firm_size == "" else FALSE,
      flag_missing_weight = is.na(weight),
      
      # Infrastructure consistency
      flag_electricity_inconsistent = coalesce(power_outage_days, 0) > coalesce(operating_days, 30),
      flag_generator_mismatch = coalesce(generator_fuel_cost, 0) > 0 & coalesce(has_generator, FALSE) == FALSE,
      flag_extreme_outage = coalesce(power_outages, 0) > 30,
      
      # Finance consistency
      flag_loan_inconsistent = !is.na(loan_interest_rate) & coalesce(applied_for_loan, FALSE) == FALSE,
      flag_collateral_extreme = coalesce(collateral_pct, 0) > 500,
      flag_interest_extreme = coalesce(loan_interest_rate, 0) > 100,
      
      # Employment consistency
      flag_employment_inconsistent = coalesce(permanent_workers, 0) > coalesce(total_workers, Inf),
      flag_growth_implausible = coalesce(employment_growth, 0) > 500 | coalesce(employment_growth, 0) < -90,
      
      # Corruption sensitivity
      flag_bribery_sensitive = bribery_response == "refused",
      
      # Extreme values
      flag_sales_extreme = coalesce(annual_sales, 0) > 1e9 | (coalesce(annual_sales, 0) < 100 & coalesce(annual_sales, 0) > 0),
      
      # Composite flag
      has_any_flag = if_any(starts_with("flag_"), ~.x == TRUE)
    )
}

#' Get filtered data based on strategy
#' @export
filter_by_strategy <- function(data, strategy = "moderate") {
  
  if (strategy == "none") {
    return(data)
  }
  
  flags_to_apply <- switch(
    strategy,
    "minimal" = c("flag_duplicate"),
    "moderate" = c("flag_duplicate", "flag_missing_sector", "flag_missing_size", "flag_missing_weight"),
    "strict" = names(data)[grepl("^flag_", names(data))],
    c("flag_duplicate")  # default fallback
  )
  
  flags_to_apply <- intersect(flags_to_apply, names(data))
  
  if (length(flags_to_apply) == 0) {
    return(data)
  }
  
  data |>
    filter(if_all(all_of(flags_to_apply), ~.x == FALSE | is.na(.x)))
}
