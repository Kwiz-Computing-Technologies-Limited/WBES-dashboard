# app/logic/data_loader.R
# Data Loading and Processing Module for WBES Dashboard

box::use(

  haven[read_dta],
  dplyr[...],
  tidyr[pivot_longer, pivot_wider, drop_na],
  stringr[str_to_title, str_trim],
  readr[read_csv],
  purrr[map_dfr],
  logger[log_info, log_warn, log_error]
)

#' Load WBES Data from Stata files
#' @param data_path Path to data directory
#' @return List containing processed datasets
#' @export
load_wbes_data <- function(data_path = "data/") {

  log_info("Loading WBES data...")


  # Check for data files
  dta_files <- list.files(data_path, pattern = "\\.dta$", full.names = TRUE)
  csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)


  if (length(dta_files) == 0 && length(csv_files) == 0) {
    log_warn("No data files found. Loading sample data.")
    return(load_sample_data())
  }

  # Load Stata files
  if (length(dta_files) > 0) {
    data_list <- lapply(dta_files, function(f) {
      tryCatch({
        log_info(paste("Loading:", basename(f)))
        read_dta(f, encoding = "latin1")
      }, error = function(e) {
        log_error(paste("Error loading", f, ":", e$message))
        NULL
      })
    })
    names(data_list) <- tools::file_path_sans_ext(basename(dta_files))
  } else {
    data_list <- lapply(csv_files, function(f) {
      tryCatch({
        read_csv(f, show_col_types = FALSE)
      }, error = function(e) {
        log_error(paste("Error loading", f, ":", e$message))
        NULL
      })
    })
    names(data_list) <- tools::file_path_sans_ext(basename(csv_files))
  }

  # Process and standardize
  processed <- process_wbes_data(data_list)

  log_info("Data loading complete.")
  return(processed)
}

#' Process raw WBES data into standardized format
#' @param data_list List of raw data frames
#' @return Processed data structure
process_wbes_data <- function(data_list) {
  # Apply data quality filters and standardization
  # This will vary based on actual data structure

  list(
    raw = data_list,
    indicators = extract_indicators(data_list),
    countries = extract_countries(data_list),
    metadata = extract_metadata(data_list)
  )
}

#' Extract standardized indicators
extract_indicators <- function(data_list) {
  # Key WBES indicator mappings
  indicator_map <- list(
    # Infrastructure
    power_outages = c("c6", "infrast1"),
    power_outage_hours = c("c7", "infrast2"),
    generator_use = c("c8", "infrast3"),
    water_insufficiency = c("c16", "infrast4"),


    # Access to Finance
    has_bank_account = c("k5", "fin1"),
    has_credit_line = c("k8", "fin2"),
    loan_application = c("k16", "fin3"),
    loan_rejected = c("k17", "fin4"),
    collateral_pct = c("k14", "fin5"),

    # Corruption & Governance
    bribery_incidence = c("j7a", "corr1"),
    informal_payment_pct = c("j7b", "corr2"),
    mgmt_time_regulations = c("j2", "corr3"),

    # Competition & Performance
    capacity_utilization = c("f1", "perf1"),
    export_pct = c("d3c", "perf2"),
    employment_growth = c("l1", "perf3"),
    sales_growth = c("d2", "perf4"),

    # Firm characteristics
    firm_size = c("a6a", "size1"),
    firm_age = c("b5", "age1"),
    female_owner = c("b4", "gender1"),
    foreign_ownership = c("b2b", "fdi1")
  )

  indicator_map
}

#' Extract country metadata
extract_countries <- function(data_list) {
  # Will be populated from actual data
  # Returns unique countries with ISO codes
  NULL
}

#' Extract survey metadata
extract_metadata <- function(data_list) {
  list(
    source = "World Bank Enterprise Surveys",
    url = "https://www.enterprisesurveys.org",
    methodology = "Stratified random sampling",
    coverage = "168 economies, 253,000+ firms"
  )
}

#' Load sample/demo data when real data unavailable
#' @return Sample dataset for demonstration
#' @export
load_sample_data <- function() {

  # Generate realistic sample data for demonstration
  set.seed(42)

  countries <- c(
    "Kenya", "Nigeria", "South Africa", "Ghana", "Ethiopia",
    "Tanzania", "Uganda", "Rwanda", "Senegal", "Cote d'Ivoire",
    "Egypt", "Morocco", "Tunisia", "Botswana", "Zambia",
    "India", "Bangladesh", "Vietnam", "Indonesia", "Philippines",
    "Brazil", "Mexico", "Colombia", "Peru", "Chile",
    "Poland", "Turkey", "Romania", "Bulgaria", "Serbia"
  )

  regions <- c(
    rep("Sub-Saharan Africa", 15),
    rep("South Asia", 2),
    rep("East Asia & Pacific", 3),
    rep("Latin America & Caribbean", 5),
    rep("Europe & Central Asia", 5)
  )

  income_groups <- c(
    "Lower middle income", "Lower middle income", "Upper middle income",
    "Lower middle income", "Low income", "Lower middle income",
    "Low income", "Low income", "Lower middle income", "Lower middle income",
    "Lower middle income", "Lower middle income", "Lower middle income",
    "Upper middle income", "Lower middle income",
    "Lower middle income", "Lower middle income", "Lower middle income",
    "Lower middle income", "Lower middle income",
    "Upper middle income", "Upper middle income", "Upper middle income",
    "Upper middle income", "High income",
    "High income", "Upper middle income", "Upper middle income",
    "Upper middle income", "Upper middle income"
  )

  years <- c(2019, 2020, 2021, 2022, 2023)

  # Generate country-level indicators
  country_data <- data.frame(
    country = rep(countries, each = length(years)),
    region = rep(regions, each = length(years)),
    income_group = rep(income_groups, each = length(years)),
    year = rep(years, length(countries)),
    stringsAsFactors = FALSE
  )

  n <- nrow(country_data)

  # Infrastructure indicators
  country_data$power_outages_per_month <- round(runif(n, 0.5, 15) +
    ifelse(country_data$region == "Sub-Saharan Africa", 5, 0), 1)
  country_data$avg_outage_duration_hrs <- round(runif(n, 0.5, 8) +
    ifelse(country_data$region == "Sub-Saharan Africa", 2, 0), 1)
  country_data$firms_with_generator_pct <- round(runif(n, 10, 70) +
    ifelse(country_data$region == "Sub-Saharan Africa", 15, 0), 1)
  country_data$water_insufficiency_pct <- round(runif(n, 5, 40), 1)

  # Access to Finance
  country_data$firms_with_bank_account_pct <- round(runif(n, 60, 98), 1)
  country_data$firms_with_credit_line_pct <- round(runif(n, 15, 55), 1)
  country_data$loan_rejection_rate_pct <- round(runif(n, 5, 35), 1)
  country_data$collateral_required_pct <- round(runif(n, 100, 300), 0)
  country_data$finance_obstacle_pct <- round(runif(n, 15, 50), 1)

  # Corruption & Governance
  country_data$bribery_incidence_pct <- round(runif(n, 5, 45), 1)
  country_data$informal_payments_pct_sales <- round(runif(n, 0.5, 5), 2)
  country_data$mgmt_time_regulations_pct <- round(runif(n, 5, 35), 1)
  country_data$days_to_get_license <- round(runif(n, 10, 120), 0)

  # Competition & Performance
  country_data$capacity_utilization_pct <- round(runif(n, 55, 85), 1)
  country_data$export_firms_pct <- round(runif(n, 5, 35), 1)
  country_data$annual_employment_growth_pct <- round(runif(n, -5, 15), 1)
  country_data$annual_sales_growth_pct <- round(runif(n, -10, 25), 1)

  # Firm demographics
  country_data$female_ownership_pct <- round(runif(n, 10, 45), 1)
  country_data$foreign_ownership_pct <- round(runif(n, 5, 30), 1)
  country_data$formal_training_pct <- round(runif(n, 15, 60), 1)

  # Sample sizes and weights
  country_data$sample_size <- round(runif(n, 150, 1500), 0)
  country_data$response_rate_pct <- round(runif(n, 45, 85), 1)

  # Data quality flags
  country_data$data_quality_score <- round(runif(n, 0.6, 1.0), 2)
  country_data$missing_rate_pct <- round(runif(n, 2, 25), 1)

  # Aggregate latest year data
  latest_data <- country_data[country_data$year == max(country_data$year), ]

  # Regional aggregates
  regional_data <- aggregate(
    cbind(
      power_outages_per_month, avg_outage_duration_hrs,
      firms_with_credit_line_pct, bribery_incidence_pct,
      capacity_utilization_pct, female_ownership_pct
    ) ~ region,
    data = latest_data,
    FUN = mean
  )

  list(
    country_panel = country_data,
    latest = latest_data,
    regional = regional_data,
    countries = countries,
    regions = unique(regions),
    years = years,
    metadata = list(
      source = "World Bank Enterprise Surveys (Sample Data)",
      url = "https://www.enterprisesurveys.org",
      note = "This is simulated data for demonstration purposes",
      generated = Sys.Date()
    ),
    quality = generate_quality_metadata()
  )
}

#' Generate data quality metadata
#' @return Data quality documentation
generate_quality_metadata <- function() {
  list(
    issues = list(
      list(
        id = "DQ001",
        category = "Missing Data",
        severity = "Medium",
        indicator = "power_outages_per_month",
        description = "Missing values in 12% of observations for power outage frequency",
        affected_countries = c("Ethiopia", "Uganda", "Tanzania"),
        filter_applied = "Records with NA excluded from aggregations",
        r_code = "filter(!is.na(power_outages_per_month))"
      ),
      list(
        id = "DQ002",
        category = "Outliers",
        severity = "High",
        indicator = "collateral_required_pct",
        description = "Extreme values (>500%) detected in collateral requirements",
        affected_countries = c("Nigeria", "Bangladesh"),
        filter_applied = "Winsorized at 99th percentile",
        r_code = "mutate(collateral_required_pct = pmin(collateral_required_pct, quantile(collateral_required_pct, 0.99, na.rm = TRUE)))"
      ),
      list(
        id = "DQ003",
        category = "Temporal Gaps",
        severity = "Low",
        indicator = "All indicators",
        description = "Survey waves not available for all years; latest available year used",
        affected_countries = c("Rwanda", "Senegal"),
        filter_applied = "Forward-fill from most recent survey",
        r_code = "fill(everything(), .direction = 'down')"
      ),
      list(
        id = "DQ004",
        category = "Sample Size",
        severity = "Medium",
        indicator = "All indicators",
        description = "Small sample sizes (<200 firms) reduce reliability of estimates",
        affected_countries = c("Botswana", "Rwanda"),
        filter_applied = "Confidence intervals widened; flagged in visualizations",
        r_code = "mutate(low_sample_flag = sample_size < 200)"
      ),
      list(
        id = "DQ005",
        category = "Response Bias",
        severity = "Medium",
        indicator = "bribery_incidence_pct",
        description = "Sensitive questions may have underreporting bias",
        affected_countries = "All countries",
        filter_applied = "None - documented as limitation",
        r_code = "# No filter; noted in methodology documentation"
      ),
      list(
        id = "DQ006",
        category = "Definition Changes",
        severity = "Low",
        indicator = "female_ownership_pct",
        description = "Definition of 'female ownership' changed in 2019 survey wave",
        affected_countries = "All countries",
        filter_applied = "Pre-2019 data flagged; use caution in time series",
        r_code = "mutate(definition_change_flag = year < 2019)"
      )
    ),

    filters = list(
      list(
        name = "Infrastructure Analysis",
        description = "Filters applied for infrastructure constraint analysis",
        logic = list(
          "1. Exclude firms with missing power outage data",
          "2. Winsorize extreme outage durations (>48 hours)",
          "3. Apply sampling weights for national representativeness",
          "4. Flag countries with sample size < 200"
        ),
        r_code = "
wbes_data |>
  filter(!is.na(power_outages_per_month)) |>
  mutate(
    avg_outage_duration_hrs = pmin(avg_outage_duration_hrs, 48),
    low_sample_flag = sample_size < 200
  ) |>
  group_by(country, year) |>
  summarise(
    weighted_outages = weighted.mean(power_outages_per_month, w = sample_weight),
    .groups = 'drop'
  )"
      ),
      list(
        name = "Access to Finance Analysis",
        description = "Filters applied for financial access analysis",
        logic = list(
          "1. Exclude firms with inconsistent responses (has loan but no bank account)",
          "2. Winsorize collateral requirements at 99th percentile",
          "3. Separate analysis for SMEs vs large firms",
          "4. Apply sector-specific adjustments"
        ),
        r_code = "
wbes_data |>
  filter(!(has_loan == 1 & has_bank_account == 0)) |>
  mutate(
    collateral_required_pct = pmin(
      collateral_required_pct,
      quantile(collateral_required_pct, 0.99, na.rm = TRUE)
    ),
    size_category = case_when(
      employees < 20 ~ 'Small',
      employees < 100 ~ 'Medium',
      TRUE ~ 'Large'
    )
  )"
      ),
      list(
        name = "Corruption Analysis",
        description = "Filters applied for governance/corruption analysis",
        logic = list(
          "1. Apply survey weights to account for stratification",
          "2. Flag responses with potential social desirability bias",
          "3. Exclude countries with <50% response rate on sensitive questions",
          "4. Use multiple imputation for partial non-response"
        ),
        r_code = "
wbes_data |>
  filter(corruption_response_rate >= 0.50) |>
  mutate(
    bribery_adjusted = case_when(
      response_time < 5 ~ bribery_incidence * 1.2,
      TRUE ~ bribery_incidence
    )
  ) |>
  group_by(country) |>
  summarise(
    bribery_pct = weighted.mean(bribery_adjusted, w = sample_weight),
    .groups = 'drop'
  )"
      ),
      list(
        name = "Cross-Country Comparison",
        description = "Standardization for valid cross-country benchmarking",
        logic = list(
          "1. Use only comparable survey years (2019-2023)",
          "2. Apply purchasing power parity adjustments where relevant",
          "3. Standardize firm size definitions across countries",
          "4. Weight by sector composition for like-with-like comparison"
        ),
        r_code = "
wbes_data |>
  filter(year >= 2019) |>
  mutate(
    size_standardized = case_when(
      employees < 20 ~ 'Small (5-19)',
      employees < 100 ~ 'Medium (20-99)',
      TRUE ~ 'Large (100+)'
    )
  ) |>
  group_by(country, size_standardized) |>
  summarise(across(where(is.numeric), ~weighted.mean(.x, w = sample_weight, na.rm = TRUE)))"
      )
    ),

    methodology_notes = list(
      "Sampling weights should be applied for all population-level estimates",
      "Standard errors account for complex survey design (stratification, clustering)",
      "Missing data handled via listwise deletion unless otherwise specified",
      "All monetary values converted to 2023 USD using PPP rates",
      "Firm size categorization follows World Bank SME definitions"
    )
  )
}

#' Calculate data completeness metrics
#' @param data Dataset to analyze
#' @return Completeness statistics
#' @export
calculate_completeness <- function(data) {
  if (is.null(data)) return(NULL)

  total_cells <- prod(dim(data))
  missing_cells <- sum(is.na(data))

  list(
    total_observations = nrow(data),
    total_variables = ncol(data),
    total_cells = total_cells,
    missing_cells = missing_cells,
    completeness_pct = round((1 - missing_cells/total_cells) * 100, 2),
    missing_by_column = sapply(data, function(x) sum(is.na(x))),
    complete_cases = sum(complete.cases(data))
  )
}
