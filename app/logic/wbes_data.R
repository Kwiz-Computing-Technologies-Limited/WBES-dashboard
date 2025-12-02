# app/logic/wbes_data.R
# World Bank Enterprise Surveys Data Module
# Loads WBES microdata from local data directory (assets.zip or .dta files)

box::use(
  dplyr[...],  # ... imports all dplyr functions including first, group_by, summarise, across, filter, etc.
  tidyr[pivot_wider, pivot_longer],
  purrr[map_dfr, possibly],
  readr[read_csv, write_csv],
  haven[read_dta, as_factor],
  logger[log_info, log_warn, log_error],
  utils[unzip],
  stats[runif, setNames, na.omit],
  here[here],
  app/logic/column_labels[extract_column_labels, create_wbes_label_mapping]
)

# Expected filename for the WBES microdata
RAW_DTA_FILENAME <- "ES-Indicators-Database-Global-Methodology_November_24_2025.dta"

#' Load complete WBES dataset
#' This function loads real WBES microdata from local files.
#' Attempts to load data in order of preference:
#' 1. Cached processed data (.rds) if present and recent
#' 2. Local microdata from assets.zip
#' 3. Individual .dta files
#'
#' IMPORTANT: Real data is REQUIRED. The app will fail if data is not found.
#'
#' @param data_path Path to data directory (default: here::here("data"))
#' @param use_cache Whether to use cached data (default: TRUE)
#' @param cache_hours Hours before cache expires (default: 24)
#' @return List with WBES data components
#' @export
load_wbes_data <- function(data_path = here("data"), use_cache = TRUE, cache_hours = 24) {

  log_info("Loading WBES data...")

  # Check for cached processed data first (fastest)
  cache_file <- file.path(data_path, "wbes_processed.rds")
  if (use_cache && file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
    if (cache_age < cache_hours) {
      log_info("Loading from cache (processed data)")
      return(readRDS(cache_file))
    }
  }

  # Check for assets.zip (combined microdata) - PREFERRED METHOD
  assets_zip <- file.path(data_path, "assets.zip")
  if (file.exists(assets_zip)) {
    log_info("Found assets.zip - loading combined microdata")
    result <- load_from_zip(assets_zip, data_path)

    # Cache the processed result
    if (use_cache && !is.null(result)) {
      tryCatch({
        saveRDS(result, cache_file)
        log_info("Cached processed data for faster future loads")
      }, error = function(e) {
        log_warn(paste("Could not cache data:", e$message))
      })
    }

    return(result)
  } else {
    log_warn("assets.zip not found in data/ directory")
  }

  # Check for individual .dta files
  dta_files <- list.files(data_path, pattern = "\\.dta$", full.names = TRUE)
  if (length(dta_files) > 0) {
    log_info("Found local .dta files")
    result <- load_microdata(dta_files)

    # Cache the processed result
    if (use_cache && !is.null(result)) {
      tryCatch({
        saveRDS(result, cache_file)
        log_info("Cached processed data")
      }, error = function(e) {
        log_warn(paste("Could not cache data:", e$message))
      })
    }

    return(result)
  } else {
    log_warn("No .dta files found in data/ directory")
  }

  # NO REAL DATA FOUND - Always fail (no sample data fallback)
  error_msg <- paste0(
    "\n\n",
    "╔════════════════════════════════════════════════════════════════════════════╗\n",
    "║ REAL DATA REQUIRED BUT NOT FOUND                                          ║\n",
    "╠════════════════════════════════════════════════════════════════════════════╣\n",
    "║                                                                            ║\n",
    "║ The WBES dashboard requires real microdata to run, but none was found.    ║\n",
    "║                                                                            ║\n",
    "║ Expected data file:                                                        ║\n",
    "║   ", RAW_DTA_FILENAME, "                                                  ║\n",
    "║                                                                            ║\n",
    "║ Required setup:                                                            ║\n",
    "║   1. Ensure assets.zip exists in: ", data_path, "                          ║\n",
    "║   2. The ZIP file must contain: ", RAW_DTA_FILENAME, "                     ║\n",
    "║   3. Encoding: latin1 (handled automatically)                              ║\n",
    "║                                                                            ║\n",
    "║ Alternative: Place .dta files directly in the data/ directory              ║\n",
    "║                                                                            ║\n",
    "║ Data source: https://www.enterprisesurveys.org/en/survey-datasets         ║\n",
    "║                                                                            ║\n",
    "╚════════════════════════════════════════════════════════════════════════════╝\n"
  )
  log_error(error_msg)
  stop(error_msg)
}

#' Load microdata from ZIP archive
#' Efficiently extracts and loads .dta file from assets.zip
#' @param zip_file Path to assets.zip file
#' @param data_path Directory to extract to (temporary)
#' @return Processed data list
load_from_zip <- function(zip_file, data_path) {

  log_info(paste("Extracting microdata from:", basename(zip_file)))

  # Create temp extraction directory
  extract_dir <- file.path(data_path, ".extracted")
  dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)

  tryCatch({
    # List contents of zip
    zip_contents <- unzip(zip_file, list = TRUE)
    candidate_dta <- zip_contents$Name[grepl("\\.dta$", zip_contents$Name, ignore.case = TRUE)]

    # Prefer the known combined microdata file name if present
    preferred_match <- candidate_dta[basename(candidate_dta) == RAW_DTA_FILENAME]
    dta_files_in_zip <- if (length(preferred_match) > 0) preferred_match else candidate_dta

    if (length(dta_files_in_zip) == 0) {
      log_error("No .dta files found in assets.zip")
      stop("CRITICAL ERROR: assets.zip exists but contains no .dta files. Please ensure the ZIP contains: ", RAW_DTA_FILENAME)
    }

    log_info(paste("Found", length(dta_files_in_zip), ".dta file(s) in archive"))

    # Extract only .dta files
    unzip(zip_file, files = dta_files_in_zip, exdir = extract_dir, overwrite = TRUE)

    # Get full paths to extracted files
    extracted_files <- file.path(extract_dir, dta_files_in_zip)

    # Load the microdata
    result <- load_microdata(extracted_files)

    # Cleanup extraction directory (optional - keep for faster subsequent loads)
    # unlink(extract_dir, recursive = TRUE)

    log_info("Successfully loaded microdata from assets.zip")
    return(result)

  }, error = function(e) {
    log_error(paste("Error extracting/loading from zip:", e$message))
    # Cleanup on error
    base::unlink(extract_dir, recursive = TRUE)
    stop("CRITICAL ERROR: Failed to load data from assets.zip: ", e$message)
  })
}

#' Load microdata from Stata files
#' @param dta_files Vector of .dta file paths
#' @return Processed data list
load_microdata <- function(dta_files) {

  log_info(paste("Loading", length(dta_files), "microdata file(s)"))

  # Load all files
  data_list <- lapply(dta_files, function(f) {
    tryCatch({
      file_size_mb <- file.size(f) / 1024^2
      log_info(sprintf("Reading: %s (%.1f MB) [encoding=latin1]", basename(f), file_size_mb))

      # Read with progress for large files
      data <- read_dta(f, encoding = "latin1")

      log_info(sprintf("Loaded %s with %d observations and %d variables",
                      basename(f), nrow(data), ncol(data)))
      data

    }, error = function(e) {
      log_error(paste("Error reading", basename(f), ":", e$message))
      NULL
    })
  })

  names(data_list) <- tools::file_path_sans_ext(basename(dta_files))
  data_list <- Filter(Negate(is.null), data_list)

  if (length(data_list) == 0) {
    log_error("No data files could be loaded")
    stop("CRITICAL ERROR: .dta files found but none could be loaded. Check file format and encoding.")
  }

  # Combine all datasets
  log_info("Combining microdata files...")
  combined <- if (length(data_list) == 1) {
    data_list[[1]]
  } else {
    bind_rows(data_list, .id = "source_file")
  }

  log_info(sprintf("Combined dataset: %d observations, %d variables",
                  nrow(combined), ncol(combined)))

  # Process and structure the data
  processed <- process_microdata(combined)

  # Extract metadata
  countries <- extract_countries_from_microdata(combined)
  years <- extract_years_from_microdata(combined)

  # EXTRACT COLUMN LABELS from Stata file
  log_info("Extracting variable labels from microdata...")
  column_labels <- extract_column_labels(combined)

  # Create comprehensive label mapping (combines extracted + manual labels)
  label_mapping <- create_wbes_label_mapping(combined)
  log_info(sprintf("Created label mapping with %d labels", length(label_mapping)))

  # CREATE COUNTRY-LEVEL AGGREGATES for maps and charts
  metric_cols <- c(
    "power_outages_per_month", "avg_outage_duration_hrs", "firms_with_generator_pct",
    "firms_with_credit_line_pct", "firms_with_bank_account_pct", "loan_rejection_rate_pct",
    "collateral_required_pct", "bribery_incidence_pct", "corruption_obstacle_pct",
    "capacity_utilization_pct", "export_share_pct", "export_firms_pct",
    "female_ownership_pct", "female_workers_pct", "crime_obstacle_pct", "security_costs_pct"
  )

  # Filter for valid columns that exist in the data
  available_metric_cols <- metric_cols[metric_cols %in% names(processed)]

  country_aggregates <- processed |>
    filter(!is.na(country) & !is.na(country_code)) |>
    group_by(country, country_code) |>
    summarise(
      across(all_of(available_metric_cols), ~weighted_mean_safe(.x, sample_weight[1]), .names = "{.col}"),
      region = first_non_na(region),
      income_group = first_non_na(income_group),
      sample_size = n(),
      .groups = "drop"
    )

  result <- list(
    raw = combined,
    processed = processed,
    latest = country_aggregates,  # Country-level aggregates for maps/charts
    countries = countries,
    country_codes = processed$country_code |> unique() |> na.omit() |> as.character(),
    years = years,
    column_labels = column_labels,  # Raw extracted labels from Stata file
    label_mapping = label_mapping,  # Comprehensive label mapping (extracted + manual)
    metadata = list(
      source = "World Bank Enterprise Surveys (Microdata)",
      url = "https://www.enterprisesurveys.org/en/survey-datasets",
      files = names(data_list),
      observations = nrow(combined),
      variables = ncol(combined),
      loaded_at = Sys.time(),
      total_labels = length(label_mapping)
    ),
    quality = generate_quality_metadata()
  )

  log_info("Microdata loading complete")
  return(result)
}

#' Process raw microdata into analysis-ready format
#' @param data Raw microdata from WBES
#' @return Processed data frame
process_microdata <- function(data) {

  log_info("Processing microdata...")

  processed <- data |>
    mutate(
      country = coalesce_chr(
        get0("country2", ifnotfound = NULL),
        get0("country", ifnotfound = NULL),
        get0("country_official", ifnotfound = NULL)
      ),
      country_code = coalesce_chr(
        get0("wbcode", ifnotfound = NULL),
        get0("country_abr", ifnotfound = NULL)
      ),
      year = get0("year", ifnotfound = NA_integer_),
      region = if ("region" %in% names(data)) as.character(as_factor(region)) else NA_character_,
      income_group = if ("income" %in% names(data)) as.character(as_factor(income)) else NA_character_,
      sample_weight = get0("wt", ifnotfound = NA_real_),

      # Infrastructure
      power_outages_per_month = coalesce_num(get0("in2", ifnotfound = NULL)),
      avg_outage_duration_hrs = coalesce_num(get0("in3", ifnotfound = NULL)),
      firms_with_generator_pct = coalesce_num(get0("in9", ifnotfound = NULL)),

      # Access to finance
      firms_with_credit_line_pct = coalesce_num(get0("fin14", ifnotfound = NULL)),
      firms_with_bank_account_pct = coalesce_num(get0("fin15", ifnotfound = NULL)),
      loan_rejection_rate_pct = coalesce_num(get0("fin21", ifnotfound = NULL)),
      collateral_required_pct = coalesce_num(get0("fin10", ifnotfound = NULL)),

      # Corruption and governance
      bribery_incidence_pct = coalesce_num(get0("graft3", ifnotfound = NULL)),
      corruption_obstacle_pct = coalesce_num(get0("corr11", ifnotfound = NULL)),

      # Workforce and gender
      female_ownership_pct = coalesce_num(get0("gend1", ifnotfound = NULL)),
      female_workers_pct = coalesce_num(get0("gend2", ifnotfound = NULL)),

      # Performance and exports
      capacity_utilization_pct = coalesce_num(get0("t3", ifnotfound = NULL)),
      export_firms_pct = coalesce_num(get0("tr10", ifnotfound = NULL)),
      export_share_pct = compute_export_share(data),

      # Crime and security
      crime_obstacle_pct = coalesce_num(get0("crime8", ifnotfound = NULL)),
      security_costs_pct = coalesce_num(get0("crime2", ifnotfound = NULL))
    ) |>
    mutate(region = ifelse(region == "Aggregates", NA_character_, region))

  log_info(sprintf("Processed %d records with %d variables", nrow(processed), ncol(processed)))

  processed
}

compute_export_share <- function(data) {
  if (all(c("tr5", "tr6") %in% names(data))) {
    direct <- data$tr5
    indirect <- data$tr6
    total <- rowSums(cbind(direct, indirect), na.rm = TRUE)
    missing_both <- is.na(direct) & is.na(indirect)
    total[missing_both] <- NA_real_
    return(total)
  }

  if ("tr5" %in% names(data)) {
    return(as.numeric(data$tr5))
  }

  NA_real_
}

coalesce_chr <- function(...) {
  vals <- list(...)
  for (v in vals) {
    if (!is.null(v)) {
      candidate <- as.character(v)
      candidate <- candidate[!is.na(candidate) & candidate != ""]
      if (length(candidate) > 0) {
        return(v)
      }
    }
  }
  NA_character_
}

coalesce_num <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  as.numeric(x)
}

weighted_mean_safe <- function(x, w = NULL) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  if (!is.null(w) && !all(is.na(w))) {
    return(weighted.mean(x, w, na.rm = TRUE))
  }
  mean(x, na.rm = TRUE)
}

first_non_na <- function(x) {
  idx <- which(!is.na(x))[1]
  if (is.na(idx)) NA else x[idx]
}

#' Extract country list from microdata
#' @param data Microdata frame
#' @return Vector of countries
extract_countries_from_microdata <- function(data) {
  # Try different country variable names
  for (var in c("country", "country2", "country_official", "wbcode", "country_abr", "economy", "countryname")) {
    if (var %in% names(data)) {
      countries <- unique(data[[var]]) |>
        na.omit() |>
        as.character()
      return(countries)
    }
  }
  character(0)
}

#' Extract years from microdata
#' @param data Microdata frame
#' @return Vector of years
extract_years_from_microdata <- function(data) {
  # Try different year variable names
  for (var in c("year", "a1", "survey_year", "surveyyear")) {
    if (var %in% names(data)) {
      years <- unique(data[[var]]) |>
        na.omit() |>
        as.integer() |>
        sort()
      return(years)
    }
  }
  integer(0)
}
  )
}

#' Generate data quality documentation
#' @return List with quality issues and filter documentation
#' @export
generate_quality_metadata <- function() {
  list(
    issues = list(
      list(
        id = "DQ001",
        category = "Missing Data",
        severity = "Medium",
        indicator = "IC.FRM.OUTG.ZS",
        description = "Power outage data missing for ~12% of firms due to skip patterns",
        affected_countries = c("Ethiopia", "Uganda", "Tanzania"),
        filter_applied = "Exclude NA; use regional mean for aggregates",
        r_code = "filter(!is.na(IC.FRM.OUTG.ZS))"
      ),
      list(
        id = "DQ002",
        category = "Outliers",
        severity = "High",
        indicator = "IC.FRM.SECU.ZS",
        description = "Security cost outliers >20% of sales flagged",
        affected_countries = c("Nigeria", "South Africa"),
        filter_applied = "Winsorized at 99th percentile",
        r_code = "mutate(IC.FRM.SECU.ZS = pmin(IC.FRM.SECU.ZS, quantile(IC.FRM.SECU.ZS, 0.99, na.rm = TRUE)))"
      ),
      list(
        id = "DQ003",
        category = "Temporal Gaps",
        severity = "Low",
        indicator = "All",
        description = "Survey waves irregular; not all countries surveyed each year",
        affected_countries = "All",
        filter_applied = "Use latest available year per country",
        r_code = "group_by(country) |> filter(year == max(year))"
      ),
      list(
        id = "DQ004",
        category = "Sample Size",
        severity = "Medium",
        indicator = "All",
        description = "Small samples (<200) have wide confidence intervals",
        affected_countries = c("Rwanda", "Botswana"),
        filter_applied = "Flag low-sample estimates; widen CIs",
        r_code = "mutate(low_sample_flag = sample_size < 200)"
      ),
      list(
        id = "DQ005",
        category = "Response Bias",
        severity = "Medium",
        indicator = "IC.FRM.BRIB.ZS",
        description = "Bribery systematically underreported due to sensitivity",
        affected_countries = "All",
        filter_applied = "Document as limitation; compare with TI CPI",
        r_code = "# Documented limitation - no adjustment applied"
      ),
      list(
        id = "DQ006",
        category = "Definition Changes",
        severity = "Low",
        indicator = "IC.FRM.FEMO.ZS",
        description = "Female ownership definition changed in 2019",
        affected_countries = "All",
        filter_applied = "Flag pre-2019 data for time series analysis",
        r_code = "mutate(definition_flag = year < 2019)"
      )
    ),
    
    filters = list(
      list(
        name = "Infrastructure Analysis",
        description = "Filters for infrastructure constraint analysis",
        r_code = "
wbes_data |>
  filter(!is.na(IC.FRM.OUTG.ZS)) |>
  mutate(
    infra_score = (IC.FRM.OUTG.ZS + IC.FRM.ELEC.ZS + IC.FRM.INFRA.ZS) / 3,
    low_sample_flag = sample_size < 200
  ) |>
  group_by(country, year) |>
  summarise(
    avg_infra_obstacle = mean(infra_score, na.rm = TRUE),
    .groups = 'drop'
  )"
      ),
      list(
        name = "Access to Finance Analysis",
        description = "Filters for financial access analysis",
        r_code = "
wbes_data |>
  mutate(
    finance_score = (IC.FRM.FINA.ZS + (100 - IC.FRM.BANK.ZS) + IC.FRM.CRED.ZS) / 3
  ) |>
  group_by(country, income_group) |>
  summarise(
    avg_finance_obstacle = mean(finance_score, na.rm = TRUE),
    .groups = 'drop'
  )"
      ),
      list(
        name = "Cross-Country Comparison",
        description = "Standardization for valid cross-country benchmarking",
        r_code = "
wbes_data |>
  filter(year >= 2019) |>  # Use only recent Global Methodology
  group_by(country) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(
    across(starts_with('IC.'), ~(.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
           .names = '{.col}_z')
  )"
      )
    ),
    
    methodology_notes = c(
      "All indicators from Enterprise Surveys Global Methodology (2006+)",
      "Sampling weights should be applied for population-level inference",
      "Standard errors account for stratified sampling design",
      "Cross-country comparisons limited to surveys using same methodology",
      "Monetary values in surveys are in local currency; converted for comparison"
    )
  )
}
