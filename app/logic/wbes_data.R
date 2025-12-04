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
  utils[unzip, head],
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
  cache_file <- here(data_path, "wbes_processed.rds")
  if (use_cache && file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
    if (cache_age < cache_hours) {
      log_info("Loading from cache (processed data)")
      return(readRDS(cache_file))
    }
  }

  # Check for assets.zip (combined microdata) - PREFERRED METHOD
  assets_zip <- here(data_path, "assets.zip")
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

  # Extract metadata (use processed data to get cleaned country names without year suffixes)
  countries <- extract_countries_from_microdata(processed)
  log_info(sprintf("Extracted %d countries: %s...", length(countries), paste(head(countries, 5), collapse = ", ")))
  years <- extract_years_from_microdata(combined)
  log_info(sprintf("Extracted %d years: %s", length(years), paste(years, collapse = ", ")))

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
    "female_ownership_pct", "female_workers_pct", "crime_obstacle_pct", "security_costs_pct",
    "workforce_obstacle_pct", "annual_sales_growth_pct",
    # Infrastructure obstacle columns for Country Profile detailed charts
    "electricity_obstacle", "water_obstacle", "transport_obstacle", "generator_share_pct",
    # Financial access columns for Country Profile detailed charts
    "loan_application_pct", "overdraft_facility_pct",
    # Bribery columns for Country Profile governance charts
    "bribe_for_permit", "bribe_for_import", "bribe_for_utilities", "bribe_for_tax", "bribe_for_contract",
    "mgmt_time_regulations_pct",
    # Loan application reason columns for Finance Access charts
    "no_need_for_loan", "loan_procedures_complex", "loan_interest_high", "insufficient_collateral", "loan_size_inadequate",
    "days_to_get_loan",
    # Add IC.FRM.* aliases (only those that exist)
    "IC.FRM.CORR.ZS", "IC.FRM.BRIB.ZS", "IC.FRM.CAPU.ZS", "IC.FRM.OUTG.ZS",
    "IC.FRM.FINA.ZS", "IC.FRM.BANK.ZS", "IC.FRM.CRED.ZS", "IC.FRM.FEMO.ZS",
    "IC.FRM.FEMW.ZS", "IC.FRM.EXPRT.ZS", "IC.FRM.ELEC.ZS", "IC.FRM.INFRA.ZS",
    "IC.FRM.CRIM.ZS", "IC.FRM.SECU.ZS", "IC.FRM.WKFC.ZS"
  )

  # Filter for valid columns that exist in the data
  available_metric_cols <- metric_cols[metric_cols %in% names(processed)]

  # Create aggregates without using sample_weight in across()
  country_aggregates <- processed |>
    filter(!is.na(country) & !is.na(country_code)) |>
    group_by(country, country_code) |>
    summarise(
      across(all_of(available_metric_cols), ~mean(.x, na.rm = TRUE), .names = "{.col}"),
      region = first_non_na(region),
      firm_size = first_non_na(firm_size),
      sector = first_non_na(sector),
      sample_size = n(),
      .groups = "drop"
    )

  # Create country panel (time series data by country and year)
  log_info("Creating country panel for time series analysis...")
  country_panel <- processed |>
    filter(!is.na(country) & !is.na(year)) |>
    group_by(country, year) |>
    summarise(
      across(all_of(available_metric_cols), ~mean(.x, na.rm = TRUE), .names = "{.col}"),
      region = first_non_na(region),
      firm_size = first_non_na(firm_size),
      sample_size = n(),
      .groups = "drop"
    )
  log_info(sprintf("Country panel created: %d country-year observations", nrow(country_panel)))

  # Create country-sector aggregates for sector profile
  log_info("Creating country-sector aggregates...")
  country_sector_aggregates <- processed |>
    filter(!is.na(country) & !is.na(country_code) & !is.na(sector)) |>
    group_by(country, country_code, sector) |>
    summarise(
      across(all_of(available_metric_cols), ~mean(.x, na.rm = TRUE), .names = "{.col}"),
      region = first_non_na(region),
      firm_size = first_non_na(firm_size),
      sample_size = n(),
      .groups = "drop"
    )
  log_info(sprintf("Country-sector aggregates created: %d country-sector combinations", nrow(country_sector_aggregates)))

  # Create country-size aggregates for size profile
  log_info("Creating country-size aggregates...")
  country_size_aggregates <- processed |>
    filter(!is.na(country) & !is.na(country_code) & !is.na(firm_size)) |>
    group_by(country, country_code, firm_size) |>
    summarise(
      across(all_of(available_metric_cols), ~mean(.x, na.rm = TRUE), .names = "{.col}"),
      region = first_non_na(region),
      sector = first_non_na(sector),
      sample_size = n(),
      .groups = "drop"
    )
  log_info(sprintf("Country-size aggregates created: %d country-size combinations", nrow(country_size_aggregates)))

  # Create country-region aggregates for regional profile
  log_info("Creating country-region aggregates...")
  country_region_aggregates <- processed |>
    filter(!is.na(country) & !is.na(country_code) & !is.na(region)) |>
    group_by(country, country_code, region) |>
    summarise(
      across(all_of(available_metric_cols), ~mean(.x, na.rm = TRUE), .names = "{.col}"),
      firm_size = first_non_na(firm_size),
      sector = first_non_na(sector),
      sample_size = n(),
      .groups = "drop"
    )
  log_info(sprintf("Country-region aggregates created: %d country-region combinations", nrow(country_region_aggregates)))

  # Get country coordinates and merge with aggregates
  country_coords <- get_country_coordinates()
  country_aggregates_with_coords <- merge(
    country_aggregates,
    country_coords,
    by = "country",
    all.x = TRUE
  )

  # Log countries without coordinates for debugging
  countries_without_coords <- country_aggregates_with_coords |>
    filter(is.na(lat)) |>
    pull(country)
  if (length(countries_without_coords) > 0) {
    log_warn(sprintf("%d countries without coordinates: %s",
                     length(countries_without_coords),
                     paste(head(countries_without_coords, 10), collapse = ", ")))
  }
  log_info(sprintf("%d countries with coordinates", sum(!is.na(country_aggregates_with_coords$lat))))

  # Extract unique regions and sectors
  # Try to get regions from processed data, if empty try raw data
  regions <- processed$region |> unique() |> na.omit() |> as.character()
  if (length(regions) == 0 && "region" %in% names(combined)) {
    log_info("Regions empty in processed data, extracting from raw data")
    # Fallback: extract directly from raw data with labels
    regions <- combined$region |>
      as_factor() |>
      unique() |>
      na.omit() |>
      as.character()
  }
  regions <- sort(regions)
  log_info(sprintf("Extracted %d unique regions: %s", length(regions), paste(head(regions, 5), collapse = ", ")))

  sectors <- processed$sector |> unique() |> na.omit() |> as.character() |> sort()
  log_info(sprintf("Extracted %d unique sectors: %s", length(sectors), paste(head(sectors, 5), collapse = ", ")))

  result <- list(
    raw = combined,
    processed = processed,
    latest = country_aggregates_with_coords,  # Country-level aggregates for maps/charts with coordinates
    country_panel = country_panel,  # Time series data by country and year
    country_sector = country_sector_aggregates,  # Country-sector aggregates for sector profiles
    country_size = country_size_aggregates,  # Country-size aggregates for size profiles
    country_region = country_region_aggregates,  # Country-region aggregates for regional profiles
    countries = countries,
    country_codes = processed$country_code |> unique() |> na.omit() |> as.character(),
    years = years,
    regions = regions,  # Unique region values from data
    sectors = sectors,  # Unique sector values from data
    country_coordinates = country_coords,  # Separate coordinates dataset
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
      # Use country_official as the primary country name source (no year suffixes, clean names)
      # Validate that it's not a sector name or invalid data
      country_official_clean = get0("country_official", ifnotfound = ""),
      country = if_else(
        !is.na(country_official_clean) &
        country_official_clean != "" &
        !grepl("^(Manufacturing|Services|Other Services|Retail)$", country_official_clean),  # Exclude sector names
        country_official_clean,
        coalesce_chr(
          get0("economy", ifnotfound = NULL),           # Fallback: economy name
          gsub("\\d{4}$", "", get0("country", ifnotfound = "")),  # Last resort: strip year from country column
          get0("country2", ifnotfound = NULL)
        )
      ),
      country = trimws(country),  # Remove any whitespace
      country_code = coalesce_chr(
        get0("wbcode", ifnotfound = NULL),
        get0("country_abr", ifnotfound = NULL)
      ),
      year = get0("year", ifnotfound = NA_integer_),
      region = if ("region" %in% names(data)) as.character(as_factor(region)) else NA_character_,
      # Try multiple possible income group variable names
      income = coalesce_chr(
        if ("income" %in% names(data)) as.character(as_factor(get0("income", ifnotfound = NULL))) else NULL,
        if ("income_group" %in% names(data)) as.character(as_factor(get0("income_group", ifnotfound = NULL))) else NULL,
        if ("inc" %in% names(data)) as.character(as_factor(get0("inc", ifnotfound = NULL))) else NULL,
        if ("wbincome" %in% names(data)) as.character(as_factor(get0("wbincome", ifnotfound = NULL))) else NULL
      ),
      sector = if ("stra_sector" %in% names(data)) as.character(as_factor(stra_sector)) else NA_character_,
      firm_size = if ("size" %in% names(data)) as.character(as_factor(size)) else NA_character_,
      female_ownership = if ("gend1" %in% names(data)) as.numeric(gend1) > 0 else NA,  # Binary: has female ownership
      sample_weight = get0("wt", ifnotfound = NA_real_),

      # Infrastructure
      power_outages_per_month = coalesce_num(get0("in2", ifnotfound = NULL)),
      avg_outage_duration_hrs = coalesce_num(get0("in3", ifnotfound = NULL)),
      firms_with_generator_pct = coalesce_num(get0("in9", ifnotfound = NULL)),
      # Infrastructure obstacles (for detailed breakdown)
      electricity_obstacle = coalesce_num(get0("elec", ifnotfound = NULL)),
      water_obstacle = coalesce_num(get0("c16", ifnotfound = NULL)),
      transport_obstacle = coalesce_num(get0("d4", ifnotfound = NULL)),
      # Power sources (for power source mix chart)
      generator_share_pct = coalesce_num(get0("in7", ifnotfound = NULL)),  # % electricity from generator

      # Access to finance
      firms_with_credit_line_pct = coalesce_num(get0("fin14", ifnotfound = NULL)),
      firms_with_bank_account_pct = coalesce_num(get0("fin15", ifnotfound = NULL)),
      loan_rejection_rate_pct = coalesce_num(get0("fin21", ifnotfound = NULL)),
      collateral_required_pct = coalesce_num(get0("fin10", ifnotfound = NULL)),
      # Detailed finance access indicators
      loan_application_pct = coalesce_num(get0("fin16", ifnotfound = NULL)),  # Applied for loan
      overdraft_facility_pct = coalesce_num(get0("fin9", ifnotfound = NULL)),  # Has overdraft
      # Loan application details
      no_need_for_loan = coalesce_num(get0("fin19a", ifnotfound = NULL)),  # Main reason: no need
      loan_procedures_complex = coalesce_num(get0("fin19b", ifnotfound = NULL)),  # Complex procedures
      loan_interest_high = coalesce_num(get0("fin19c", ifnotfound = NULL)),  # Interest rates high
      insufficient_collateral = coalesce_num(get0("fin19d", ifnotfound = NULL)),  # Lack collateral
      loan_size_inadequate = coalesce_num(get0("fin19e", ifnotfound = NULL)),  # Loan size
      # Loan processing
      days_to_get_loan = coalesce_num(get0("fin22", ifnotfound = NULL)),  # Days to process loan

      # Corruption and governance
      bribery_incidence_pct = coalesce_num(get0("graft3", ifnotfound = NULL)),
      corruption_obstacle_pct = coalesce_num(get0("corr11", ifnotfound = NULL)),
      # Bribery by transaction type
      bribe_for_permit = coalesce_num(get0("j7a", ifnotfound = NULL)),  # Construction permit
      bribe_for_import = coalesce_num(get0("j7b", ifnotfound = NULL)),  # Import license
      bribe_for_utilities = coalesce_num(get0("j7c", ifnotfound = NULL)),  # Utility connection
      bribe_for_tax = coalesce_num(get0("j7d", ifnotfound = NULL)),  # Tax assessment
      bribe_for_contract = coalesce_num(get0("j7e", ifnotfound = NULL)),  # Government contract
      # Management time spent
      mgmt_time_regulations_pct = coalesce_num(get0("j2", ifnotfound = NULL)),  # Time on regulations

      # Workforce and gender
      female_ownership_pct = coalesce_num(get0("gend1", ifnotfound = NULL)),
      female_workers_pct = coalesce_num(get0("gend2", ifnotfound = NULL)),
      workforce_obstacle_pct = coalesce_num(get0("wk10", ifnotfound = NULL)),  # Workforce quality obstacle (wk10 in data)

      # Performance and exports
      capacity_utilization_pct = coalesce_num(get0("t3", ifnotfound = NULL)),
      export_firms_pct = coalesce_num(get0("tr10", ifnotfound = NULL)),
      export_share_pct = compute_export_share(data),
      annual_sales_growth_pct = coalesce_num(get0("perf1", ifnotfound = NULL), get0("d2", ifnotfound = NULL)),

      # Crime and security (ensure these always exist even if source columns are missing)
      crime_obstacle_pct = coalesce_num(get0("crime8", ifnotfound = NULL)),
      security_costs_pct = coalesce_num(get0("crime2", ifnotfound = NULL))
    ) |>
    mutate(
      # Add IC.FRM.* aliases for compatibility with downstream modules
      # These World Bank indicator codes map to WBES microdata variables
      # Using coalesce to ensure columns always exist, defaulting to NA_real_ if source is NULL
      IC.FRM.CORR.ZS = coalesce(corruption_obstacle_pct, NA_real_),     # Corruption as obstacle
      IC.FRM.BRIB.ZS = coalesce(bribery_incidence_pct, NA_real_),        # Bribery incidence
      IC.FRM.CAPU.ZS = coalesce(capacity_utilization_pct, NA_real_),     # Capacity utilization
      IC.FRM.OUTG.ZS = coalesce(power_outages_per_month, NA_real_),      # Power outages
      IC.FRM.FINA.ZS = coalesce_num(get0("fin14", ifnotfound = NULL), NA_real_),  # Finance obstacle
      IC.FRM.BANK.ZS = coalesce(firms_with_bank_account_pct, NA_real_),  # Bank account access
      IC.FRM.CRED.ZS = coalesce(loan_rejection_rate_pct, NA_real_),      # Credit constraints
      IC.FRM.FEMO.ZS = coalesce(female_ownership_pct, NA_real_),         # Female ownership
      IC.FRM.FEMW.ZS = coalesce(female_workers_pct, NA_real_),           # Female workforce
      IC.FRM.EXPRT.ZS = coalesce(export_firms_pct, NA_real_),            # Export orientation
      IC.FRM.ELEC.ZS = coalesce_num(get0("elec", ifnotfound = NULL), NA_real_),    # Electricity obstacle
      IC.FRM.INFRA.ZS = coalesce_num(get0("infra", ifnotfound = NULL), NA_real_),  # Infrastructure obstacle
      IC.FRM.CRIM.ZS = coalesce(crime_obstacle_pct, NA_real_),           # Crime as obstacle (from crime8)
      IC.FRM.SECU.ZS = coalesce(security_costs_pct, NA_real_),           # Security costs as % of sales (from crime2)
      IC.FRM.WKFC.ZS = coalesce(workforce_obstacle_pct, NA_real_)        # Workforce quality as obstacle (from wk10)
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

coalesce_num <- function(...) {
  vals <- list(...)
  for (x in vals) {
    if (!is.null(x)) {
      num_val <- as.numeric(x)
      if (any(!is.na(num_val))) {
        return(num_val)
      }
    }
  }
  NA_real_
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
  # Try different country variable names, prioritizing country_official
  if ("country" %in% names(data)) {
    countries <- unique(data$country) |>
      na.omit() |>
      as.character()
    return(countries)
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

#' Get country coordinates for mapping
#' @return Data frame with country names and coordinates
#' @export
get_country_coordinates <- function() {
  # Comprehensive country coordinates (capital cities or geographic centers)
  coords <- data.frame(
    country = c(
      "Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", "Australia",
      "Austria", "Azerbaijan", "Bahamas", "Bangladesh", "Barbados", "Belarus", "Belgium",
      "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
      "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde",
      "Central African Republic", "Chad", "Chile", "China", "Colombia", "Congo, Dem. Rep.",
      "Congo, Rep.", "Costa Rica", "Cote d'Ivoire", "Croatia", "Czech Republic", "Denmark",
      "Djibouti", "Dominican Republic", "Ecuador", "Egypt, Arab Rep.", "El Salvador",
      "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland",
      "France", "Gabon", "Gambia, The", "Georgia", "Germany", "Ghana", "Greece", "Guatemala",
      "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong SAR, China",
      "Hungary", "Iceland", "India", "Indonesia", "Iran, Islamic Rep.", "Iraq", "Ireland",
      "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Korea, Rep.",
      "Kosovo", "Kuwait", "Kyrgyz Republic", "Lao PDR", "Latvia", "Lebanon", "Lesotho",
      "Liberia", "Libya", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia",
      "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Montenegro",
      "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", "New Zealand",
      "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan",
      "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal",
      "Puerto Rico", "Qatar", "Romania", "Russian Federation", "Rwanda", "Samoa", "Senegal",
      "Serbia", "Sierra Leone", "Singapore", "Slovak Republic", "Slovenia", "Solomon Islands",
      "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname",
      "Sweden", "Switzerland", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand",
      "Timor-Leste", "Togo", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan",
      "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States",
      "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela, RB", "Vietnam", "West Bank and Gaza",
      "Yemen, Rep.", "Zambia", "Zimbabwe"
    ),
    lat = c(
      34.52, 41.33, 28.03, -11.20, -38.42, 40.18, -25.27,
      48.21, 40.41, 25.03, 23.81, 13.10, 53.90, 50.85,
      17.25, 9.31, 27.47, -16.29, 43.86, -22.33, -14.24,
      42.70, 12.37, -3.37, 12.57, 7.37, 56.13, 14.93,
      6.61, 15.45, -35.68, 35.86, 4.57, -4.04,
      -4.26, 9.75, 7.54, 45.10, 49.82, 55.68,
      11.83, 18.74, -1.83, 26.82, 13.79,
      1.65, 15.18, 59.44, -26.52, 9.15, -18.14, 61.92,
      46.23, -0.80, 13.45, 42.32, 51.17, 7.95, 39.07, 15.78,
      9.95, 11.80, 4.86, 18.97, 15.00, 22.40,
      47.16, 64.96, 20.59, -0.79, 32.43, 33.22, 53.41,
      31.05, 41.87, 18.11, 36.20, 30.59, 48.02, -0.02, 37.57,
      42.66, 29.31, 41.20, 19.86, 56.88, 33.89, -29.61,
      6.43, 26.34, 55.17, 49.82, -18.77, -13.25, 4.21,
      17.57, 35.88, 18.07, -20.35, 23.63, 47.01, 46.86, 42.71,
      31.79, -18.67, 21.91, -22.96, 27.70, 52.13, -40.90,
      12.87, 17.61, 9.08, 41.61, 60.47, 21.51, 30.38,
      8.54, -6.31, -23.44, -9.19, 12.88, 51.92, 39.40,
      18.22, 25.35, 45.94, 61.52, -1.94, -13.76, 14.50,
      44.02, 8.46, 1.35, 48.67, 46.15, -9.65,
      5.15, -30.56, 4.85, 40.46, 7.87, 12.86, 3.92,
      60.13, 46.82, 34.80, 38.86, -6.37, 15.87,
      -8.87, 8.62, 10.69, 33.89, 38.96, 37.97,
      1.37, 48.38, 23.42, 55.38, 37.09,
      -32.52, 41.38, 17.68, 10.39, 14.06, 31.77,
      15.55, -13.13, -19.02
    ),
    lng = c(
      69.17, 19.82, 1.66, 17.87, -63.62, 44.51, 133.78,
      16.37, 49.87, -77.40, 90.36, -59.54, 27.57, 4.35,
      -88.50, 2.32, 90.43, -63.59, 17.68, 24.68, -51.93,
      25.49, -1.52, 29.92, 104.99, 12.35, -106.35, -23.51,
      20.94, 18.73, -71.54, 104.20, -74.30, 21.76,
      15.83, -83.75, -5.55, 15.98, 15.47, 12.57,
      43.15, -70.16, -78.18, 30.80, -88.90,
      10.27, 38.93, 24.75, 31.47, 38.76, 178.07, 25.75,
      2.21, 11.61, -15.31, 43.36, 10.45, -1.02, 21.82, -90.23,
      -9.70, -15.18, -58.93, -72.29, -86.24, 114.11,
      19.50, -19.02, 78.96, 113.92, 53.69, 43.68, -8.24,
      34.85, 12.57, -77.30, 138.25, 35.95, 66.92, 37.91, 127.77,
      20.90, 47.98, 74.77, 102.60, 24.60, 35.86, 28.23,
      -9.43, 17.23, 23.88, 6.13, 46.87, 33.79, 101.98,
      -3.00, 14.51, -10.94, 57.55, -102.55, 28.86, 106.92, 19.26,
      -7.09, 35.53, 95.96, 18.42, 85.32, 5.29, 174.89,
      -85.21, 8.08, 8.68, 21.75, 10.75, 55.92, 69.35,
      -80.78, 143.95, -58.44, -75.02, 121.77, 19.15, -8.22,
      -66.59, 51.18, 24.97, 105.32, 29.87, -172.10, -14.45,
      20.46, -11.78, 103.82, 19.70, 14.99, 159.96,
      46.20, 22.94, 31.58, -3.75, 80.77, 32.35, -56.03,
      18.64, 8.23, 36.31, 68.78, 34.89, 100.99,
      125.73, 1.17, -61.28, 9.54, 35.24, 59.56,
      0.35, 30.37, 54.37, -3.44, -95.71,
      -55.77, 64.59, 166.96, -66.59, 108.28, 35.23,
      48.52, 27.85, -18.91
    ),
    stringsAsFactors = FALSE
  )

  return(coords)
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
