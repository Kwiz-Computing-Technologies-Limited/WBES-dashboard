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
  arrow[as_arrow_table],
  app/logic/column_labels[extract_column_labels, create_wbes_label_mapping],
  app/logic/wb_integration[get_or_fetch_wb_data, get_wb_income_classifications, enrich_wbes_with_income],
  app/logic/parquet_cache[is_parquet_cache_fresh, load_wbes_parquet, save_wbes_parquet, to_df,
                          lazy_filter, lazy_select, lazy_summarise, is_lazy_arrow, force_collect]
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

  # Check for Parquet cache first (fastest and most efficient)
  if (use_cache && is_parquet_cache_fresh(data_path, cache_hours)) {
    parquet_data <- load_wbes_parquet(data_path)
    if (!is.null(parquet_data)) {
      log_info("Loaded from Parquet cache")
      gc()  # Explicit memory cleanup
      return(parquet_data)
    }
  }

  # Check for assets.zip (combined microdata) - PREFERRED METHOD
  assets_zip <- here(data_path, "assets.zip")
  if (file.exists(assets_zip)) {
    log_info("Found assets.zip - loading combined microdata")
    result <- load_from_zip(assets_zip, data_path)

    # Cache the processed result as Parquet
    if (use_cache && !is.null(result)) {
      tryCatch({
        save_wbes_parquet(result, data_path)
        log_info("Cached processed data as Parquet for faster future loads")
      }, error = function(e) {
        log_warn(paste("Could not cache data as Parquet:", e$message))
      })
    }

    gc()  # Explicit memory cleanup
    return(result)
  } else {
    log_warn("assets.zip not found in data/ directory")
  }

  # Check for individual .dta files
  dta_files <- list.files(data_path, pattern = "\\.dta$", full.names = TRUE)
  if (length(dta_files) > 0) {
    log_info("Found local .dta files")
    result <- load_microdata(dta_files)

    # Cache the processed result as Parquet
    if (use_cache && !is.null(result)) {
      tryCatch({
        save_wbes_parquet(result, data_path)
        log_info("Cached processed data as Parquet")
      }, error = function(e) {
        log_warn(paste("Could not cache data as Parquet:", e$message))
      })
    }

    gc()  # Explicit memory cleanup
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

#' Load microdata from ZIP archive (zero-extraction for Parquet, temp for .dta)
#'
#' This function supports two modes:
#' 1. PREFERRED: Read .parquet files directly from ZIP (zero extraction, pure in-memory)
#' 2. FALLBACK: Extract .dta files to tempdir() (required by haven::read_dta limitations)
#'
#' @param zip_file Path to assets.zip file
#' @param data_path Directory path
#' @return Processed data list
load_from_zip <- function(zip_file, data_path) {

  log_info(paste("Reading microdata from ZIP:", basename(zip_file)))

  tryCatch({
    # List contents of zip
    zip_contents <- unzip(zip_file, list = TRUE)

    # FIRST: Look for .parquet files (can read directly, zero extraction)
    parquet_files <- zip_contents$Name[grepl("\\.parquet$", zip_contents$Name, ignore.case = TRUE)]

    if (length(parquet_files) > 0) {
      log_info(paste("Found", length(parquet_files), "Parquet file(s) - reading directly from ZIP (zero extraction)"))

      # Read Parquet files directly from ZIP using raw bytes (NO EXTRACTION)
      data_list <- lapply(parquet_files, function(pq_file) {
        log_info(paste("  Reading", basename(pq_file), "from ZIP..."))

        # Create connection to file inside ZIP
        zip_conn <- unz(zip_file, pq_file)

        # Read raw bytes
        raw_data <- readBin(zip_conn, "raw", n = 2e9)  # 2GB max per file
        close(zip_conn)

        # Parse Parquet from raw bytes (in-memory, no disk writes!)
        arrow_table <- arrow::read_parquet(raw_data, as_data_frame = TRUE)

        rm(raw_data)  # Free memory
        return(arrow_table)
      })

      names(data_list) <- tools::file_path_sans_ext(basename(parquet_files))

      # Combine datasets if multiple
      if (length(data_list) == 1) {
        raw_data <- data_list[[1]]
      } else {
        log_info("Combining multiple Parquet files...")
        raw_data <- dplyr::bind_rows(data_list)
      }

      rm(data_list)
      gc()

      # Process the raw data
      result <- process_microdata(raw_data)
      return(result)
    }

    # FALLBACK: Look for .dta files (requires temp extraction due to haven::read_dta limitation)
    candidate_dta <- zip_contents$Name[grepl("\\.dta$", zip_contents$Name, ignore.case = TRUE)]

    # Prefer the known combined microdata file name if present
    preferred_match <- candidate_dta[basename(candidate_dta) == RAW_DTA_FILENAME]
    dta_files_in_zip <- if (length(preferred_match) > 0) preferred_match else candidate_dta

    if (length(dta_files_in_zip) == 0) {
      log_error("No .parquet or .dta files found in assets.zip")
      stop("CRITICAL ERROR: assets.zip exists but contains no data files. Please ensure the ZIP contains Parquet or .dta files.")
    }

    log_info(paste("Found", length(dta_files_in_zip), ".dta file(s) in archive"))
    log_warn("NOTE: .dta format requires temporary extraction. Convert to Parquet and re-zip for true zero-extraction reading.")

    # Extract .dta to tempdir (unavoidable for .dta format)
    temp_dir <- tempdir()
    unzip(zip_file, files = dta_files_in_zip, exdir = temp_dir, overwrite = TRUE)
    extracted_files <- file.path(temp_dir, dta_files_in_zip)

    # Load the microdata
    result <- load_microdata(extracted_files)

    # Cleanup temporary files
    unlink(extracted_files)
    gc()  # Explicit memory cleanup after loading

    log_info("Successfully loaded microdata from assets.zip (direct read)")
    return(result)

  }, error = function(e) {
    log_error(paste("Error reading from zip:", e$message))
    gc()  # Cleanup memory on error
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

  # Save file names before removing data_list (needed for metadata)
  loaded_file_names <- names(data_list)

  combined <- if (length(data_list) == 1) {
    data_list[[1]]
  } else {
    bind_rows(data_list, .id = "source_file")
  }

  # Clean up data_list to free memory
  rm(data_list)
  gc()  # Explicit memory cleanup after combining

  log_info(sprintf("Combined dataset: %d observations, %d variables",
                  nrow(combined), ncol(combined)))

  # Process and structure the data
  processed <- process_microdata(combined)
  gc()  # Explicit memory cleanup after processing

  # Enrich with World Bank income classifications
  log_info("Enriching data with World Bank income classifications...")
  processed <- tryCatch({
    enrich_wbes_with_income(processed)
  }, error = function(e) {
    log_warn(paste("Failed to enrich with WB income data:", e$message))
    log_warn("Continuing with original income column (may contain NAs)")
    processed
  })
  gc()  # Explicit memory cleanup after enrichment

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

  # SEQUENTIAL AGGREGATION: More efficient than parallel for large dataframes

  # (Parallel processing has too much overhead copying 1GB+ data to workers)
  log_info("Creating aggregates sequentially...")

  # Country aggregates
  log_info("  Creating country aggregates...")
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

  # Country-year panel
  log_info("  Creating country-year panel...")
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

  # Country-sector aggregates
  log_info("  Creating country-sector aggregates...")
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

  # Country-size aggregates
  log_info("  Creating country-size aggregates...")
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

  # Country-region aggregates
  log_info("  Creating country-region aggregates...")
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

  # Regional aggregates (aggregate by region only - needed by domain modules)
  log_info("  Creating regional aggregates...")
  regional_aggregates <- processed |>
    filter(!is.na(region)) |>
    group_by(region) |>
    summarise(
      across(all_of(available_metric_cols), ~mean(.x, na.rm = TRUE), .names = "{.col}"),
      countries_count = length(unique(country[!is.na(country)])),
      sample_size = n(),
      .groups = "drop"
    )

  # Log results
  log_info(sprintf("Country aggregates created: %d countries", nrow(country_aggregates)))
  log_info(sprintf("Country panel created: %d country-year observations", nrow(country_panel)))
  log_info(sprintf("Country-sector aggregates created: %d combinations", nrow(country_sector_aggregates)))
  log_info(sprintf("Country-size aggregates created: %d combinations", nrow(country_size_aggregates)))
  log_info(sprintf("Country-region aggregates created: %d combinations", nrow(country_region_aggregates)))
  log_info(sprintf("Regional aggregates created: %d regions", nrow(regional_aggregates)))

  # Explicit memory cleanup after parallel operations
  gc()

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
    regional = regional_aggregates,  # Region-level aggregates for domain modules
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
      files = loaded_file_names,
      observations = nrow(combined),
      variables = ncol(combined),
      loaded_at = Sys.time(),
      total_labels = length(label_mapping)
    ),
    quality = generate_quality_metadata()
  )

  # Explicit memory cleanup before returning
  gc()

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
      # Income will be enriched from World Bank data after processing
      # For now, try to extract from existing columns (will likely be NA)
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
#' @note Coordinates generated using rnaturalearth package (2025-12-22)
#'       Uses geographic centroids for accurate country positioning
get_country_coordinates <- function() {
  # Accurate country coordinates (geographic centroids from Natural Earth data)
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
      33.818756, 41.134874, 28.055993, -12.270614, -34.683988, 40.287328, -25.764182,
      47.592293, 40.286662, 24.280019, 23.860666, 13.181450, 53.519521, 50.638895,
      17.198299, 9.635612, 27.412138, -16.681984, 44.171474, -22.165245, -10.639049,
      42.771241, 12.270726, -3.359215, 12.719495, 5.681168, 60.417767, 16.000000,
      6.572299, 15.275359, -36.076247, 36.691407, 3.903480, -2.865488,
      -0.836992, 9.976131, 7.625878, 45.075893, 49.737997, 55.974982,
      11.748307, 18.895116, -1.424110, 26.459803, 13.739864,
      1.705406, 15.361124, 58.672902, -26.557546, 8.614401, -17.427546, 64.255136,
      42.955140, -0.586447, 13.450953, 42.172942, 51.050577, 7.947700, 39.047960, 15.690120,
      10.437319, 12.047751, 4.789643, 18.934575, 14.829080, 22.398261,
      47.165913, 64.997884, 22.776643, -2.264068, 32.527336, 33.009666, 53.162533,
      31.432979, 42.715686, 18.157451, 37.460880, 31.238703, 48.361671, 0.599449, 36.373367,
      42.570199, 29.333279, 41.474310, 18.487418, 56.859522, 33.921599, -29.579064,
      6.451958, 27.016716, 55.323770, 49.766782, -19.316484, -13.204425, 3.811140,
      17.315139, 35.921472, 20.220694, -20.277620, 23.924155, 47.187173, 46.956204, 42.788003,
      29.766859, -17.195858, 21.112907, -22.069690, 28.258043, 52.226920, -41.702334,
      12.844993, 17.412008, 9.590946, 41.595192, 66.360323, 20.585775, 29.883945,
      8.520233, -6.468581, -23.208909, -9.107150, 11.729343, 52.111836, 39.606210,
      18.228580, 25.304862, 45.848414, 66.031722, -1.990325, -13.753401, 14.364811,
      44.211320, 8.562492, 1.358762, 48.711281, 46.115890, -8.923117,
      4.739406, -28.974209, 7.308697, 40.207654, 7.611038, 15.967966, 4.129848,
      62.383656, 46.798896, 35.016777, 38.536549, -6.268248, 15.082585,
      -8.829322, 8.520780, 10.457173, 34.088955, 39.137013, 39.125471,
      1.274232, 49.195608, 23.905320, 54.011650, 44.743732,
      -32.791341, 41.775401, -16.216969, 7.122274, 16.544298, 31.900000,
      15.917281, -13.455525, -18.997047
    ),
    lng = c(
      65.926000, 20.051846, 2.631188, 17.531783, -64.762615, 44.938312, 134.303829,
      14.110430, 47.547842, -76.590160, 90.247104, -59.559785, 28.014955, 4.645938,
      -88.711068, 2.326615, 90.402160, -64.698439, 17.774892, 23.808613, -53.215734,
      25.209133, -1.761138, 29.875228, 104.905338, 12.733944, -96.548627, -24.000000,
      20.461833, 18.634997, -71.056191, 103.378050, -73.077032, 23.637747,
      15.221142, -84.189870, -5.569103, 16.401053, 15.319441, 10.042212,
      42.560079, -70.505241, -78.749298, 29.878270, -88.870957,
      10.341728, 38.857274, 25.545033, 31.481870, 39.609792, 178.512028, 26.194973,
      -6.676141, 11.788742, -15.396323, 43.524256, 10.364372, -1.216990, 22.964285, -90.367212,
      -10.934246, -14.949924, -58.979921, -72.686763, -86.617615, 114.113750,
      19.379268, -18.580034, 79.563911, 117.179730, 54.440876, 43.773676, -8.145718,
      34.991138, 12.201049, -77.314396, 137.660006, 36.757684, 67.245503, 37.796228, 127.834771,
      20.872603, 47.587780, 74.509086, 103.774934, 24.920447, 35.877696, 28.228962,
      -9.319833, 18.066793, 23.897100, 6.072158, 46.742890, 34.282848, 109.699343,
      -3.590473, 14.405377, -10.369917, 57.571265, -102.170985, 28.465619, 103.123972, 19.238631,
      -8.727848, 35.602728, 96.494756, 17.195020, 83.946844, 4.991548, 172.921344,
      -85.031941, 9.331610, 8.079587, 21.680596, 12.745897, 56.062131, 69.168440,
      -80.118776, 145.200497, -58.443433, -74.420742, 122.919280, 19.419706, -8.528130,
      -66.473068, 51.184652, 24.983182, 95.641377, 29.919983, -172.165305, -14.469448,
      20.806606, -11.792612, 103.817256, 19.468972, 14.800523, 159.622343,
      45.679293, 25.193438, 30.251334, -3.703536, 80.701632, 29.907465, -55.912431,
      16.266950, 8.206282, 38.484462, 71.026854, 34.801811, 101.000648,
      125.845108, 0.963860, -61.265902, 9.556129, 35.172801, 59.446422,
      32.368665, 31.315584, 54.292670, -2.758142, -103.629563,
      -56.025206, 63.303093, 167.675346, -66.170544, 106.346508, 35.200000,
      47.567787, 27.808582, 29.851013
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
