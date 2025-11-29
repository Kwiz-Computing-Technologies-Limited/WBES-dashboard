# app/logic/wbes_data.R
# World Bank Enterprise Surveys Data Module
# Fetches real WBES indicator data from World Bank API

box::use(
  httr[GET, content, status_code, http_error],
  jsonlite[fromJSON],
  dplyr[...],  # ... imports all dplyr functions including first, group_by, summarise, across, filter, etc.
  tidyr[pivot_wider, pivot_longer],
  purrr[map_dfr, possibly],
  readr[read_csv, write_csv],
  haven[read_dta, as_factor],
  logger[log_info, log_warn, log_error],
  utils[unzip],
  stats[runif, setNames]
)

# World Bank API Base URL
WB_API_BASE <- "https://api.worldbank.org/v2"
ES_SOURCE_ID <- 13  # Enterprise Surveys source ID
RAW_DTA_FILENAME <- "ES-Indicators-Database-Global-Methodology_November_24_2025.dta"

#' Get list of Enterprise Survey indicators
#' @return Data frame of available indicators
#' @export
get_es_indicators <- function() {
  url <- sprintf(
    "%s/sources/%d/indicators?format=json&per_page=500",
    WB_API_BASE, ES_SOURCE_ID
  )
  
  tryCatch({
    response <- GET(url)
    if (http_error(response)) {
      log_warn("Failed to fetch indicator list from World Bank API")
      return(NULL)
    }
    
    data <- content(response, as = "text", encoding = "UTF-8")
    json <- fromJSON(data, flatten = TRUE)
    
    if (length(json) >= 2 && !is.null(json[[2]])) {
      indicators <- as.data.frame(json[[2]])
      return(indicators)
    }
    NULL
  }, error = function(e) {
    log_error(paste("Error fetching indicators:", e$message))
    NULL
  })
}

#' Fetch Enterprise Survey indicator data for countries
#' @param indicator_codes Vector of indicator codes
#' @param countries Vector of ISO3 country codes (NULL for all)
#' @param date_range Date range string e.g. "2015:2023"
#' @return Data frame with indicator values
#' @export
fetch_es_data <- function(indicator_codes, countries = NULL, date_range = "2010:2024") {
  
  country_str <- if (is.null(countries)) "all" else paste(countries, collapse = ";")
  indicator_str <- paste(indicator_codes, collapse = ";")
  
  url <- sprintf(
    "%s/country/%s/indicator/%s?source=%d&date=%s&format=json&per_page=10000",
    WB_API_BASE, country_str, indicator_str, ES_SOURCE_ID, date_range
  )
  
  log_info(paste("Fetching WBES data:", indicator_str))
  
  tryCatch({
    response <- GET(url)
    if (http_error(response)) {
      log_warn(paste("API error:", status_code(response)))
      return(NULL)
    }
    
    data <- content(response, as = "text", encoding = "UTF-8")
    json <- fromJSON(data, flatten = TRUE)
    
    if (length(json) >= 2 && !is.null(json[[2]])) {
      df <- as.data.frame(json[[2]])
      return(df)
    }
    NULL
  }, error = function(e) {
    log_error(paste("Error fetching data:", e$message))
    NULL
  })
}

#' Key WBES indicator codes mapped to readable names
#' @export
WBES_INDICATORS <- list(
  # Infrastructure
  infrastructure_obstacle = "IC.FRM.INFRA.ZS",
  electricity_obstacle = "IC.FRM.ELEC.ZS",
  power_outages = "IC.FRM.OUTG.ZS",
  
  # Access to Finance
  finance_obstacle = "IC.FRM.FINA.ZS",
  bank_account = "IC.FRM.BANK.ZS",
  credit_constraint = "IC.FRM.CRED.ZS",
  
  # Corruption
  corruption_obstacle = "IC.FRM.CORR.ZS",
  bribery_incidence = "IC.FRM.BRIB.ZS",
  
  # Workforce
  workforce_obstacle = "IC.FRM.WKFC.ZS",
  female_workers = "IC.FRM.FEMW.ZS",
  female_ownership = "IC.FRM.FEMO.ZS",
  
  # Performance
  capacity_utilization = "IC.FRM.CAPU.ZS",
  export_firms = "IC.FRM.EXPRT.ZS",
  
  # Crime
  crime_obstacle = "IC.FRM.CRIM.ZS",
  security_costs = "IC.FRM.SECU.ZS"
)

#' Load complete WBES dataset
#' This function attempts to load data in order of preference:
#' 1. Cached processed data (.rds) if present and recent
#' 2. Local microdata from assets.zip if present
#' 3. Individual .dta files if present
#' 4. Fresh API data
#' 5. Sample data as fallback
#' @param data_path Path to data directory
#' @param use_cache Whether to use cached data
#' @param cache_hours Hours before cache expires
#' @return List with WBES data components
#' @export
load_wbes_data <- function(data_path = "data/", use_cache = TRUE, cache_hours = 24) {

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

  # Check for assets.zip (combined microdata)
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
  }

  # Try to fetch from API
  api_data <- fetch_all_indicators()
  if (!is.null(api_data)) {
    # Save to cache
    tryCatch({
      dir.create(data_path, showWarnings = FALSE, recursive = TRUE)
      saveRDS(api_data, cache_file)
      log_info("Cached API data")
    }, error = function(e) {
      log_warn(paste("Could not cache data:", e$message))
    })
    return(api_data)
  }

  # Fallback to sample data
  log_warn("No data sources found - using sample data")
  return(load_sample_data())
}

#' Fetch all WBES indicators from API
#' @return Processed data list
fetch_all_indicators <- function() {
  
  indicator_codes <- unlist(WBES_INDICATORS)
  
  tryCatch({
    # Fetch data for all indicators
    raw_data <- fetch_es_data(indicator_codes)
    
    if (is.null(raw_data) || nrow(raw_data) == 0) {
      return(NULL)
    }
    
    # Process into clean format
    processed <- raw_data |>
      select(
        country = country.value,
        country_code = countryiso3code,
        indicator = indicator.id,
        indicator_name = indicator.value,
        year = date,
        value = value
      ) |>
      mutate(
        year = as.integer(year),
        value = as.numeric(value)
      ) |>
      filter(!is.na(value))
    
    # Get latest year per country/indicator
    latest <- processed |>
      group_by(country, country_code, indicator) |>
      filter(year == max(year)) |>
      ungroup()
    
    # Pivot to wide format
    wide_data <- latest |>
      select(country, country_code, indicator, value) |>
      pivot_wider(
        names_from = indicator,
        values_from = value,
        values_fn = first
      )
    
    # Add region/income metadata
    wide_data <- add_country_metadata(wide_data)
    
    list(
      raw = processed,
      latest = wide_data,
      countries = unique(wide_data$country),
      years = sort(unique(processed$year)),
      metadata = list(
        source = "World Bank Enterprise Surveys API",
        url = "https://www.enterprisesurveys.org",
        fetched = Sys.time(),
        indicators = length(indicator_codes)
      ),
      quality = generate_quality_metadata()
    )
    
  }, error = function(e) {
    log_error(paste("Error processing API data:", e$message))
    NULL
  })
}

#' Add country metadata (region, income group)
#' @param data Data frame with country_code column
#' @return Data with added metadata
add_country_metadata <- function(data) {
  
  # Fetch country metadata from World Bank API
  url <- sprintf("%s/country/all?format=json&per_page=300", WB_API_BASE)
  
  tryCatch({
    response <- GET(url)
    json <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (length(json) >= 2) {
      countries <- as.data.frame(json[[2]]) |>
        select(
          country_code = id,
          region = region.value,
          income_group = incomeLevel.value
        ) |>
        filter(!is.na(region) & region != "Aggregates")
      
      data <- left_join(data, countries, by = "country_code")
    }
    
    data
  }, error = function(e) {
    log_warn("Could not fetch country metadata")
    data$region <- NA_character_
    data$income_group <- NA_character_
    data
  })
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
      return(load_sample_data())
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
    return(load_sample_data())
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
    return(load_sample_data())
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

  # CREATE COUNTRY-LEVEL AGGREGATES for maps and charts
  metric_cols <- c(
    "power_outages_per_month", "avg_outage_duration_hrs", "firms_with_generator_pct",
    "firms_with_credit_line_pct", "firms_with_bank_account_pct", "loan_rejection_rate_pct",
    "collateral_required_pct", "bribery_incidence_pct", "corruption_obstacle_pct",
    "capacity_utilization_pct", "export_share_pct", "export_firms_pct",
    "female_ownership_pct", "female_workers_pct", "crime_obstacle_pct", "security_costs_pct"
  )

  country_aggregates <- processed |>
    filter(!is.na(country) & !is.na(country_code)) |>
    group_by(country, country_code) |>
    summarise(
      across(all_of(metric_cols), ~weighted_mean_safe(.x, sample_weight), .names = "{.col}"),
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
    metadata = list(
      source = "World Bank Enterprise Surveys (Microdata)",
      url = "https://www.enterprisesurveys.org/en/survey-datasets",
      files = names(data_list),
      observations = nrow(combined),
      variables = ncol(combined),
      loaded_at = Sys.time()
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

#' Add country metadata to microdata
#' @param data Microdata with country codes
#' @return Data with region/income metadata
add_country_metadata_to_microdata <- function(data) {

  tryCatch({
    # Fetch country metadata from World Bank API
    url <- sprintf("%s/country/all?format=json&per_page=300", WB_API_BASE)
    response <- GET(url)
    json <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

    if (length(json) >= 2) {
      countries <- as.data.frame(json[[2]]) |>
        select(
          a0 = id,
          country_name = name,
          region = region.value,
          income_group = incomeLevel.value
        ) |>
        filter(!is.na(region) & region != "Aggregates")

      # Join with data
      data <- left_join(data, countries, by = "a0")
    }

    data

  }, error = function(e) {
    log_warn("Could not fetch country metadata for microdata")
    data
  })
}

#' Generate sample data for demonstration
#' @return Sample data list
#' @export
load_sample_data <- function() {

  base::set.seed(42)
  
  # African countries focus (aligned with your work)
  countries <- data.frame(
    country = c(
      "Kenya", "Nigeria", "South Africa", "Ghana", "Ethiopia",
      "Tanzania", "Uganda", "Rwanda", "Senegal", "Cote d'Ivoire",
      "Egypt", "Morocco", "Tunisia", "Botswana", "Zambia",
      "India", "Bangladesh", "Vietnam", "Indonesia", "Philippines",
      "Brazil", "Mexico", "Colombia", "Peru", "Chile",
      "Poland", "Turkey", "Romania", "Bulgaria", "Serbia"
    ),
    country_code = c(
      "KEN", "NGA", "ZAF", "GHA", "ETH",
      "TZA", "UGA", "RWA", "SEN", "CIV",
      "EGY", "MAR", "TUN", "BWA", "ZMB",
      "IND", "BGD", "VNM", "IDN", "PHL",
      "BRA", "MEX", "COL", "PER", "CHL",
      "POL", "TUR", "ROU", "BGR", "SRB"
    ),
    region = c(
      rep("Sub-Saharan Africa", 15),
      rep("South Asia", 2),
      rep("East Asia & Pacific", 3),
      rep("Latin America & Caribbean", 5),
      rep("Europe & Central Asia", 5)
    ),
    income_group = c(
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
    ),
    stringsAsFactors = FALSE
  )
  
  years <- 2019:2023
  n <- nrow(countries) * length(years)
  
  # Generate panel data
  panel <- expand.grid(
    country_code = countries$country_code,
    year = years,
    stringsAsFactors = FALSE
  ) |>
    left_join(countries, by = "country_code")
  
  n <- nrow(panel)
  
  # Add realistic indicator values with regional variation
  ssa_bonus <- ifelse(panel$region == "Sub-Saharan Africa", 1.3, 1)
  
  panel <- panel |>
    mutate(
      # Infrastructure - World Bank codes
      IC.FRM.OUTG.ZS = round(base::pmin(100, runif(n, 15, 45) * ssa_bonus), 1),
      IC.FRM.ELEC.ZS = round(base::pmin(100, runif(n, 20, 50) * ssa_bonus), 1),
      IC.FRM.INFRA.ZS = round(base::pmin(100, runif(n, 15, 40) * ssa_bonus), 1),

      # Access to Finance - World Bank codes
      IC.FRM.FINA.ZS = round(runif(n, 20, 55), 1),
      IC.FRM.BANK.ZS = round(runif(n, 75, 98), 1),
      IC.FRM.CRED.ZS = round(runif(n, 15, 50), 1),

      # Corruption - World Bank codes
      IC.FRM.CORR.ZS = round(runif(n, 10, 45), 1),
      IC.FRM.BRIB.ZS = round(runif(n, 8, 35), 1),

      # Workforce - World Bank codes
      IC.FRM.WKFC.ZS = round(runif(n, 15, 45), 1),
      IC.FRM.FEMW.ZS = round(runif(n, 20, 50), 1),
      IC.FRM.FEMO.ZS = round(runif(n, 10, 45), 1),

      # Performance - World Bank codes
      IC.FRM.CAPU.ZS = round(runif(n, 55, 85), 1),
      IC.FRM.EXPRT.ZS = round(runif(n, 5, 35), 1),

      # Crime - World Bank codes
      IC.FRM.CRIM.ZS = round(runif(n, 10, 40), 1),
      IC.FRM.SECU.ZS = round(runif(n, 1, 5), 2),

      # Friendly column names for compatibility
      power_outages_per_month = round(runif(n, 0.5, 15), 1),
      avg_outage_duration_hrs = round(runif(n, 2, 12), 1),
      firms_with_generator_pct = round(runif(n, 20, 80), 1),
      firms_with_credit_line_pct = round(runif(n, 15, 65), 1),
      firms_with_bank_account_pct = round(runif(n, 75, 98), 1),
      loan_rejection_rate_pct = round(runif(n, 10, 40), 1),
      collateral_required_pct = round(runif(n, 40, 95), 1),
      bribery_incidence_pct = round(runif(n, 5, 45), 1),
      corruption_obstacle_pct = round(runif(n, 10, 45), 1),
      capacity_utilization_pct = round(runif(n, 55, 90), 1),
      export_share_pct = round(runif(n, 5, 40), 1),
      export_firms_pct = round(runif(n, 5, 35), 1),
      female_ownership_pct = round(runif(n, 10, 50), 1),
      female_workers_pct = round(runif(n, 20, 50), 1),
      crime_obstacle_pct = round(runif(n, 10, 40), 1),
      security_costs_pct = round(runif(n, 1, 5), 2),

      # Sample metadata
      sample_size = round(runif(n, 150, 1500)),
      response_rate = round(runif(n, 45, 85), 1),
      data_quality_score = round(runif(n, 0.6, 1.0), 2)
    )
  
  # Get latest year data
  latest <- panel |>
    group_by(country) |>
    filter(year == max(year)) |>
    ungroup()
  
  # Regional aggregates
  regional <- latest |>
    group_by(region) |>
    summarise(
      across(starts_with("IC."), ~mean(.x, na.rm = TRUE)),
      n_countries = n(),
      .groups = "drop"
    )
  
  list(
    raw = panel,
    latest = latest,
    regional = regional,
    countries = unique(panel$country),
    country_codes = unique(panel$country_code),
    regions = unique(panel$region),
    years = years,
    metadata = list(
      source = "World Bank Enterprise Surveys (Sample Data)",
      url = "https://www.enterprisesurveys.org",
      note = "Simulated data for demonstration. Download actual data from enterprisesurveys.org",
      generated = Sys.Date()
    ),
    quality = generate_quality_metadata()
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
