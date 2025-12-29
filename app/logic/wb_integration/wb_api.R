# app/logic/wb_integration/wb_api.R
# World Bank API Integration - Core API functions
# Uses wbstats package to fetch World Bank data

box::use(
  wbstats[wb_cachelist, wb_data, wb_search],
  dplyr[...],
  tidyr[...],
  stats[na.omit],
  logger[log_info, log_warn, log_error],
  countrycode[countrycode],
  here[here]
)

#' Get World Bank income classifications for all countries (current)
#'
#' Fetches current income level classifications from World Bank API
#' Returns: Low income, Lower middle income, Upper middle income, High income
#'
#' @return Data frame with columns: iso3c, country_name, income_level
#' @export
get_wb_income_classifications <- function() {
  log_info("Fetching World Bank income classifications (current)...")

  tryCatch({
    # Get country metadata from World Bank cache
    wb_countries <- wb_cachelist$countries

    # Filter and select relevant columns
    income_data <- wb_countries |>
      filter(
        !is.na(income_level),
        income_level != "Aggregates",  # Exclude regional/global aggregates
        income_level != "Not classified"
      ) |>
      select(
        iso3c,
        country_name = country,
        income_level
      ) |>
      mutate(
        # Standardize income level names
        income_level = standardize_income_level(income_level)
      )

    log_info(sprintf("Fetched income classifications for %d countries", nrow(income_data)))

    return(income_data)

  }, error = function(e) {
    log_error(paste("Failed to fetch WB income classifications:", e$message))
    return(NULL)
  })
}

#' Standardize income level names to short format
#'
#' @param income_level Vector of income level strings
#' @return Standardized income level strings
standardize_income_level <- function(income_level) {
  case_when(
    grepl("Low income", income_level, ignore.case = TRUE) &
      !grepl("Lower|Upper", income_level, ignore.case = TRUE) ~ "Low",
    grepl("Lower middle", income_level, ignore.case = TRUE) ~ "Lower-Middle",
    grepl("Upper middle", income_level, ignore.case = TRUE) ~ "Upper-Middle",
    grepl("High income", income_level, ignore.case = TRUE) ~ "High",
    TRUE ~ income_level
  )
}

#' Get historical World Bank income classifications by year
#'
#' Fetches GNI per capita data and derives income classifications for each year.
#' World Bank income thresholds are updated annually, so this uses the thresholds
#' appropriate for each year.
#'
#' @param start_year Start year (default: 2006, when WBES Global Methodology began)
#' @param end_year End year (default: current year)
#' @return Data frame with columns: iso3c, year, income_level
#' @export
get_wb_income_classifications_historical <- function(start_year = 2006, end_year = NULL) {
  if (is.null(end_year)) {
    end_year <- as.integer(format(Sys.Date(), "%Y"))
  }

  log_info(sprintf("Fetching historical income classifications (%d-%d)...", start_year, end_year))

  tryCatch({
    # Fetch GNI per capita (Atlas method) - used for income classification
    # NY.GNP.PCAP.CD is GNI per capita, Atlas method (current US$)
    gni_data <- wb_data(
      indicator = "NY.GNP.PCAP.CD",
      start_date = start_year,
      end_date = end_year,
      return_wide = FALSE
    )

    if (is.null(gni_data) || nrow(gni_data) == 0) {
      log_warn("No GNI data returned from World Bank API")
      return(NULL)
    }

    # World Bank income thresholds by fiscal year (July 1 of year to June 30 of next)
    # These are the thresholds that determine income classification
    # Source: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
    income_thresholds <- tribble(
      ~year, ~low_upper, ~lower_middle_upper, ~upper_middle_upper,
      2006, 905, 3595, 11115,
      2007, 935, 3705, 11455,
      2008, 975, 3855, 11905,
      2009, 995, 3945, 12195,
      2010, 1005, 3975, 12275,
      2011, 1025, 4035, 12475,
      2012, 1035, 4085, 12615,
      2013, 1045, 4125, 12745,
      2014, 1045, 4125, 12745,
      2015, 1045, 4125, 12735,
      2016, 1025, 4035, 12475,
      2017, 1005, 3955, 12235,
      2018, 1025, 3995, 12375,
      2019, 1036, 4045, 12535,
      2020, 1036, 4045, 12535,
      2021, 1046, 4095, 12695,
      2022, 1085, 4255, 13205,
      2023, 1135, 4465, 13845,
      2024, 1145, 4515, 14005,
      2025, 1145, 4515, 14005  # Use 2024 thresholds for 2025 until updated
    )

    # Clean and prepare GNI data
    gni_clean <- gni_data |>
      filter(!is.na(value)) |>
      select(iso3c, year = date, gni_per_capita = value) |>
      mutate(year = as.integer(year))

    # Join with thresholds and classify
    income_historical <- gni_clean |>
      left_join(income_thresholds, by = "year") |>
      mutate(
        income_level = case_when(
          is.na(gni_per_capita) ~ NA_character_,
          is.na(low_upper) ~ NA_character_,  # No threshold for this year
          gni_per_capita <= low_upper ~ "Low",
          gni_per_capita <= lower_middle_upper ~ "Lower-Middle",
          gni_per_capita <= upper_middle_upper ~ "Upper-Middle",
          TRUE ~ "High"
        )
      ) |>
      filter(!is.na(income_level)) |>
      select(iso3c, year, income_level)

    log_info(sprintf(
      "Derived historical income classifications: %d country-year observations",
      nrow(income_historical)
    ))

    return(income_historical)

  }, error = function(e) {
    log_error(paste("Failed to fetch historical income classifications:", e$message))
    return(NULL)
  })
}

#' Map WBES country names to ISO3 codes
#'
#' Uses countrycode package + manual mappings for edge cases
#'
#' @param wbes_countries Vector of country names from WBES data
#' @return Data frame with columns: country, iso3c
#' @export
map_wbes_countries_to_iso3 <- function(wbes_countries) {
  log_info("Mapping WBES country names to ISO3 codes...")

  # Manual mappings for known edge cases in WBES data
  manual_mappings <- tribble(
    ~country, ~iso3c,
    "Congo, Dem. Rep.", "COD",
    "Congo, Rep.", "COG",
    "Egypt, Arab Rep.", "EGY",
    "Gambia, The", "GMB",
    "Iran, Islamic Rep.", "IRN",
    "Kyrgyz Republic", "KGZ",
    "Lao PDR", "LAO",
    "Macedonia, FYR", "MKD",
    "North Macedonia", "MKD",
    "Slovak Republic", "SVK",
    "Venezuela, RB", "VEN",
    "Yemen, Rep.", "YEM",
    "Cote d'Ivoire", "CIV",
    "Côte d'Ivoire", "CIV",
    "West Bank and Gaza", "PSE",
    "Korea, Rep.", "KOR",
    "South Korea", "KOR",
    "Vietnam", "VNM",
    "Viet Nam", "VNM"
  )

  # Create mapping data frame
  country_mapping <- tibble(country = unique(wbes_countries)) |>
    filter(!is.na(country), country != "")

  # First, try manual mappings
  country_mapping <- country_mapping |>
    left_join(manual_mappings, by = "country")

  # For remaining countries, use countrycode
  country_mapping <- country_mapping |>
    mutate(
      iso3c = if_else(
        is.na(iso3c),
        countrycode(
          country,
          origin = "country.name",
          destination = "iso3c",
          warn = FALSE
        ),
        iso3c
      )
    )

  # Log mapping results
  successful_mappings <- sum(!is.na(country_mapping$iso3c))
  failed_mappings <- sum(is.na(country_mapping$iso3c))

  log_info(sprintf(
    "Country mapping complete: %d successful, %d failed",
    successful_mappings,
    failed_mappings
  ))

  # Warn about failed mappings
  if (failed_mappings > 0) {
    failed_countries <- country_mapping |>
      filter(is.na(iso3c)) |>
      pull(country)
    log_warn(paste("Failed to map countries:", paste(failed_countries, collapse = ", ")))
  }

  return(country_mapping)
}

#' Enrich WBES data with World Bank income classifications
#'
#' Merges WB income data into WBES dataset using ISO3 country codes AND year.
#' Uses historical income classifications to match the survey year, with fallback
#' to current classifications for missing years.
#'
#' @param wbes_data WBES data frame with country and year columns
#' @return WBES data frame with added income column
#' @export
enrich_wbes_with_income <- function(wbes_data) {
  log_info("Enriching WBES data with World Bank income classifications (by country AND year)...")

  # Map WBES countries to ISO3 first
  country_iso_map <- map_wbes_countries_to_iso3(wbes_data$country)

  # Merge ISO3 codes into WBES data
  wbes_with_iso3 <- wbes_data |>
    left_join(country_iso_map, by = "country")

  # Get the year range from WBES data
  wbes_years <- wbes_with_iso3$year |> unique() |> na.omit() |> as.integer()
  if (length(wbes_years) == 0) {
    log_warn("No valid years in WBES data, using current income classifications")
    wbes_years <- c(as.integer(format(Sys.Date(), "%Y")))
  }
  min_year <- min(wbes_years, na.rm = TRUE)
  max_year <- max(wbes_years, na.rm = TRUE)

  # Get historical income classifications
  wb_income_historical <- get_wb_income_classifications_historical(
    start_year = min_year,
    end_year = max_year
  )

  # Get current classifications as fallback
  wb_income_current <- get_wb_income_classifications()

  if (is.null(wb_income_historical) && is.null(wb_income_current)) {
    log_error("Cannot enrich data - both historical and current WB income fetch failed")
    return(wbes_data)
  }

  # Merge historical income classifications by country AND year
  if (!is.null(wb_income_historical)) {
    wbes_enriched <- wbes_with_iso3 |>
      left_join(
        wb_income_historical |> select(iso3c, year, wb_income_historical = income_level),
        by = c("iso3c", "year")
      )
    log_info(sprintf(
      "Matched %d rows with historical income data",
      sum(!is.na(wbes_enriched$wb_income_historical))
    ))
  } else {
    wbes_enriched <- wbes_with_iso3 |>
      mutate(wb_income_historical = NA_character_)
  }

  # Fallback to current classifications for missing values
  if (!is.null(wb_income_current)) {
    wbes_enriched <- wbes_enriched |>
      left_join(
        wb_income_current |> select(iso3c, wb_income_current = income_level),
        by = "iso3c"
      )
  } else {
    wbes_enriched <- wbes_enriched |>
      mutate(wb_income_current = NA_character_)
  }

  # Apply income: prefer historical (year-matched), fallback to current, then existing
  wbes_enriched <- wbes_enriched |>
    mutate(
      income = coalesce(wb_income_historical, wb_income_current, income)
    ) |>
    select(-any_of(c("wb_income_historical", "wb_income_current")))

  # Log enrichment results
  total_rows <- nrow(wbes_enriched)
  enriched_rows <- sum(!is.na(wbes_enriched$income))

  log_info(sprintf(
    "Income enrichment complete: %d/%d rows enriched (%.1f%%)",
    enriched_rows,
    total_rows,
    (enriched_rows / total_rows) * 100
  ))

  # Log unique income levels found
  income_levels <- wbes_enriched$income |> unique() |> na.omit() |> sort()
  log_info(sprintf("Income levels: %s", paste(income_levels, collapse = ", ")))

  return(wbes_enriched)
}

#' Fetch World Bank indicator data
#'
#' Generic function to fetch any WB indicator by code
#'
#' @param indicator_code WB indicator code (e.g., "NY.GDP.PCAP.CD")
#' @param start_year Start year for data (default: 2006)
#' @param end_year End year for data (default: current year)
#' @return Data frame with indicator values by country and year
#' @export
fetch_wb_indicator <- function(indicator_code, start_year = 2006, end_year = format(Sys.Date(), "%Y")) {
  log_info(sprintf("Fetching WB indicator: %s (%s-%s)", indicator_code, start_year, end_year))

  tryCatch({
    indicator_data <- wb_data(
      indicator = indicator_code,
      start_date = start_year,
      end_date = end_year
    )

    log_info(sprintf("Fetched %d observations for indicator %s", nrow(indicator_data), indicator_code))

    return(indicator_data)

  }, error = function(e) {
    log_error(paste("Failed to fetch WB indicator:", e$message))
    return(NULL)
  })
}

#' Search World Bank indicators by keyword
#'
#' Utility function to search for WB indicators
#'
#' @param keyword Search term
#' @return Data frame of matching indicators
#' @export
search_wb_indicators <- function(keyword) {

  log_info(sprintf("Searching WB indicators for: %s", keyword))

  tryCatch({
    results <- wb_search(pattern = keyword)
    log_info(sprintf("Found %d matching indicators", nrow(results)))
    return(results)

  }, error = function(e) {
    log_error(paste("Failed to search WB indicators:", e$message))
    return(NULL)
  })
}

# ============================================================
# World Bank Databank Indicators for Country Profile Enhancement
# ============================================================

#' Define World Bank Databank indicators for country context
#'
#' Returns a list of indicator codes organized by category
#' These complement WBES firm-level data with macro-level context
#'
#' @return Named list of indicator codes by category
#' @export
get_wb_databank_indicators <- function() {
  # Using unnamed vectors to ensure wbstats uses indicator codes as column names
  list(
    # Tier 1 - Essential Macro Context
    economic = c(
      "NY.GDP.PCAP.CD",   # GDP per capita
      "NY.GDP.PCAP.KD.ZG", # GDP growth
      "FP.CPI.TOTL.ZG",   # Inflation
      "NY.GNP.PCAP.CD"    # GNI per capita
    ),
    trade = c(
      "NE.EXP.GNFS.ZS",   # Exports % GDP
      "NE.IMP.GNFS.ZS",   # Imports % GDP
      "BX.KLT.DINV.CD.WD" # FDI net inflows
    ),
    # Tier 2 - Sector-Specific Context
    infrastructure = c(
      "EG.ELC.ACCS.ZS",   # Electricity access
      "IT.NET.USER.ZS",   # Internet users
      "IT.CEL.SETS.P2"    # Mobile subscriptions
    ),
    finance = c(
      "FS.AST.PRVT.GD.ZS" # Domestic credit % GDP
    ),
    labor = c(
      "SL.UEM.TOTL.ZS",   # Unemployment
      "SL.TLF.CACT.ZS",   # Labor force participation
      "SL.TLF.CACT.FE.ZS" # Female labor force
    ),
    # Tier 3 - Governance (Worldwide Governance Indicators)
    governance = c(
      "GE.EST",  # Gov effectiveness
      "RQ.EST",  # Regulatory quality
      "RL.EST",  # Rule of law
      "CC.EST",  # Control of corruption
      "PV.EST",  # Political stability
      "VA.EST"   # Voice & accountability
    )
    # Note: "Doing Business" indicators (IC.REG.*) removed - discontinued by World Bank in 2021
  )
}

#' Fetch multiple World Bank indicators for a set of countries
#'
#' Fetches all specified indicators and returns a wide-format data frame
#' with one row per country-year
#'
#' @param iso3_codes Vector of ISO3 country codes
#' @param indicators Named vector or list of indicator codes
#' @param start_year Start year (default: 2010)
#' @param end_year End year (default: current year)
#' @return Data frame with columns: iso3c, year, and one column per indicator
#' @export
fetch_wb_indicators_batch <- function(iso3_codes, indicators = NULL, start_year = 2010, end_year = NULL) {
  if (is.null(end_year)) {
    end_year <- as.integer(format(Sys.Date(), "%Y"))
  }

  # Default to Tier 1 indicators if none specified

if (is.null(indicators)) {
    all_indicators <- get_wb_databank_indicators()
    indicators <- c(
      all_indicators$economic,
      all_indicators$trade,
      all_indicators$infrastructure,
      all_indicators$governance
    )
  }

  # Flatten if list and remove names to ensure wbstats uses indicator codes as column names
  if (is.list(indicators)) {
    indicator_codes <- unlist(indicators, use.names = FALSE)
  } else {
    indicator_codes <- unname(indicators)
  }

  log_info(sprintf(
    "Fetching %d WB indicators for %d countries (%d-%d)",
    length(indicator_codes), length(iso3_codes), start_year, end_year
  ))

  tryCatch({
    # Fetch all indicators in one call
    wb_result <- wb_data(
      indicator = indicator_codes,
      country = iso3_codes,
      start_date = start_year,
      end_date = end_year
    )

    if (is.null(wb_result) || nrow(wb_result) == 0) {
      log_warn("No data returned from World Bank API")
      return(NULL)
    }

    # Clean up column names - remove indicator prefixes
    # wbstats returns columns like "NY.GDP.PCAP.CD" directly
    log_info(sprintf("Fetched %d rows of WB indicator data", nrow(wb_result)))

    return(wb_result)

  }, error = function(e) {
    log_error(paste("Failed to fetch WB indicators batch:", e$message))
    return(NULL)
  })
}

#' Get World Bank Databank data for a country matching a target year
#'
#' Fetches WB data closest to the specified target year (e.g., WBES survey year)
#' Returns values for each indicator, preferring data from the target year
#' Falls back to nearest available year within +/- 2 years
#' Handles discontinued indicators (like Doing Business) gracefully
#'
#' @param iso3_code Single ISO3 country code
#' @param indicators Named list of indicator codes (default: all Tier 1-3)
#' @param target_year Target year to match (e.g., WBES survey year). If NULL, uses latest data.
#' @return Named list with indicator values and metadata
#' @export
get_wb_country_context <- function(iso3_code, indicators = NULL, target_year = NULL) {
  if (is.null(indicators)) {
    indicators <- get_wb_databank_indicators()
  }

  # If no target year specified, fetch most recent data (last 5 years)
  if (is.null(target_year)) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    start_year <- current_year - 5
    end_year <- current_year
    log_info(sprintf("Fetching WB context for country: %s (latest data)", iso3_code))
  } else {
    # Fetch data around the target year (+/- 2 years for fallback)
    target_year <- as.integer(target_year)
    start_year <- target_year - 2
    end_year <- target_year + 2
    log_info(sprintf("Fetching WB context for country: %s (target year: %d)", iso3_code, target_year))
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  # Initialize result in parent environment for proper scoping
  result <- list(
    iso3c = iso3_code,
    target_year = target_year,
    data_years = list()
  )

  # Helper function to extract value closest to target year from WB data result
  # If target_year is NULL, returns the latest value
  extract_best_value <- function(wb_data_result, code, target_yr = NULL) {
    if (code %in% names(wb_data_result)) {
      non_na_rows <- wb_data_result[!is.na(wb_data_result[[code]]), ]
      if (nrow(non_na_rows) > 0) {
        if (!is.null(target_yr)) {
          # Find row closest to target year
          non_na_rows$year_diff <- abs(as.integer(non_na_rows$date) - target_yr)
          best_row <- non_na_rows[which.min(non_na_rows$year_diff), ]
        } else {
          # Get most recent year
          best_row <- non_na_rows[which.max(non_na_rows$date), ]
        }
        return(list(
          value = best_row[[code]],
          year = best_row$date
        ))
      }
    }
    return(NULL)
  }

  # Fetch each category separately to handle failures gracefully
  for (category_name in names(indicators)) {
    category_codes <- unlist(indicators[[category_name]])

    # Try batch fetch first
    fetch_result <- tryCatch({
      wb_data(
        indicator = category_codes,
        country = iso3_code,
        start_date = start_year,
        end_date = end_year,
        return_wide = TRUE
      )
    }, error = function(e) {
      log_warn(sprintf("Batch fetch failed for %s: %s", category_name, substr(e$message, 1, 80)))
      NULL
    })

    if (!is.null(fetch_result) && nrow(fetch_result) > 0) {
      # Extract values from batch result, preferring target year
      for (code in category_codes) {
        extracted <- extract_best_value(fetch_result, code, target_year)
        if (!is.null(extracted)) {
          result[[code]] <- extracted$value
          result$data_years[[code]] <- extracted$year
        }
      }
    } else {
      # Fallback: fetch each indicator individually
      for (code in category_codes) {
        single_result <- tryCatch({
          wb_data(
            indicator = code,
            country = iso3_code,
            start_date = start_year,
            end_date = end_year,
            return_wide = TRUE
          )
        }, error = function(e) {
          NULL
        })

        if (!is.null(single_result) && nrow(single_result) > 0) {
          extracted <- extract_best_value(single_result, code, target_year)
          if (!is.null(extracted)) {
            result[[code]] <- extracted$value
            result$data_years[[code]] <- extracted$year
          }
        }
      }
    }
  }

  # Count successful indicators
  all_codes <- unlist(indicators, use.names = TRUE)
  success_count <- sum(sapply(all_codes, function(code) {
    !is.null(result[[code]]) && !is.na(result[[code]])
  }))

  if (success_count == 0) {
    log_warn(sprintf("No WB data available for country: %s", iso3_code))
    return(NULL)
  }

  log_info(sprintf("Retrieved %d indicators for %s", success_count, iso3_code))
  return(result)
}

#' Get World Bank Databank data for all WBES countries
#'
#' Fetches macro indicators for all countries in WBES dataset
#' with caching support
#'
#' @param wbes_countries Vector of country names from WBES
#' @param use_cache Whether to use cached data (default: TRUE)
#' @return Data frame with WB indicators merged by iso3c
#' @export
get_wb_databank_for_wbes <- function(wbes_countries, use_cache = TRUE) {
  # Map countries to ISO3
  country_mapping <- map_wbes_countries_to_iso3(wbes_countries)
  iso3_codes <- country_mapping$iso3c[!is.na(country_mapping$iso3c)]

  if (length(iso3_codes) == 0) {
    log_warn("No valid ISO3 codes mapped from WBES countries")
    return(NULL)
  }

  # Define cache name
  cache_name <- "wb_databank_indicators"

  # Use caching if enabled
  if (use_cache) {
    # Import cache functions
    box::use(app/logic/wb_integration/wb_cache[
      is_wb_cache_fresh,
      read_wb_cache,
      write_wb_cache
    ])

    if (is_wb_cache_fresh(cache_name, max_age_hours = 168)) {  # 1 week cache
      cached_data <- read_wb_cache(cache_name)
      if (!is.null(cached_data)) {
        log_info("Using cached WB Databank data")
        return(cached_data)
      }
    }
  }

  # Fetch fresh data
  indicators <- get_wb_databank_indicators()
  all_codes <- unlist(indicators)

  wb_data_result <- fetch_wb_indicators_batch(
    iso3_codes = iso3_codes,
    indicators = all_codes,
    start_year = 2006  # Match WBES coverage
  )

  if (!is.null(wb_data_result) && use_cache) {
    box::use(app/logic/wb_integration/wb_cache[write_wb_cache])
    write_wb_cache(cache_name, wb_data_result)
  }

  return(wb_data_result)
}

#' Format World Bank indicator value for display
#'
#' Formats values with appropriate units and precision
#'
#' @param value Numeric value
#' @param indicator_code WB indicator code
#' @return Formatted string
#' @export
format_wb_indicator <- function(value, indicator_code) {
  if (is.na(value)) return("N/A")

  # Define formatting rules by indicator
  format_rules <- list(
    "NY.GDP.PCAP.CD" = list(prefix = "$", suffix = "", decimals = 0, scale = 1),
    "NY.GDP.PCAP.KD.ZG" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "FP.CPI.TOTL.ZG" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "NY.GNP.PCAP.CD" = list(prefix = "$", suffix = "", decimals = 0, scale = 1),
    "NE.EXP.GNFS.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "NE.IMP.GNFS.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "BX.KLT.DINV.CD.WD" = list(prefix = "$", suffix = "B", decimals = 1, scale = 1e-9),
    "EG.ELC.ACCS.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "IT.NET.USER.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "IT.CEL.SETS.P2" = list(prefix = "", suffix = "/100", decimals = 0, scale = 1),
    "FS.AST.PRVT.GD.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "SL.UEM.TOTL.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "SL.TLF.CACT.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "SL.TLF.CACT.FE.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1),
    "GE.EST" = list(prefix = "", suffix = "", decimals = 2, scale = 1),
    "RQ.EST" = list(prefix = "", suffix = "", decimals = 2, scale = 1),
    "RL.EST" = list(prefix = "", suffix = "", decimals = 2, scale = 1),
    "CC.EST" = list(prefix = "", suffix = "", decimals = 2, scale = 1),
    "PV.EST" = list(prefix = "", suffix = "", decimals = 2, scale = 1),
    "VA.EST" = list(prefix = "", suffix = "", decimals = 2, scale = 1),
    "IC.REG.DURS" = list(prefix = "", suffix = " days", decimals = 0, scale = 1),
    "IC.REG.COST.PC.ZS" = list(prefix = "", suffix = "%", decimals = 1, scale = 1)
  )

  rule <- format_rules[[indicator_code]]
  if (is.null(rule)) {
    # Default formatting
    return(format(round(value, 1), big.mark = ","))
  }

  scaled_value <- value * rule$scale
  formatted <- format(round(scaled_value, rule$decimals), big.mark = ",", nsmall = rule$decimals)
  paste0(rule$prefix, formatted, rule$suffix)
}

# ============================================================
# WB Data Prefetching for App Startup
# ============================================================

#' Prefetch World Bank data for all WBES countries
#'
#' Fetches WB Databank indicators for all countries in WBES dataset at startup.
#' This avoids repeated API calls when users navigate between modules.
#' Data is organized by country ISO3 code for easy lookup.
#'
#' @param wbes_countries Vector of country names from WBES data
#' @param timeout_seconds Timeout in seconds for the entire operation (default: 300 = 5 minutes)
#' @param progress_callback Optional function(current, total, country) for progress updates
#' @return Named list with WB data indexed by country name, or NULL if fetch fails
#' @export
prefetch_wb_data_for_countries <- function(wbes_countries, timeout_seconds = 300, progress_callback = NULL) {
  log_info(sprintf("Prefetching WB data for %d countries (timeout: %d seconds)...",
                   length(wbes_countries), timeout_seconds))

  # Map WBES countries to ISO3 codes
  country_mapping <- map_wbes_countries_to_iso3(wbes_countries)
  valid_mapping <- country_mapping[!is.na(country_mapping$iso3c), ]

  if (nrow(valid_mapping) == 0) {
    log_warn("No valid country mappings found for prefetch")
    return(NULL)
  }

  log_info(sprintf("Found %d valid ISO3 mappings out of %d countries",
                   nrow(valid_mapping), length(wbes_countries)))

  # Get all indicator codes
  indicators <- get_wb_databank_indicators()
  all_codes <- unlist(indicators)

  # Set timeout options
  old_timeout <- getOption("timeout")
  options(timeout = timeout_seconds)

  # Fetch data for all countries using batch API call
  result <- tryCatch({
    # Use batch fetch for efficiency - one API call for all countries
    log_info("Fetching WB indicators in batch for all countries...")

    batch_data <- fetch_wb_indicators_batch(
      iso3_codes = valid_mapping$iso3c,
      indicators = all_codes,
      start_year = 2006  # Match WBES coverage
    )

    if (is.null(batch_data) || nrow(batch_data) == 0) {
      log_warn("Batch fetch returned no data")
      return(NULL)
    }

    log_info(sprintf("Batch fetch successful: %d rows of data", nrow(batch_data)))

    # Organize data by country for easy lookup
    # Create a named list with country names as keys
    prefetched <- list()
    prefetched$raw_data <- batch_data
    prefetched$country_mapping <- valid_mapping
    prefetched$indicators <- indicators
    prefetched$fetch_time <- Sys.time()

    # Create lookup by ISO3 code
    prefetched$by_iso3 <- split(batch_data, batch_data$iso3c)

    # Create lookup by country name (WBES format)
    prefetched$by_country <- list()
    for (i in seq_len(nrow(valid_mapping))) {
      country_name <- valid_mapping$country[i]
      iso3_code <- valid_mapping$iso3c[i]
      if (iso3_code %in% names(prefetched$by_iso3)) {
        prefetched$by_country[[country_name]] <- prefetched$by_iso3[[iso3_code]]
      }
    }

    log_info(sprintf("WB data prefetch complete: %d countries cached",
                     length(prefetched$by_country)))

    prefetched

  }, error = function(e) {
    log_error(sprintf("WB data prefetch failed: %s", e$message))
    NULL
  }, finally = {
    # Restore original timeout
    options(timeout = old_timeout)
  })

  result
}

#' Get cached WB country context from prefetched data
#'
#' Retrieves WB indicators for a single country from the prefetched cache.
#' Falls back to live API call if not in cache.
#'
#' @param iso3_code ISO3 country code
#' @param prefetched_data Data from prefetch_wb_data_for_countries()
#' @param target_year Target year for data extraction (WBES survey year)
#' @param fallback_to_api If TRUE, fetch from API when not in cache
#' @return Named list with indicator values matching get_wb_country_context() format
#' @export
get_wb_context_from_cache <- function(iso3_code, prefetched_data, target_year = NULL,
                                       fallback_to_api = TRUE) {
  # Check if we have cached data for this country
  if (!is.null(prefetched_data) && !is.null(prefetched_data$by_iso3[[iso3_code]])) {
    country_data <- prefetched_data$by_iso3[[iso3_code]]

    # Debug: log available columns
    available_cols <- names(country_data)
    log_info(sprintf("Cache lookup for %s: found %d rows, %d columns",
                     iso3_code, nrow(country_data), length(available_cols)))

    # Build result in same format as get_wb_country_context()
    result <- list(
      iso3c = iso3_code,
      target_year = target_year,
      data_years = list()
    )

    # Extract values for each indicator
    indicators <- get_wb_databank_indicators()
    all_codes <- unlist(indicators)

    # Check which indicator columns are actually present
    present_codes <- intersect(all_codes, available_cols)
    log_info(sprintf("Found %d/%d indicator columns in cache for %s",
                     length(present_codes), length(all_codes), iso3_code))

    for (code in all_codes) {
      if (code %in% available_cols) {
        extracted <- extract_cached_value(country_data, code, target_year)
        if (!is.null(extracted)) {
          result[[code]] <- extracted$value
          result$data_years[[code]] <- extracted$year
        }
      }
    }

    # Count successful indicators
    success_count <- sum(sapply(all_codes, function(code) {
      !is.null(result[[code]]) && !is.na(result[[code]])
    }))

    if (success_count > 0) {
      log_info(sprintf("Retrieved %d cached indicators for %s", success_count, iso3_code))
      return(result)
    } else {
      log_warn(sprintf("No valid indicators extracted from cache for %s", iso3_code))
    }
  } else {
    log_warn(sprintf("No cached data found for %s", iso3_code))
  }

  # Fallback to live API call if requested
  if (fallback_to_api) {
    log_info(sprintf("Cache miss for %s, fetching from API...", iso3_code))
    return(get_wb_country_context(iso3_code, target_year = target_year))
  }

  NULL
}

#' Extract best value from cached data for a specific indicator
#'
#' @param cached_data Data frame with WB data for a country
#' @param indicator_code WB indicator code
#' @param target_year Target year (NULL for latest)
#' @return List with value and year, or NULL
#' @export
extract_cached_value <- function(cached_data, indicator_code, target_year = NULL) {
  # Check if indicator exists in the data
  if (!indicator_code %in% names(cached_data)) {
    return(NULL)
  }

  # Filter to non-NA values for this indicator
  indicator_values <- cached_data[[indicator_code]]
  non_na_idx <- !is.na(indicator_values)

  if (!any(non_na_idx)) {
    return(NULL)
  }

  non_na_rows <- cached_data[non_na_idx, , drop = FALSE]

  if (nrow(non_na_rows) == 0) {
    return(NULL)
  }

  # Safely extract and convert dates
  dates <- non_na_rows$date
  if (is.null(dates) || length(dates) == 0) {
    # If no date column, just return first non-NA value
    return(list(
      value = non_na_rows[[indicator_code]][1],
      year = NA
    ))
  }

  # Convert dates to numeric, handling various formats
  numeric_dates <- suppressWarnings(as.numeric(dates))
  if (all(is.na(numeric_dates))) {
    # Dates might be character or Date class, try to extract year
    numeric_dates <- suppressWarnings(as.numeric(format(as.Date(dates), "%Y")))
  }

  if (all(is.na(numeric_dates))) {
    # Still can't parse dates, return first available value
    return(list(
      value = non_na_rows[[indicator_code]][1],
      year = dates[1]
    ))
  }

  # Find best row based on target year or most recent
  if (!is.null(target_year)) {
    target_yr <- suppressWarnings(as.numeric(target_year))
    if (!is.na(target_yr)) {
      year_diffs <- abs(numeric_dates - target_yr)
      best_idx <- which.min(year_diffs)
    } else {
      best_idx <- which.max(numeric_dates)
    }
  } else {
    # Get most recent year
    best_idx <- which.max(numeric_dates)
  }

  list(
    value = non_na_rows[[indicator_code]][best_idx],
    year = dates[best_idx]
  )
}

#' Get human-readable label for World Bank indicator
#'
#' @param indicator_code WB indicator code
#' @return Human-readable label
#' @export
get_wb_indicator_label <- function(indicator_code) {
  labels <- list(
    # Economic
    "NY.GDP.PCAP.CD" = "GDP per Capita",
    "NY.GDP.PCAP.KD.ZG" = "GDP Growth",
    "FP.CPI.TOTL.ZG" = "Inflation Rate",
    "NY.GNP.PCAP.CD" = "GNI per Capita",
    # Trade
    "NE.EXP.GNFS.ZS" = "Exports (% GDP)",
    "NE.IMP.GNFS.ZS" = "Imports (% GDP)",
    "BX.KLT.DINV.CD.WD" = "FDI Net Inflows",
    # Infrastructure
    "EG.ELC.ACCS.ZS" = "Electricity Access",
    "IT.NET.USER.ZS" = "Internet Users",
    "IT.CEL.SETS.P2" = "Mobile Subscriptions",
    # Finance
    "FS.AST.PRVT.GD.ZS" = "Domestic Credit (% GDP)",
    "FB.CBK.BRCH.P5" = "Bank Branches per 100k",
    # Labor
    "SL.UEM.TOTL.ZS" = "Unemployment Rate",
    "SL.TLF.CACT.ZS" = "Labor Force Participation",
    "SL.TLF.CACT.FE.ZS" = "Female Labor Force",
    # Governance (WGI)
    "GE.EST" = "Gov't Effectiveness",
    "RQ.EST" = "Regulatory Quality",
    "RL.EST" = "Rule of Law",
    "CC.EST" = "Control of Corruption",
    "PV.EST" = "Political Stability",
    "VA.EST" = "Voice & Accountability",
    # Business
    "IC.REG.DURS" = "Days to Start Business",
    "IC.REG.COST.PC.ZS" = "Cost to Start Business"
  )

  label <- labels[[indicator_code]]
  if (is.null(label)) {
    return(indicator_code)
  }
  label
}
