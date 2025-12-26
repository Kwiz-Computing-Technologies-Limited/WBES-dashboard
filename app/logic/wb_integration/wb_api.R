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
