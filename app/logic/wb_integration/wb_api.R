# app/logic/wb_integration/wb_api.R
# World Bank API Integration - Core API functions
# Uses wbstats package to fetch World Bank data

box::use(
  wbstats[wb_cachelist, wb_data, wb_search],
  dplyr[...],
  tidyr[...],
  logger[log_info, log_warn, log_error],
  countrycode[countrycode],
  here[here]
)

#' Get World Bank income classifications for all countries
#'
#' Fetches current income level classifications from World Bank API
#' Returns: Low income, Lower middle income, Upper middle income, High income
#'
#' @return Data frame with columns: iso3c, country_name, income_level
#' @export
get_wb_income_classifications <- function() {
  log_info("Fetching World Bank income classifications...")

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
        income_level = case_when(
          income_level == "Low income" ~ "Low",
          income_level == "Lower middle income" ~ "Lower-Middle",
          income_level == "Upper middle income" ~ "Upper-Middle",
          income_level == "High income" ~ "High",
          TRUE ~ income_level
        )
      )

    log_info(sprintf("Fetched income classifications for %d countries", nrow(income_data)))

    return(income_data)

  }, error = function(e) {
    log_error(paste("Failed to fetch WB income classifications:", e$message))
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
#' Merges WB income data into WBES dataset using ISO3 country codes
#'
#' @param wbes_data WBES data frame with country column
#' @return WBES data frame with added income_level column
#' @export
enrich_wbes_with_income <- function(wbes_data) {
  log_info("Enriching WBES data with World Bank income classifications...")

  # Get WB income data
  wb_income <- get_wb_income_classifications()
  if (is.null(wb_income)) {
    log_error("Cannot enrich data - WB income fetch failed")
    return(wbes_data)
  }

  # Map WBES countries to ISO3
  country_iso_map <- map_wbes_countries_to_iso3(wbes_data$country)

  # Merge ISO3 codes into WBES data
  wbes_with_iso3 <- wbes_data |>
    left_join(country_iso_map, by = "country")

  # Merge income classifications
  wbes_enriched <- wbes_with_iso3 |>
    left_join(
      wb_income |> select(iso3c, wb_income_level = income_level),
      by = "iso3c"
    ) |>
    mutate(
      # Use WB income level if available, fallback to existing income column
      income = coalesce(wb_income_level, income)
    ) |>
    select(-wb_income_level)  # Remove temporary column

  # Log enrichment results
  total_rows <- nrow(wbes_enriched)
  enriched_rows <- sum(!is.na(wbes_enriched$income))

  log_info(sprintf(
    "Income enrichment complete: %d/%d rows enriched (%.1f%%)",
    enriched_rows,
    total_rows,
    (enriched_rows / total_rows) * 100
  ))

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
