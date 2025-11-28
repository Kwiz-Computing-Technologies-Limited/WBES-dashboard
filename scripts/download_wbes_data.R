# scripts/download_wbes_data.R
# Download World Bank Enterprise Surveys Microdata
# Requires valid WBES account credentials from enterprisesurveys.org

library(httr)
library(rvest)
library(dplyr)
library(haven)
library(logger)

#' Authenticate with Enterprise Surveys portal
#' @param email Your WBES account email
#' @param password Your WBES account password
#' @return Session handle with authentication cookies
wbes_login <- function(email, password) {

  log_info("Authenticating with Enterprise Surveys portal...")

  # Login URL
  login_url <- "https://login.enterprisesurveys.org/login"

  # Create session
  session <- session(login_url)

  # Get login form
  form <- session %>%
    html_form() %>%
    .[[1]]

  # Fill in credentials
  filled_form <- form %>%
    html_form_set(
      email = email,
      password = password
    )

  # Submit login
  response <- session_submit(session, filled_form)

  if (status_code(response) == 200) {
    log_info("Successfully authenticated")
    return(response)
  } else {
    log_error("Authentication failed")
    stop("Login failed. Please check your credentials.")
  }
}

#' Download microdata file from WBES portal
#' @param session Authenticated session
#' @param country_code ISO3 country code (e.g., "KEN", "NGA")
#' @param year Survey year
#' @param output_dir Directory to save files
#' @return Path to downloaded file
download_country_data <- function(session, country_code, year, output_dir = "data/") {

  log_info(paste("Downloading data for", country_code, year))

  # Construct download URL
  # Format: https://microdata.enterprisesurveys.org/index.php/catalog/{id}/get-microdata
  # Note: Actual URL structure depends on WBES portal API

  download_url <- sprintf(
    "https://microdata.enterprisesurveys.org/index.php/catalog/download/%s_%s",
    country_code, year
  )

  # Download file
  output_file <- file.path(output_dir, sprintf("%s_%s.dta", country_code, year))

  GET(
    download_url,
    write_disk(output_file, overwrite = TRUE),
    handle = session$handle
  )

  if (file.exists(output_file)) {
    log_info(paste("Downloaded:", output_file))
    return(output_file)
  } else {
    log_warn(paste("Failed to download:", country_code, year))
    return(NULL)
  }
}

#' Get list of available datasets from WBES catalog
#' @param session Authenticated session
#' @return Data frame with available surveys
get_available_surveys <- function(session) {

  catalog_url <- "https://www.enterprisesurveys.org/en/survey-datasets"

  page <- read_html(session_jump_to(session, catalog_url))

  # Parse catalog page for available surveys
  # This would need to be customized based on actual page structure

  surveys <- data.frame(
    country = character(),
    country_code = character(),
    year = integer(),
    catalog_id = character(),
    stringsAsFactors = FALSE
  )

  return(surveys)
}

#' Main function to download WBES data
#' @param email WBES account email
#' @param password WBES account password
#' @param countries Vector of ISO3 country codes (NULL for all available)
#' @param years Vector of years (NULL for latest)
#' @param output_dir Output directory
#' @export
download_wbes_data <- function(email, password,
                               countries = NULL,
                               years = NULL,
                               output_dir = "data/") {

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Authenticate
  session <- wbes_login(email, password)

  # Get available surveys
  log_info("Fetching available datasets...")
  available <- get_available_surveys(session)

  # Filter by requested countries and years
  if (!is.null(countries)) {
    available <- available %>% filter(country_code %in% countries)
  }
  if (!is.null(years)) {
    available <- available %>% filter(year %in% years)
  }

  # Download each dataset
  downloaded <- list()
  for (i in 1:nrow(available)) {
    file <- download_country_data(
      session,
      available$country_code[i],
      available$year[i],
      output_dir
    )
    if (!is.null(file)) {
      downloaded <- c(downloaded, file)
    }
  }

  log_info(paste("Downloaded", length(downloaded), "datasets"))

  return(invisible(downloaded))
}

# Example usage (commented out):
# download_wbes_data(
#   email = "your.email@example.com",
#   password = "your_password",
#   countries = c("KEN", "NGA", "GHA", "ZAF", "ETH"),
#   years = c(2018, 2019, 2020, 2021, 2022, 2023),
#   output_dir = "data/"
# )
