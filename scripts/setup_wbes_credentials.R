# scripts/setup_wbes_credentials.R
# Configure WBES authentication credentials

#' Setup WBES credentials
#'
#' This function helps you securely store your WBES portal credentials
#' for automated data downloads.
#'
#' @param email Your WBES account email
#' @param password Your WBES account password (will be stored in .Renviron)
#' @export
setup_wbes_credentials <- function(email = NULL, password = NULL) {

  cat("=== WBES Credentials Setup ===\n\n")

  # Interactive mode if credentials not provided
  if (is.null(email)) {
    email <- readline(prompt = "Enter your WBES email: ")
  }

  if (is.null(password)) {
    password <- readline(prompt = "Enter your WBES password: ")
  }

  # Path to .Renviron file
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

  # Read existing .Renviron
  if (file.exists(renviron_path)) {
    renviron_lines <- readLines(renviron_path)
  } else {
    renviron_lines <- character()
  }

  # Remove old WBES credentials if they exist
  renviron_lines <- renviron_lines[!grepl("^WBES_EMAIL=|^WBES_PASSWORD=", renviron_lines)]

  # Add new credentials
  new_lines <- c(
    renviron_lines,
    "",
    "# World Bank Enterprise Surveys Credentials",
    sprintf("WBES_EMAIL=%s", email),
    sprintf("WBES_PASSWORD=%s", password)
  )

  # Write to .Renviron
  writeLines(new_lines, renviron_path)

  cat("\n✓ Credentials saved to", renviron_path, "\n")
  cat("✓ Restart R session for changes to take effect\n")
  cat("\nTo use credentials in R:\n")
  cat("  Sys.getenv('WBES_EMAIL')\n")
  cat("  Sys.getenv('WBES_PASSWORD')\n\n")

  invisible(TRUE)
}

#' Get WBES credentials from environment
#' @return List with email and password
#' @export
get_wbes_credentials <- function() {
  email <- Sys.getenv("WBES_EMAIL")
  password <- Sys.getenv("WBES_PASSWORD")

  if (email == "" || password == "") {
    stop(
      "WBES credentials not found. Please run:\n",
      "  source('scripts/setup_wbes_credentials.R')\n",
      "  setup_wbes_credentials()"
    )
  }

  list(email = email, password = password)
}

# If running interactively, prompt for setup
if (interactive() && !exists(".wbes_credentials_setup")) {
  cat("\nWould you like to setup WBES credentials now? (y/n): ")
  response <- readline()
  if (tolower(response) == "y") {
    setup_wbes_credentials()
  }
  .wbes_credentials_setup <- TRUE
}
