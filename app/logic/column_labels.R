# app/logic/column_labels.R
# Column Label Management for WBES Dashboard
# Extracts and manages descriptive column names from Stata labels

box::use(
  haven[read_dta],
  labelled[var_label, set_variable_labels],
  dplyr[...],
  purrr[map_chr, imap],
  logger[log_info, log_warn]
)

#' Extract variable labels from a Stata dataset
#' @param data Data frame (from haven::read_dta)
#' @return Named vector of labels (name = column name, value = descriptive label)
#' @export
extract_column_labels <- function(data) {

  if (is.null(data) || nrow(data) == 0) {
    log_warn("Cannot extract labels from NULL or empty dataset")
    return(character(0))
  }

  log_info("Extracting variable labels from dataset...")

  # Get labels using labelled package
  labels <- labelled::var_label(data)

  # Convert to named character vector
  label_vec <- map_chr(labels, function(x) {
    if (is.null(x) || is.na(x) || x == "") {
      return(NA_character_)
    }
    as.character(x)
  })

  # Filter out NAs and keep only labeled columns
  label_vec <- label_vec[!is.na(label_vec)]

  log_info(sprintf("Extracted %d variable labels from %d columns",
                   length(label_vec), ncol(data)))

  return(label_vec)
}

#' Get descriptive label for a column
#' @param column_name Original column name
#' @param label_map Named vector of labels (from extract_column_labels)
#' @return Descriptive label or the original column name if not found
#' @export
get_column_label <- function(column_name, label_map) {
  if (column_name %in% names(label_map)) {
    return(label_map[[column_name]])
  }
  # Return cleaned version of column name if no label found
  return(clean_column_name(column_name))
}

#' Clean a column name for display (fallback when no label exists)
#' @param column_name Original column name
#' @return Human-readable version
clean_column_name <- function(column_name) {
  column_name |>
    gsub("_", " ", x = _) |>
    gsub("\\.", " ", x = _) |>
    tools::toTitleCase()
}

#' Create a complete mapping of WBES column names to descriptive labels
#' This combines extracted labels with manually defined labels for key indicators
#' @param data Optional data frame to extract labels from
#' @return Named vector of column labels
#' @export
create_wbes_label_mapping <- function(data = NULL) {

  # Start with manually defined labels for key indicators
  manual_labels <- c(
    # Country/Survey metadata
    "country" = "Country",
    "country2" = "Country Name",
    "country_code" = "Country Code",
    "wbcode" = "World Bank Code",
    "country_abr" = "Country Abbreviation",
    "year" = "Survey Year",
    "region" = "Region",
    "income" = "Income Group",
    "wt" = "Sample Weight",

    # Infrastructure indicators
    "in2" = "Number of Power Outages per Month",
    "in3" = "Average Duration of Power Outages (hours)",
    "in9" = "Firms with Generator (% of firms)",
    "infrast1" = "Power Outages Frequency",
    "infrast2" = "Power Outage Duration",
    "infrast3" = "Generator Ownership",
    "infrast4" = "Water Supply Insufficiency",
    "IC.FRM.OUTG.ZS" = "Firms Experiencing Power Outages (%)",
    "IC.FRM.ELEC.ZS" = "Electricity as Major Obstacle (%)",
    "IC.FRM.INFRA.ZS" = "Infrastructure as Major Obstacle (%)",

    # Access to Finance indicators
    "fin14" = "Firms with Credit Line (% of firms)",
    "fin15" = "Firms with Bank Account (% of firms)",
    "fin21" = "Loan Application Rejection Rate (%)",
    "fin10" = "Collateral Required for Loan (% of loan value)",
    "IC.FRM.FINA.ZS" = "Access to Finance as Major Obstacle (%)",
    "IC.FRM.BANK.ZS" = "Firms with Bank Account (%)",
    "IC.FRM.CRED.ZS" = "Firms with Credit Constraint (%)",

    # Corruption and Governance
    "graft3" = "Bribery Incidence (% of firms)",
    "corr11" = "Corruption as Major Obstacle (%)",
    "IC.FRM.CORR.ZS" = "Corruption as Major Obstacle (%)",
    "IC.FRM.BRIB.ZS" = "Bribery Incidence (%)",

    # Workforce and Gender
    "gend1" = "Female Ownership (% ownership by women)",
    "gend2" = "Female Workers (% of workforce)",
    "IC.FRM.WKFC.ZS" = "Workforce Quality as Major Obstacle (%)",
    "IC.FRM.FEMW.ZS" = "Female Workers (% of workforce)",
    "IC.FRM.FEMO.ZS" = "Female Ownership (%)",

    # Performance and Exports
    "t3" = "Capacity Utilization (%)",
    "tr10" = "Exporting Firms (% of firms)",
    "tr5" = "Direct Exports (% of sales)",
    "tr6" = "Indirect Exports (% of sales)",
    "IC.FRM.CAPU.ZS" = "Capacity Utilization (%)",
    "IC.FRM.EXPRT.ZS" = "Exporting Firms (%)",

    # Crime and Security
    "crime8" = "Crime as Major Obstacle (%)",
    "crime2" = "Security Costs (% of sales)",
    "IC.FRM.CRIM.ZS" = "Crime as Major Obstacle (%)",
    "IC.FRM.SECU.ZS" = "Security Costs (% of sales)",

    # Processed/Calculated columns
    "power_outages_per_month" = "Power Outages per Month",
    "avg_outage_duration_hrs" = "Average Outage Duration (hours)",
    "firms_with_generator_pct" = "Firms with Generator (%)",
    "firms_with_credit_line_pct" = "Firms with Credit Line (%)",
    "firms_with_bank_account_pct" = "Firms with Bank Account (%)",
    "loan_rejection_rate_pct" = "Loan Rejection Rate (%)",
    "collateral_required_pct" = "Collateral Required (% of loan)",
    "bribery_incidence_pct" = "Bribery Incidence (%)",
    "corruption_obstacle_pct" = "Corruption as Obstacle (%)",
    "capacity_utilization_pct" = "Capacity Utilization (%)",
    "export_firms_pct" = "Exporting Firms (%)",
    "export_share_pct" = "Export Share (% of sales)",
    "female_ownership_pct" = "Female Ownership (%)",
    "female_workers_pct" = "Female Workers (%)",
    "crime_obstacle_pct" = "Crime as Obstacle (%)",
    "security_costs_pct" = "Security Costs (% of sales)",
    "sample_weight" = "Sample Weight",
    "sample_size" = "Sample Size",
    "response_rate" = "Response Rate (%)",
    "data_quality_score" = "Data Quality Score"
  )

  # If data provided, extract labels from it
  if (!is.null(data)) {
    extracted_labels <- extract_column_labels(data)
    # Merge with manual labels, preferring manual labels when there's overlap
    all_labels <- c(extracted_labels, manual_labels)
    # Remove duplicates, keeping the last occurrence (manual labels)
    all_labels <- all_labels[!duplicated(names(all_labels), fromLast = TRUE)]
  } else {
    all_labels <- manual_labels
  }

  log_info(sprintf("Created label mapping with %d labels", length(all_labels)))

  return(all_labels)
}

#' Apply descriptive labels to a data frame's columns
#' Renames columns using descriptive labels where available
#' @param data Data frame to relabel
#' @param label_map Named vector of labels
#' @param keep_original If TRUE, keeps original names and only adds labels attribute
#' @return Data frame with descriptive column names or labels
#' @export
apply_descriptive_labels <- function(data, label_map, keep_original = FALSE) {

  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }

  if (keep_original) {
    # Just add labels as attribute, don't rename
    labelled::var_label(data) <- as.list(label_map[names(data)])
    return(data)
  }

  # Rename columns using labels
  new_names <- sapply(names(data), function(col) {
    get_column_label(col, label_map)
  })

  names(data) <- new_names

  log_info(sprintf("Applied descriptive labels to %d columns", ncol(data)))

  return(data)
}

#' Get a formatted label for UI display
#' @param column_name Original column name
#' @param label_map Named vector of labels
#' @param include_units If TRUE, preserves unit information in parentheses
#' @return Formatted label string
#' @export
format_label_for_ui <- function(column_name, label_map, include_units = TRUE) {
  label <- get_column_label(column_name, label_map)

  # Already formatted if it has the label
  if (include_units) {
    return(label)
  }

  # Remove units in parentheses if requested
  label <- gsub("\\s*\\([^)]+\\)\\s*$", "", label)
  return(label)
}

#' Create a data frame mapping original names to descriptive names
#' Useful for documentation and data dictionaries
#' @param label_map Named vector of labels
#' @return Data frame with columns: original_name, descriptive_label
#' @export
create_label_dictionary <- function(label_map) {
  data.frame(
    original_name = names(label_map),
    descriptive_label = unname(label_map),
    stringsAsFactors = FALSE
  ) |>
    arrange(original_name)
}

#' Get all column labels for a specific category
#' @param category Category name (e.g., "infrastructure", "finance", "corruption")
#' @param label_map Full label mapping
#' @return Named vector of labels for that category
#' @export
get_category_labels <- function(category, label_map) {

  category_patterns <- list(
    infrastructure = c("^in\\d+", "^infrast", "IC\\.FRM\\.(OUTG|ELEC|INFRA)", "power_", "generator", "water"),
    finance = c("^fin\\d+", "IC\\.FRM\\.(FINA|BANK|CRED)", "credit", "loan", "bank", "collateral"),
    corruption = c("^graft", "^corr", "IC\\.FRM\\.(CORR|BRIB)", "bribery", "corruption"),
    workforce = c("^gend", "IC\\.FRM\\.(WKFC|FEMW|FEMO)", "female_", "worker"),
    performance = c("^t\\d+", "^tr\\d+", "IC\\.FRM\\.(CAPU|EXPRT)", "capacity", "export"),
    crime = c("^crime", "IC\\.FRM\\.(CRIM|SECU)", "crime", "security")
  )

  if (!category %in% names(category_patterns)) {
    return(character(0))
  }

  patterns <- category_patterns[[category]]
  pattern_regex <- paste(patterns, collapse = "|")

  matching_labels <- label_map[grepl(pattern_regex, names(label_map), ignore.case = TRUE)]

  return(matching_labels)
}
