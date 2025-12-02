# Extract variable labels from WBES .dta file
# This script reads the microdata and extracts variable labels for mapping

library(haven)

# Function to extract all variable labels from a .dta file
extract_all_labels <- function(dta_path, encoding = "latin1") {

  cat("Reading:", dta_path, "\n")
  df <- read_dta(dta_path, encoding = encoding)

  cat("Data structure:\n")
  cat(sprintf("  Observations: %d\n", nrow(df)))
  cat(sprintf("  Variables: %d\n", ncol(df)))

  # Extract labels for all variables
  labels <- sapply(names(df), function(varname) {
    label <- attr(df[[varname]], "label")
    if (is.null(label) || length(label) == 0) {
      return(NA_character_)
    }
    return(as.character(label))
  })

  # Create a data frame of variable mappings
  var_info <- data.frame(
    variable = names(df),
    label = labels,
    type = sapply(df, class),
    stringsAsFactors = FALSE
  )

  # Show sample of data for each variable to understand scales
  cat("\nVariable Labels and Sample Values:\n")
  cat(strrep("=", 80), "\n")

  for (i in 1:min(50, nrow(var_info))) {
    var <- var_info$variable[i]
    label <- var_info$label[i]

    # Get sample values (non-NA)
    sample_vals <- df[[var]][!is.na(df[[var]])]
    if (length(sample_vals) > 0) {
      sample_vals <- head(unique(sample_vals), 10)
      sample_str <- paste(sample_vals, collapse = ", ")
    } else {
      sample_str <- "All NA"
    }

    # Get value range if numeric
    if (is.numeric(df[[var]])) {
      range_vals <- range(df[[var]], na.rm = TRUE)
      range_str <- sprintf(" [Range: %.2f to %.2f]", range_vals[1], range_vals[2])
    } else {
      range_str <- ""
    }

    cat(sprintf("%s: %s%s\n", var, ifelse(is.na(label), "(no label)", label), range_str))
    cat(sprintf("  Sample: %s\n\n", sample_str))
  }

  return(var_info)
}

# Main execution
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    cat("Usage: Rscript extract_labels.R <path_to_dta_file>\n")
    cat("Example: Rscript extract_labels.R data/.extracted/data.dta\n")
    quit(status = 1)
  }

  dta_file <- args[1]

  if (!file.exists(dta_file)) {
    cat("Error: File not found:", dta_file, "\n")
    quit(status = 1)
  }

  var_info <- extract_all_labels(dta_file)

  # Save to CSV for reference
  output_file <- "variable_labels.csv"
  write.csv(var_info, output_file, row.names = FALSE)
  cat("\nVariable labels saved to:", output_file, "\n")
}
