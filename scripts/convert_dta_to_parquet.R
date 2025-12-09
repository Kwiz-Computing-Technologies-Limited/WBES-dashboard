#!/usr/bin/env Rscript
# Convert .dta files to Parquet format for zero-extraction ZIP reading
#
# This script converts Stata .dta files to Parquet format, which enables
# the dashboard to read data directly from assets.zip without any disk extraction.
#
# Usage:
#   Rscript scripts/convert_dta_to_parquet.R
#
# This will:
#   1. Extract .dta file(s) from assets.zip
#   2. Convert to Parquet format
#   3. Create new assets_parquet.zip with Parquet files
#   4. Optionally replace original assets.zip

library(haven)
library(arrow)
library(logger)

log_info("WBES .dta to Parquet Converter")
log_info("=" %R% 50)

# Configuration
data_dir <- here::here("data")
assets_zip <- file.path(data_dir, "assets.zip")
output_zip <- file.path(data_dir, "assets_parquet.zip")
temp_dir <- tempfile("wbes_convert_")

# Check if assets.zip exists
if (!file.exists(assets_zip)) {
  stop("ERROR: assets.zip not found in data/ directory")
}

log_info(paste("Input ZIP:", assets_zip))
log_info(paste("Output ZIP:", output_zip))

# Create temp directory
dir.create(temp_dir, showWarnings = FALSE)
log_info(paste("Working directory:", temp_dir))

# Extract .dta files from ZIP
log_info("Step 1: Extracting .dta files from assets.zip...")
zip_contents <- unzip(assets_zip, list = TRUE)
dta_files <- zip_contents$Name[grepl("\\.dta$", zip_contents$Name, ignore.case = TRUE)]

if (length(dta_files) == 0) {
  stop("ERROR: No .dta files found in assets.zip")
}

log_info(paste("Found", length(dta_files), ".dta file(s)"))

# Extract .dta files
unzip(assets_zip, files = dta_files, exdir = temp_dir)
extracted_paths <- file.path(temp_dir, dta_files)

# Convert each .dta file to Parquet
log_info("Step 2: Converting .dta to Parquet...")

parquet_files <- c()

for (i in seq_along(extracted_paths)) {
  dta_file <- extracted_paths[i]
  dta_name <- basename(dta_file)
  parquet_name <- sub("\\.dta$", ".parquet", dta_name, ignore.case = TRUE)
  parquet_path <- file.path(temp_dir, parquet_name)

  log_info(paste("  Converting:", dta_name, "->", parquet_name))

  # Read .dta file
  log_info("    Reading .dta file...")
  dta_data <- read_dta(dta_file, encoding = "latin1")

  # Get file size
  dta_size_mb <- file.size(dta_file) / (1024^2)
  log_info(sprintf("    .dta size: %.2f MB", dta_size_mb))
  log_info(sprintf("    Rows: %d, Columns: %d", nrow(dta_data), ncol(dta_data)))

  # Convert factors to character (for better compatibility)
  log_info("    Converting factors to character...")
  dta_data <- dta_data %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  # Write as Parquet with ZSTD compression
  log_info("    Writing Parquet file...")
  write_parquet(dta_data, parquet_path, compression = "zstd", compression_level = 6)

  # Get Parquet file size
  parquet_size_mb <- file.size(parquet_path) / (1024^2)
  compression_ratio <- (1 - parquet_size_mb / dta_size_mb) * 100

  log_info(sprintf("    Parquet size: %.2f MB (%.1f%% compression)", parquet_size_mb, compression_ratio))

  parquet_files <- c(parquet_files, parquet_path)

  # Clean up .dta file
  unlink(dta_file)
  gc()
}

# Create new ZIP with Parquet files
log_info("Step 3: Creating assets_parquet.zip...")

# Change to temp directory so we don't include full paths in ZIP
current_dir <- getwd()
setwd(temp_dir)

parquet_names <- basename(parquet_files)
zip_result <- utils::zip(output_zip, files = parquet_names, flags = "-9jq")

setwd(current_dir)

if (zip_result == 0) {
  log_info(paste("SUCCESS: Created", output_zip))

  # Show file sizes
  original_size_mb <- file.size(assets_zip) / (1024^2)
  parquet_zip_size_mb <- file.size(output_zip) / (1024^2)
  total_compression <- (1 - parquet_zip_size_mb / original_size_mb) * 100

  log_info("")
  log_info("Conversion Summary:")
  log_info(sprintf("  Original assets.zip: %.2f MB", original_size_mb))
  log_info(sprintf("  New assets_parquet.zip: %.2f MB", parquet_zip_size_mb))
  log_info(sprintf("  Total compression: %.1f%%", total_compression))
  log_info("")

  # Ask user if they want to replace original
  log_info("Next steps:")
  log_info("  1. TEST the new file: Rename assets_parquet.zip to assets.zip")
  log_info("  2. Run the dashboard and verify it loads correctly")
  log_info("  3. If successful, delete the old .dta-based ZIP")
  log_info("")
  log_info("Manual replacement:")
  log_info(paste("  mv", assets_zip, file.path(data_dir, "assets_dta_backup.zip")))
  log_info(paste("  mv", output_zip, assets_zip))

} else {
  stop("ERROR: Failed to create ZIP file")
}

# Cleanup temp directory
unlink(temp_dir, recursive = TRUE)

log_info("Conversion complete!")
