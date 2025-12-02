#!/usr/bin/env Rscript
# Script to download WBES microdata from Google Drive
# This script downloads the assets.zip file containing the real WBES microdata

# Configuration
GDRIVE_FILE_ID <- "1CFQ3djtgNlfKKB_2Ru7GHvkRGaKYkeuZ"
GDRIVE_URL <- sprintf("https://drive.google.com/uc?export=download&id=%s", GDRIVE_FILE_ID)

# Get script directory and paths
script_dir <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1]), mustWork = FALSE))
if (length(script_dir) == 0 || script_dir == ".") {
  script_dir <- getwd()
}
project_root <- dirname(script_dir)
data_dir <- file.path(project_root, "data")
assets_zip <- file.path(data_dir, "assets.zip")

cat(rep("=", 60), "\n", sep = "")
cat("WBES Dashboard - Data Download Script (R)\n")
cat(rep("=", 60), "\n", sep = "")
cat("\n")
cat("This script will download the real WBES microdata\n")
cat("from Google Drive and set it up for the dashboard.\n")
cat("\n")

# Check if file already exists
if (file.exists(assets_zip)) {
  cat("⚠️  WARNING: assets.zip already exists in data/\n")
  cat("\n")
  response <- readline(prompt = "Do you want to overwrite it? (y/N): ")
  if (!tolower(trimws(response)) %in% c("y", "yes")) {
    cat("Download cancelled.\n")
    quit(status = 0)
  }
  cat("Removing existing assets.zip...\n")
  file.remove(assets_zip)
}

# Create data directory if it doesn't exist
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

cat("\n")
cat("Downloading assets.zip from Google Drive...\n")
cat("File ID:", GDRIVE_FILE_ID, "\n")
cat("Destination:", assets_zip, "\n")
cat("\n")

# Download function
download_success <- FALSE
error_msg <- NULL

# Method 1: Try using curl (via download.file with method="curl")
if (!download_success) {
  cat("Attempting download with curl method...\n")
  tryCatch({
    download.file(
      url = GDRIVE_URL,
      destfile = assets_zip,
      method = "curl",
      quiet = FALSE,
      mode = "wb"
    )
    if (file.exists(assets_zip) && file.size(assets_zip) > 0) {
      cat("✅ Download successful with curl!\n")
      download_success <- TRUE
    }
  }, error = function(e) {
    error_msg <<- e$message
    cat("Curl method failed:", e$message, "\n")
  })
}

# Method 2: Try using wget (via download.file with method="wget")
if (!download_success) {
  cat("Attempting download with wget method...\n")
  tryCatch({
    download.file(
      url = GDRIVE_URL,
      destfile = assets_zip,
      method = "wget",
      quiet = FALSE,
      mode = "wb"
    )
    if (file.exists(assets_zip) && file.size(assets_zip) > 0) {
      cat("✅ Download successful with wget!\n")
      download_success <- TRUE
    }
  }, error = function(e) {
    error_msg <<- e$message
    cat("Wget method failed:", e$message, "\n")
  })
}

# Method 3: Try using libcurl (via download.file with method="libcurl")
if (!download_success) {
  cat("Attempting download with libcurl method...\n")
  tryCatch({
    download.file(
      url = GDRIVE_URL,
      destfile = assets_zip,
      method = "libcurl",
      quiet = FALSE,
      mode = "wb"
    )
    if (file.exists(assets_zip) && file.size(assets_zip) > 0) {
      cat("✅ Download successful with libcurl!\n")
      download_success <- TRUE
    }
  }, error = function(e) {
    error_msg <<- e$message
    cat("Libcurl method failed:", e$message, "\n")
  })
}

# Method 4: Try using googledrive package if available
if (!download_success) {
  if (requireNamespace("googledrive", quietly = TRUE)) {
    cat("Attempting download with googledrive package...\n")
    tryCatch({
      googledrive::drive_deauth()  # No authentication needed for public files
      googledrive::drive_download(
        as_id(GDRIVE_FILE_ID),
        path = assets_zip,
        overwrite = TRUE
      )
      if (file.exists(assets_zip) && file.size(assets_zip) > 0) {
        cat("✅ Download successful with googledrive package!\n")
        download_success <- TRUE
      }
    }, error = function(e) {
      error_msg <<- e$message
      cat("googledrive package method failed:", e$message, "\n")
    })
  }
}

# Check if download was successful
if (!download_success) {
  cat("\n")
  cat("❌ Automatic download failed.\n")
  cat("\n")
  cat("Please download manually:\n")
  cat("1. Visit: https://drive.google.com/file/d/", GDRIVE_FILE_ID, "/view?usp=sharing\n", sep = "")
  cat("2. Click 'Download' button\n")
  cat("3. Save as:", assets_zip, "\n")
  cat("\n")
  cat("Or install googledrive package for easier downloads:\n")
  cat("  install.packages('googledrive')\n")
  cat("  Then run this script again\n")
  cat("\n")
  quit(status = 1)
}

# Verify the downloaded file
cat("\n")
cat("Verifying downloaded file...\n")

if (!file.exists(assets_zip)) {
  cat("❌ Error: File not found after download\n")
  quit(status = 1)
}

file_size <- file.size(assets_zip)
if (file_size < 1000000) {  # Less than 1MB is suspicious
  cat("⚠️  Warning: File size is suspiciously small (", file_size, " bytes)\n", sep = "")
  cat("    This might be an error page instead of the actual file.\n")
  cat("    Try downloading manually from the Google Drive link.\n")
  quit(status = 1)
}

# Format file size
if (file_size > 1024^3) {
  size_str <- sprintf("%.2f GB", file_size / 1024^3)
} else if (file_size > 1024^2) {
  size_str <- sprintf("%.2f MB", file_size / 1024^2)
} else if (file_size > 1024) {
  size_str <- sprintf("%.2f KB", file_size / 1024)
} else {
  size_str <- sprintf("%d bytes", file_size)
}

cat("✅ File size:", size_str, "\n")

# Check if it's a valid ZIP file
tryCatch({
  zip_contents <- unzip(assets_zip, list = TRUE)
  cat("✅ Valid ZIP file\n")
  cat("\n")
  cat("ZIP file contents:\n")
  for (i in 1:nrow(zip_contents)) {
    cat(sprintf("  - %s (%s)\n",
                zip_contents$Name[i],
                format(zip_contents$Length[i], big.mark = ",")))
  }
}, error = function(e) {
  cat("❌ Error: Downloaded file is not a valid ZIP archive\n")
  cat("    Try downloading manually from the Google Drive link.\n")
  cat("    Error:", e$message, "\n")
  quit(status = 1)
})

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("✅ SUCCESS! Data download complete\n")
cat(rep("=", 60), "\n", sep = "")
cat("\n")
cat("The WBES microdata is now available at:\n")
cat(" ", assets_zip, "\n")
cat("\n")
cat("Next steps:\n")
cat("1. Start the dashboard: R -e 'rhino::app()'\n")
cat("2. The app will automatically load the real data\n")
cat("3. Check the logs for 'Found assets.zip - loading combined microdata'\n")
cat("\n")
cat("To verify the data was loaded correctly:\n")
cat("  - Check metadata in the app\n")
cat("  - Look for 100,000+ observations (not ~150 from sample data)\n")
cat("  - Verify descriptive column labels are present\n")
cat("\n")
