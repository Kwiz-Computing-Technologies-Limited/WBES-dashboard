#!/bin/bash
# Script to download WBES microdata from Google Drive
# This script downloads the assets.zip file containing the real WBES microdata

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DATA_DIR="$PROJECT_ROOT/data"
ASSETS_ZIP="$DATA_DIR/assets.zip"

# Google Drive file ID
GDRIVE_FILE_ID="1CFQ3djtgNlfKKB_2Ru7GHvkRGaKYkeuZ"
GDRIVE_URL="https://drive.google.com/uc?export=download&id=$GDRIVE_FILE_ID"

echo "================================================"
echo "WBES Dashboard - Data Download Script"
echo "================================================"
echo ""
echo "This script will download the real WBES microdata"
echo "from Google Drive and set it up for the dashboard."
echo ""

# Check if assets.zip already exists
if [ -f "$ASSETS_ZIP" ]; then
    echo "⚠️  WARNING: assets.zip already exists in data/"
    echo ""
    read -p "Do you want to overwrite it? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Download cancelled."
        exit 0
    fi
    echo "Removing existing assets.zip..."
    rm "$ASSETS_ZIP"
fi

# Create data directory if it doesn't exist
mkdir -p "$DATA_DIR"

echo ""
echo "Downloading assets.zip from Google Drive..."
echo "URL: $GDRIVE_URL"
echo "Destination: $ASSETS_ZIP"
echo ""

# Try different download methods

# Method 1: Try curl
if command -v curl &> /dev/null; then
    echo "Attempting download with curl..."
    if curl -L "$GDRIVE_URL" -o "$ASSETS_ZIP" --max-time 300 2>&1 | tee /tmp/curl_output.log; then
        if [ -f "$ASSETS_ZIP" ] && [ -s "$ASSETS_ZIP" ]; then
            echo "✅ Download successful with curl!"
            DOWNLOAD_SUCCESS=true
        fi
    fi
fi

# Method 2: Try wget if curl failed
if [ -z "$DOWNLOAD_SUCCESS" ] && command -v wget &> /dev/null; then
    echo "Attempting download with wget..."
    if wget "$GDRIVE_URL" -O "$ASSETS_ZIP" --timeout=300 2>&1 | tee /tmp/wget_output.log; then
        if [ -f "$ASSETS_ZIP" ] && [ -s "$ASSETS_ZIP" ]; then
            echo "✅ Download successful with wget!"
            DOWNLOAD_SUCCESS=true
        fi
    fi
fi

# Method 3: Try gdown if available (best for Google Drive)
if [ -z "$DOWNLOAD_SUCCESS" ] && command -v gdown &> /dev/null; then
    echo "Attempting download with gdown..."
    if gdown "https://drive.google.com/uc?id=$GDRIVE_FILE_ID" -O "$ASSETS_ZIP"; then
        if [ -f "$ASSETS_ZIP" ] && [ -s "$ASSETS_ZIP" ]; then
            echo "✅ Download successful with gdown!"
            DOWNLOAD_SUCCESS=true
        fi
    fi
fi

# Check if download was successful
if [ -z "$DOWNLOAD_SUCCESS" ]; then
    echo ""
    echo "❌ Automatic download failed."
    echo ""
    echo "Please download manually:"
    echo "1. Visit: https://drive.google.com/file/d/$GDRIVE_FILE_ID/view?usp=sharing"
    echo "2. Click 'Download' button"
    echo "3. Save as: $ASSETS_ZIP"
    echo ""
    echo "Or install gdown for easier Google Drive downloads:"
    echo "  pip install gdown"
    echo "  Then run this script again"
    echo ""
    exit 1
fi

# Verify the downloaded file
echo ""
echo "Verifying downloaded file..."

if [ ! -f "$ASSETS_ZIP" ]; then
    echo "❌ Error: File not found after download"
    exit 1
fi

FILE_SIZE=$(stat -f%z "$ASSETS_ZIP" 2>/dev/null || stat -c%s "$ASSETS_ZIP" 2>/dev/null)
if [ "$FILE_SIZE" -lt 1000000 ]; then
    echo "⚠️  Warning: File size is suspiciously small ($FILE_SIZE bytes)"
    echo "    This might be an error page instead of the actual file."
    echo "    Try downloading manually from the Google Drive link."
    exit 1
fi

echo "✅ File size: $(numfmt --to=iec-i --suffix=B $FILE_SIZE 2>/dev/null || echo "$FILE_SIZE bytes")"

# Check if it's a valid ZIP file
if command -v unzip &> /dev/null; then
    if unzip -t "$ASSETS_ZIP" &>/dev/null; then
        echo "✅ Valid ZIP file"
    else
        echo "❌ Error: Downloaded file is not a valid ZIP archive"
        echo "    Try downloading manually from the Google Drive link."
        exit 1
    fi
fi

# List contents
echo ""
echo "ZIP file contents:"
unzip -l "$ASSETS_ZIP" 2>/dev/null || echo "Could not list contents"

echo ""
echo "================================================"
echo "✅ SUCCESS! Data download complete"
echo "================================================"
echo ""
echo "The WBES microdata is now available at:"
echo "  $ASSETS_ZIP"
echo ""
echo "Next steps:"
echo "1. Start the dashboard: R -e 'rhino::app()'"
echo "2. The app will automatically load the real data"
echo "3. Check the logs for 'Found assets.zip - loading combined microdata'"
echo ""
echo "To verify the data was loaded correctly:"
echo "  - Check metadata in the app"
echo "  - Look for 100,000+ observations (not ~150 from sample data)"
echo "  - Verify descriptive column labels are present"
echo ""
