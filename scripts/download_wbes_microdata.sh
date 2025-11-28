#!/bin/bash
# scripts/download_wbes_microdata.sh
# Download WBES microdata files using authenticated session

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DATA_DIR="data"
COOKIE_FILE=".wbes_cookies.txt"
LOGIN_URL="https://login.enterprisesurveys.org/login"

echo -e "${GREEN}=== WBES Microdata Downloader ===${NC}\n"

# Check if credentials are provided
if [ -z "$WBES_EMAIL" ] || [ -z "$WBES_PASSWORD" ]; then
    echo -e "${YELLOW}Please provide your WBES credentials:${NC}"
    read -p "Email: " WBES_EMAIL
    read -s -p "Password: " WBES_PASSWORD
    echo ""
fi

# Create data directory
mkdir -p "$DATA_DIR"

echo -e "\n${GREEN}Step 1: Authenticating...${NC}"

# Login and save cookies
curl -s -c "$COOKIE_FILE" \
    -X POST "$LOGIN_URL" \
    -d "email=$WBES_EMAIL" \
    -d "password=$WBES_PASSWORD" \
    -d "remember=1" \
    > /dev/null

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Authentication successful${NC}"
else
    echo -e "${RED}✗ Authentication failed${NC}"
    rm -f "$COOKIE_FILE"
    exit 1
fi

echo -e "\n${GREEN}Step 2: Downloading microdata files...${NC}\n"

# Define datasets to download
# Format: "country_code year catalog_id"
declare -a DATASETS=(
    "KEN 2018 3798"
    "KEN 2013 2378"
    "NGA 2014 2987"
    "GHA 2013 2367"
    "ZAF 2020 4010"
    "ETH 2015 2954"
    "TZA 2013 2362"
    "UGA 2013 2361"
    "RWA 2019 3906"
    "SEN 2014 2987"
)

# Download each dataset
for dataset in "${DATASETS[@]}"; do
    read -r country year catalog_id <<< "$dataset"

    filename="${country}_${year}.dta"
    download_url="https://microdata.enterprisesurveys.org/index.php/catalog/${catalog_id}/download/1"

    echo -e "Downloading ${YELLOW}${country} ${year}${NC}..."

    curl -s -b "$COOKIE_FILE" \
        -L "$download_url" \
        -o "$DATA_DIR/$filename"

    if [ -f "$DATA_DIR/$filename" ] && [ -s "$DATA_DIR/$filename" ]; then
        size=$(du -h "$DATA_DIR/$filename" | cut -f1)
        echo -e "${GREEN}✓ Downloaded: ${filename} (${size})${NC}"
    else
        echo -e "${RED}✗ Failed: ${filename}${NC}"
    fi
done

# Cleanup
rm -f "$COOKIE_FILE"

echo -e "\n${GREEN}=== Download Complete ===${NC}"
echo -e "\nFiles saved to: ${GREEN}$DATA_DIR/${NC}"
echo -e "Total files: ${GREEN}$(ls -1 $DATA_DIR/*.dta 2>/dev/null | wc -l)${NC}\n"

echo -e "${YELLOW}Next steps:${NC}"
echo -e "  1. Restart your Shiny app"
echo -e "  2. The app will automatically detect and use the microdata files\n"
