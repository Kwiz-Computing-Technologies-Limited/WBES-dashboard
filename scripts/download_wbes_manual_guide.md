# WBES Microdata Download Guide

This guide explains how to download World Bank Enterprise Surveys microdata files for use with the dashboard.

## Option 1: Manual Download (Recommended)

### Step 1: Access the Portal

1. Go to https://www.enterprisesurveys.org/en/survey-datasets
2. Log in with your WBES credentials
3. Accept the terms of use if prompted

### Step 2: Download Country Datasets

Download `.dta` (Stata format) files for these countries:

**Priority Countries (African Focus):**
- Kenya (2018, 2013)
- Nigeria (2014)
- South Africa (2020)
- Ghana (2013)
- Ethiopia (2015)
- Tanzania (2013)
- Uganda (2013)
- Rwanda (2019)
- Senegal (2014)
- Côte d'Ivoire (2016)

**Additional Countries:**
- Egypt (2020)
- Morocco (2019)
- Tunisia (2020)
- Botswana (2019)
- Zambia (2019)
- India (2014)
- Bangladesh (2013)
- Vietnam (2015)
- Indonesia (2015)
- Brazil (2009)

### Step 3: Extract and Place Files

1. Download the zip files for each country
2. Extract the `.dta` files (usually named like `Kenya-2018-full-data.dta`)
3. Rename for consistency: `KEN_2018.dta`, `NGA_2014.dta`, etc.
4. Place all `.dta` files in the `data/` directory of your project:

```
WBES-dashboard/
└── data/
    ├── KEN_2018.dta
    ├── NGA_2014.dta
    ├── GHA_2013.dta
    ├── ZAF_2020.dta
    └── ... (other countries)
```

### Step 4: Restart the App

Once files are in place, restart your Shiny app:

```r
shiny::runApp()
```

The app will automatically detect the `.dta` files and load real data instead of sample data.

---

## Option 2: Automated Download with R

### Prerequisites

```r
install.packages(c("httr2", "rvest", "haven", "logger"))
```

### Setup Credentials

Run once to store your credentials:

```r
source("scripts/setup_wbes_credentials.R")
setup_wbes_credentials(
  email = "your.email@example.com",
  password = "your_password"
)
```

### Download Data

```r
source("scripts/download_wbes_data.R")

# Download specific countries
download_wbes_data(
  email = Sys.getenv("WBES_EMAIL"),
  password = Sys.getenv("WBES_PASSWORD"),
  countries = c("KEN", "NGA", "GHA", "ZAF", "ETH"),
  output_dir = "data/"
)
```

---

## Option 3: Shell Script (Linux/Mac)

### Make Script Executable

```bash
chmod +x scripts/download_wbes_microdata.sh
```

### Run with Credentials

```bash
export WBES_EMAIL="your.email@example.com"
export WBES_PASSWORD="your_password"
./scripts/download_wbes_microdata.sh
```

Or run interactively (will prompt for credentials):

```bash
./scripts/download_wbes_microdata.sh
```

---

## Verify Data Loading

After placing files in `data/`, check that they're being loaded:

### Check Logs

Look for this in your R console when the app starts:

```
INFO [2025-11-28 10:00:00] Loading WBES data...
INFO [2025-11-28 10:00:00] Found local microdata files
INFO [2025-11-28 10:00:01] Reading: KEN_2018.dta
INFO [2025-11-28 10:00:02] Reading: NGA_2014.dta
...
```

Instead of:

```
WARN [2025-11-28 10:00:00] API fetch failed. To use actual WBES data, please:
WARN [2025-11-28 10:00:00] Falling back to sample data...
```

### Check Data Source

In the app, look at the bottom of any page. You should see:

> Data Source: World Bank Enterprise Surveys (Microdata)

Instead of:

> Data Source: World Bank Enterprise Surveys (Sample Data)

---

## Troubleshooting

### "Login failed" Error

- Verify your email and password are correct
- Check if your account is active at https://www.enterprisesurveys.org
- Ensure you've accepted the latest terms of use

### Files Not Being Detected

Check:
```r
list.files("data/", pattern = "\\.dta$")
```

Should return a list of `.dta` files. If empty:
- Verify files are in the correct directory
- Check file extensions (must be `.dta`)
- Ensure files aren't corrupted (try opening in Stata)

### Large File Sizes

Microdata files can be large (50-200 MB each). Ensure you have:
- Sufficient disk space (~5 GB recommended for 30+ countries)
- Stable internet connection for downloads
- Patience - downloads may take 10-30 minutes

---

## Data Structure

Each `.dta` file contains firm-level microdata with variables like:

- `a0` - Country
- `a1` - Year
- `a4a` - Region
- `b8` - Sector
- `c6` - Capacity utilization
- `c30a` - Exports
- `d2` - Finance obstacle
- `d30a` - Corruption obstacle
- `e1` - Power outages
- ... and many more

The app automatically aggregates this to country-level indicators.

---

## Data Update Schedule

WBES releases new surveys periodically:
- Most countries: Every 3-5 years
- Some countries: Annual or biennial
- Check https://www.enterprisesurveys.org for latest releases

To update:
1. Download new datasets from portal
2. Place in `data/` directory
3. Restart app (it will automatically use the most recent year per country)

---

## Need Help?

- WBES Portal Support: https://www.enterprisesurveys.org/en/support
- Data Documentation: https://www.enterprisesurveys.org/en/methodology
- API Documentation: https://datahelpdesk.worldbank.org/knowledgebase/topics/125589
