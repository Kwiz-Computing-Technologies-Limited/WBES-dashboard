# WBES Data Setup Guide

Your dashboard is currently using **sample data**. Follow this guide to use **actual WBES microdata** with your login credentials.

## Quick Start (Recommended)

### Option 1: Get Complete Dataset List

**Step 1:** Generate comprehensive list of ALL available WBES datasets

```r
source("scripts/fetch_all_wbes_data.R")
surveys <- main()
```

This will:
- Fetch complete catalog of 140+ countries and 400+ surveys
- Export list to `wbes_all_datasets.csv` for your reference
- Show summary of available datasets

**Step 2:** Review the complete dataset list

```r
# View the CSV file
surveys <- read.csv("wbes_all_datasets.csv")
View(surveys)
```

**Step 3:** Download datasets manually from WBES portal

Visit https://www.enterprisesurveys.org/en/survey-datasets and download the `.dta` files for countries you need.

**Step 4:** Place files in `data/` directory and restart app

```r
shiny::runApp()
```

---

### Option 2: Manual Download (Most Reliable)

If automated downloads don't work due to portal authentication complexity:

**Step 1:** Visit https://www.enterprisesurveys.org/en/survey-datasets

**Step 2:** Log in with your credentials

**Step 3:** Download `.dta` files for ALL countries available:

The WBES catalog includes 140+ countries with 400+ surveys from 2006-2024:

**Download by Region:**
- **Africa:** 50+ countries (Kenya, Nigeria, South Africa, Ghana, Ethiopia, Tanzania, Uganda, Rwanda, Senegal, etc.)
- **Asia:** 20+ countries (India, Bangladesh, Vietnam, Indonesia, Philippines, Pakistan, etc.)
- **Latin America:** 30+ countries (Brazil, Mexico, Colombia, Peru, Chile, Argentina, etc.)
- **Europe & Central Asia:** 30+ countries (Poland, Turkey, Romania, Bulgaria, Serbia, Ukraine, etc.)
- **Middle East:** 10+ countries (Egypt, Morocco, Tunisia, Jordan, Lebanon, etc.)

**To download ALL datasets:**
1. Use the "Bulk Download" option if available on the portal
2. Or download country-by-country (recommended: start with your region of interest)
3. Or use the `wbes_all_datasets.csv` file to systematically download each one

**Step 4:** Extract and rename files:
```bash
# Rename to format: COUNTRY-CODE_YEAR.dta
# Examples:
mv "Kenya-2018-full-data.dta" "data/KEN_2018.dta"
mv "Nigeria-2014-full-data.dta" "data/NGA_2014.dta"
```

**Step 5:** Restart your Shiny app

---

### Option 3: Shell Script (Linux/Mac)

```bash
# Make executable (first time only)
chmod +x scripts/download_wbes_microdata.sh

# Run with environment variables
export WBES_EMAIL="your.email@example.com"
export WBES_PASSWORD="your_password"
./scripts/download_wbes_microdata.sh
```

Or run interactively (will prompt for credentials):

```bash
./scripts/download_wbes_microdata.sh
```

---

## Verify Data is Loading

### Check Console Logs

When you start the app, look for:

✅ **SUCCESS** - Using real data:
```
INFO [2025-11-28 10:00:00] Loading WBES data...
INFO [2025-11-28 10:00:00] Found local microdata files
INFO [2025-11-28 10:00:01] Loading 10 microdata files
INFO [2025-11-28 10:00:01] Reading: KEN_2018.dta
INFO [2025-11-28 10:00:02] Reading: NGA_2014.dta
...
```

❌ **STILL USING SAMPLE DATA** - Need to add files:
```
WARN [2025-11-28 03:41:50] API fetch failed. To use actual WBES data, please:
WARN [2025-11-28 03:41:50]   1. Download .dta microdata files from enterprisesurveys.org
WARN [2025-11-28 03:41:50]   2. Place them in the 'data/' directory
WARN [2025-11-28 03:41:50] Falling back to sample data for demonstration purposes
```

### Check Files are Present

```r
# List .dta files in data directory
list.files("data/", pattern = "\\.dta$")
```

Should return:
```
[1] "KEN_2018.dta" "NGA_2014.dta" "GHA_2013.dta" ...
```

---

## What Data is Needed?

The dashboard uses these 17 indicators (automatically extracted from microdata):

### Infrastructure (3)
- IC.FRM.INFRA.ZS - Infrastructure obstacle
- IC.FRM.ELEC.ZS - Electricity obstacle
- IC.FRM.OUTG.ZS - Power outages

### Finance Access (3)
- IC.FRM.FINA.ZS - Finance obstacle
- IC.FRM.BANK.ZS - Bank account
- IC.FRM.CRED.ZS - Credit constraint

### Corruption (2)
- IC.FRM.CORR.ZS - Corruption obstacle
- IC.FRM.BRIB.ZS - Bribery incidence

### Workforce (3)
- IC.FRM.WKFC.ZS - Workforce obstacle
- IC.FRM.FEMW.ZS - Female workers
- IC.FRM.FEMO.ZS - Female ownership

### Performance (2)
- IC.FRM.CAPU.ZS - Capacity utilization
- IC.FRM.EXPRT.ZS - Export participation

### Crime (2)
- IC.FRM.CRIM.ZS - Crime obstacle
- IC.FRM.SECU.ZS - Security costs

---

## Troubleshooting

### Issue: "Authentication failed"

**Solutions:**
- Verify credentials at https://www.enterprisesurveys.org
- Accept latest terms of use on the portal
- Try manual download instead

### Issue: "No .dta files detected"

**Check:**
```r
list.files("data/")
```

**Ensure:**
- Files are in `data/` directory (not subdirectories)
- Files have `.dta` extension
- Files aren't corrupted (check file sizes > 1MB)

### Issue: "Still seeing sample data warning"

**Verify:**
1. Files are in correct location: `WBES-dashboard/data/*.dta`
2. Files have correct naming: `KEN_2018.dta`, `NGA_2014.dta`, etc.
3. You've restarted the Shiny app after adding files

### Issue: Downloads are slow

**Normal behavior:**
- Each file is 50-200 MB
- Total download time: 10-30 minutes for 10 countries
- Ensure stable internet connection

---

## File Structure

After setup, your project should look like:

```
WBES-dashboard/
├── app/
├── data/                    ← Place .dta files here
│   ├── KEN_2018.dta        ← Real microdata
│   ├── NGA_2014.dta
│   ├── GHA_2013.dta
│   └── ...
├── scripts/
│   ├── setup_wbes_credentials.R
│   ├── wbes_downloader.R
│   └── download_wbes_manual_guide.md
└── WBES_DATA_SETUP.md      ← This file
```

---

## Security Notes

- Credentials stored in `~/.Renviron` are accessible only to your user account
- Never commit `.Renviron` or passwords to git
- The `.dta` files contain confidential firm-level data - handle according to WBES terms
- Add `data/*.dta` to `.gitignore` (already configured)

---

## Getting Help

**For download/authentication issues:**
- WBES Support: https://www.enterprisesurveys.org/en/support

**For dashboard issues:**
- Check app logs in R console
- Review `app/logic/wbes_data.R` for data loading logic

**Detailed guides:**
- See `scripts/download_wbes_manual_guide.md` for step-by-step instructions
- See inline documentation in download scripts
