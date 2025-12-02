# WBES Data Directory

This directory is for World Bank Enterprise Surveys data files.

## ⚠️ IMPORTANT: Real Data Required

**The dashboard currently defaults to SAMPLE DATA because no real WBES microdata is present.**

To use real WBES microdata, follow the setup instructions below.

---

## Data Sources

Download WBES microdata from: https://www.enterprisesurveys.org/en/survey-datasets

**Required File:** `ES-Indicators-Database-Global-Methodology_November_24_2025.dta`

---

## Quick Setup (Recommended)

### Step 1: Download the Data

1. Visit https://www.enterprisesurveys.org/en/survey-datasets
2. Register/login if required
3. Download: **ES-Indicators-Database-Global-Methodology_November_24_2025.dta**
   - File encoding: `latin1`
   - Format: Stata `.dta`
   - Size: ~100-500 MB (typical for full microdata)

### Step 2: Create assets.zip

```bash
# Navigate to your download directory
cd ~/Downloads

# Create the ZIP file containing the .dta file
zip assets.zip ES-Indicators-Database-Global-Methodology_November_24_2025.dta

# Move to the WBES dashboard data directory
mv assets.zip /path/to/WBES-dashboard/data/
```

### Step 3: Verify Setup

Run the app. You should see:
- ✅ "Found assets.zip - loading combined microdata" in logs
- ✅ Real country data in all visualizations
- ✅ Descriptive column labels extracted from Stata file

---

## Data Loading Priority

The app loads data in this order:

1. **✅ Cached processed data** (`.rds` file) - fastest, loads in seconds
2. **📦 assets.zip** - extracts and processes microdata, then caches
3. **📄 Individual .dta files** - processes and caches
4. **🎯 Sample data** - demonstration data (WITH PROMINENT WARNING)

### When Real Data is Missing

**Current Behavior:** The app displays a prominent warning message and loads sample/simulated data.

**To Require Real Data (Fail Loudly):**
In `app/main.R`, set `require_real_data = TRUE`:

```r
data <- load_wbes_data(
  data_path = "data/",
  use_cache = TRUE,
  cache_hours = 24,
  require_real_data = TRUE  # Add this parameter
)
```

This will cause the app to **fail with a clear error message** if real data is not found, preventing accidental use of sample data in production.

---

## Column Labels & Descriptive Names

### Automatic Label Extraction

When real WBES microdata is loaded, the dashboard:

1. **Extracts variable labels** from the Stata `.dta` file using `haven::read_dta()`
2. **Combines** extracted labels with manually-defined labels for key indicators
3. **Stores** the comprehensive mapping in `wbes_data$label_mapping`
4. **Makes labels available** to all dashboard modules for consistent UI display

### Using Labels in Modules

Example from a dashboard module:

```r
# Access the label mapping
labels <- wbes_data()$label_mapping

# Get descriptive label for a column
label <- get_column_label("in2", labels)
# Returns: "Number of Power Outages per Month"

# Format for UI display
ui_label <- format_label_for_ui("in2", labels)

# Get all infrastructure-related labels
infra_labels <- get_category_labels("infrastructure", labels)
```

### Label Categories

The system recognizes these categories:
- **infrastructure**: Power, water, transport indicators
- **finance**: Credit, loans, banking indicators
- **corruption**: Bribery, governance indicators
- **workforce**: Employment, gender indicators
- **performance**: Capacity utilization, exports
- **crime**: Security, crime-related indicators

---

## Alternative Formats

If you don't use `assets.zip`, the app supports:

### Option 1: Individual .dta files
Place `.dta` files directly in this directory:
```
data/
├── Kenya_2023.dta
├── Nigeria_2023.dta
└── Ghana_2023.dta
```

### Option 2: CSV files
Alternative format (less efficient, loses Stata labels):
```
data/
└── wbes_combined.csv
```

**Note:** CSV files do not preserve variable labels from Stata, so only manual labels will be available.

---

## Performance & Caching

### First Load
- **Duration:** 30-60 seconds for large microdata files
- **Process:** Extracts from ZIP → Reads .dta → Processes → Caches

### Subsequent Loads
- **Duration:** < 5 seconds
- **Process:** Loads from cache (`data/wbes_processed.rds`)

### Cache Management

**Cache location:** `data/wbes_processed.rds` (generated automatically)

**Cache expiration:** 24 hours (configurable in `config.yml`)

**Force refresh:**
```bash
# Delete cache to force reload from source
rm data/wbes_processed.rds
```

**Extracted files:** Stored in `data/.extracted/` (can be deleted to save space)

---

## File Organization

### For Single Global Dataset (Recommended)
```
data/
└── assets.zip
    └── ES-Indicators-Database-Global-Methodology_November_24_2025.dta
```

### For Multiple Country Files
```
data/
├── SubSaharanAfrica_2023.dta
├── SouthAsia_2023.dta
└── LatinAmerica_2023.dta
```

### For Survey Waves
```
data/
├── WBES_Wave_2021.dta
├── WBES_Wave_2022.dta
└── WBES_Wave_2023.dta
```

---

## Troubleshooting

### Problem: "WARNING: USING SAMPLE DATA"

**Cause:** No real data files found in `data/` directory.

**Solution:**
1. Verify `assets.zip` exists: `ls -lh data/assets.zip`
2. Check ZIP contents: `unzip -l data/assets.zip`
3. Ensure .dta file is named correctly
4. Check file permissions

### Problem: "Error reading .dta file"

**Cause:** Encoding issues or corrupted file.

**Solution:**
1. Verify file encoding is `latin1`
2. Re-download the file
3. Check the extraction log for specific error messages

### Problem: "Cached data is sample data"

**Cause:** Cache was created when only sample data was available.

**Solution:**
```bash
# Remove the cached sample data
rm data/wbes_processed.rds

# Restart the app - it will reload from assets.zip
```

### Problem: "No column labels available"

**Cause:** Data loaded from CSV instead of Stata .dta file.

**Solution:**
- Use `.dta` files (Stata format) to preserve variable labels
- CSV files only get manual labels, not extracted labels

---

## Data Validation

### Verify Real Data is Loaded

Check the app logs or metadata:

```r
# In R console or app
wbes_data <- load_wbes_data()

# Check metadata
print(wbes_data$metadata$source)
# Should show: "World Bank Enterprise Surveys (Microdata)"
# NOT: "World Bank Enterprise Surveys (Sample Data)"

# Check observations
print(wbes_data$metadata$observations)
# Should show: 100,000+ observations (real data)
# NOT: ~150 observations (sample data)

# Check labels
print(length(wbes_data$label_mapping))
# Should show: 100+ labels with extracted Stata labels
```

### Column Label Dictionary

Generate a data dictionary of all labels:

```r
library(app/logic)
labels <- wbes_data$label_mapping
dictionary <- create_label_dictionary(labels)
write.csv(dictionary, "data/column_dictionary.csv")
```

---

## Notes

- **Large files** (>100MB) benefit most from the caching system
- **Cache expires** after 24 hours by default (configurable)
- **Extracted files** are kept to avoid re-extracting the ZIP
- **The app logs** all data loading steps for debugging
- **Sample weights** are preserved from microdata for proper aggregation
- **Variable labels** are automatically extracted from Stata metadata

---

## Security & Privacy

- Data files are excluded from git via `.gitignore`
- Never commit microdata to the repository
- Ensure compliance with World Bank data usage terms
- Cache files contain processed data - treat with same security as source files

---

## Support

If you encounter issues:
1. Check the app logs in the R console
2. Verify file integrity (MD5/SHA checksums)
3. Consult the World Bank Enterprise Surveys documentation
4. Review the code in `app/logic/wbes_data.R` for data loading logic
