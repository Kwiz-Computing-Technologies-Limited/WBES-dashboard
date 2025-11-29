# WBES Data Directory

This directory is for World Bank Enterprise Surveys data files.

## Data Sources

Download WBES microdata from: https://www.enterprisesurveys.org/en/survey-datasets

## Recommended Setup (Most Efficient)

**For optimal performance, use the combined microdata ZIP file:**

1. Download the combined WBES microdata .dta file from the Enterprise Surveys website (currently `ES-Indicators-Database-Global-Methodology_November_24_2025.dta`, encoded in `latin1`)
2. Create a ZIP archive named `assets.zip` containing the .dta file(s)
3. Place `assets.zip` in this directory
4. The app will automatically:
   - Extract the .dta file(s) on first load
   - Process and cache the data for faster subsequent loads
   - Use the cached version until data is updated

**Benefits:**
- Single file to manage
- Automatic caching (loads in seconds after first run)
- Efficient storage (compressed)
- Easy to update (just replace assets.zip)

## Alternative Formats

If you don't use `assets.zip`, the app supports:

1. **Individual .dta files** - Place .dta files directly in this directory
2. **CSV files** - Alternative format (less efficient for large datasets)

## Data Loading Priority

The app loads data in this order:
1. ✅ **Cached processed data** (`.rds` file) - fastest, loads in seconds
2. 📦 **assets.zip** - extracts and processes microdata, then caches
3. 📄 **Individual .dta files** - processes and caches
4. 🎯 **Sample data** - demonstration data if no other sources available

## File Organization

If using individual files (not assets.zip), you can organize by:
- Country: `Kenya_2023.dta`
- Region: `SubSaharanAfrica_2023.dta`
- Survey wave: `WBES_Wave_2023.dta`

## Performance Tips

1. **First load:** May take 30-60 seconds for large microdata files
2. **Subsequent loads:** Usually < 5 seconds thanks to caching
3. **Cache location:** `data/wbes_processed.rds` (generated automatically)
4. **Cache refresh:** Delete the `.rds` file to force reload from source
5. **Extracted files:** Stored in `data/.extracted/` (can be deleted to save space)

## Notes

- Large files (>100MB) benefit most from the caching system
- Cache expires after 24 hours (configurable)
- Extracted files are kept to avoid re-extracting the ZIP
- The app logs all data loading steps for debugging
