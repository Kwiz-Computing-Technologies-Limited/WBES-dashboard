# WBES Data Directory

This directory is for World Bank Enterprise Surveys data files.

## Data Sources

Download WBES microdata from: https://www.enterprisesurveys.org/en/survey-datasets

## Recommended Setup (Most Efficient)

**For optimal performance, use the combined microdata ZIP file:**

1. Download the combined WBES microdata .dta file from the Enterprise Surveys website
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
4. 🌐 **World Bank API** - fetches aggregate indicators (limited coverage)
5. 🎯 **Sample data** - demonstration data if no other sources available

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

## Quick Start: Copy Your Data File

If you've already downloaded WBES microdata, copy it to this directory:

```bash
# Example: If your assets.zip is in ~/Downloads/wbes_dashboard/data/
cp ~/Downloads/wbes_dashboard/data/assets.zip /path/to/WBES-dashboard/data/

# Or create a symbolic link (alternative)
ln -s ~/Downloads/wbes_dashboard/data/assets.zip /path/to/WBES-dashboard/data/assets.zip
```

After copying, the app will automatically detect and load the real data instead of sample data.

## Troubleshooting

**App shows "Sample Data" instead of real data:**
- Verify `assets.zip` is in this directory: `ls -lh data/assets.zip`
- Check the app logs for errors during data loading
- Ensure the ZIP contains .dta files: `unzip -l data/assets.zip`
- Delete cache to force reload: `rm data/wbes_processed.rds`

**Encoding errors when loading .dta files:**
- The app now uses `encoding = "latin1"` for international character support
- If issues persist, check the .dta file format version

**Slow loading:**
- First load of microdata (254K+ observations) may take 30-60 seconds
- Subsequent loads use cache and complete in ~5 seconds
- Check cache file exists: `ls -lh data/wbes_processed.rds`

## Notes

- Large files (>100MB) benefit most from the caching system
- Cache expires after 24 hours (configurable in `config.yml`)
- Extracted files are kept in `data/.extracted/` to avoid re-extracting
- The app logs all data loading steps for debugging
- Variable transformations now use actual WBES scales (0-4 for obstacles, binary for yes/no, etc.)
