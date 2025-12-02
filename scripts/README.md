# WBES Dashboard Scripts

This directory contains utility scripts for the WBES dashboard.

## Data Download Scripts

### download_data.sh (Bash)

Automated script to download WBES microdata from Google Drive.

**Usage:**
```bash
bash scripts/download_data.sh
```

**Features:**
- Tries multiple download methods (curl, wget, gdown)
- Checks if file already exists (asks before overwriting)
- Verifies file size and ZIP integrity
- Shows clear error messages with manual download instructions

**Requirements:**
- One of: curl, wget, or gdown
- unzip (for verification)

---

### download_data.py (Python)

Python-based download script for Google Drive files.

**Usage:**
```bash
python3 scripts/download_data.py
```

**Features:**
- Pure Python implementation (no external dependencies required)
- Progress indicator during download
- Handles Google Drive confirmation tokens for large files
- ZIP file validation
- Clear error messages

**Requirements:**
- Python 3.6+
- Standard library only (urllib, zipfile, pathlib)

---

## Google Drive Link

The WBES microdata file is hosted on Google Drive:

🔗 **Direct Link:** https://drive.google.com/file/d/1CFQ3djtgNlfKKB_2Ru7GHvkRGaKYkeuZ/view?usp=sharing

**File ID:** `1CFQ3djtgNlfKKB_2Ru7GHvkRGaKYkeuZ`

---

## Manual Download

If automated scripts fail due to network restrictions:

1. Visit the Google Drive link above
2. Click the **Download** button in Google Drive UI
3. Save the file as `assets.zip`
4. Move it to: `data/assets.zip` in the project root

---

## Troubleshooting

### "Tunnel connection failed: 403 Forbidden"

This error indicates network restrictions are blocking Google Drive access.

**Solutions:**
- Download manually from the Google Drive web interface
- Try from a different network
- Use a VPN if corporate firewall is blocking
- Contact your network administrator

### "File size is suspiciously small"

The download might have received an error page instead of the actual file.

**Solutions:**
- Try downloading manually
- Check if you need to confirm download in Google Drive (for large files)
- Verify you're not hitting rate limits

### "Not a valid ZIP archive"

The downloaded file is corrupted or incomplete.

**Solutions:**
- Delete `data/assets.zip`
- Try downloading again
- Compare file size with expected size (should be 100+ MB)
- Download manually from Google Drive

---

## File Structure

After successful download, you should have:

```
data/
└── assets.zip
    └── ES-Indicators-Database-Global-Methodology_November_24_2025.dta
```

The ZIP file should contain the Stata `.dta` file with WBES microdata.

---

## Verification

After download, verify the setup:

```r
# In R console
source("app/logic/wbes_data.R")
data <- load_wbes_data("data/")

# Check it's real data (not sample)
print(data$metadata$source)
# Should show: "World Bank Enterprise Surveys (Microdata)"

# Check observation count
print(data$metadata$observations)
# Should show: 100,000+ (real data)
# NOT: ~150 (sample data)
```

---

## Notes

- Scripts are designed to be idempotent (safe to run multiple times)
- Existing files are only overwritten with user confirmation
- All downloads go to `data/assets.zip`
- The `.dta` file encoding is `latin1` (handled automatically by haven::read_dta)
