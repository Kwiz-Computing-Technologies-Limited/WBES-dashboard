# Batch 0A Testing Checklist

## Pre-Launch Setup

- [ ] Ensure data/assets.zip exists
- [ ] Clear old cache if needed: `rm -rf data/wbes_processed.rds data/.extracted`
- [ ] Launch app: `rhino::app()`

---

## 1. Initial Load & Performance Testing

### First Load (Cache Building)
- [ ] **App starts successfully**
- [ ] **Check console for logs:**
  - "Loading WBES data..."
  - "Found assets.zip - loading combined microdata"
  - "Reading microdata directly from ZIP"
  - "Enriching data with World Bank income classifications"
  - "Creating aggregates in parallel..."
  - "Using X cores for parallel aggregation"
  - "Cached processed data as Parquet"
- [ ] **Measure initial load time**: _____ seconds
- [ ] **Check memory usage** (Activity Monitor/Task Manager): _____ MB

### Cache Verification
- [ ] **Check data directory:**
  - `data/processed_wbes_metadata.rds` exists
  - `data/wbes_processed_cache/` directory exists
  - Parquet files present: raw.parquet, processed.parquet, latest.parquet, etc.
- [ ] **Measure cache size**: _____ MB (should be ~150-250 MB)

### Second Load (From Cache)
- [ ] **Restart app**: Stop and run `rhino::app()` again
- [ ] **Check console logs:**
  - "Loading from Parquet cache"
  - "Loaded X.parquet as Arrow table"
- [ ] **Measure cached load time**: _____ seconds (should be <1 second)
- [ ] **Verify data loaded correctly** (charts render)

---

## 2. World Bank Income Integration

### Income Filter Functionality
- [ ] **Income filter populates** with options:
  - All Income Levels
  - High
  - Upper-Middle
  - Lower-Middle
  - Low
- [ ] **Select "High"** - charts update
- [ ] **Select "Lower-Middle"** - charts update differently
- [ ] **Select "All Income Levels"** - shows all data

### Income Filter Reactivity Across Tabs
Test income filter changes on each tab:
- [ ] **Overview** - KPIs and charts update
- [ ] **Regional Profile** - data filters correctly
- [ ] **Sector Profile** - data filters correctly
- [ ] **Size Profile** - data filters correctly
- [ ] **Country Profile** - data filters correctly
- [ ] **Cross-Country Benchmark** - data filters correctly
- [ ] **Cross-Sector Benchmark** - data filters correctly
- [ ] **Cross-Regional Benchmark** - data filters correctly
- [ ] **Cross-Size Benchmark** - data filters correctly
- [ ] **Infrastructure** - data filters correctly
- [ ] **Access to Finance** - data filters correctly
- [ ] **Corruption & Governance** - data filters correctly
- [ ] **Workforce & Gender** - data filters correctly
- [ ] **Business Performance** - data filters correctly
- [ ] **Crime & Security** - data filters correctly

---

## 3. Custom Sectors Feature

### Create Custom Sector Group
- [ ] **Click "Create Group" button** under Sector filter
- [ ] **Modal opens** with title "Create Custom Sector Group"
- [ ] **Enter name**: "Tech & Services"
- [ ] **Select sectors**: Manufacturing, Services
- [ ] **Click "Save Sector Group"**
- [ ] **Modal closes**
- [ ] **Sector filter updates** with new group in dropdown
- [ ] **"[Custom] Tech & Services" appears** in dropdown

### Use Custom Sector Group
- [ ] **Select "[Custom] Tech & Services"** from dropdown
- [ ] **Charts update** to show only selected sectors
- [ ] **Navigate to different tabs** - filter persists
- [ ] **Data shows correct sectors** (Manufacturing + Services only)

### Manage Custom Sector Groups
- [ ] **Click "Manage" button** under Sector filter
- [ ] **Modal shows** existing custom sector groups
- [ ] **"Tech & Services" listed** with sector count
- [ ] **Click delete button** (trash icon)
- [ ] **Group is removed** from list
- [ ] **Close modal**
- [ ] **Sector filter updates** - custom group removed

### Create Multiple Custom Groups
- [ ] **Create 2-3 different sector groups**
- [ ] **All appear in dropdown** with "[Custom]" prefix
- [ ] **Can switch between** standard and custom groups
- [ ] **Filtering works correctly** for each group

---

## 4. Business Environment Radar Consistency

### Test with All Filters = "All"
- [ ] **Set all filters to "All"**:
  - Region: All Regions
  - Sector: All Sectors
  - Firm Size: All Sizes
  - Income: All Income Levels
  - Year: All Years

- [ ] **Navigate to Regional Profile**
- [ ] **Screenshot radar chart** - Note values for each dimension
- [ ] **Navigate to Sector Profile**
- [ ] **Compare radar chart** - Should be IDENTICAL to Regional
- [ ] **Navigate to Size Profile**
- [ ] **Compare radar chart** - Should be IDENTICAL to Regional and Sector

**Expected**: All three radar charts show the same values when no filters applied

### Test with Specific Filters
- [ ] **Set Region = "AFR"** (Africa)
- [ ] **Regional Profile radar** shows AFR-specific data
- [ ] **Sector/Size Profile radars** also reflect AFR filter (global filters apply)

- [ ] **Set Sector = "Manufacturing"**
- [ ] **Sector Profile radar** shows Manufacturing-specific data
- [ ] **Regional/Size Profile radars** also reflect Manufacturing filter

---

## 5. Survey Year Filter

### Year Filter Functionality
- [ ] **Year filter shows** available years
- [ ] **Select specific year** (e.g., 2019)
- [ ] **Charts update** to show only 2019 data
- [ ] **Navigate between tabs** - year filter persists
- [ ] **Select multiple years** (if supported)
- [ ] **Select "All Years"** - shows all data

### Year Filter on Time-Series Data
- [ ] **Navigate to tabs with panel data** (if any)
- [ ] **Year filter affects** time-series charts
- [ ] **Trend lines update** based on selected years

---

## 6. Custom Regions (Verify Still Working)

### Create Custom Region
- [ ] **Click "Create Region" button** under Region filter
- [ ] **Modal opens** correctly
- [ ] **Create custom region**: "East Africa" (Kenya, Uganda, Tanzania)
- [ ] **Save successfully**
- [ ] **Appears in filter dropdown**

### Use Custom Region
- [ ] **Select custom region** from dropdown
- [ ] **Charts update** to show only selected countries
- [ ] **Cross-Regional Benchmark** includes custom region
- [ ] **Custom region aggregation** calculates correctly

---

## 7. Filter Interactions & Reset

### Combined Filters
- [ ] **Select**: Region=AFR, Sector=Manufacturing, Income=Lower-Middle
- [ ] **All filters apply simultaneously**
- [ ] **Charts show** intersection of all filters
- [ ] **Navigate between tabs** - all filters persist

### Reset All Filters
- [ ] **Click "Reset All Filters" button**
- [ ] **All filters** return to "All"
- [ ] **Charts update** to show global data
- [ ] **Works from any tab**

---

## 8. Performance & Stability

### Memory Management
- [ ] **Monitor memory** during 10+ minutes of use
- [ ] **Memory remains stable** (no continuous growth)
- [ ] **No memory leaks** after switching tabs 20+ times
- [ ] **gc() calls working** (check console for any warnings)

### Parallel Processing
- [ ] **First load uses multiple cores** (check console logs)
- [ ] **"Using X cores for parallel aggregation"** appears
- [ ] **Aggregations complete faster** than sequential would

### App Responsiveness
- [ ] **Tab switching is fast** (<1 second)
- [ ] **Filter changes are smooth** (no lag)
- [ ] **Charts render quickly** (<2 seconds)
- [ ] **No console errors** during normal use

### Arrow Tables
- [ ] **Data operations work** with Arrow tables
- [ ] **Charts render correctly** from Arrow data
- [ ] **No unexpected errors** related to Arrow/Parquet
- [ ] **to_df() conversion** only when necessary

---

## 9. Error Handling

### Missing Data
- [ ] **Select filter combination with no data**
- [ ] **App shows appropriate message** (not error)
- [ ] **No console errors**

### Invalid Selections
- [ ] **Try edge cases** (empty selections, etc.)
- [ ] **App handles gracefully**

---

## 10. Data Quality & Accuracy

### Income Data Enrichment
- [ ] **Check specific country** (e.g., Kenya)
- [ ] **Verify income classification** is correct (Kenya = Lower-Middle)
- [ ] **Cross-reference** with World Bank data if possible
- [ ] **Check console log**: "Income enrichment complete: X/Y rows enriched (Z%)"

### Radar Chart Values
- [ ] **Verify radar values** make sense
- [ ] **Infrastructure score** relates to power outages (inverted)
- [ ] **Finance Access** relates to credit line access
- [ ] **All values** between 0-100

### Global Aggregates
- [ ] **Overview tab KPIs** show correct totals
- [ ] **Country count** matches expected
- [ ] **Firm count** is reasonable
- [ ] **Percentages** are within 0-100 range

---

## 11. Console Log Verification

Check for these log messages (no errors):
- [ ] "Loading WBES data..."
- [ ] "Loading from Parquet cache" (second load)
- [ ] "Enriching data with World Bank income classifications"
- [ ] "Income enrichment complete: X/Y rows enriched"
- [ ] "Creating aggregates in parallel..."
- [ ] "Using X cores for parallel aggregation"
- [ ] "Country aggregates created: X countries"
- [ ] "Cached processed data as Parquet"
- [ ] No ERROR messages
- [ ] No WARNING messages (except expected ones)

---

## 12. File System Verification

### Cache Structure
```
data/
├── assets.zip                      ✓ Original data
├── processed_wbes_metadata.rds     ✓ Metadata (~5-10 MB)
└── wbes_processed_cache/           ✓ Parquet files
    ├── raw.parquet                 ✓
    ├── processed.parquet           ✓
    ├── latest.parquet              ✓
    ├── country_panel.parquet       ✓
    ├── country_sector.parquet      ✓
    ├── country_size.parquet        ✓
    ├── country_region.parquet      ✓
    └── country_coordinates.parquet ✓
```

### Old Files Removed
- [ ] No `data/wbes_processed.rds` (old cache)
- [ ] No `data/.extracted/` directory (old extraction)
- [ ] Temp files cleaned up

---

## Performance Benchmarks

Record these metrics:

| Metric | Target | Actual | Pass/Fail |
|--------|--------|--------|-----------|
| **First Load Time** | 10-15s | _____ | _____ |
| **Cached Load Time** | <1s | _____ | _____ |
| **Cache Size** | ~150-250 MB | _____ MB | _____ |
| **Peak Memory Usage** | <2 GB | _____ MB | _____ |
| **Tab Switch Time** | <1s | _____ | _____ |
| **Filter Change Time** | <1s | _____ | _____ |
| **Parallel Cores Used** | n-1 | _____ | _____ |

---

## Issues Found

Document any issues discovered:

### Issue 1:
- **Description**:
- **Steps to Reproduce**:
- **Expected**:
- **Actual**:
- **Severity**: Critical / High / Medium / Low

### Issue 2:
- **Description**:
- **Steps to Reproduce**:
- **Expected**:
- **Actual**:
- **Severity**: Critical / High / Medium / Low

---

## Sign-Off

- [ ] **All critical tests passed**
- [ ] **No blocking issues found**
- [ ] **Performance meets targets**
- [ ] **Ready to merge to main**

**Tested by**: _________________
**Date**: _________________
**App Version**: Batch 0A
**Notes**:

---

## Next Steps After Testing

If all tests pass:
1. Commit all changes with comprehensive message
2. Push to branch
3. Merge to main
4. Proceed to Batch 0B planning

If issues found:
1. Document in "Issues Found" section
2. Prioritize and fix critical issues
3. Re-test affected areas
4. Sign off when ready
