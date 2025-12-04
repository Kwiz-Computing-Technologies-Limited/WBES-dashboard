# Chart Audit and Fixes

## Issues Identified

### 1. Income Column - All NA Values
**Status**: CRITICAL
**Action**: Replace all `income` references with `firm_size` throughout the app
**Affected Modules**:
- infrastructure.R
- finance_access.R
- overview.R
- mod_custom_analysis.R
- mod_corruption.R
- mod_crime.R
- mod_performance.R
- mod_workforce.R

### 2. Survey Year Filter - Not Reactive
**Status**: TO INVESTIGATE
**Action**: Check if year filter causes any reactivity, if not remove
**Affected Modules**:
- mod_infrastructure.R
- mod_overview.R

### 3. Chart Variable Issues

#### Country Profile Tab - Infrastructure Obstacles
**Chart**: "Bars rank which infrastructure services firms flag as biggest obstacles"
**Issue**: Variables `electricity_obstacle`, `water_obstacle`, `transport_obstacle` may be blank
**Variables to check**: `elec` (IC.FRM.ELEC.ZS), `c16`, `d4` (or IC.FRM.INFRA.ZS components)
**File**: app/view/mod_country_profile.R:347-388

#### Country Profile Tab - Bribery by Transaction
**Chart**: "Bribery prevalence by transaction type"
**Issue**: Variables may be blank
**Variables**: `bribe_for_permit` (j7a), `bribe_for_import` (j7b), `bribe_for_utilities` (j7c), `bribe_for_tax` (j7d), `bribe_for_contract` (j7e)
**File**: app/view/mod_country_profile.R:553-607

#### Country Profile Tab - Management Time
**Chart**: "Management time spent on regulatory tasks"
**Issue**: Variable may be blank
**Variable**: `mgmt_time_regulations_pct` (j2)
**File**: app/view/mod_country_profile.R:609-651

#### Custom Analysis - Country Comparison
**Chart**: "Bars compare the selected indicator across chosen countries"
**Issue**: Selected indicator variables may be blank
**File**: app/view/mod_custom_analysis.R

#### Infrastructure Tab - Outages vs Productivity
**Chart**: Scatter plot
**Error**: `Tibble columns must have compatible sizes. Size 2: Columns x and y. Size 7: Column text.`
**Issue**: Data length mismatch in plotly
**File**: app/view/mod_infrastructure.R or infrastructure.R

#### Finance Access Tab - Reasons for Not Applying
**Chart**: "The pie breaks down why firms opt out of loan applications"
**Issue**: Variables may be blank
**Variables**: `no_need_for_loan` (fin19a), `loan_procedures_complex` (fin19b), `loan_interest_high` (fin19c), `insufficient_collateral` (fin19d), `loan_size_inadequate` (fin19e)
**File**: app/view/mod_finance_access.R

#### Finance Access Tab - Collateral Box Plots
**Chart**: "Box plots summarize collateral requested as a share of loan value"
**Error**: `factor level [3] is duplicated`
**Issue**: Duplicate factor levels
**File**: app/view/mod_finance_access.R:547

#### Finance Access Tab - Processing Time
**Chart**: "Processing time distributions show how quickly banks deliver decisions"
**Issue**: Variable may be blank
**Variable**: `days_to_get_loan` (fin22)
**File**: app/view/mod_finance_access.R

#### Corruption Tab - Regional Corruption
**Chart**: "Stacked bars compare corruption pressure across regions"
**Issue**: Variables may be blank
**Variables**: IC.FRM.CORR.ZS, IC.FRM.BRIB.ZS
**File**: app/view/mod_corruption.R

#### Corruption Tab - Income Box Plots
**Chart**: "Box plots summarize corruption responses by income tier"
**Issue**: Uses `income` variable which is all NA
**Action**: Replace with `firm_size`
**File**: app/view/mod_corruption.R

#### Workforce Tab - Female Participation Trends
**Chart**: "Lines trace how female employment indicators evolve across income groups"
**Issue**: Uses `income` variable which is all NA
**Action**: Replace with `firm_size` or `region`
**File**: app/view/mod_workforce.R

#### Workforce Tab - Female Ownership by Region
**Chart**: "Bars summarize female ownership prevalence by region"
**Issue**: Variables may be blank
**Variables**: IC.FRM.FEMO.ZS (gend1)
**File**: app/view/mod_workforce.R

#### Workforce Tab - Income Comparison
**Chart**: "Income-tier box plots show how workforce obstacles differ"
**Issue**: Uses `income` variable which is all NA
**Action**: Replace with `firm_size`
**File**: app/view/mod_workforce.R

#### Performance Tab - Regional Performance
**Chart**: "Regional averages reveal how performance differs across geographies"
**Issue**: Variables may be blank
**Variables**: IC.FRM.CAPU.ZS, IC.FRM.EXPRT.ZS
**File**: app/view/mod_performance.R

#### Performance Tab - Capacity vs Obstacles
**Chart**: "Scatter points show how operational capacity aligns with reported obstacles"
**Issue**: Variables may be blank
**Variables**: IC.FRM.CAPU.ZS, IC.FRM.OUTG.ZS or IC.FRM.INFRA.ZS
**File**: app/view/mod_performance.R

#### Performance Tab - Income Performance
**Chart**: "Box plots summarize performance dispersion within each income tier"
**Issue**: Uses `income` variable which is all NA
**Action**: Replace with `firm_size`
**File**: app/view/mod_performance.R

#### Performance Tab - Export vs Infrastructure
**Chart**: "Scatter compares export participation with infrastructure reliability"
**Issue**: Variables may be blank
**Variables**: IC.FRM.EXPRT.ZS, IC.FRM.OUTG.ZS or IC.FRM.ELEC.ZS
**File**: app/view/mod_performance.R

#### Crime Tab - Regional Security
**Chart**: "Regional averages reveal how security pressures vary across geographies"
**Issue**: Variables may be blank
**Variables**: IC.FRM.CRIM.ZS, IC.FRM.SECU.ZS
**File**: app/view/mod_crime.R

#### Crime Tab - Income Security
**Chart**: "Box plots summarize crime and security costs across income tiers"
**Issue**: Uses `income` variable which is all NA
**Action**: Replace with `firm_size`
**File**: app/view/mod_crime.R

### 4. Duplicate Charts

#### Crime Tab - Security Costs
**Chart 1**: "Bars translate security spending into percentage of sales"
**Chart 2**: "Bars benchmark crime-related obstacles or security costs by country" (when indicator = Security Costs)
**Issue**: These are duplicates
**Action**: Remove Chart 1
**File**: app/view/mod_crime.R

## Execution Plan

### Phase 1: Replace Income with Firm Size (HIGH PRIORITY)
1. Update wbes_data.R to ensure firm_size is in metric_cols
2. Replace all `income` filter references with `firm_size`
3. Update all box plots and charts using income to use firm_size
4. Update dropdown labels from "Income Group" to "Firm Size"
5. Update test files

### Phase 2: Fix Chart Variables (HIGH PRIORITY)
1. Verify which source columns have data
2. Update variable mappings in wbes_data.R if needed
3. Fix charts with missing/blank variables
4. Add fallback/placeholder messages for charts with no data

### Phase 3: Fix Chart Errors (HIGH PRIORITY)
1. Fix Tibble size mismatch in Infrastructure scatter plot
2. Fix duplicate factor levels in Finance collateral box plot

### Phase 4: Remove Survey Year Filter (MEDIUM PRIORITY)
1. Check if year filter has any reactive behavior
2. If not reactive, remove from UI and server

### Phase 5: Remove Duplicate Chart (LOW PRIORITY)
1. Remove duplicate security costs chart from Crime tab

### Phase 6: Update Documentation and Tests
1. Update test files to use firm_size instead of income
2. Update README and documentation
3. Add warnings for charts with limited data availability
