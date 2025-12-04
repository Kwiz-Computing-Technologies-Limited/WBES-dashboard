# Income Filter Tests Documentation

This document describes the comprehensive test suite for income variable validation and filter functionality in the WBES Dashboard.

## Overview

The test suite ensures that:
1. The `income` column exists and contains valid data
2. Income filter dropdowns are properly populated without NA values
3. Income filtering logic works correctly across all modules
4. Modules handle edge cases gracefully

## Test Files

### 1. `test-income_filters.R`
Unit tests for income column data validation and filter logic.

**Test Suites:**
- **Income column in processed data**: Validates that income column exists, has proper data type, contains valid World Bank classifications, and has reasonable coverage
- **Income filter dropdown creation**: Tests the pattern for creating dropdown choices (NA filtering, sorting, named vectors)
- **Income filter logic**: Tests filtering operations for different scenarios
- **Income filter consistency**: Ensures the standard pattern works across modules
- **Income data quality checks**: Validates completeness, no duplicates, reasonable distribution
- **Integration with region filter**: Tests combined filter operations

**Key Tests:**
- ✅ Income column exists in all datasets (country_panel, latest)
- ✅ Income values are character type
- ✅ Income values match World Bank classifications (Low, Lower middle, Upper middle, High income)
- ✅ No empty strings in income column
- ✅ At least 80% of countries have income data
- ✅ Dropdown creation excludes NA values
- ✅ Dropdown choices are sorted alphabetically
- ✅ Filter logic correctly handles "all", specific values, and NA input
- ✅ Combined region + income filters work correctly

### 2. `test-income_filter_modules.R`
Integration tests for Shiny modules that use income filters.

**Modules Tested:**
- Infrastructure module (`app/view/infrastructure.R`)
- Finance Access module (`app/view/finance_access.R`)
- Overview module (`app/view/overview.R`)
- Custom Analysis module (`app/view/mod_custom_analysis.R`)

**Test Suites:**
- **Infrastructure module**: Tests updateSelectInput and filtered reactive
- **Finance Access module**: Tests dropdown updates and sorting
- **Overview module**: Tests combined region + income filters
- **Custom Analysis module**: Tests multi-select income filter
- **Cross-module consistency**: Ensures all modules produce identical dropdown choices
- **Error handling**: Tests edge cases (empty data, single income group)

**Key Tests:**
- ✅ `updateSelectInput("income")` called with valid choices
- ✅ No NA values in dropdown choices
- ✅ "All" option included as first choice
- ✅ Filtered reactive excludes NA income values
- ✅ Filtered reactive returns all data when income = "all"
- ✅ Income choices sorted alphabetically
- ✅ Multi-select income filter works (Custom Analysis module)
- ✅ Combined country + region + income filters work
- ✅ All modules produce consistent dropdown choices
- ✅ Graceful handling of edge cases

## Running the Tests

### Run All Tests
```r
# From R console in project root
library(testthat)
library(box)

# Set box module path
options(box.path = getwd())

# Run all tests
test_dir("tests/testthat")
```

### Run Income Tests Only
```r
# Run income filter unit tests
test_file("tests/testthat/test-income_filters.R")

# Run income filter module tests
test_file("tests/testthat/test-income_filter_modules.R")
```

### Run Tests from Command Line
```bash
cd /path/to/wbes_dashboard
Rscript -e "testthat::test_dir('tests/testthat')"
```

### Run Specific Test Suite
```r
# Run only data validation tests
test_file("tests/testthat/test-income_filters.R",
          filter = "Income column in processed data")

# Run only module integration tests
test_file("tests/testthat/test-income_filter_modules.R",
          filter = "Infrastructure module")
```

## Test Coverage

### Data Layer (test-income_filters.R)
- ✅ Income column existence (2 tests)
- ✅ Income data quality (5 tests)
- ✅ Income dropdown creation (5 tests)
- ✅ Income filter logic (6 tests)
- ✅ Filter consistency (3 tests)
- ✅ Data quality checks (3 tests)
- ✅ Integration tests (2 tests)

**Total: 26 tests**

### Module Layer (test-income_filter_modules.R)
- ✅ Infrastructure module (3 tests)
- ✅ Finance Access module (2 tests)
- ✅ Overview module (2 tests)
- ✅ Custom Analysis module (3 tests)
- ✅ Cross-module consistency (2 tests)
- ✅ Error handling (2 tests)

**Total: 14 tests**

**Grand Total: 40 tests**

## Expected Test Output

All tests should pass with output similar to:

```
Test suite: Income column in processed data
✓ income column exists in country_panel
✓ income column exists in latest dataset
✓ income column has at least one non-NA value
✓ income values are character type
✓ income values are valid World Bank classifications
✓ income has no empty strings
✓ income distribution is reasonable
✓ income maps correctly to countries

Test suite: Income filter dropdown creation
✓ unique income values can be extracted without NA
✓ income values can be sorted alphabetically
✓ income dropdown choices include 'All' option
✓ income dropdown has named vector structure
✓ no NA in income dropdown choices

[ ... additional test output ... ]

══ Results ════════════════════════════════════════════════════════
Duration: 2.5 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 40 ]
```

## Failure Investigation

If tests fail, check:

1. **Data Loading Issues**
   - Verify `load_sample_data()` or `load_wbes_data()` works
   - Check that data files exist in `data/` directory
   - Ensure `income` column is properly processed in `app/logic/wbes_data.R`

2. **Module Issues**
   - Verify module server functions match expected signatures
   - Check that `observeEvent(wbes_data(), {...})` is implemented
   - Ensure `updateSelectInput(session, "income", ...)` is called
   - Verify filtered reactive applies NA filtering

3. **Data Quality Issues**
   - Check if real data has <80% income completeness
   - Verify income values match World Bank classifications
   - Ensure no empty strings in income column

## Maintenance

When modifying income filter code:

1. **Update tests** if changing:
   - Income dropdown creation pattern
   - Filter logic conditions
   - Module server function signatures

2. **Add tests** when:
   - Creating new modules with income filters
   - Adding new income-related features
   - Fixing bugs related to income filtering

3. **Run tests** before:
   - Committing changes to income filter code
   - Creating pull requests
   - Deploying to production

## CI/CD Integration

To integrate with CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Run R tests
  run: |
    Rscript -e "testthat::test_dir('tests/testthat')"
```

## Related Files

- **Data Processing**: `app/logic/wbes_data.R:375-380` (income column extraction)
- **Modules with Income Filters**:
  - `app/view/infrastructure.R:88-110`
  - `app/view/finance_access.R:89-111`
  - `app/view/overview.R:103-127`
  - `app/view/mod_custom_analysis.R:218-251`
  - `app/view/mod_corruption.R`
  - `app/view/mod_crime.R`
  - `app/view/mod_performance.R`
  - `app/view/mod_workforce.R`

## Standard Pattern Reference

All modules should follow this pattern:

```r
# In observeEvent(wbes_data(), {...})
incomes_vec <- unique(d$income) |>
  stats::na.omit() |>
  as.character() |>
  sort()
incomes <- c("All" = "all", setNames(incomes_vec, incomes_vec))
shiny::updateSelectInput(session, "income", choices = incomes)

# In filtered reactive
if (input$income != "all" && !is.na(input$income)) {
  d <- d |> filter(!is.na(income) & income == input$income)
}
```

This pattern ensures:
- ✅ No NA in dropdown choices
- ✅ Sorted alphabetically
- ✅ "All" option included
- ✅ Proper NA handling in filter logic

## Contact

For questions or issues with these tests, refer to:
- Main documentation: `README.md`
- Data loading tests: `tests/testthat/test-data_loader.R`
- Module tests: `tests/testthat/test-main.R`
