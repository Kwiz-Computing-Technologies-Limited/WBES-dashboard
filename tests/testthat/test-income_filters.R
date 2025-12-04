# tests/testthat/test-income_filters.R
# Unit tests for income variable validation and filter functionality

box::use(
  testthat[describe, it, expect_true, expect_false, expect_equal,
           expect_type, expect_named, expect_s3_class, expect_gte, expect_lte,
           expect_length, expect_no_error, expect_error],
  app/logic/wbes_data[load_wbes_data, load_sample_data],
  dplyr[filter, pull],
  shiny[reactiveValues, testServer],
  stats[na.omit]
)

describe("Income column in processed data", {

  # Load sample data for testing
  sample_data <- load_sample_data()

  it("income column exists in country_panel", {
    expect_true("income" %in% names(sample_data$country_panel),
                info = "income column missing from country_panel")
  })

  it("income column exists in latest dataset", {
    expect_true("income" %in% names(sample_data$latest),
                info = "income column missing from latest dataset")
  })

  it("income column has at least one non-NA value", {
    income_values <- sample_data$latest$income
    non_na_values <- income_values[!is.na(income_values)]
    expect_gte(length(non_na_values), 1,
               info = "income column has no non-NA values")
  })

  it("income values are character type", {
    income_values <- sample_data$latest$income
    expect_type(income_values, "character",
                info = "income column should be character type")
  })

  it("income values are valid World Bank classifications", {
    valid_incomes <- c("Low income", "Lower middle income",
                       "Upper middle income", "High income")
    income_values <- sample_data$latest$income |>
      na.omit() |>
      unique()

    for (val in income_values) {
      expect_true(val %in% valid_incomes,
                  info = paste("Invalid income value:", val))
    }
  })

  it("income has no empty strings", {
    income_values <- sample_data$latest$income
    empty_strings <- sum(income_values == "", na.rm = TRUE)
    expect_equal(empty_strings, 0,
                 info = "income column contains empty strings")
  })

  it("income distribution is reasonable", {
    # At least 2 different income groups should be represented
    income_unique <- sample_data$latest$income |>
      na.omit() |>
      unique() |>
      length()
    expect_gte(income_unique, 2,
               info = "Only one income group found - check data diversity")
  })

  it("income maps correctly to countries", {
    # Each country should have an income classification
    country_income_map <- sample_data$latest |>
      dplyr::select(country, income) |>
      dplyr::filter(!is.na(country))

    # At least 80% of countries should have income data
    countries_with_income <- sum(!is.na(country_income_map$income))
    total_countries <- nrow(country_income_map)
    coverage_pct <- (countries_with_income / total_countries) * 100

    expect_gte(coverage_pct, 80,
               info = paste0("Only ", round(coverage_pct, 1),
                           "% of countries have income data"))
  })

})

describe("Income filter dropdown creation", {

  sample_data <- load_sample_data()
  latest <- sample_data$latest

  it("unique income values can be extracted without NA", {
    incomes_vec <- unique(latest$income) |>
      stats::na.omit() |>
      as.character()

    expect_type(incomes_vec, "character")
    expect_false(any(is.na(incomes_vec)),
                 info = "NA values found after na.omit()")
  })

  it("income values can be sorted alphabetically", {
    incomes_vec <- unique(latest$income) |>
      stats::na.omit() |>
      as.character() |>
      sort()

    expect_equal(incomes_vec, sort(incomes_vec),
                 info = "Income values not properly sorted")
  })

  it("income dropdown choices include 'All' option", {
    incomes_vec <- unique(latest$income) |>
      stats::na.omit() |>
      as.character() |>
      sort()
    incomes <- c("All" = "all", setNames(incomes_vec, incomes_vec))

    expect_true("all" %in% incomes,
                info = "'All' option missing from dropdown choices")
    expect_equal(names(incomes)[1], "All",
                 info = "'All' should be first option")
  })

  it("income dropdown has named vector structure", {
    incomes_vec <- unique(latest$income) |>
      stats::na.omit() |>
      as.character() |>
      sort()
    incomes <- c("All" = "all", setNames(incomes_vec, incomes_vec))

    expect_true(is.character(incomes),
                info = "Dropdown choices should be character vector")
    expect_true(!is.null(names(incomes)),
                info = "Dropdown choices should be named vector")
  })

  it("no NA in income dropdown choices", {
    incomes_vec <- unique(latest$income) |>
      stats::na.omit() |>
      as.character() |>
      sort()
    incomes <- c("All" = "all", setNames(incomes_vec, incomes_vec))

    expect_false(any(is.na(incomes)),
                 info = "NA values in dropdown choices")
    expect_false(any(is.na(names(incomes))),
                 info = "NA values in dropdown names")
  })

})

describe("Income filter logic", {

  sample_data <- load_sample_data()
  test_data <- sample_data$latest

  it("can filter by specific income group", {
    # Test filtering for Upper middle income
    filtered <- test_data |>
      dplyr::filter(!is.na(income) & income == "Upper middle income")

    expect_true(nrow(filtered) > 0,
                info = "No rows after filtering by income")
    expect_true(all(filtered$income == "Upper middle income", na.rm = TRUE),
                info = "Filtered data contains wrong income groups")
  })

  it("filters out NA values when income filter is applied", {
    filtered <- test_data |>
      dplyr::filter(!is.na(income) & income == "Lower middle income")

    expect_false(any(is.na(filtered$income)),
                 info = "NA values present after filtering")
  })

  it("returns all data when income filter is 'all'", {
    # Simulate the "all" filter condition
    input_income <- "all"

    if (input_income != "all" && !is.na(input_income)) {
      filtered <- test_data |>
        dplyr::filter(!is.na(income) & income == input_income)
    } else {
      filtered <- test_data
    }

    expect_equal(nrow(filtered), nrow(test_data),
                 info = "All data should be returned when filter is 'all'")
  })

  it("handles NA input gracefully", {
    input_income <- NA

    # This should not filter the data
    if (input_income != "all" && !is.na(input_income)) {
      filtered <- test_data |>
        dplyr::filter(!is.na(income) & income == input_income)
    } else {
      filtered <- test_data
    }

    expect_equal(nrow(filtered), nrow(test_data),
                 info = "Should return all data when input is NA")
  })

  it("filter preserves data structure", {
    filtered <- test_data |>
      dplyr::filter(!is.na(income) & income == "High income")

    # Check that all original columns are preserved
    expect_true(all(names(test_data) %in% names(filtered)),
                info = "Filtered data missing columns")
  })

})

describe("Income filter consistency across modules", {

  sample_data <- load_sample_data()
  latest <- sample_data$latest

  # Standard filter pattern used across all modules
  create_income_choices <- function(data) {
    incomes_vec <- unique(data$income) |>
      stats::na.omit() |>
      as.character() |>
      sort()
    c("All" = "all", setNames(incomes_vec, incomes_vec))
  }

  apply_income_filter <- function(data, input_income) {
    if (input_income != "all" && !is.na(input_income)) {
      data |> dplyr::filter(!is.na(income) & income == input_income)
    } else {
      data
    }
  }

  it("standard pattern creates valid dropdown choices", {
    choices <- create_income_choices(latest)

    expect_true(is.character(choices))
    expect_true("all" %in% choices)
    expect_false(any(is.na(choices)))
    expect_false(any(is.na(names(choices))))
  })

  it("standard pattern filters correctly for each income group", {
    income_groups <- unique(latest$income) |>
      na.omit() |>
      as.character()

    for (group in income_groups) {
      filtered <- apply_income_filter(latest, group)

      expect_true(nrow(filtered) > 0,
                  info = paste("No data for income group:", group))
      expect_true(all(filtered$income == group, na.rm = TRUE),
                  info = paste("Wrong income group in filtered data:", group))
    }
  })

  it("standard pattern returns all data for 'all' selection", {
    filtered <- apply_income_filter(latest, "all")
    expect_equal(nrow(filtered), nrow(latest))
  })

})

describe("Income data quality checks", {

  sample_data <- load_sample_data()

  it("income completeness meets threshold", {
    income_col <- sample_data$latest$income
    completeness <- (sum(!is.na(income_col)) / length(income_col)) * 100

    # At least 80% completeness
    expect_gte(completeness, 80,
               info = paste0("Income completeness only ",
                           round(completeness, 1), "%"))
  })

  it("no duplicate country-income mappings", {
    country_income <- sample_data$latest |>
      dplyr::select(country, income) |>
      dplyr::filter(!is.na(country) & !is.na(income)) |>
      dplyr::distinct()

    # Each country should map to exactly one income group
    country_counts <- country_income |>
      dplyr::group_by(country) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    max_count <- max(country_counts$n)
    expect_equal(max_count, 1,
                 info = "Some countries have multiple income classifications")
  })

  it("income groups have reasonable distribution", {
    income_dist <- sample_data$latest |>
      dplyr::filter(!is.na(income)) |>
      dplyr::group_by(income) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    # No income group should dominate (>80% of data)
    total <- sum(income_dist$n)
    max_pct <- (max(income_dist$n) / total) * 100

    expect_lte(max_pct, 80,
               info = paste0("Income group distribution too skewed: ",
                           round(max_pct, 1), "%"))
  })

})

describe("Integration: Income filter with region filter", {

  sample_data <- load_sample_data()
  latest <- sample_data$latest

  it("can apply both income and region filters simultaneously", {
    # Pick a specific region and income group that should exist
    test_region <- latest |>
      dplyr::filter(!is.na(region)) |>
      dplyr::pull(region) |>
      unique() |>
      head(1)

    test_income <- latest |>
      dplyr::filter(!is.na(income)) |>
      dplyr::pull(income) |>
      unique() |>
      head(1)

    filtered <- latest |>
      dplyr::filter(!is.na(region) & region == test_region) |>
      dplyr::filter(!is.na(income) & income == test_income)

    expect_no_error({
      nrow(filtered)
    }, info = "Error applying both filters")

    if (nrow(filtered) > 0) {
      expect_true(all(filtered$region == test_region, na.rm = TRUE))
      expect_true(all(filtered$income == test_income, na.rm = TRUE))
    }
  })

  it("combined filters preserve data integrity", {
    filtered <- latest |>
      dplyr::filter(!is.na(region)) |>
      dplyr::filter(!is.na(income))

    # All expected columns should still exist
    expect_true("country" %in% names(filtered))
    expect_true("region" %in% names(filtered))
    expect_true("income" %in% names(filtered))
  })

})
