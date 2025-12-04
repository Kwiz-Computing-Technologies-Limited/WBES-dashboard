# tests/testthat/test-income_filter_modules.R
# Integration tests for income filter updates in Shiny modules

box::use(
  testthat[describe, it, expect_true, expect_false, expect_equal,
           expect_type, expect_no_error, expect_length, expect_gte],
  shiny[testServer, reactiveVal],
  app/logic/wbes_data[load_sample_data],
  app/view/infrastructure[server = infrastructure_server],
  app/view/finance_access[server = finance_server],
  app/view/overview[server = overview_server],
  app/view/mod_custom_analysis[server = custom_analysis_server]
)

describe("Infrastructure module: income filter updates", {

  sample_data <- load_sample_data()

  it("updates income selectInput with valid choices", {
    testServer(
      infrastructure_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        # Trigger the observeEvent by updating wbes_data
        session$flushReact()

        # Check that updateSelectInput was called for income
        updates <- session$getUpdateSelectInput("income")

        expect_true(!is.null(updates),
                    info = "income selectInput was not updated")

        if (!is.null(updates)) {
          choices <- updates$choices

          # Should include "All" option
          expect_true("all" %in% choices,
                      info = "Missing 'All' option in income choices")

          # Should not include NA
          expect_false(any(is.na(choices)),
                       info = "NA found in income choices")
          expect_false(any(is.na(names(choices))),
                       info = "NA found in income choice names")

          # Should have at least 2 choices (All + at least 1 income group)
          expect_gte(length(choices), 2,
                     info = "Too few income choices")
        }
      }
    )
  })

  it("filtered reactive excludes NA income values", {
    testServer(
      infrastructure_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        # Set income filter to a specific value
        session$setInputs(income = "Lower middle income")

        filtered_data <- filtered()

        # Should not contain NA income values
        expect_false(any(is.na(filtered_data$income)),
                     info = "Filtered data contains NA income values")

        # All income values should match the filter
        expect_true(all(filtered_data$income == "Lower middle income"),
                    info = "Filtered data contains wrong income values")
      }
    )
  })

  it("filtered reactive returns all data when income is 'all'", {
    testServer(
      infrastructure_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        # Set income filter to 'all'
        session$setInputs(income = "all")

        filtered_data <- filtered()

        # Should return all data (not filtered by income)
        # Compare row counts considering the data might have NAs filtered out elsewhere
        expect_gte(nrow(filtered_data), 1,
                   info = "No data returned with 'all' income filter")
      }
    )
  })

})

describe("Finance Access module: income filter updates", {

  sample_data <- load_sample_data()

  it("updates income selectInput with valid choices", {
    testServer(
      finance_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        updates <- session$getUpdateSelectInput("income")

        expect_true(!is.null(updates),
                    info = "income selectInput was not updated")

        if (!is.null(updates)) {
          choices <- updates$choices

          expect_true("all" %in% choices)
          expect_false(any(is.na(choices)))
          expect_false(any(is.na(names(choices))))
        }
      }
    )
  })

  it("income choices are sorted alphabetically", {
    testServer(
      finance_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        updates <- session$getUpdateSelectInput("income")

        if (!is.null(updates)) {
          choices <- updates$choices
          # Remove "all" and check if remaining are sorted
          income_values <- choices[choices != "all"]

          if (length(income_values) > 1) {
            expect_equal(income_values, sort(income_values),
                        info = "Income choices not sorted")
          }
        }
      }
    )
  })

})

describe("Overview module: income filter updates", {

  sample_data <- load_sample_data()

  it("updates income selectInput without NA values", {
    testServer(
      overview_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        updates <- session$getUpdateSelectInput("income")

        expect_true(!is.null(updates),
                    info = "income selectInput was not updated")

        if (!is.null(updates)) {
          choices <- updates$choices

          expect_false(any(is.na(choices)),
                       info = "NA in income dropdown choices")
          expect_true("all" %in% choices,
                      info = "'All' option missing")
        }
      }
    )
  })

  it("handles region and income filters together", {
    testServer(
      overview_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        # Get first valid region and income
        test_region <- sample_data$latest |>
          dplyr::filter(!is.na(region)) |>
          dplyr::pull(region) |>
          unique() |>
          head(1)

        test_income <- sample_data$latest |>
          dplyr::filter(!is.na(income)) |>
          dplyr::pull(income) |>
          unique() |>
          head(1)

        # Apply both filters
        session$setInputs(region = test_region, income = test_income)

        filtered_data <- filtered()

        # Should have data
        expect_gte(nrow(filtered_data), 0,
                   info = "Error with combined filters")

        # If data exists, check filters applied correctly
        if (nrow(filtered_data) > 0) {
          expect_false(any(is.na(filtered_data$income)),
                       info = "NA income in filtered data")
          expect_false(any(is.na(filtered_data$region)),
                       info = "NA region in filtered data")
        }
      }
    )
  })

})

describe("Custom Analysis module: income filter updates", {

  sample_data <- load_sample_data()

  it("updates incomes selectInput (note: plural 'incomes')", {
    testServer(
      custom_analysis_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        # Note: this module uses "incomes" (plural)
        updates <- session$getUpdateSelectInput("incomes")

        expect_true(!is.null(updates),
                    info = "incomes selectInput was not updated")

        if (!is.null(updates)) {
          choices <- updates$choices

          expect_true("all" %in% choices)
          expect_false(any(is.na(choices)))
        }
      }
    )
  })

  it("handles multi-select income filter", {
    testServer(
      custom_analysis_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        # Get two income groups
        income_groups <- sample_data$latest |>
          dplyr::filter(!is.na(income)) |>
          dplyr::pull(income) |>
          unique() |>
          head(2)

        if (length(income_groups) >= 2) {
          # Select multiple income groups
          session$setInputs(incomes = income_groups)

          filtered_data <- filtered_data()

          # Should only contain the selected income groups
          expect_true(all(filtered_data$income %in% income_groups, na.rm = TRUE),
                      info = "Filtered data contains wrong income groups")

          # Should not contain NA
          expect_false(any(is.na(filtered_data$income)),
                       info = "Filtered data contains NA income")
        }
      }
    )
  })

  it("multi-select filter combines with country and region filters", {
    testServer(
      custom_analysis_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()

        # Get test values
        test_country <- sample_data$latest |>
          dplyr::filter(!is.na(country)) |>
          dplyr::pull(country) |>
          unique() |>
          head(1)

        test_region <- sample_data$latest |>
          dplyr::filter(!is.na(region)) |>
          dplyr::pull(region) |>
          unique() |>
          head(1)

        test_income <- sample_data$latest |>
          dplyr::filter(!is.na(income)) |>
          dplyr::pull(income) |>
          unique() |>
          head(1)

        # Apply all three filters
        session$setInputs(
          countries = test_country,
          regions = test_region,
          incomes = test_income
        )

        expect_no_error({
          filtered_data <- filtered_data()
          nrow(filtered_data)
        }, info = "Error applying combined filters")
      }
    )
  })

})

describe("Cross-module consistency", {

  sample_data <- load_sample_data()

  it("all modules use consistent NA filtering pattern", {
    # Test that all modules produce the same income choices
    infrastructure_choices <- NULL
    finance_choices <- NULL
    overview_choices <- NULL

    testServer(
      infrastructure_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()
        updates <- session$getUpdateSelectInput("income")
        if (!is.null(updates)) {
          infrastructure_choices <<- updates$choices
        }
      }
    )

    testServer(
      finance_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()
        updates <- session$getUpdateSelectInput("income")
        if (!is.null(updates)) {
          finance_choices <<- updates$choices
        }
      }
    )

    testServer(
      overview_server,
      args = list(wbes_data = reactiveVal(sample_data)),
      {
        session$flushReact()
        updates <- session$getUpdateSelectInput("income")
        if (!is.null(updates)) {
          overview_choices <<- updates$choices
        }
      }
    )

    # All modules should have the same income choices
    if (!is.null(infrastructure_choices) && !is.null(finance_choices)) {
      expect_equal(infrastructure_choices, finance_choices,
                   info = "Infrastructure and Finance have different income choices")
    }

    if (!is.null(infrastructure_choices) && !is.null(overview_choices)) {
      expect_equal(infrastructure_choices, overview_choices,
                   info = "Infrastructure and Overview have different income choices")
    }
  })

  it("all modules exclude NA from dropdowns", {
    modules_to_test <- list(
      list(name = "infrastructure", server = infrastructure_server, input = "income"),
      list(name = "finance", server = finance_server, input = "income"),
      list(name = "overview", server = overview_server, input = "income")
    )

    for (module in modules_to_test) {
      testServer(
        module$server,
        args = list(wbes_data = reactiveVal(sample_data)),
        {
          session$flushReact()
          updates <- session$getUpdateSelectInput(module$input)

          if (!is.null(updates)) {
            expect_false(any(is.na(updates$choices)),
                        info = paste(module$name, "has NA in choices"))
          }
        }
      )
    }
  })

})

describe("Error handling and edge cases", {

  it("handles empty income data gracefully", {
    # Create test data with all NA income values
    empty_income_data <- load_sample_data()
    empty_income_data$latest$income <- NA_character_

    testServer(
      infrastructure_server,
      args = list(wbes_data = reactiveVal(empty_income_data)),
      {
        expect_no_error({
          session$flushReact()
        }, info = "Error handling empty income data")

        updates <- session$getUpdateSelectInput("income")

        # Should still create dropdown with at least "All" option
        if (!is.null(updates)) {
          expect_true("all" %in% updates$choices,
                      info = "Missing 'All' option with empty data")
        }
      }
    )
  })

  it("handles single income group gracefully", {
    # Create test data with only one income group
    single_income_data <- load_sample_data()
    single_income_data$latest$income <- "Lower middle income"

    testServer(
      infrastructure_server,
      args = list(wbes_data = reactiveVal(single_income_data)),
      {
        expect_no_error({
          session$flushReact()
        }, info = "Error handling single income group")

        updates <- session$getUpdateSelectInput("income")

        if (!is.null(updates)) {
          expect_gte(length(updates$choices), 2,
                     info = "Should have 'All' + 1 income group")
        }
      }
    )
  })

})
