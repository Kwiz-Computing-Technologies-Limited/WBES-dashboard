# tests/testthat/test-shared_filters.R
# Tests for the shared filtering logic that drives every module's reactions.
# Uses a small synthetic frame (no data files needed) so it is fast + hermetic.

options(box.path = here::here())

box::use(
  testthat[test_that, expect_equal, expect_true, expect_setequal],
  app/logic/shared_filters[apply_common_filters, get_filter_choices, remove_na_columns]
)

sample_df <- function() {
  data.frame(
    country   = c("Kenya", "Kenya", "Nigeria", "Ghana", "Ghana"),
    region    = c("AFR", "AFR", "AFR", "AFR", "AFR"),
    income    = c("Low", "Low", "Lower-Middle", "Lower-Middle", "Lower-Middle"),
    sector    = c("Manufacturing", "Services", "Manufacturing", "Services", "Retail"),
    firm_size = c("Small", "Large", "Small", "Medium", "Small"),
    year      = c(2020, 2023, 2021, 2019, 2022),
    value     = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
}

test_that("'all' filters are no-ops", {
  d <- sample_df()
  out <- apply_common_filters(d, region_value = "all", sector_value = "all",
                              firm_size_value = "all", income_value = "all",
                              year_value = "all")
  expect_equal(nrow(out), nrow(d))
})

test_that("income filter selects only matching rows", {
  d <- sample_df()
  out <- apply_common_filters(d, income_value = "Low")
  expect_true(all(out$income == "Low"))
  expect_equal(nrow(out), 2)
})

test_that("firm_size and sector filters combine", {
  d <- sample_df()
  out <- apply_common_filters(d, sector_value = "Manufacturing",
                              firm_size_value = "Small")
  expect_equal(nrow(out), 2)
  expect_setequal(out$country, c("Kenya", "Nigeria"))
})

test_that("year = 'latest' keeps the most recent row per country", {
  d <- sample_df()
  out <- apply_common_filters(d, year_value = "latest")
  expect_equal(nrow(out), 3)                       # one per country
  expect_equal(out$year[out$country == "Kenya"], 2023)
  expect_equal(out$year[out$country == "Ghana"], 2022)
})

test_that("explicit year vector filters to those years", {
  d <- sample_df()
  out <- apply_common_filters(d, year_value = c("2020", "2021"))
  expect_setequal(out$year, c(2020, 2021))
})

test_that("get_filter_choices excludes NA and prepends an 'all' option", {
  d <- sample_df()
  d$income[1] <- NA
  choices <- get_filter_choices(d, "income", add_all = TRUE, all_label = "All Income Levels")
  expect_equal(unname(choices[1]), "all")
  expect_true(all(c("Low", "Lower-Middle") %in% names(choices)))
})

test_that("remove_na_columns drops all-NA columns", {
  d <- sample_df()
  d$empty <- NA
  out <- remove_na_columns(d)
  expect_true(!"empty" %in% names(out))
  expect_true("value" %in% names(out))
})
