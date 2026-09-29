# tests/testthat/test-public_data.R
# Disclosure control for the public data set: no indicator resting on fewer
# than MIN_CELL respondents, and no published table that can be differenced
# against the cells to recover a withheld firm.

options(box.path = here::here())

box::use(
  testthat[test_that, expect_equal, expect_true, expect_false],
  app/logic/public_data[suppress_small_items, aggregate_cells, public_table],
  app/logic/shared_filters[parse_country_list]
)

# Kenya 2018: a Retail cell (6 firms) and a Food cell (5 firms) published with
# their sector, plus one pooled cell (5 firms, sector withheld) that holds the
# country's lone Garments firm among others.
cells <- function() {
  data.frame(
    country = "Kenya", country_code = "KEN", year = 2018,
    sector = c("Retail", "Food", NA),
    pooled = c(0L, 0L, 1L),
    n_firms = c(6L, 5L, 5L),
    credit = c(50, 20, 80), credit__n = c(6L, 5L, 5L),
    exports = c(10, 30, 40), exports__n = c(2L, 5L, 5L),
    stringsAsFactors = FALSE
  )
}

test_that("an indicator answered by fewer than 5 firms in a cell is blanked", {
  out <- suppress_small_items(cells(), 5)
  expect_true(is.na(out$exports[1]))
  expect_equal(out$exports__n[1], 0L)
  expect_equal(out$exports[2:3], c(30, 40))      # 5 respondents is enough
  expect_equal(out$credit, c(50, 20, 80))        # untouched
})

test_that("aggregates are respondent-weighted means over the published cells", {
  agg <- aggregate_cells(suppress_small_items(cells(), 5), c("country", "year"))
  expect_equal(agg$sample_size, 16L)
  expect_equal(agg$credit, (50 * 6 + 20 * 5 + 80 * 5) / 16)
  expect_equal(agg$exports, (30 * 5 + 40 * 5) / 10)   # the blanked value is out
})

test_that("a withheld key is left out of that breakdown, as in the app's filters", {
  agg <- aggregate_cells(cells(), c("country", "sector"))
  expect_equal(sort(agg$sector), c("Food", "Retail"))
})

test_that("a public table cannot be differenced against the cells", {
  c5 <- suppress_small_items(cells(), 5)
  # The table as built from ALL firms: it counts the Garments firm hidden in
  # the pooled cell, and has a Garments row of its own.
  template <- data.frame(
    country = "Kenya", country_code = "KEN",
    sector = c("Retail", "Food", "Garments"),
    region = "AFR",
    sample_size = c(6L, 5L, 1L),
    credit = c(50, 20, 100), exports = c(10, 30, 0),
    stringsAsFactors = FALSE
  )
  pub <- public_table(template, c5, c("country", "country_code", "sector"))

  # The one-firm Garments row is gone; nothing in the table rests on it.
  expect_false("Garments" %in% pub$sector)
  # Every published total and mean is exactly what the cells give, so
  # subtracting cells from the table leaves nothing.
  published <- aggregate_cells(c5, c("country", "country_code", "sector"))
  m <- merge(pub, published, by = c("country", "country_code", "sector"))
  expect_equal(m$sample_size.x, m$sample_size.y)
  expect_equal(m$credit.x, m$credit.y)
  # Descriptive columns and the column order are kept.
  expect_equal(names(pub), names(template))
  expect_equal(unique(pub$region), "AFR")
})

countries <- c("Kenya", "Uganda", "Korea, Rep.", "Congo, Dem. Rep.", "Congo, Rep.")

test_that("country lists parse with pipes, commas, spaces and commas in names", {
  expect_equal(parse_country_list("Kenya,Uganda", countries), c("Kenya", "Uganda"))
  expect_equal(parse_country_list("Kenya, Uganda", countries), c("Kenya", "Uganda"))
  expect_equal(parse_country_list("Kenya|Korea, Rep.", countries), c("Kenya", "Korea, Rep."))
  expect_equal(parse_country_list("Congo, Dem. Rep.,Kenya", countries),
               c("Congo, Dem. Rep.", "Kenya"))
  expect_equal(parse_country_list("Congo, Rep.,Congo, Dem. Rep.", countries),
               c("Congo, Rep.", "Congo, Dem. Rep."))
  expect_equal(parse_country_list("Atlantis,Kenya,Kenya", countries), "Kenya")
  expect_equal(parse_country_list(NULL, countries), character(0))
  expect_equal(parse_country_list("", countries), character(0))
})
