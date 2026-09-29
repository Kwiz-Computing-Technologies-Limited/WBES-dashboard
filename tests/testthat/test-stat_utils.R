# tests/testthat/test-stat_utils.R
# Significance tests must not run on aggregated-cell stand-ins: they reproduce
# group means but carry no firm-level spread, so any p-value would be invented.

options(box.path = here::here())

box::use(
  testthat[test_that, expect_true, expect_false, expect_equal, expect_match],
  app/logic/stat_utils[is_cell_expanded, calculate_correlation_matrix,
                       format_correlation_table, anova_with_tukey,
                       format_anova_results]
)

real_rows <- function() {
  set.seed(1)
  data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    x = rnorm(30), y = rnorm(30)
  )
}

cell_rows <- function() {
  d <- real_rows()
  d$cell_id <- rep(1:3, each = 10)
  d
}

test_that("cell rows are recognised by their marker", {
  expect_true(is_cell_expanded(cell_rows()))
  expect_false(is_cell_expanded(real_rows()))
  expect_false(is_cell_expanded(NULL))
})

test_that("correlations run on real rows and are refused on cell rows", {
  ok <- calculate_correlation_matrix(real_rows(), c("x", "y"))
  expect_equal(dim(ok$correlation), c(2L, 2L))

  refused <- calculate_correlation_matrix(cell_rows(), c("x", "y"))
  expect_true(isTRUE(refused$unavailable))
  html <- as.character(format_correlation_table(refused))
  expect_match(html, "public aggregated data")
  expect_false(grepl("<table", html))
})

test_that("ANOVA runs on real rows and is refused on cell rows", {
  ok <- anova_with_tukey(real_rows(), "x", "group")
  expect_false(identical(ok$test_type, "unavailable"))

  refused <- anova_with_tukey(cell_rows(), "x", "group")
  expect_equal(refused$test_type, "unavailable")
  expect_match(as.character(format_anova_results(refused)), "public aggregated data")
})
