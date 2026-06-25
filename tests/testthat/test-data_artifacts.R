# tests/testthat/test-data_artifacts.R
# Tests for the precomputed-artifact data layer (app/logic/data_artifacts.R).
# These verify the contract the app relies on at app-scope load time.

options(box.path = here::here())

box::use(
  testthat[test_that, expect_true, expect_false, expect_equal, expect_gt, skip],
  app/logic/data_artifacts[artifacts_available, load_precomputed]
)

data_path <- here::here("data")

test_that("artifacts_available() is a logical scalar", {
  res <- artifacts_available(data_path)
  expect_true(is.logical(res))
  expect_equal(length(res), 1L)
})

test_that("load_precomputed() returns the app data contract", {
  if (!artifacts_available(data_path)) {
    skip("No build artifacts present (run scripts/build_data.R)")
  }

  d <- load_precomputed(data_path)

  # Required elements the modules consume.
  required <- c("latest", "country_panel", "country_sector", "country_size",
                "country_region", "regional", "processed", "countries",
                "country_codes", "years", "regions", "sectors",
                "label_mapping", "metadata", "quality")
  for (el in required) {
    expect_true(el %in% names(d), info = paste("missing element:", el))
  }

  # raw microdata must NOT be shipped at runtime.
  expect_false("raw" %in% names(d))

  # Aggregates are real, non-empty data frames.
  expect_true(is.data.frame(d$latest))
  expect_gt(nrow(d$latest), 0)
  expect_gt(nrow(d$processed), 0)
  expect_true("country" %in% names(d$latest))
})

test_that("country dimension is internally consistent", {
  if (!artifacts_available(data_path)) {
    skip("No build artifacts present (run scripts/build_data.R)")
  }
  d <- load_precomputed(data_path)
  expect_gt(length(d$countries), 0)
  expect_gt(length(d$years), 0)
  # latest holds one row per country aggregate.
  expect_equal(nrow(d$latest), length(unique(d$latest$country)))
})
