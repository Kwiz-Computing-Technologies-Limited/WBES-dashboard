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

  # Required elements the modules consume. The firm-level table is optional:
  # it is private (git-ignored) and absent from a fresh checkout; see the
  # fresh-checkout test below.
  required <- c("latest", "country_panel", "country_sector", "country_size",
                "country_region", "regional", "countries",
                "country_codes", "years", "regions", "sectors",
                "label_mapping", "metadata", "quality", "firm_data_mode")
  for (el in required) {
    expect_true(el %in% names(d), info = paste("missing element:", el))
  }

  # raw microdata must NOT be shipped at runtime.
  expect_false("raw" %in% names(d))

  # Aggregates are real, non-empty data frames.
  expect_true(is.data.frame(d$latest))
  expect_gt(nrow(d$latest), 0)
  expect_true("country" %in% names(d$latest))
  if (!is.null(d$processed)) {
    expect_gt(nrow(d$processed), 0)
  }
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

test_that("a fresh checkout (committed aggregates only) loads without firm data", {
  committed <- c("latest", "country_panel", "country_sector", "country_size",
                 "country_region", "regional", "country_coordinates")
  src <- file.path(data_path, "processed")
  if (!all(file.exists(file.path(src, c(paste0(committed, ".parquet"), "meta.rds"))))) {
    skip("Committed aggregate artifacts not present")
  }
  fresh <- file.path(tempfile("fresh-checkout-"), "data")
  dir.create(file.path(fresh, "processed"), recursive = TRUE)
  file.copy(file.path(src, c(paste0(committed, ".parquet"), "meta.rds", "wb_macro.rds")),
            file.path(fresh, "processed"))

  # No private artifact, no download URL, no forced mode.
  old <- Sys.getenv(c("WBES_PROCESSED_URL", "WBES_DATA_MODE"), unset = NA)
  Sys.unsetenv(c("WBES_PROCESSED_URL", "WBES_DATA_MODE"))
  on.exit({
    restore <- old[!is.na(old)]
    if (length(restore) > 0) do.call(Sys.setenv, as.list(restore))
  }, add = TRUE)

  d <- load_precomputed(fresh)
  expect_null(d$processed)
  expect_equal(d$firm_data_mode, "none")
  expect_gt(nrow(d$latest), 0)
})
