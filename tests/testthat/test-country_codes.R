# tests/testthat/test-country_codes.R
# Country codes decide which microdata rows survive aggregation (rows without
# one are dropped), so a file that carries codes only in `a0` must still work,
# and a file whose `a0` is the questionnaire type must not grow fake codes.

options(box.path = here::here())

box::use(
  testthat[test_that, expect_equal, expect_true],
  app/logic/wbes_data[resolve_country_code, as_iso3, iso3_to_name]
)

test_that("wbcode wins, then country_abr, row by row", {
  code <- resolve_country_code(
    wbcode = c("KEN", NA, ""),
    country_abr = c("XXX", "UGA", "TZA")
  )
  expect_equal(code, c("KEN", "UGA", "TZA"))
})

test_that("a0 is used when it is the only column holding an ISO3 code", {
  expect_equal(resolve_country_code(a0 = c("ken", "GHA")), c("KEN", "GHA"))
})

test_that("a0 as questionnaire type (e.g. 3 = Core) is never taken as a code", {
  core <- structure(c(3, 1), labels = c(Core = 3, Other = 1))
  expect_equal(as_iso3(core), c(NA_character_, NA_character_))
  # With no code anywhere, the country name decides.
  expect_equal(resolve_country_code(a0 = core, country = c("Kenya", "Rwanda")),
               c("KEN", "RWA"))
})

test_that("names fill only rows no column could code", {
  code <- resolve_country_code(wbcode = c("NGA", NA),
                               country = c("Kenya", "Ghana"))
  expect_equal(code, c("NGA", "GHA"))
  expect_true(is.na(resolve_country_code(country = "Atlantis")))
})

test_that("a code-only file still gets country names", {
  expect_equal(iso3_to_name(c("KEN", NA, "KEN")), c("Kenya", NA, "Kenya"))
})
