# tests/testthat/test-security.R
# Visitor-typed text must stay text: custom region/sector names go into onclick
# handlers, report title/author into a downloadable HTML file, and a signed
# data URL must not reach the logs through an error message.

options(box.path = here::here())

box::use(
  testthat[test_that, expect_equal, expect_false, expect_true, expect_match],
  jsonlite[fromJSON],
  app/logic/shared_filters[set_input_value_js],
  app/logic/data_artifacts[redact_urls],
  app/view/mod_custom_analysis[generate_html_report]
)

test_that("a region name cannot break out of the onclick JavaScript", {
  evil <- "x');alert(document.cookie);//"
  js <- set_input_value_js("main-edit_region_name", evil)
  # The value is one JSON string literal, which decodes back to the name as typed.
  args <- regmatches(js, regexec("^Shiny\\.setInputValue\\((\".*\"), (\".*\"), \\{priority: 'event'\\}\\)$", js))[[1]]
  expect_equal(length(args), 3L)
  expect_equal(fromJSON(args[[2]]), "main-edit_region_name")
  expect_equal(fromJSON(args[[3]]), evil)
  expect_false(grepl("'x'", js, fixed = TRUE))
})

test_that("names with quotes, backslashes and non-ASCII survive intact", {
  for (name in c('He said "hi"', "back\\slash", "Côte d'Ivoire group")) {
    js <- set_input_value_js("id", name)
    value <- sub("^Shiny\\.setInputValue\\(\"id\", (.*), \\{priority: 'event'\\}\\)$", "\\1", js)
    expect_equal(fromJSON(value), name)
  }
})

test_that("report title and author are escaped in the downloaded HTML", {
  html <- generate_html_report(
    data = data.frame(country = c("Kenya", "Ghana")),
    indicators = c("a", "b"),
    title = "</title><script>alert(1)</script>",
    author = "<img src=x onerror=alert(2)>"
  )
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_false(grepl("<img", html, fixed = TRUE))
  expect_match(html, "&lt;/title&gt;&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_match(html, "<strong>Countries Analyzed:</strong> 2", fixed = TRUE)
})

test_that("signed URLs are redacted from logged download errors", {
  msg <- "cannot open URL 'https://firebasestorage.googleapis.com/v0/b/x/o/p.parquet?alt=media&token=SECRET-123'"
  out <- redact_urls(msg)
  expect_false(grepl("SECRET-123", out, fixed = TRUE))
  expect_match(out, "https://firebasestorage.googleapis.com/v0/b/x/o/p.parquet?<redacted>'", fixed = TRUE)
  expect_equal(redact_urls("no url here"), "no url here")
  expect_equal(redact_urls("see https://example.com/file.csv now"),
               "see https://example.com/file.csv now")
})
