# tests/testthat/test-page_markup.R
# shinyapps.io injects its own <script> tags immediately after the first
# occurrence of the text "<body" in the served page. The page's head carries
# inline JavaScript from app/main.R; if that JavaScript (even a comment in it)
# contains the literal tag, the injection lands inside the script, cuts it in
# two and shows its source as page text -- which broke the live deploy once.

test_that("page code in main.R never contains a literal body or head tag", {
  src <- readLines(here::here("app/main.R"), warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]          # R comments are never served
  offending <- grep("<\\s*/?\\s*(body|head)\\b", code, value = TRUE)
  testthat::expect_equal(offending, character(0))
})
