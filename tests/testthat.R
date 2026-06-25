# tests/testthat.R
# Test entrypoint for wbes.dashboard (Rhino: also runnable via rhino::test_r()).

library(testthat)

# box must NOT be attached (it is namespace-only); test files call box::use().
# box resolves app/logic/* and app/view/* relative to the project root.
options(box.path = getwd())

test_dir("tests/testthat")
