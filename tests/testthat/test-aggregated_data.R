# tests/testthat/test-aggregated_data.R
# The public aggregated mode (WBES_DATA_MODE=aggregated): cells expand to
# stand-in rows that reproduce group means, carry a marker, and report how many
# firms a breakdown leaves out. Synthetic cells only, so it runs without data.

options(box.path = here::here())

box::use(
  testthat[test_that, expect_equal, expect_true, expect_false, expect_null, expect_match],
  app/logic/data_artifacts[expand_cells, is_aggregated, finish_etl_data,
                           withheld_firms, withheld_note]
)

# Two published cells for Kenya plus one pooled cell (sector withheld) and one
# pooled further (sector and size withheld), as build_public_cells.R writes them.
sample_cells <- function() {
  data.frame(
    country = "Kenya", year = 2018,
    sector = c("Retail", "Food", NA, NA),
    firm_size = c("Small", "Small", "Medium", NA),
    female_ownership = c(TRUE, FALSE, FALSE, NA),
    pooled = c(0L, 0L, 1L, 2L),
    n_firms = c(6L, 10L, 5L, 5L),
    credit = c(40, 70, 20, 90),
    credit__n = c(5L, 10L, 4L, 2L),
    stringsAsFactors = FALSE
  )
}

test_that("expand_cells gives one row per firm with the answered count non-missing", {
  rows <- expand_cells(sample_cells())
  expect_equal(nrow(rows), 26L)
  answered <- tapply(!is.na(rows$credit), rows$cell_id, sum)
  expect_equal(as.integer(answered), c(5L, 10L, 4L, 2L))
})

test_that("expanded rows reproduce the firm-level mean of any cell union", {
  cells <- sample_cells()
  rows <- expand_cells(cells)
  firm_mean <- sum(cells$credit * cells$credit__n) / sum(cells$credit__n)
  expect_equal(mean(rows$credit, na.rm = TRUE), firm_mean)
  retail_food <- rows[!is.na(rows$sector), ]
  expect_equal(mean(retail_food$credit, na.rm = TRUE), (40 * 5 + 70 * 10) / 15)
})

test_that("expanded rows are marked with the cell they came from", {
  rows <- expand_cells(sample_cells())
  expect_true("cell_id" %in% names(rows))
  expect_false(any(grepl("__n$", names(rows))))
})

test_that("only an explicit aggregated mode counts as aggregated", {
  expect_true(is_aggregated(list(firm_data_mode = "aggregated")))
  expect_false(is_aggregated(list(firm_data_mode = "firm")))
  # The full-ETL fallback historically had no mode at all.
  expect_false(is_aggregated(list(processed = data.frame(x = 1))))
})

test_that("the full-ETL fallback is marked as firm-level data", {
  etl <- finish_etl_data(list(processed = data.frame(x = 1:3), raw = 1, wb_macro = 2))
  expect_equal(etl$firm_data_mode, "firm")
  expect_null(etl$raw)
  expect_null(etl$wb_macro)
  expect_equal(finish_etl_data(list(processed = NULL))$firm_data_mode, "none")
})

test_that("withheld_firms counts the firms each breakdown leaves out", {
  rows <- expand_cells(sample_cells())
  expect_equal(withheld_firms(rows, "sector"), 10L)          # both pooled cells
  expect_equal(withheld_firms(rows, "firm_size"), 5L)        # only the stage-2 cell
  expect_equal(withheld_firms(rows, "female_ownership"), 0L)
  # Real firm rows have no pooling at all.
  expect_equal(withheld_firms(data.frame(sector = "Retail"), "sector"), 0L)
})

test_that("withheld_note reports the count and share, or nothing", {
  rows <- expand_cells(sample_cells())
  note <- withheld_note(rows, "sector")
  expect_match(note, "10 of 26 firms")
  expect_match(note, "38.5%")
  expect_match(note, "by sector")
  # Sector withholds more firms than size, so a combined breakdown uses it.
  expect_match(withheld_note(rows, c("sector", "firm_size")), "10 of 26")
  expect_null(withheld_note(rows, "female_ownership"))
  expect_null(withheld_note(rows, character(0)))
})
