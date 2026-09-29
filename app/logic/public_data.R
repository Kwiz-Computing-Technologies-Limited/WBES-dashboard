# app/logic/public_data.R
# Disclosure control for the public (aggregated) data set.
#
# Two ways a published cell set can still reveal individual firms:
#
# 1. Item-level: a cell has at least MIN_CELL firms, but a given question may
#    have been answered by fewer. Its mean over 1-4 respondents is their own
#    answers. suppress_small_items() blanks those values.
#
# 2. Differencing: a table published beside the cells, computed from ALL firms
#    (e.g. a country-sector mean over firms whose sector was withheld from the
#    cells), minus the cells it covers isolates the withheld firms. Every public
#    table is therefore rebuilt from the published cells alone
#    (public_table()), so subtracting cells from it recovers nothing new.
#
# Cells come from scripts/build_public_cells.R: `<col>` is the mean over the
# firms that answered and `<col>__n` how many did.

box::use(
  dplyr[group_by, summarise, across, all_of, inner_join],
  stats[complete.cases]
)

#' Indicator columns of a cell table (those with a `<col>__n` count)
#' @param cells Cell table
#' @return Character vector of indicator column names
#' @export
cell_indicators <- function(cells) {
  sub("__n$", "", grep("__n$", names(cells), value = TRUE))
}

#' Blank indicator values answered by fewer than `min_cell` firms in a cell
#'
#' @param cells Cell table
#' @param min_cell Smallest number of respondents a published mean may rest on
#' @return The cell table with those means set to NA and their counts to 0
#' @export
suppress_small_items <- function(cells, min_cell) {
  for (col in cell_indicators(cells)) {
    n <- cells[[paste0(col, "__n")]]
    small <- !is.na(n) & n > 0 & n < min_cell
    cells[[col]][small] <- NA_real_
    cells[[paste0(col, "__n")]][small] <- 0L
  }
  cells
}

#' Aggregate cells to a grouping, as the firm-level tables would be
#'
#' Means are weighted by respondents, so they equal the firm-level mean over
#' the firms the cells publish. Rows whose key is withheld (NA) are left out,
#' exactly as the firm-level filters leave out firms with that value missing.
#'
#' @param cells Cell table (after suppress_small_items())
#' @param keys Grouping columns
#' @return One row per key combination: keys, sample_size and indicator means
#' @export
aggregate_cells <- function(cells, keys) {
  inds <- cell_indicators(cells)
  cells <- cells[complete.cases(cells[keys]), , drop = FALSE]
  sums <- cells
  for (col in inds) {
    n <- cells[[paste0(col, "__n")]]
    sums[[col]] <- ifelse(n > 0, cells[[col]] * n, 0)
  }
  out <- sums |>
    group_by(across(all_of(keys))) |>
    summarise(
      sample_size = sum(n_firms),
      across(all_of(c(inds, paste0(inds, "__n"))), sum),
      .groups = "drop"
    )
  for (col in inds) {
    n <- out[[paste0(col, "__n")]]
    out[[col]] <- ifelse(n > 0, out[[col]] / n, NA_real_)
  }
  out[, c(keys, "sample_size", inds)]
}

#' Rebuild a published table from the cells
#'
#' Keeps the template's rows (for the key combinations the cells cover) and its
#' descriptive columns (names, coordinates, income group...), and replaces
#' every indicator the cells carry, plus sample_size, with values aggregated
#' from the cells. Indicators the cells do not carry are kept as they are:
#' with no cell-level values of them there is nothing to subtract.
#'
#' @param template The table as built from all firms
#' @param cells Cell table (after suppress_small_items())
#' @param keys The template's grouping columns
#' @return The public version of the table
#' @export
public_table <- function(template, cells, keys) {
  agg <- aggregate_cells(cells, keys)
  replaced <- intersect(c("sample_size", cell_indicators(cells)), names(template))
  kept <- template[, setdiff(names(template), replaced), drop = FALSE]
  out <- inner_join(kept, agg[, c(keys, replaced)], by = keys)
  out[, names(template)]
}
