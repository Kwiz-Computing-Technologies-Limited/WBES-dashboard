# app/logic/wb_integration/__init__.R
# World Bank Integration Module - Public API

box::use(
  app/logic/wb_integration/wb_api[
    get_wb_income_classifications,
    get_wb_income_classifications_historical,
    map_wbes_countries_to_iso3,
    enrich_wbes_with_income,
    fetch_wb_indicator,
    search_wb_indicators
  ],
  app/logic/wb_integration/wb_cache[
    is_wb_cache_fresh,
    read_wb_cache,
    write_wb_cache,
    get_or_fetch_wb_data,
    clear_wb_cache,
    get_wb_cache_status
  ]
)

#' @export
get_wb_income_classifications <- get_wb_income_classifications

#' @export
get_wb_income_classifications_historical <- get_wb_income_classifications_historical

#' @export
map_wbes_countries_to_iso3 <- map_wbes_countries_to_iso3

#' @export
enrich_wbes_with_income <- enrich_wbes_with_income

#' @export
fetch_wb_indicator <- fetch_wb_indicator

#' @export
search_wb_indicators <- search_wb_indicators

#' @export
is_wb_cache_fresh <- is_wb_cache_fresh

#' @export
read_wb_cache <- read_wb_cache

#' @export
write_wb_cache <- write_wb_cache

#' @export
get_or_fetch_wb_data <- get_or_fetch_wb_data

#' @export
clear_wb_cache <- clear_wb_cache

#' @export
get_wb_cache_status <- get_wb_cache_status
