# app/logic/__init__.R
# Logic module exports

box::use(app/logic/wbes_data)

#' @export
load_wbes_data <- wbes_data$load_wbes_data

#' @export
load_sample_data <- wbes_data$load_sample_data

#' @export
generate_quality_metadata <- wbes_data$generate_quality_metadata

#' @export
WBES_INDICATORS <- wbes_data$WBES_INDICATORS
