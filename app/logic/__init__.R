# app/logic/__init__.R
# Logic module exports

box::use(app/logic/wbes_data)
box::use(app/logic/column_labels)

#' @export
load_wbes_data <- wbes_data$load_wbes_data

#' @export
load_sample_data <- wbes_data$load_sample_data

#' @export
generate_quality_metadata <- wbes_data$generate_quality_metadata

# Column label management
#' @export
extract_column_labels <- column_labels$extract_column_labels

#' @export
create_wbes_label_mapping <- column_labels$create_wbes_label_mapping

#' @export
get_column_label <- column_labels$get_column_label

#' @export
apply_descriptive_labels <- column_labels$apply_descriptive_labels

#' @export
format_label_for_ui <- column_labels$format_label_for_ui

#' @export
create_label_dictionary <- column_labels$create_label_dictionary

#' @export
get_category_labels <- column_labels$get_category_labels
