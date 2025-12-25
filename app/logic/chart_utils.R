# app/logic/chart_utils.R
# Chart utilities including captions and IDs

box::use(
  shiny[tags, icon, downloadButton, div]
)

#' Generate a unique chart ID
#' @param chart_name Name of the chart
#' @param timestamp Whether to include timestamp
#' @return Unique ID string
#' @export
generate_chart_id <- function(chart_name, timestamp = TRUE) {
  if (timestamp) {
    paste0("WBES-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(chart_name, 1, 4))
  } else {
    paste0("WBES-", substr(chart_name, 1, 8))
  }
}

#' Create chart caption HTML
#' @param chart_name Name of the chart for ID generation
#' @return HTML tags for caption
#' @export
create_chart_caption <- function(chart_name = "chart") {
  chart_id <- generate_chart_id(chart_name)
  tags$div(
    class = "chart-caption text-muted small mt-2",
    style = "font-size: 0.75rem; border-top: 1px solid #dee2e6; padding-top: 0.5rem;",
    tags$span("Source: World Bank Enterprise Surveys | "),
    tags$span("Created by: Kwiz Computing Technologies | "),
    tags$span(paste0("ID: ", chart_id))
  )
}

#' Create chart container with download button and caption
#' @param ns Namespace function
#' @param output_id Output ID
#' @param height Chart height
#' @param title Optional title
#' @param chart_type Type of chart for proper download
#' @return HTML container
#' @export
chart_with_caption <- function(ns, output_id, height = "400px", title = NULL, chart_type = "plotly") {
  div(
    class = "position-relative chart-container",
    if (!is.null(title)) tags$h4(title, class = "text-primary-teal mb-2"),
    div(
      class = "position-absolute",
      style = "top: 5px; right: 10px; z-index: 100;",
      downloadButton(
        ns(paste0("dl_", output_id)),
        label = "",
        icon = icon("download"),
        class = "btn-sm btn-outline-secondary",
        title = "Download chart"
      )
    ),
    if (chart_type == "plotly") {
      plotly::plotlyOutput(ns(output_id), height = height)
    } else if (chart_type == "leaflet") {
      leaflet::leafletOutput(ns(output_id), height = height)
    },
    create_chart_caption(output_id)
  )
}

#' Create map container with download button and caption
#' @param ns Namespace function
#' @param output_id Output ID
#' @param height Map height
#' @param title Optional title
#' @return HTML container
#' @export
map_with_caption <- function(ns, output_id, height = "350px", title = NULL) {
  div(
    class = "position-relative map-container",
    if (!is.null(title)) tags$h4(title, class = "text-primary-teal mb-2"),
    div(
      class = "position-absolute",
      style = "top: 5px; right: 10px; z-index: 1000;",
      downloadButton(
        ns(paste0("dl_", output_id)),
        label = "",
        icon = icon("download"),
        class = "btn-sm btn-outline-secondary",
        title = "Download map as image"
      )
    ),
    leaflet::leafletOutput(ns(output_id), height = height),
    create_chart_caption(output_id)
  )
}
