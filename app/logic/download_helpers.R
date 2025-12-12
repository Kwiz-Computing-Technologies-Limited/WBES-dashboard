# app/logic/download_helpers.R
# Reusable download helper functions for charts and data

box::use(
 shiny[downloadButton, downloadHandler, icon, tags, div, NS],
 plotly[plotly_IMAGE],
 htmlwidgets[saveWidget],
 utils[write.csv]
)

#' Create a download button with consistent styling
#' @param id The button id (namespaced)
#' @param label Button label
#' @param icon_name Icon name (default: "download")
#' @export
download_btn <- function(id, label = "Download", icon_name = "download") {
 downloadButton(
   id,
   label = label,
   icon = icon(icon_name),
   class = "btn-sm btn-outline-secondary"
 )
}

#' Create a small inline download button
#' @param id The button id (namespaced)
#' @param title Tooltip title
#' @export
download_btn_small <- function(id, title = "Download") {
 downloadButton(
   id,
   label = "",
   icon = icon("download"),
   class = "btn-sm btn-link p-0 ms-2",
   title = title,
   style = "font-size: 0.85em;"
 )
}

#' Create download handler for CSV data
#' @param data_reactive Reactive expression returning data frame
#' @param filename_prefix Prefix for the filename
#' @export
csv_download_handler <- function(data_reactive, filename_prefix = "data") {
 downloadHandler(
   filename = function() {
     paste0(filename_prefix, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
   },
   content = function(file) {
     data <- if (is.reactive(data_reactive)) data_reactive() else data_reactive
     write.csv(data, file, row.names = FALSE)
   },
   contentType = "text/csv"
 )
}

#' Create download handler for plotly chart as PNG
#' @param plot_reactive Reactive expression returning plotly object
#' @param filename_prefix Prefix for the filename
#' @param width Image width in pixels
#' @param height Image height in pixels
#' @export
plotly_png_download_handler <- function(plot_reactive, filename_prefix = "chart", width = 1200, height = 800) {
 downloadHandler(
   filename = function() {
     paste0(filename_prefix, "_", format(Sys.Date(), "%Y%m%d"), ".png")
   },
   content = function(file) {
     p <- if (is.reactive(plot_reactive)) plot_reactive() else plot_reactive
     # Use plotly's export - requires kaleido or orca
     tryCatch({
       plotly_IMAGE(p, format = "png", width = width, height = height, out_file = file)
     }, error = function(e) {
       # Fallback: save as HTML if image export fails
       html_file <- sub("\\.png$", ".html", file)
       saveWidget(p, html_file, selfcontained = TRUE)
       file.rename(html_file, file)
     })
   },
   contentType = "image/png"
 )
}

#' Create download handler for plotly chart as HTML (interactive)
#' @param plot_reactive Reactive expression returning plotly object
#' @param filename_prefix Prefix for the filename
#' @export
plotly_html_download_handler <- function(plot_reactive, filename_prefix = "chart") {
 downloadHandler(
   filename = function() {
     paste0(filename_prefix, "_", format(Sys.Date(), "%Y%m%d"), ".html")
   },
   content = function(file) {
     p <- if (is.reactive(plot_reactive)) plot_reactive() else plot_reactive
     saveWidget(p, file, selfcontained = TRUE)
   },
   contentType = "text/html"
 )
}

#' Create a panel with chart and download buttons
#' @param ns Namespace function
#' @param output_id ID of the plotly output
#' @param title Panel title
#' @param download_prefix Prefix for download filenames
#' @export
chart_panel_with_download <- function(ns, output_id, title = NULL, download_prefix = "chart") {
 div(
   class = "chart-panel position-relative",
   if (!is.null(title)) tags$h6(title, class = "mb-2"),
   div(
     class = "position-absolute top-0 end-0 p-2",
     style = "z-index: 100;",
     download_btn_small(ns(paste0(output_id, "_dl_csv")), "Download Data (CSV)"),
     download_btn_small(ns(paste0(output_id, "_dl_html")), "Download Chart (HTML)")
   ),
   plotly$plotlyOutput(ns(output_id))
 )
}

#' Check if object is reactive
#' @param x Object to check
is.reactive <- function(x) {
 inherits(x, "reactive") || inherits(x, "reactiveVal") || is.function(x)
}
