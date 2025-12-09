# app/logic/wbes_map.R
# Reusable WBES Interactive Map Component
# Creates leaflet maps with country-level data visualization

box::use(
  leaflet[leaflet, addTiles, addCircleMarkers, addLegend, setView, leafletOutput,
          renderLeaflet, colorNumeric, labelFormat],
  dplyr[left_join, select, mutate, filter, arrange, desc],
  RColorBrewer[brewer.pal],
  scales[rescale],
  shiny[icon, p],
  bslib[card, card_header, card_body]
)

#' Create interactive map for WBES data
#'
#' @param data Data frame with country-level data
#' @param coordinates Data frame with country coordinates (lat, lon, country)
#' @param indicator_col Name of indicator column to visualize
#' @param indicator_label Human-readable label for indicator
#' @param color_palette Color palette ("Blues", "Reds", "Greens", "YlOrRd", etc.)
#' @param reverse_colors Reverse color scale (TRUE for "higher is worse" indicators)
#' @return Leaflet map object
#' @export
create_wbes_map <- function(
  data,
  coordinates,
  indicator_col,
  indicator_label = indicator_col,
  color_palette = "YlOrRd",
  reverse_colors = FALSE
) {

  # Merge data with coordinates
  map_data <- data |>
    left_join(coordinates, by = "country") |>
    filter(!is.na(lat), !is.na(lon), !is.na(.data[[indicator_col]]))

  if (nrow(map_data) == 0) {
    # Return empty map with message
    return(
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 20, zoom = 2)
    )
  }

  # Create color palette
  indicator_values <- map_data[[indicator_col]]
  value_range <- range(indicator_values, na.rm = TRUE)

  # Generate color function
  if (reverse_colors) {
    pal_colors <- rev(brewer.pal(9, color_palette))
  } else {
    pal_colors <- brewer.pal(9, color_palette)
  }

  color_fn <- colorNumeric(
    palette = pal_colors,
    domain = value_range,
    na.color = "#CCCCCC"
  )

  # Create map
  map <- leaflet(map_data) |>
    addTiles() |>
    setView(lng = 0, lat = 20, zoom = 2) |>
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = ~rescale(get(indicator_col), to = c(5, 20)),
      color = ~color_fn(get(indicator_col)),
      fillColor = ~color_fn(get(indicator_col)),
      fillOpacity = 0.7,
      stroke = TRUE,
      weight = 1,
      opacity = 0.8,
      popup = ~paste0(
        "<strong>", country, "</strong><br/>",
        indicator_label, ": ", round(get(indicator_col), 1), "%"
      ),
      label = ~country
    ) |>
    addLegend(
      position = "bottomright",
      pal = color_fn,
      values = ~get(indicator_col),
      title = indicator_label,
      opacity = 0.8,
      labFormat = labelFormat(suffix = "%")
    )

  return(map)
}

#' Create simplified choropleth-style map
#'
#' For when you want simpler circle markers sized by value
#'
#' @param data Data with country, lat, lon, value
#' @param value_col Column name for value to display
#' @param title Map legend title
#' @return Leaflet map
#' @export
create_simple_map <- function(data, value_col, title = "Value") {

  # Filter valid coordinates
  map_data <- data |>
    filter(!is.na(lat), !is.na(lon), !is.na(.data[[value_col]]))

  if (nrow(map_data) == 0) {
    return(
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 20, zoom = 2)
    )
  }

  # Color palette
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = map_data[[value_col]]
  )

  leaflet(map_data) |>
    addTiles() |>
    setView(lng = 0, lat = 20, zoom = 2) |>
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 8,
      fillColor = ~pal(get(value_col)),
      fillOpacity = 0.7,
      color = "white",
      weight = 1,
      popup = ~paste0(
        "<strong>", country, "</strong><br/>",
        title, ": ", round(get(value_col), 1)
      )
    ) |>
    addLegend(
      "bottomright",
      pal = pal,
      values = ~get(value_col),
      title = title,
      opacity = 0.8
    )
}

#' Get centroid coordinates for countries
#'
#' Extracts lat/lon from WBES data's country_coordinates table
#'
#' @param wbes_data WBES data list
#' @return Data frame with country, lat, lon
#' @export
get_country_coordinates <- function(wbes_data) {
  if (is.null(wbes_data$country_coordinates)) {
    # Return empty data frame
    return(data.frame(
      country = character(),
      lat = numeric(),
      lon = numeric()
    ))
  }

  coords <- wbes_data$country_coordinates |>
    select(country, lat, lon)

  return(coords)
}

#' Helper to add map to module UI
#'
#' Standardized map card for consistent appearance
#'
#' @param ns Namespace function from NS(id)
#' @param output_id Output ID for the map
#' @param title Card title
#' @param height Map height (default "400px")
#' @return Shiny UI card with map
#' @export
map_card_ui <- function(ns, output_id, title = "Geographic Distribution", height = "400px") {
  bslib::card(
    bslib::card_header(shiny::icon("map-marked-alt"), paste(" ", title)),
    bslib::card_body(
      leafletOutput(ns(output_id), height = height),
      shiny::p(
        class = "text-muted small mt-2",
        "Interactive map showing geographic distribution. Click markers for details; zoom and pan to explore."
      )
    )
  )
}
