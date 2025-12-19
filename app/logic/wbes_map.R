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
  bslib[card, card_header, card_body],
  stats[setNames]
)

#' Normalize country names for matching
#'
#' Handles common discrepancies like "Bahamas, The" vs "Bahamas"
#'
#' @param name Country name to normalize
#' @return Normalized country name
normalize_country_name <- function(name) {
  if (is.na(name)) return(NA_character_)

  # Remove ", The" suffix

  name <- gsub(",\\s*The$", "", name)

  # Remove "The " prefix
  name <- gsub("^The\\s+", "", name)

  # Common replacements (World Bank naming conventions -> standard names)
  replacements <- c(
    "Congo, Dem. Rep." = "Democratic Republic of the Congo",
    "Congo, Rep." = "Republic of the Congo",
    "Egypt, Arab Rep." = "Egypt",
    "Iran, Islamic Rep." = "Iran",
    "Korea, Rep." = "South Korea",
    "Lao PDR" = "Laos",
    "Macedonia, FYR" = "North Macedonia",
    "Micronesia, Fed. Sts." = "Micronesia",
    "Slovak Republic" = "Slovakia",
    "Venezuela, RB" = "Venezuela",
    "Yemen, Rep." = "Yemen",
    "Kyrgyz Republic" = "Kyrgyzstan",
    "Gambia, The" = "Gambia",
    "Bahamas, The" = "Bahamas",
    "Cabo Verde" = "Cape Verde",
    "CÃ´te d'Ivoire" = "Cote d'Ivoire",
    "Côte d'Ivoire" = "Cote d'Ivoire"
  )

  if (name %in% names(replacements)) {
    return(replacements[[name]])
  }

  return(name)
}

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

  # Validate inputs
 if (is.null(data) || nrow(data) == 0) {
    return(
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 20, zoom = 2)
    )
  }

  if (is.null(coordinates) || nrow(coordinates) == 0) {
    return(
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 20, zoom = 2)
    )
  }

  if (!indicator_col %in% names(data)) {
    return(
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 20, zoom = 2)
    )
  }

  # Handle both 'lon' and 'lng' column names for compatibility
  if ("lng" %in% names(coordinates) && !"lon" %in% names(coordinates)) {
    coordinates$lon <- coordinates$lng
  }

  # Ensure coordinates has required columns
  if (!all(c("country", "lat", "lon") %in% names(coordinates))) {
    return(
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 20, zoom = 2)
    )
  }

  # Check if data already has lat/lon or lat/lng columns
  has_embedded_coords <- ("lat" %in% names(data) && ("lon" %in% names(data) || "lng" %in% names(data)))

  if (has_embedded_coords) {
    # Use embedded coordinates
    map_data <- data

    # Handle lng -> lon naming
    if ("lng" %in% names(map_data) && !"lon" %in% names(map_data)) {
      map_data$lon <- map_data$lng
    }
  } else {
    # Normalize country names for better matching
    data$country_normalized <- sapply(data$country, normalize_country_name)
    coordinates$country_normalized <- sapply(coordinates$country, normalize_country_name)

    # Merge data with coordinates using normalized names
    map_data <- data |>
      left_join(
        coordinates |> select(country_normalized, lat, lon),
        by = "country_normalized"
      )
  }

  # Check if lat/lon columns exist after processing
  if (!all(c("lat", "lon") %in% names(map_data))) {
    return(
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 20, zoom = 2)
    )
  }

  # Filter to valid rows
  map_data <- map_data |>
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

  # Calculate auto-zoom bounds based on data points
  lat_range <- range(map_data$lat, na.rm = TRUE)
  lon_range <- range(map_data$lon, na.rm = TRUE)
  center_lat <- mean(lat_range)
  center_lon <- mean(lon_range)

  # Create map
  map <- leaflet(map_data) |>
    addTiles() |>
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

  # Auto-zoom to fit data points
  if (nrow(map_data) > 1) {
    map <- map |>
      leaflet::fitBounds(
        lng1 = lon_range[1] - 2,
        lat1 = lat_range[1] - 2,
        lng2 = lon_range[2] + 2,
        lat2 = lat_range[2] + 2
      )
  } else {
    map <- map |>
      setView(lng = center_lon, lat = center_lat, zoom = 5)
  }

  return(map)
}

#' Create simplified choropleth-style map
#'
#' For when you want simpler circle markers sized by value
#'
#' @param data Data with country, lat, lon (or lng), value
#' @param value_col Column name for value to display
#' @param title Map legend title
#' @return Leaflet map
#' @export
create_simple_map <- function(data, value_col, title = "Value") {

  # Handle both 'lon' and 'lng' column names
  if ("lng" %in% names(data) && !"lon" %in% names(data)) {
    data$lon <- data$lng
  }

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

  # Calculate auto-zoom bounds based on data points
  lat_range <- range(map_data$lat, na.rm = TRUE)
  lon_range <- range(map_data$lon, na.rm = TRUE)
  center_lat <- mean(lat_range)
  center_lon <- mean(lon_range)

  map <- leaflet(map_data) |>
    addTiles() |>
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

  # Auto-zoom to fit data points
  if (nrow(map_data) > 1) {
    map <- map |>
      leaflet::fitBounds(
        lng1 = lon_range[1] - 2,
        lat1 = lat_range[1] - 2,
        lng2 = lon_range[2] + 2,
        lat2 = lat_range[2] + 2
      )
  } else {
    map <- map |>
      setView(lng = center_lon, lat = center_lat, zoom = 5)
  }

  return(map)
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

  coords <- wbes_data$country_coordinates

  # Handle both 'lon' and 'lng' column names
 if ("lng" %in% names(coords) && !"lon" %in% names(coords)) {
    coords$lon <- coords$lng
  }

  coords <- coords |>
    select(country, lat, lon)

  return(coords)
}

#' Create interactive map with 3 dimensions: color, size, and grouping
#'
#' Enhanced map supporting color (indicator value), size (secondary metric),
#' and grouping (categorical variable for visual distinction)
#'
#' @param data Data frame with country-level data
#' @param coordinates Data frame with country coordinates
#' @param color_col Column for color encoding (continuous indicator)
#' @param size_col Column for size encoding (optional, defaults to color_col)
#' @param group_col Column for grouping/categorical (e.g., region, sector, firm_size)
#' @param color_label Label for color legend
#' @param size_label Label for size (shown in tooltip)
#' @param group_label Label for grouping dimension
#' @param color_palette Color palette name
#' @param reverse_colors Reverse color scale
#' @return Leaflet map object
#' @export
create_wbes_map_3d <- function(
  data,
  coordinates,
  color_col,
  size_col = NULL,
  group_col = NULL,
  color_label = color_col,
  size_label = NULL,
  group_label = NULL,
  color_palette = "YlOrRd",
  reverse_colors = FALSE
) {

  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    return(leaflet() |> addTiles() |> setView(lng = 0, lat = 20, zoom = 2))
  }

  if (!color_col %in% names(data)) {
    return(leaflet() |> addTiles() |> setView(lng = 0, lat = 20, zoom = 2))
  }

  # Default size_col to color_col if not provided
  if (is.null(size_col)) size_col <- color_col
  if (is.null(size_label)) size_label <- size_col

  # Handle coordinates
  if ("lng" %in% names(coordinates) && !"lon" %in% names(coordinates)) {
    coordinates$lon <- coordinates$lng
  }

  # Check if data has embedded coordinates
  has_embedded_coords <- ("lat" %in% names(data) && ("lon" %in% names(data) || "lng" %in% names(data)))

  if (has_embedded_coords) {
    map_data <- data
    if ("lng" %in% names(map_data) && !"lon" %in% names(map_data)) {
      map_data$lon <- map_data$lng
    }
  } else {
    data$country_normalized <- sapply(data$country, normalize_country_name)
    coordinates$country_normalized <- sapply(coordinates$country, normalize_country_name)
    map_data <- data |>
      left_join(coordinates |> select(country_normalized, lat, lon), by = "country_normalized")
  }

  if (!all(c("lat", "lon") %in% names(map_data))) {
    return(leaflet() |> addTiles() |> setView(lng = 0, lat = 20, zoom = 2))
  }

  # Filter valid rows
  map_data <- map_data |>
    filter(!is.na(lat), !is.na(lon), !is.na(.data[[color_col]]))

  if (nrow(map_data) == 0) {
    return(leaflet() |> addTiles() |> setView(lng = 0, lat = 20, zoom = 2))
  }

  # Color palette
  if (reverse_colors) {
    pal_colors <- rev(brewer.pal(9, color_palette))
  } else {
    pal_colors <- brewer.pal(9, color_palette)
  }

  color_fn <- colorNumeric(
    palette = pal_colors,
    domain = range(map_data[[color_col]], na.rm = TRUE),
    na.color = "#CCCCCC"
  )

  # Size scaling
  if (size_col %in% names(map_data)) {
    size_values <- map_data[[size_col]]
    size_values[is.na(size_values)] <- min(size_values, na.rm = TRUE)
    map_data$marker_size <- rescale(size_values, to = c(5, 20))
  } else {
    map_data$marker_size <- 10
  }

  # Group-based stroke colors (to distinguish groups visually)
  group_colors <- c(
    "#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D",
    "#F4A460", "#dc3545", "#9c27b0", "#3f51b5", "#009688"
  )

  if (!is.null(group_col) && group_col %in% names(map_data)) {
    groups <- unique(map_data[[group_col]])
    groups <- groups[!is.na(groups)]
    group_color_map <- setNames(
      rep(group_colors, length.out = length(groups)),
      groups
    )
    map_data$stroke_color <- group_color_map[as.character(map_data[[group_col]])]
    map_data$stroke_color[is.na(map_data$stroke_color)] <- "#333333"
  } else {
    map_data$stroke_color <- "#333333"
  }

  # Build popup content
  map_data$popup_content <- paste0(
    "<strong>", map_data$country, "</strong><br/>",
    color_label, ": ", round(map_data[[color_col]], 1),
    if (size_col != color_col && size_col %in% names(map_data)) {
      paste0("<br/>", size_label, ": ", round(map_data[[size_col]], 1))
    } else "",
    if (!is.null(group_col) && group_col %in% names(map_data)) {
      paste0("<br/>", if (!is.null(group_label)) group_label else group_col, ": ", map_data[[group_col]])
    } else ""
  )

  # Calculate auto-zoom bounds based on data points
  lat_range <- range(map_data$lat, na.rm = TRUE)
  lon_range <- range(map_data$lon, na.rm = TRUE)

  # Calculate center
  center_lat <- mean(lat_range)
  center_lon <- mean(lon_range)

  # Create map with auto-fit bounds
  map <- leaflet(map_data) |>
    addTiles() |>
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = ~marker_size,
      color = ~stroke_color,
      fillColor = ~color_fn(get(color_col)),
      fillOpacity = 0.7,
      stroke = TRUE,
      weight = 2,
      opacity = 0.9,
      popup = ~popup_content,
      label = ~country
    ) |>
    addLegend(
      position = "bottomright",
      pal = color_fn,
      values = ~get(color_col),
      title = color_label,
      opacity = 0.8
    )

  # Fit bounds to data with padding
  if (nrow(map_data) > 1) {
    map <- map |>
      leaflet::fitBounds(
        lng1 = lon_range[1] - 2,
        lat1 = lat_range[1] - 2,
        lng2 = lon_range[2] + 2,
        lat2 = lat_range[2] + 2
      )
  } else {
    # Single point - use setView with reasonable zoom
    map <- map |>
      setView(lng = center_lon, lat = center_lat, zoom = 5)
  }

  return(map)
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
