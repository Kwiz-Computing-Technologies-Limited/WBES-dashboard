# app/view/mod_overview.R
# Dashboard Overview Module

box::use(
 shiny[moduleServer, NS, reactive, req, tags, HTML, icon, div, h2, h3, h4, p, span, br,
        fluidRow, column, selectInput, sliderInput, actionButton, observeEvent, renderUI, uiOutput,
        showModal, removeModal, textInput, selectizeInput, modalDialog, modalButton, updateSelectInput,
        downloadButton, downloadHandler],
 bslib[card, card_header, card_body, value_box, layout_columns],
 plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
 leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, addCircleMarkers,
         setView, colorNumeric, addLegend, labelFormat],
 dplyr[filter, arrange, desc, mutate, summarise, group_by, n, first, across, any_of],
 stats[setNames, na.omit, density, median],
 scales[rescale],
 htmlwidgets[saveWidget],
 utils[write.csv],
 app/logic/shared_filters[apply_common_filters],
 app/logic/custom_regions[filter_by_region],
 app/logic/chart_utils[create_chart_caption, map_with_caption]
)

# Helper function to create chart container with download button and caption
chart_with_download <- function(ns, output_id, height = "400px", title = NULL) {
  div(
    class = "position-relative",
    if (!is.null(title)) h4(title, class = "text-primary-teal mb-2"),
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
    plotlyOutput(ns(output_id), height = height),
    create_chart_caption(output_id)
  )
}

#' @export
ui <- function(id) {
 ns <- NS(id)

 tags$div(
   class = "overview-container",

   # Header Section
   fluidRow(
     column(12,
       tags$div(
         class = "page-header mb-4",
         h2(
           icon("globe"),
           "Global Business Environment Overview",
           class = "text-primary-teal"
         ),
         p(
           class = "lead text-muted",
           "Comprehensive benchmarking of business environments across 168 economies using World Bank Enterprise Survey data"
         )
       )
     )
   ),

   # KPI Value Boxes
   fluidRow(
     class = "mb-4",
     column(3, uiOutput(ns("kpi_countries"))),
     column(3, uiOutput(ns("kpi_firms"))),
     column(3, uiOutput(ns("kpi_years"))),
     column(3, uiOutput(ns("kpi_indicators")))
   ),


   # Main Content - Map and Top Constraints
   fluidRow(
     class = "mb-4",
     column(8,
       card(
         card_header(icon("map-marked-alt"), "Business Environment Map"),
         card_body(
          selectInput(
            ns("map_indicator"),
            "Select Indicator:",
            choices = c(
              "Power Outages (per month)" = "power_outages_per_month",
              "Access to Credit (%)" = "firms_with_credit_line_pct",
              "Bribery Incidence (%)" = "bribery_incidence_pct",
              "Capacity Utilization (%)" = "capacity_utilization_pct"
            ),
            width = "300px"
          ),
          map_with_caption(ns, "world_map", height = "450px", title = "Business Environment by Country")
        )
      )
    ),
    column(4,
      card(
        card_header(icon("exclamation-triangle"), "Top Business Obstacles"),
        card_body(
          chart_with_download(ns, "obstacles_chart", height = "500px", title = "Top Business Constraints"),
          p(
            class = "text-muted small mt-2",
            "Bars rank the most frequently cited obstacles among surveyed firms, making it easy to see which constraints dominate the business landscape."
          )
        )
      )
    )
  ),

   # Regional Comparison
   fluidRow(
     class = "mb-4",
     column(12,
      card(
        card_header(icon("chart-bar"), "Regional Comparison - Key Indicators"),
        card_body(
          chart_with_download(ns, "regional_comparison", title = "Regional Performance Comparison"),
          p(
            class = "text-muted small mt-2",
            "Grouped bars compare infrastructure reliability, access to finance, and bribery exposure across regions, highlighting where each region performs strongest."
          )
        )
      )
    )
  ),

   # Bottom Row - Quick Stats
   fluidRow(
     column(6,
       card(
        card_header(
          icon("bolt"),
          "Infrastructure Quality Index",
          class = "card-header-secondary"
        ),
        card_body(
          chart_with_download(ns, "infrastructure_gauge", height = "250px", title = "Infrastructure Index"),
          p(
            class = "text-muted small mt-2",
            "The gauge summarizes regional infrastructure strength on a 0–100 scale; the threshold line marks the target resilience benchmark."
          )
        )
      )
    ),
    column(6,
      card(
        card_header(
          icon("university"),
          "Financial Access Index",
          class = "card-header-secondary"
        ),
        card_body(
          chart_with_download(ns, "finance_gauge", height = "250px", title = "Finance Access Index"),
          p(
            class = "text-muted small mt-2",
            "This dial tracks how easily firms secure formal credit; scores below the threshold highlight markets where access remains constrained."
          )
        )
      )
    )
  ),

  # Density Plots Section
  fluidRow(
    class = "mb-4 mt-4",
    column(12,
      h3(icon("chart-area"), "Distribution of Key Indicators", class = "text-primary-teal mb-3"),
      p(class = "text-muted", "Density plots showing the distribution of key business environment metrics across countries")
    )
  ),

  fluidRow(
    class = "mb-4",
    column(4,
      card(
        card_header(icon("users"), "Female Workforce Participation"),
        card_body(
          chart_with_download(ns, "density_female_workers", height = "280px"),
          p(class = "text-muted small mt-2", "Distribution of female workers as percentage of total workforce across countries")
        )
      )
    ),
    column(4,
      card(
        card_header(icon("venus"), "Female Ownership"),
        card_body(
          chart_with_download(ns, "density_female_ownership", height = "280px"),
          p(class = "text-muted small mt-2", "Distribution of firms with female ownership participation")
        )
      )
    ),
    column(4,
      card(
        card_header(icon("industry"), "Capacity Utilization"),
        card_body(
          chart_with_download(ns, "density_capacity", height = "280px"),
          p(class = "text-muted small mt-2", "Distribution of firm capacity utilization rates")
        )
      )
    )
  ),

  fluidRow(
    class = "mb-4",
    column(4,
      card(
        card_header(icon("bolt"), "Power Outages"),
        card_body(
          chart_with_download(ns, "density_power_outages", height = "280px"),
          p(class = "text-muted small mt-2", "Distribution of monthly power outage frequency")
        )
      )
    ),
    column(4,
      card(
        card_header(icon("hand-holding-usd"), "Bribery Incidence"),
        card_body(
          chart_with_download(ns, "density_bribery", height = "280px"),
          p(class = "text-muted small mt-2", "Distribution of bribery incidence rates across countries")
        )
      )
    ),
    column(4,
      card(
        card_header(icon("ship"), "Export Participation"),
        card_body(
          chart_with_download(ns, "density_exports", height = "280px"),
          p(class = "text-muted small mt-2", "Distribution of firms engaged in export activities")
        )
      )
    )
  )
)
}

#' @export
server <- function(id, wbes_data, global_filters = NULL) {
 moduleServer(id, function(input, output, session) {
   ns <- session$ns

   # Filtered data reactive - uses global filters from sidebar
   filtered_data <- reactive({
     req(wbes_data())

     filters <- if (!is.null(global_filters)) global_filters() else NULL

     # Determine data source based on active filters
     use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                  !all(filters$year %in% c("all", NA))

     # Check if sector filter is active (not "all", not empty, not NA)
     sector_filter_active <- !is.null(filters$sector) &&
                             length(filters$sector) > 0 &&
                             !all(filters$sector %in% c("all", "", NA))

     # Use processed data when sector filter is active (has sector column for filtering)
     # Then re-aggregate to country level after filtering
     if (sector_filter_active && !is.null(wbes_data()$processed)) {
       data <- wbes_data()$processed

       # Apply all filters to processed data
       if (!is.null(filters)) {
         data <- apply_common_filters(
           data,
           region_value = filters$region,
           sector_value = filters$sector,
           firm_size_value = filters$firm_size,
           income_value = filters$income,
           year_value = filters$year,
           custom_regions = filters$custom_regions,
           custom_sectors = filters$custom_sectors,
           filter_by_region_fn = filter_by_region
         )
       }

       # Re-aggregate to country level after filtering
       if (!is.null(data) && nrow(data) > 0) {
         # Get numeric columns for aggregation
         metric_cols <- names(data)[sapply(data, is.numeric)]
         metric_cols <- setdiff(metric_cols, c("year", "sample_size", "lat", "lng"))

         data <- data |>
           group_by(country) |>
           summarise(
             across(any_of(metric_cols), ~mean(.x, na.rm = TRUE)),
             region = first(region[!is.na(region)]),
             income = first(income[!is.na(income)]),
             sample_size = n(),
             .groups = "drop"
           )
       }
     } else {
       # Use pre-aggregated data when no sector filter
       data <- if (use_panel) wbes_data()$country_panel else wbes_data()$latest

       # Apply filters (excluding sector since aggregated data can't filter by sector)
       if (!is.null(filters)) {
         data <- apply_common_filters(
           data,
           region_value = filters$region,
           sector_value = "all",  # Skip sector filter on aggregated data
           firm_size_value = filters$firm_size,
           income_value = filters$income,
           year_value = filters$year,
           custom_regions = filters$custom_regions,
           custom_sectors = filters$custom_sectors,
           filter_by_region_fn = filter_by_region
         )
       }
     }

     # Add coordinates for maps
     if (!is.null(data) && nrow(data) > 0 && !is.null(wbes_data()$country_coordinates)) {
       coords <- wbes_data()$country_coordinates
       if ("lat" %in% names(coords) && "lng" %in% names(coords) && !"lat" %in% names(data)) {
         data <- merge(data, coords, by = "country", all.x = TRUE)
       }
     }

     data
   })

   # KPI Boxes
   output$kpi_countries <- renderUI({
     req(filtered_data())
     # Count distinct countries from filtered data (respects region/income filters)
     n_countries <- length(unique(filtered_data()$country[!is.na(filtered_data()$country)]))
     tags$div(
       class = "kpi-box",
       tags$div(class = "kpi-value", n_countries),
       tags$div(class = "kpi-label", "Countries Covered")
     )
   })

   output$kpi_firms <- renderUI({
     req(filtered_data())
     # Sum actual sample_size to get total firms surveyed
     n_firms <- sum(filtered_data()$sample_size, na.rm = TRUE)
     # Format with K or M suffix
     firms_label <- if (n_firms >= 1000000) {
       paste0(round(n_firms / 1000000, 1), "M")
     } else if (n_firms >= 1000) {
       paste0(round(n_firms / 1000, 1), "K")
     } else {
       format(n_firms, big.mark = ",")
     }
     tags$div(
       class = "kpi-box kpi-box-info",
       tags$div(class = "kpi-value", firms_label),
       tags$div(class = "kpi-label", "Firms Surveyed")
     )
   })

   output$kpi_years <- renderUI({
     req(wbes_data())
     tags$div(
       class = "kpi-box kpi-box-success",
       tags$div(class = "kpi-value", length(wbes_data()$years)),
       tags$div(class = "kpi-label", "Survey Years")
     )
   })

   output$kpi_indicators <- renderUI({
     tags$div(
       class = "kpi-box kpi-box-warning",
       tags$div(class = "kpi-value", "150+"),
       tags$div(class = "kpi-label", "Indicators")
     )
   })

   # World Map - with color and size encoding
   output$world_map <- renderLeaflet({
     req(filtered_data())

     # Get data with coordinates (already merged in wbes_data)
     data <- filtered_data()

     # Check if lat/lng columns exist
     has_coords <- "lat" %in% names(data) && "lng" %in% names(data)

     if (!has_coords) {
       return(
         leaflet() |>
           addTiles() |>
           setView(lng = 20, lat = 10, zoom = 2)
       )
     }

     indicator <- input$map_indicator

     # Handle lon/lng naming
     if ("lng" %in% names(data) && !"lon" %in% names(data)) {
       data$lon <- data$lng
     }

     data <- data[!is.na(data$lat) & !is.na(data$lon), ]

     if (indicator %in% names(data)) {
       data <- data[!is.na(data[[indicator]]), ]
     }

     if (nrow(data) > 0 && indicator %in% names(data)) {
       # Map indicator labels
       indicator_labels <- c(
         "power_outages_per_month" = "Power Outages/Month",
         "firms_with_credit_line_pct" = "Credit Access %",
         "bribery_incidence_pct" = "Bribery Incidence %",
         "capacity_utilization_pct" = "Capacity Utilization %"
       )
       label <- if (indicator %in% names(indicator_labels)) indicator_labels[[indicator]] else indicator

       # Color palette - YlOrRd for consistency with other maps
       pal <- colorNumeric(
         palette = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026"),
         domain = data[[indicator]],
         na.color = "#808080"
       )

       # Size scaling - rescale indicator values to marker sizes (5-20)
       size_values <- data[[indicator]]
       size_values[is.na(size_values)] <- min(size_values, na.rm = TRUE)
       data$marker_size <- rescale(size_values, to = c(6, 22))

       leaflet(data) |>
         addTiles() |>
         setView(lng = 20, lat = 10, zoom = 2) |>
         addCircleMarkers(
           lng = ~lon, lat = ~lat,
           radius = ~marker_size,
           color = ~pal(get(indicator)),
           fillColor = ~pal(get(indicator)),
           fillOpacity = 0.7,
           stroke = TRUE,
           weight = 1.5,
           opacity = 0.9,
           popup = ~paste0(
             "<strong>", country, "</strong><br>",
             label, ": ", round(get(indicator), 1),
             if ("region" %in% names(data)) paste0("<br>Region: ", region) else ""
           ),
           label = ~country
         ) |>
         addLegend(
           "bottomright",
           pal = pal,
           values = ~get(indicator),
           title = label,
           opacity = 0.8,
           labFormat = labelFormat(suffix = if (grepl("pct", indicator)) "%" else "")
         )
     } else {
       leaflet() |>
         addTiles() |>
         setView(lng = 20, lat = 10, zoom = 2)
     }
   })

   # Obstacles Chart
   output$obstacles_chart <- renderPlotly({
     req(filtered_data())
     data <- filtered_data()

     # Calculate average values for major obstacles from actual data
     obstacles <- data.frame(
       obstacle = character(),
       pct = numeric(),
       stringsAsFactors = FALSE
     )

     # Add obstacles if columns exist in data
     if ("IC.FRM.FINA.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Access to Finance",
         pct = mean(data$IC.FRM.FINA.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.ELEC.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Electricity",
         pct = mean(data$IC.FRM.ELEC.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.CORR.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Corruption",
         pct = mean(data$IC.FRM.CORR.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.INFRA.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Infrastructure",
         pct = mean(data$IC.FRM.INFRA.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.CRIM.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Crime",
         pct = mean(data$IC.FRM.CRIM.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.WKFC.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Workforce Quality",
         pct = mean(data$IC.FRM.WKFC.ZS, na.rm = TRUE)
       ))
     }

     # Remove rows with NA values
     obstacles <- obstacles[!is.na(obstacles$pct), ]

     if (nrow(obstacles) > 0) {
       # Sort and prepare for plotting
       obstacles <- arrange(obstacles, pct)
       obstacles$obstacle <- factor(obstacles$obstacle, levels = obstacles$obstacle)

       plot_ly(obstacles,
               y = ~obstacle,
               x = ~pct,
               type = "bar",
               orientation = "h",
               marker = list(
                 color = "#1B6B5F",
                 line = list(color = "#145449", width = 1)
               ),
               hovertemplate = "%{y}: %{x:.1f}%<extra></extra>") |>
         layout(
           xaxis = list(title = "% of Firms", ticksuffix = "%"),
           yaxis = list(title = ""),
           margin = list(l = 150),
           plot_bgcolor = "rgba(0,0,0,0)",
           paper_bgcolor = "rgba(0,0,0,0)"
         ) |>
         config(displayModeBar = FALSE)
     } else {
       # Return empty plot if no data
       plot_ly() |>
         layout(
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           annotations = list(
             text = "No obstacle data available",
             xref = "paper",
             yref = "paper",
             x = 0.5,
             y = 0.5,
             showarrow = FALSE
           )
         )
     }
   })

   # Regional Comparison
   output$regional_comparison <- renderPlotly({
     req(filtered_data())

     # Use filtered data (respects region and firm_size filters)
     data <- filtered_data()

     # Calculate regional aggregates from filtered data
     if (!is.null(data) && "region" %in% names(data)) {
       regional <- data |>
         filter(!is.na(region)) |>
         group_by(region) |>
         summarise(
           power_outages_per_month = mean(power_outages_per_month, na.rm = TRUE),
           firms_with_credit_line_pct = mean(firms_with_credit_line_pct, na.rm = TRUE),
           bribery_incidence_pct = mean(bribery_incidence_pct, na.rm = TRUE),
           .groups = "drop"
         )

       if (nrow(regional) > 0) {
         plot_ly(regional) |>
           add_trace(
             x = ~region,
             y = ~power_outages_per_month,
             type = "bar",
             name = "Power Outages/Month",
             marker = list(color = "#1B6B5F")
           ) |>
           add_trace(
             x = ~region,
             y = ~firms_with_credit_line_pct,
             type = "bar",
             name = "Credit Access (%)",
             marker = list(color = "#F49B7A")
           ) |>
           add_trace(
             x = ~region,
             y = ~bribery_incidence_pct,
             type = "bar",
             name = "Bribery Incidence (%)",
             marker = list(color = "#6C757D")
           ) |>
           layout(
             barmode = "group",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Value"),
             legend = list(orientation = "h", y = -0.2),
             margin = list(b = 100),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)"
           ) |>
           config(displayModeBar = FALSE)
       } else {
         # Empty plot if no regional data
         plot_ly() |>
           layout(
             annotations = list(
               text = "No regional data available",
               xref = "paper",
               yref = "paper",
               x = 0.5,
               y = 0.5,
               showarrow = FALSE
             )
           )
       }
     } else {
       # Empty plot if no data
       plot_ly() |>
         layout(
           annotations = list(
             text = "No regional data available",
             xref = "paper",
             yref = "paper",
             x = 0.5,
             y = 0.5,
             showarrow = FALSE
           )
         )
     }
   })

   # Infrastructure Gauge
   output$infrastructure_gauge <- renderPlotly({
     req(filtered_data())
     data <- filtered_data()

     # Calculate infrastructure quality index from actual data
     # Higher power outages = worse infrastructure, so invert the scale
     # Scale: 100 - (average power outages * 10) or use capacity utilization as proxy
     infra_score <- 50  # default

     if ("power_outages_per_month" %in% names(data)) {
       avg_outages <- mean(data$power_outages_per_month, na.rm = TRUE)
       # Convert outages to a 0-100 score (fewer outages = better score)
       # Assume 0 outages = 100, 10+ outages = 0
       infra_score <- max(0, min(100, 100 - (avg_outages * 10)))
     }

     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = round(infra_score, 1),
       title = list(text = "Infrastructure Quality Index"),
       gauge = list(
         axis = list(range = list(0, 100)),
         bar = list(color = "#1B6B5F"),
         steps = list(
           list(range = c(0, 40), color = "#ffebee"),
           list(range = c(40, 70), color = "#fff3e0"),
           list(range = c(70, 100), color = "#e8f5e9")
         ),
         threshold = list(
           line = list(color = "#F49B7A", width = 4),
           thickness = 0.75,
           value = 75
         )
       )
     ) |>
       layout(
         margin = list(t = 50, b = 30),
         paper_bgcolor = "rgba(0,0,0,0)"
       ) |>
       config(displayModeBar = FALSE)
   })

   # Finance Gauge
   output$finance_gauge <- renderPlotly({
     req(filtered_data())
     data <- filtered_data()

     # Calculate financial access index from actual data
     finance_score <- 50  # default

     if ("firms_with_credit_line_pct" %in% names(data)) {
       finance_score <- mean(data$firms_with_credit_line_pct, na.rm = TRUE)
     }

     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = round(finance_score, 1),
       title = list(text = "Credit Access Index"),
       gauge = list(
         axis = list(range = list(0, 100)),
         bar = list(color = "#F49B7A"),
         steps = list(
           list(range = c(0, 30), color = "#ffebee"),
           list(range = c(30, 60), color = "#fff3e0"),
           list(range = c(60, 100), color = "#e8f5e9")
         ),
         threshold = list(
           line = list(color = "#1B6B5F", width = 4),
           thickness = 0.75,
           value = 50
         )
       )
     ) |>
       layout(
         margin = list(t = 50, b = 30),
         paper_bgcolor = "rgba(0,0,0,0)"
       ) |>
       config(displayModeBar = FALSE)
   })

   # ============================================================
   # Density Plots
   # ============================================================

   # Helper function to create density plot
   create_density_plot <- function(data, col_name, title, color = "#1B6B5F", x_label = "Value") {
     if (!col_name %in% names(data)) {
       return(
         plot_ly() |>
           layout(
             annotations = list(list(
               text = paste("Data not available"),
               showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5
             )),
             paper_bgcolor = "rgba(0,0,0,0)"
           ) |>
           config(displayModeBar = FALSE)
       )
     }

     values <- data[[col_name]]
     values <- values[!is.na(values)]

     if (length(values) < 3) {
       return(
         plot_ly() |>
           layout(
             annotations = list(list(
               text = "Insufficient data for density plot",
               showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5
             )),
             paper_bgcolor = "rgba(0,0,0,0)"
           ) |>
           config(displayModeBar = FALSE)
       )
     }

     # Calculate density
     dens <- density(values, na.rm = TRUE)

     # Calculate mean and median for annotations
     mean_val <- mean(values, na.rm = TRUE)
     median_val <- median(values, na.rm = TRUE)

     plot_ly() |>
       add_trace(
         x = dens$x, y = dens$y,
         type = "scatter", mode = "lines",
         fill = "tozeroy",
         fillcolor = paste0(color, "40"),
         line = list(color = color, width = 2),
         name = "Density",
         hovertemplate = paste0(x_label, ": %{x:.1f}<br>Density: %{y:.4f}<extra></extra>")
       ) |>
       add_trace(
         x = c(mean_val, mean_val), y = c(0, max(dens$y)),
         type = "scatter", mode = "lines",
         line = list(color = "#dc3545", width = 2, dash = "dash"),
         name = paste0("Mean: ", round(mean_val, 1)),
         hoverinfo = "name"
       ) |>
       add_trace(
         x = c(median_val, median_val), y = c(0, max(dens$y)),
         type = "scatter", mode = "lines",
         line = list(color = "#17a2b8", width = 2, dash = "dot"),
         name = paste0("Median: ", round(median_val, 1)),
         hoverinfo = "name"
       ) |>
       layout(
         title = list(text = title, font = list(size = 13)),
         xaxis = list(title = x_label, titlefont = list(size = 11)),
         yaxis = list(title = "Density", titlefont = list(size = 11)),
         showlegend = TRUE,
         legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center", font = list(size = 9)),
         margin = list(l = 50, r = 20, t = 40, b = 80),
         paper_bgcolor = "rgba(0,0,0,0)",
         plot_bgcolor = "rgba(0,0,0,0)"
       ) |>
       config(displayModeBar = FALSE)
   }

   # Density plot: Female Workers
   output$density_female_workers <- renderPlotly({
     req(filtered_data())
     col <- if ("female_workers_pct" %in% names(filtered_data())) "female_workers_pct" else "IC.FRM.FEMW.ZS"
     create_density_plot(filtered_data(), col, "Female Workers Distribution", "#9c27b0", "Female Workers (%)")
   })

   # Density plot: Female Ownership
   output$density_female_ownership <- renderPlotly({
     req(filtered_data())
     col <- if ("female_ownership_pct" %in% names(filtered_data())) "female_ownership_pct" else "IC.FRM.FEMO.ZS"
     create_density_plot(filtered_data(), col, "Female Ownership Distribution", "#e91e63", "Female Ownership (%)")
   })

   # Density plot: Capacity Utilization
   output$density_capacity <- renderPlotly({
     req(filtered_data())
     col <- if ("capacity_utilization_pct" %in% names(filtered_data())) "capacity_utilization_pct" else "IC.FRM.CAPU.ZS"
     create_density_plot(filtered_data(), col, "Capacity Utilization Distribution", "#2E7D32", "Capacity Utilization (%)")
   })

   # Density plot: Power Outages
   output$density_power_outages <- renderPlotly({
     req(filtered_data())
     create_density_plot(filtered_data(), "power_outages_per_month", "Power Outages Distribution", "#ff5722", "Outages per Month")
   })

   # Density plot: Bribery Incidence
   output$density_bribery <- renderPlotly({
     req(filtered_data())
     col <- if ("bribery_incidence_pct" %in% names(filtered_data())) "bribery_incidence_pct" else "IC.FRM.BRIB.ZS"
     create_density_plot(filtered_data(), col, "Bribery Incidence Distribution", "#795548", "Bribery Incidence (%)")
   })

   # Density plot: Export Participation
   output$density_exports <- renderPlotly({
     req(filtered_data())
     col <- if ("export_firms_pct" %in% names(filtered_data())) "export_firms_pct" else "IC.FRM.EXPRT.ZS"
     create_density_plot(filtered_data(), col, "Export Participation Distribution", "#1B6B5F", "Export Firms (%)")
   })

   # ============================================================
   # Download Handlers
   # ============================================================

   # Simple download handler for charts
   simple_chart_download <- function(prefix) {
     downloadHandler(
       filename = function() {
         paste0("overview_", prefix, "_", format(Sys.Date(), "%Y%m%d"), ".html")
       },
       content = function(file) {
         saveWidget(plot_ly() |> layout(title = paste("Overview -", prefix)), file, selfcontained = TRUE)
       }
     )
   }

   output$dl_obstacles_chart <- simple_chart_download("business_obstacles")
   output$dl_regional_comparison <- simple_chart_download("regional_comparison")
   output$dl_infrastructure_gauge <- simple_chart_download("infrastructure_index")
   output$dl_finance_gauge <- simple_chart_download("finance_index")
   output$dl_density_female_workers <- simple_chart_download("density_female_workers")
   output$dl_density_female_ownership <- simple_chart_download("density_female_ownership")
   output$dl_density_capacity <- simple_chart_download("density_capacity")
   output$dl_density_power_outages <- simple_chart_download("density_power_outages")
   output$dl_density_bribery <- simple_chart_download("density_bribery")
   output$dl_density_exports <- simple_chart_download("density_exports")

 })
}
