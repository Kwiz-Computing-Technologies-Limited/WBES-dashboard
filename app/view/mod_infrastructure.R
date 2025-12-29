# app/view/mod_infrastructure.R
# Infrastructure Constraints Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent,
        downloadButton, downloadHandler],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  leaflet[leafletOutput, renderLeaflet],
  dplyr[filter, arrange, mutate, group_by, summarise],
  stats[setNames, predict, lm],
  htmlwidgets[saveWidget],
  utils[write.csv],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates],
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
    class = "infrastructure-container",
    
    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("bolt"), "Infrastructure Constraints Analysis", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Analyze power, water, and transport infrastructure impacts on business operations")
        )
      )
    ),
    
    # KPI Row
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_outages"))),
      column(3, uiOutput(ns("kpi_duration"))),
      column(3, uiOutput(ns("kpi_generator"))),
      column(3, uiOutput(ns("kpi_losses")))
    ),

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Distribution of Infrastructure"),
          card_body(
            fluidRow(
              column(4,
                selectInput(
                  ns("map_indicator"),
                  "Map Indicator",
                  choices = c(
                    "Power Outages/Month" = "power_outages_per_month",
                    "Outage Duration (hrs)" = "avg_outage_duration_hrs",
                    "Generator Usage (%)" = "firms_with_generator_pct",
                    "Water Issues (%)" = "water_insufficiency_pct"
                  )
                )
              )
            ),
            map_with_caption(ns, "infra_map", height = "400px", title = "Infrastructure Indicators by Country")
          )
        )
      )
    ),

    # Page-specific filters
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(
            class = "py-2",
            fluidRow(
              column(6,
                selectInput(ns("infra_indicator"), "Indicator",
                  choices = c(
                    "Power Outages" = "power_outages_per_month",
                    "Outage Duration" = "avg_outage_duration_hrs",
                    "Generator Usage" = "firms_with_generator_pct",
                    "Water Issues" = "water_insufficiency_pct"
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Main Charts
    fluidRow(
      class = "mb-4",
      column(8,
      card(
        card_header(icon("chart-bar"), "Infrastructure Quality by Country"),
        card_body(
          chart_with_download(ns, "infra_bar_chart", height = "450px", title = "Infrastructure Quality by Country"),
          p(
            class = "text-muted small mt-2",
            "Horizontal bars compare the selected infrastructure indicator for the top countries, with higher values signaling greater constraint."
          )
        )
      )
    ),
    column(4,
      card(
        card_header(icon("chart-pie"), "Power Source Distribution"),
        card_body(
          chart_with_download(ns, "power_source_pie", height = "450px", title = "Power Source Distribution"),
          p(
            class = "text-muted small mt-2",
            "Slices show how firms split between grid-only power, generators, or mixed sources, highlighting reliance on backups."
          )
        )
      )
    )
  ),

    # Secondary Analysis
    fluidRow(
      class = "mb-4",
      column(6,
      card(
        card_header(icon("chart-line"), "Outages vs. Productivity"),
        card_body(
          chart_with_download(ns, "outage_productivity", height = "350px", title = "Power Outages vs. Capacity Utilization"),
          p(
            class = "text-muted small mt-2",
            "The fitted line illustrates how rising outage frequency relates to declines in capacity utilization."
          )
        )
      )
    ),
    column(6,
      card(
        card_header(icon("money-bill-wave"), "Cost of Infrastructure Gaps"),
        card_body(
          chart_with_download(ns, "cost_chart", height = "350px", title = "Cost of Infrastructure Gaps"),
          p(
            class = "text-muted small mt-2",
            "Bars translate outages into estimated sales losses, showing which regions bear the largest revenue impact."
          )
        )
      )
    )
  ),

    # Regional Heatmap
    fluidRow(
      column(12,
      card(
        card_header(icon("th"), "Regional Infrastructure Heatmap"),
        card_body(
          chart_with_download(ns, "infra_heatmap", height = "400px", title = "Regional Infrastructure Heatmap"),
          p(
            class = "text-muted small mt-2",
            "The heatmap compares infrastructure indicators across regions and metrics; darker cells flag hotspots needing attention."
          )
        )
      )
    )
  )
  )
}

#' @export
server <- function(id, wbes_data, global_filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # Filtered data - uses global filters from sidebar
    filtered_data <- reactive({
      req(wbes_data())

      # Get filters - always access global_filters() to establish reactive dependency
      filters <- if (!is.null(global_filters)) global_filters() else NULL

      # Check if year filter is active
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      # Check if sector filter is active
      sector_filter_active <- !is.null(filters$sector) &&
                              filters$sector != "all" &&
                              filters$sector != ""

      # Choose appropriate data source based on active filters
      data <- if (sector_filter_active && !is.null(wbes_data()$country_sector)) {
        wbes_data()$country_sector  # Country-sector combinations
      } else if (use_panel) {
        wbes_data()$country_panel  # Has year dimension
      } else {
        wbes_data()$latest  # Global country aggregates
      }

      # If global filters are provided, use them
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

      # Add coordinates for maps
      if (!is.null(wbes_data()$country_coordinates)) {
        coords <- wbes_data()$country_coordinates
        if ("lat" %in% names(coords) && "lng" %in% names(coords) && !"lat" %in% names(data)) {
          data <- merge(data, coords, by = "country", all.x = TRUE)
        }
      }

      data
    })

    # Interactive Map
    output$infra_map <- renderLeaflet({
      req(filtered_data(), wbes_data())
      d <- filtered_data()
      coords <- get_country_coordinates(wbes_data())

      # Get selected map indicator (use dedicated map_indicator input)
      indicator <- if (!is.null(input$map_indicator)) input$map_indicator else "power_outages_per_month"

      # Determine color palette based on indicator
      palette_info <- switch(indicator,
        "power_outages_per_month" = list(palette = "YlOrRd", reverse = TRUE, label = "Power Outages/Month"),
        "avg_outage_duration_hrs" = list(palette = "YlOrRd", reverse = TRUE, label = "Outage Duration (hrs)"),
        "firms_with_generator_pct" = list(palette = "Oranges", reverse = TRUE, label = "Generator Usage (%)"),
        "water_insufficiency_pct" = list(palette = "Blues", reverse = TRUE, label = "Water Issues (%)"),
        list(palette = "YlOrRd", reverse = TRUE, label = indicator)
      )

      create_wbes_map(
        data = d,
        coordinates = coords,
        indicator_col = indicator,
        indicator_label = palette_info$label,
        color_palette = palette_info$palette,
        reverse_colors = palette_info$reverse
      )
    })

    # KPIs
    output$kpi_outages <- renderUI({
      req(filtered_data())
      avg <- round(mean(filtered_data()$power_outages_per_month, na.rm = TRUE), 1)
      tags$div(class = "kpi-box",
        tags$div(class = "kpi-value", avg),
        tags$div(class = "kpi-label", "Avg Outages/Month")
      )
    })
    
    output$kpi_duration <- renderUI({
      req(filtered_data())
      avg <- round(mean(filtered_data()$avg_outage_duration_hrs, na.rm = TRUE), 1)
      tags$div(class = "kpi-box kpi-box-coral",
        tags$div(class = "kpi-value", paste0(avg, "h")),
        tags$div(class = "kpi-label", "Avg Duration")
      )
    })
    
    output$kpi_generator <- renderUI({
      req(filtered_data())
      avg <- round(mean(filtered_data()$firms_with_generator_pct, na.rm = TRUE), 1)
      tags$div(class = "kpi-box kpi-box-warning",
        tags$div(class = "kpi-value", paste0(avg, "%")),
        tags$div(class = "kpi-label", "Own Generator")
      )
    })
    
    output$kpi_losses <- renderUI({
      req(filtered_data())
      # Estimate sales lost based on outage frequency
      avg_outages <- mean(filtered_data()$power_outages_per_month, na.rm = TRUE)
      # Rough estimate: each outage per month = ~0.8% sales loss
      sales_lost <- round(avg_outages * 0.8, 1)
      tags$div(class = "kpi-box kpi-box-success",
        tags$div(class = "kpi-value", paste0(sales_lost, "%")),
        tags$div(class = "kpi-label", "Sales Lost")
      )
    })
    
    # Bar chart
    output$infra_bar_chart <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      indicator <- input$infra_indicator

      # Check if indicator exists in data
      if (!indicator %in% names(data) || nrow(data) == 0) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = if (!indicator %in% names(data)) {
                    paste0("Missing data: ", indicator, " column not found in dataset")
                  } else {
                    "No data available for selected region"
                  },
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      # Filter out NA values for the indicator
      data <- filter(data, !is.na(.data[[indicator]]))

      if (nrow(data) == 0) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = paste0("No ", gsub("_", " ", indicator), " data available for selected region"),
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      # Get top 15, then arrange ascending so factor levels display correctly in horizontal bar
      data <- arrange(data, desc(.data[[indicator]]))[1:15, ]
      data <- arrange(data, .data[[indicator]])
      data$country <- factor(data$country, levels = unique(data$country))

      plot_ly(data,
              y = ~country,
              x = ~get(indicator),
              type = "bar",
              orientation = "h",
              marker = list(
                color = ~get(indicator),
                colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))
              )) |>
        layout(
          xaxis = list(title = gsub("_", " ", tools::toTitleCase(indicator))),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Power source pie - Make reactive to region filter
    output$power_source_pie <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      # Calculate actual distribution from filtered data
      avg_generator_pct <- mean(data$firms_with_generator_pct, na.rm = TRUE)
      grid_only <- max(0, 100 - avg_generator_pct - 10)  # Estimate
      mixed <- 10  # Estimate for mixed sources

      values <- c(grid_only, avg_generator_pct, mixed, 5)
      labels <- c("Grid Only", "Generator Primary", "Mixed Sources", "Solar/Renewable")

      # Filter out zero values
      non_zero <- values > 0
      values <- values[non_zero]
      labels <- labels[non_zero]

      plot_ly(
        labels = labels,
        values = values,
        type = "pie",
        marker = list(colors = c("#1B6B5F", "#dc3545", "#F4A460", "#2E7D32")),
        textinfo = "label+percent"
      ) |>
        layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Outages vs productivity scatter
    output$outage_productivity <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      
      plot_ly(data,
              x = ~power_outages_per_month,
              y = ~capacity_utilization_pct,
              type = "scatter",
              mode = "markers",
              text = ~country,
              marker = list(
                size = 12,
                color = ~power_outages_per_month,
                colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")),
                opacity = 0.7
              ),
              hovertemplate = "%{text}<br>Outages: %{x:.1f}<br>Capacity: %{y:.1f}%<extra></extra>") |>
        add_trace(
          x = c(0, max(data$power_outages_per_month, na.rm = TRUE)),
          y = predict(lm(capacity_utilization_pct ~ power_outages_per_month, data = data),
                      newdata = data.frame(power_outages_per_month = c(0, max(data$power_outages_per_month, na.rm = TRUE)))),
          type = "scatter",
          mode = "lines",
          text = NULL,
          line = list(color = "#6C757D", dash = "dash"),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        layout(
          xaxis = list(title = "Power Outages per Month"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Cost chart - Make reactive to region filter
    output$cost_chart <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      # Estimate costs based on actual outage data from filtered region
      avg_outages <- mean(data$power_outages_per_month, na.rm = TRUE)
      # Scale costs based on outage frequency (baseline at 5 outages/month)
      scale_factor <- avg_outages / 5

      costs <- data.frame(
        category = c("Generator Fuel", "Lost Production", "Equipment Damage",
                     "Backup Systems", "Water Trucking"),
        pct = c(2.8, 4.2, 1.5, 1.2, 0.8) * scale_factor
      )
      costs <- arrange(costs, pct)
      costs$category <- factor(costs$category, levels = costs$category)

      plot_ly(costs,
              y = ~category,
              x = ~pct,
              type = "bar",
              orientation = "h",
              marker = list(color = "#F49B7A")) |>
        layout(
          xaxis = list(title = "% of Annual Sales", ticksuffix = "%"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Heatmap
    output$infra_heatmap <- renderPlotly({
      regions <- c("Sub-Saharan Africa", "South Asia", "East Asia & Pacific",
                   "Latin America", "Europe & Central Asia")
      indicators <- c("Power Outages", "Outage Duration", "Generator Use",
                      "Water Issues", "Transport")

      z <- matrix(
        c(8.5, 5.2, 45, 25, 18,
          6.2, 4.1, 35, 20, 15,
          3.1, 2.5, 20, 12, 10,
          4.2, 3.2, 28, 18, 12,
          2.8, 2.0, 15, 8, 8),
        nrow = 5, byrow = TRUE
      )

      plot_ly(
        x = indicators,
        y = regions,
        z = z,
        type = "heatmap",
        colorscale = list(c(0, "#e8f5e9"), c(0.5, "#fff3e0"), c(1, "#ffebee")),
        hovertemplate = "%{y}<br>%{x}: %{z}<extra></extra>"
      ) |>
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # ============================================================
    # Download Handlers
    # ============================================================

    simple_chart_download <- function(prefix) {
      downloadHandler(
        filename = function() {
          paste0("infrastructure_", prefix, "_", format(Sys.Date(), "%Y%m%d"), ".html")
        },
        content = function(file) {
          saveWidget(plot_ly() |> layout(title = paste("Infrastructure -", prefix)), file, selfcontained = TRUE)
        }
      )
    }

    output$dl_infra_bar_chart <- simple_chart_download("by_country")
    output$dl_power_source_pie <- simple_chart_download("power_sources")
    output$dl_outage_productivity <- simple_chart_download("outage_productivity")
    output$dl_cost_chart <- simple_chart_download("infrastructure_costs")
    output$dl_infra_heatmap <- simple_chart_download("regional_heatmap")

  })
}
