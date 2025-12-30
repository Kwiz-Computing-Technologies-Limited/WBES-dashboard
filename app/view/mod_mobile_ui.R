# app/view/mod_mobile_ui.R
# Mobile UI Module using shinyMobile (Framework7)
# Provides mobile-optimized interface for the WBES Dashboard

box::use(
  shiny[moduleServer, NS, reactive, req, tags, HTML, icon, div, h2, h3, h4, p, span,
        fluidRow, column, selectInput, actionButton, observeEvent, renderUI, uiOutput,
        updateSelectInput, downloadButton, renderText, textOutput],
  shinyMobile[f7Page, f7TabLayout, f7Navbar, f7Tabs, f7Tab, f7Card, f7Block,
              f7List, f7ListItem, f7Select, f7Button, f7Accordion,
              f7AccordionItem, f7Icon, f7Chip],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, config, add_trace],
  leaflet[leafletOutput, renderLeaflet],
  dplyr[filter, arrange, desc, mutate, summarise, group_by, n, first, any_of],
  rlang[`%||%`],
  stats[na.omit, setNames, density, median, sd],
  scales[rescale],
  utils[head],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region]
)

#' Mobile UI - Main interface
#' @export
ui <- function(id) {
  ns <- NS(id)

  f7Page(
    title = "WBES Dashboard",
    options = list(
      theme = "ios",
      dark = FALSE,
      filled = TRUE,
      color = "#1B6B5F",
      touch = list(
        tapHold = TRUE,
        tapHoldDelay = 750
      )
    ),

    f7TabLayout(
      navbar = f7Navbar(
        title = "Business Environment",
        hairline = TRUE,
        left_panel = TRUE,
        right_panel = FALSE
      ),

      # Main content tabs
      f7Tabs(
        id = ns("mobile_tabs"),
        animated = TRUE,
        swipeable = FALSE,

        # Overview Tab
        f7Tab(
          tabName = "Overview",
          icon = f7Icon("globe"),
          active = TRUE,

          # KPI Cards in grid layout (no swiper to avoid conflicts)
          f7Block(
            strong = TRUE,
            inset = TRUE,
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
              uiOutput(ns("kpi_countries_mobile")),
              uiOutput(ns("kpi_firms_mobile")),
              uiOutput(ns("kpi_years_mobile")),
              uiOutput(ns("kpi_indicators_mobile"))
            )
          ),

          # Map Card
          f7Card(
            title = "Business Environment Map",
            f7Select(
              inputId = ns("map_indicator_mobile"),
              label = "Select Indicator",
              choices = c(
                "Power Outages" = "power_outages_per_month",
                "Access to Credit" = "firms_with_credit_line_pct",
                "Bribery Incidence" = "bribery_incidence_pct",
                "Capacity Utilization" = "capacity_utilization_pct"
              )
            ),
            leafletOutput(ns("world_map_mobile"), height = "300px")
          ),

          # Top Obstacles Card
          f7Card(
            title = "Top Business Obstacles",
            plotlyOutput(ns("obstacles_chart_mobile"), height = "300px")
          ),

          # Regional Comparison Card
          f7Card(
            title = "Regional Comparison",
            plotlyOutput(ns("regional_comparison_mobile"), height = "280px")
          ),

          # Gauges in a row
          f7Block(
            strong = TRUE,
            inset = TRUE,
            tags$h4("Quality Indices", style = "color: #1B6B5F; margin-bottom: 10px;"),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
              tags$div(
                plotlyOutput(ns("infrastructure_gauge_mobile"), height = "180px")
              ),
              tags$div(
                plotlyOutput(ns("finance_gauge_mobile"), height = "180px")
              )
            )
          ),

          # Density Plots Section
          f7Card(
            title = "Distribution Analysis",
            tags$p("Distribution of key business metrics", class = "text-color-gray", style = "font-size: 12px;"),
            selectInput(
              ns("density_var_1_mobile"),
              label = "Indicator 1",
              choices = c("Loading..." = ""),
              width = "100%"
            ),
            plotlyOutput(ns("density_plot_1_mobile"), height = "200px"),
            uiOutput(ns("density_stats_1_mobile"))
          ),

          f7Card(
            title = NULL,
            selectInput(
              ns("density_var_2_mobile"),
              label = "Indicator 2",
              choices = c("Loading..." = ""),
              width = "100%"
            ),
            plotlyOutput(ns("density_plot_2_mobile"), height = "200px"),
            uiOutput(ns("density_stats_2_mobile"))
          )
        ),

        # Profiles Tab
        f7Tab(
          tabName = "Profiles",
          icon = f7Icon("person_crop_circle"),

          f7Block(
            strong = TRUE,
            inset = TRUE,
            tags$h3("Country Profile", class = "text-color-primary"),
            selectInput(
              ns("country_select_mobile"),
              label = "Select Country",
              choices = c("Loading..." = ""),
              width = "100%"
            )
          ),

          # Country KPIs
          f7Block(
            strong = TRUE,
            inset = TRUE,
            uiOutput(ns("country_kpis_mobile"))
          ),

          # Country radar chart
          f7Card(
            title = "Performance Radar",
            plotlyOutput(ns("country_radar_mobile"), height = "300px")
          ),

          # Country key indicators
          f7Card(
            title = "Key Indicators",
            f7Accordion(
              id = ns("country_indicators_accordion"),
              f7AccordionItem(
                title = "Infrastructure",
                uiOutput(ns("country_infrastructure_mobile"))
              ),
              f7AccordionItem(
                title = "Finance",
                uiOutput(ns("country_finance_mobile"))
              ),
              f7AccordionItem(
                title = "Governance",
                uiOutput(ns("country_governance_mobile"))
              )
            )
          )
        ),

        # Benchmarks Tab
        f7Tab(
          tabName = "Compare",
          icon = f7Icon("chart_bar"),

          f7Block(
            strong = TRUE,
            inset = TRUE,
            tags$h3("Cross-Country Comparison", class = "text-color-primary"),
            f7Select(
              inputId = ns("benchmark_indicator_mobile"),
              label = "Select Indicator",
              choices = c(
                "Power Outages" = "power_outages_per_month",
                "Access to Credit" = "firms_with_credit_line_pct",
                "Bribery Incidence" = "bribery_incidence_pct",
                "Capacity Utilization" = "capacity_utilization_pct",
                "Female Participation" = "pct_female_top_manager",
                "Export Intensity" = "pct_direct_exports"
              )
            )
          ),

          # Benchmark Chart
          f7Card(
            title = "Country Ranking",
            plotlyOutput(ns("benchmark_chart_mobile"), height = "400px")
          ),

          # Regional summary
          f7Card(
            title = "Regional Averages",
            plotlyOutput(ns("regional_avg_mobile"), height = "250px")
          )
        ),

        # Domains Tab
        f7Tab(
          tabName = "Domains",
          icon = f7Icon("layers"),

          f7Block(
            strong = TRUE,
            inset = TRUE,
            tags$h3("Domain Analysis", class = "text-color-primary")
          ),

          # Domain selection accordion
          f7Accordion(
            id = ns("domain_accordion"),

            f7AccordionItem(
              title = "Infrastructure",
              icon = f7Icon("bolt"),
              uiOutput(ns("domain_infrastructure_mobile"))
            ),

            f7AccordionItem(
              title = "Access to Finance",
              icon = f7Icon("creditcard"),
              uiOutput(ns("domain_finance_mobile"))
            ),

            f7AccordionItem(
              title = "Corruption",
              icon = f7Icon("exclamationmark_shield"),
              uiOutput(ns("domain_corruption_mobile"))
            ),

            f7AccordionItem(
              title = "Workforce",
              icon = f7Icon("person_2"),
              uiOutput(ns("domain_workforce_mobile"))
            ),

            f7AccordionItem(
              title = "Performance",
              icon = f7Icon("chart_line_uptrend_xyaxis"),
              uiOutput(ns("domain_performance_mobile"))
            ),

            f7AccordionItem(
              title = "Crime & Security",
              icon = f7Icon("shield"),
              uiOutput(ns("domain_crime_mobile"))
            )
          )
        ),

        # Filters Tab
        f7Tab(
          tabName = "Filters",
          icon = f7Icon("slider_horizontal_3"),

          f7Block(
            strong = TRUE,
            inset = TRUE,
            tags$h3("Data Filters", class = "text-color-primary"),
            tags$p("Apply filters to refine your analysis", class = "text-color-gray")
          ),

          f7Card(
            title = "Global Filters",

            selectInput(
              ns("mobile_region_filter"),
              label = "Region",
              choices = c("All Regions" = "all"),
              width = "100%"
            ),

            selectInput(
              ns("mobile_sector_filter"),
              label = "Sector",
              choices = c("All Sectors" = "all"),
              width = "100%"
            ),

            selectInput(
              ns("mobile_size_filter"),
              label = "Firm Size",
              choices = c("All Sizes" = "all"),
              width = "100%"
            ),

            selectInput(
              ns("mobile_income_filter"),
              label = "Income Group",
              choices = c("All Income Levels" = "all"),
              width = "100%"
            ),

            selectInput(
              ns("mobile_year_filter"),
              label = "Survey Year",
              choices = c("Latest Year" = "latest", "All Years" = "all"),
              width = "100%"
            ),

            f7Button(
              inputId = ns("reset_filters_mobile"),
              label = "Reset All Filters",
              color = "red",
              fill = FALSE
            )
          ),

          # Active filters display
          f7Block(
            strong = TRUE,
            inset = TRUE,
            uiOutput(ns("active_filters_display"))
          )
        )
      )
    )
  )
}

#' Mobile UI Server
#' @export
server <- function(id, wbes_data, global_filters, wb_prefetched_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update filter choices when data loads
    # Note: shinyMobile 2.0 updateF7Select doesn't support choices update,
    # so we use standard Shiny updateSelectInput instead
    observeEvent(wbes_data(), {
      req(wbes_data())
      data <- wbes_data()

      if (!is.null(data$latest)) {
        # Update country selector
        countries <- sort(unique(data$latest$country))
        shiny::updateSelectInput(
          session, "country_select_mobile",
          choices = stats::setNames(countries, countries)
        )

        # Update region filter
        regions <- c("All Regions" = "all", stats::setNames(
          unique(data$latest$region),
          unique(data$latest$region)
        ))
        shiny::updateSelectInput(session, "mobile_region_filter", choices = regions)

        # Update sector filter
        sectors <- c("All Sectors" = "all", stats::setNames(
          unique(na.omit(data$latest$sector)),
          unique(na.omit(data$latest$sector))
        ))
        shiny::updateSelectInput(session, "mobile_sector_filter", choices = sectors)

        # Update size filter
        sizes <- c("All Sizes" = "all", stats::setNames(
          unique(na.omit(data$latest$firm_size)),
          unique(na.omit(data$latest$firm_size))
        ))
        shiny::updateSelectInput(session, "mobile_size_filter", choices = sizes)

        # Update income filter
        incomes <- c("All Income Levels" = "all", stats::setNames(
          unique(na.omit(data$latest$income)),
          unique(na.omit(data$latest$income))
        ))
        shiny::updateSelectInput(session, "mobile_income_filter", choices = incomes)
      }
    })

    # Reactive filtered data
    filtered_data <- reactive({
      req(wbes_data())
      data <- wbes_data()$latest

      # Apply mobile filters
      if (!is.null(input$mobile_region_filter) && input$mobile_region_filter != "all") {
        data <- data |> filter(region == input$mobile_region_filter)
      }
      if (!is.null(input$mobile_sector_filter) && input$mobile_sector_filter != "all") {
        data <- data |> filter(sector == input$mobile_sector_filter)
      }
      if (!is.null(input$mobile_size_filter) && input$mobile_size_filter != "all") {
        data <- data |> filter(firm_size == input$mobile_size_filter)
      }
      if (!is.null(input$mobile_income_filter) && input$mobile_income_filter != "all") {
        data <- data |> filter(income == input$mobile_income_filter)
      }

      data
    })

    # KPI Outputs
    output$kpi_countries_mobile <- renderUI({
      req(wbes_data())
      n_countries <- length(unique(wbes_data()$latest$country))
      f7Card(
        title = "Countries",
        tags$div(
          style = "text-align: center;",
          tags$h1(n_countries, style = "color: #1B6B5F; margin: 0;"),
          tags$p("Economies covered", style = "color: #666; margin: 0;")
        )
      )
    })

    output$kpi_firms_mobile <- renderUI({
      req(wbes_data())
      n_firms <- format(nrow(wbes_data()$processed), big.mark = ",")
      f7Card(
        title = "Firms",
        tags$div(
          style = "text-align: center;",
          tags$h1(n_firms, style = "color: #F49B7A; margin: 0;"),
          tags$p("Firms surveyed", style = "color: #666; margin: 0;")
        )
      )
    })

    output$kpi_years_mobile <- renderUI({
      req(wbes_data())
      years <- wbes_data()$years
      year_range <- paste(min(years), "-", max(years))
      f7Card(
        title = "Years",
        tags$div(
          style = "text-align: center;",
          tags$h1(year_range, style = "color: #2E7D32; margin: 0; font-size: 1.5rem;"),
          tags$p("Survey period", style = "color: #666; margin: 0;")
        )
      )
    })

    output$kpi_indicators_mobile <- renderUI({
      req(wbes_data())
      n_indicators <- ncol(wbes_data()$latest) - 10  # Approximate indicator count
      f7Card(
        title = "Indicators",
        tags$div(
          style = "text-align: center;",
          tags$h1(paste0(n_indicators, "+"), style = "color: #17a2b8; margin: 0;"),
          tags$p("Business metrics", style = "color: #666; margin: 0;")
        )
      )
    })

    # World Map (simplified for mobile)
    output$world_map_mobile <- renderLeaflet({
      req(filtered_data())

      # Get data with coordinates (already merged in wbes_data)
      data <- filtered_data()

      # Check if lat/lng columns exist
      has_coords <- "lat" %in% names(data) && "lng" %in% names(data)

      if (!has_coords) {
        return(
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::setView(lng = 20, lat = 10, zoom = 2)
        )
      }

      indicator <- input$map_indicator_mobile %||% "power_outages_per_month"

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

        # Color palette - YlOrRd for consistency with desktop
        pal <- leaflet::colorNumeric(
          palette = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026"),
          domain = data[[indicator]],
          na.color = "#808080"
        )

        # Size scaling - rescale indicator values to marker sizes (5-18 for mobile)
        size_values <- data[[indicator]]
        size_values[is.na(size_values)] <- min(size_values, na.rm = TRUE)
        data$marker_size <- rescale(size_values, to = c(5, 18))

        leaflet::leaflet(data) |>
          leaflet::addTiles() |>
          leaflet::setView(lng = 20, lat = 10, zoom = 2) |>
          leaflet::addCircleMarkers(
            lng = ~lon,
            lat = ~lat,
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
          leaflet::addLegend(
            position = "bottomright",
            pal = pal,
            values = ~get(indicator),
            title = label,
            opacity = 0.8,
            labFormat = leaflet::labelFormat(suffix = if (grepl("pct", indicator)) "%" else "")
          )
      } else {
        leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::setView(lng = 20, lat = 10, zoom = 2)
      }
    })

    # Obstacles Chart (mobile optimized - horizontal bars)
    output$obstacles_chart_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      # Get obstacle indicators
      obstacle_cols <- grep("^biggest_obstacle_", names(data), value = TRUE)

      if (length(obstacle_cols) > 0 && "biggest_obstacle" %in% names(data)) {
        obstacle_data <- data |>
          group_by(biggest_obstacle) |>
          summarise(count = n(), .groups = "drop") |>
          filter(!is.na(biggest_obstacle)) |>
          arrange(desc(count)) |>
          head(8)

        plot_ly(
          obstacle_data,
          y = ~reorder(biggest_obstacle, count),
          x = ~count,
          type = "bar",
          orientation = "h",
          marker = list(color = "#1B6B5F")
        ) |>
          layout(
            xaxis = list(title = "Number of Firms"),
            yaxis = list(title = "", tickfont = list(size = 10)),
            margin = list(l = 120, r = 20, t = 10, b = 40)
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No obstacle data available",
              showarrow = FALSE,
              x = 0.5, y = 0.5, xref = "paper", yref = "paper"
            )
          )
      }
    })

    # Regional Comparison (mobile optimized)
    output$regional_comparison_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      regional_data <- data |>
        group_by(region) |>
        summarise(
          `Power Outages` = mean(power_outages_per_month, na.rm = TRUE),
          `Credit Access` = mean(firms_with_credit_line_pct, na.rm = TRUE),
          `Bribery` = mean(bribery_incidence_pct, na.rm = TRUE),
          .groups = "drop"
        ) |>
        filter(!is.na(region))

      plot_ly(regional_data, x = ~region, y = ~`Power Outages`,
              type = "bar", name = "Power Outages",
              marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~`Credit Access`, name = "Credit Access",
                  marker = list(color = "#F49B7A")) |>
        add_trace(y = ~`Bribery`, name = "Bribery",
                  marker = list(color = "#dc3545")) |>
        layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = 45, tickfont = list(size = 9)),
          yaxis = list(title = "Value"),
          legend = list(orientation = "h", y = -0.3),
          margin = list(b = 100, t = 10)
        ) |>
        config(displayModeBar = FALSE)
    })

    # Infrastructure Gauge
    output$infrastructure_gauge_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      # Calculate infrastructure quality index from actual data
      infra_score <- 50  # default

      if ("power_outages_per_month" %in% names(data)) {
        avg_outages <- mean(data$power_outages_per_month, na.rm = TRUE)
        # Convert outages to a 0-100 score (fewer outages = better score)
        infra_score <- max(0, min(100, 100 - (avg_outages * 10)))
      }

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = round(infra_score, 1),
        title = list(text = "Infrastructure", font = list(size = 12)),
        gauge = list(
          axis = list(range = list(0, 100), tickfont = list(size = 8)),
          bar = list(color = "#1B6B5F"),
          steps = list(
            list(range = c(0, 40), color = "#ffebee"),
            list(range = c(40, 70), color = "#fff3e0"),
            list(range = c(70, 100), color = "#e8f5e9")
          ),
          threshold = list(
            line = list(color = "#F49B7A", width = 3),
            thickness = 0.75,
            value = 75
          )
        )
      ) |>
        layout(
          margin = list(t = 40, b = 10, l = 20, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Finance Gauge
    output$finance_gauge_mobile <- renderPlotly({
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
        title = list(text = "Credit Access", font = list(size = 12)),
        gauge = list(
          axis = list(range = list(0, 100), tickfont = list(size = 8)),
          bar = list(color = "#F49B7A"),
          steps = list(
            list(range = c(0, 30), color = "#ffebee"),
            list(range = c(30, 60), color = "#fff3e0"),
            list(range = c(60, 100), color = "#e8f5e9")
          ),
          threshold = list(
            line = list(color = "#1B6B5F", width = 3),
            thickness = 0.75,
            value = 50
          )
        )
      ) |>
        layout(
          margin = list(t = 40, b = 10, l = 20, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # ============================================================
    # Density Plots with Dynamic Variable Selection
    # ============================================================

    # Get available numeric columns for density plots
    available_density_vars_mobile <- reactive({
      req(filtered_data())
      data <- filtered_data()

      # Get numeric columns
      numeric_cols <- names(data)[sapply(data, is.numeric)]

      # Exclude non-indicator columns
      exclude_cols <- c("lat", "lng", "lon", "year", "sample_size", "firms_count", "marker_size")
      numeric_cols <- setdiff(numeric_cols, exclude_cols)

      # Create named vector with friendly labels
      labels <- sapply(numeric_cols, function(col) {
        label <- gsub("_pct$", " (%)", col)
        label <- gsub("_per_month$", " (per month)", label)
        label <- gsub("_", " ", label)
        label <- gsub("IC\\.FRM\\.", "", label)
        label <- tools::toTitleCase(label)
        label
      })

      setNames(numeric_cols, labels)
    })

    # Update dropdown choices when data changes
    observeEvent(filtered_data(), {
      choices <- available_density_vars_mobile()
      if (length(choices) > 0) {
        defaults <- c(
          if ("female_workers_pct" %in% choices) "female_workers_pct" else if ("IC.FRM.FEMW.ZS" %in% choices) "IC.FRM.FEMW.ZS" else choices[1],
          if ("capacity_utilization_pct" %in% choices) "capacity_utilization_pct" else if ("IC.FRM.CAPU.ZS" %in% choices) "IC.FRM.CAPU.ZS" else choices[min(2, length(choices))]
        )

        shiny::updateSelectInput(session, "density_var_1_mobile", choices = choices, selected = defaults[1])
        shiny::updateSelectInput(session, "density_var_2_mobile", choices = choices, selected = defaults[2])
      }
    }, priority = -1)

    # Helper function to create density plot (mobile optimized)
    create_density_plot_mobile <- function(data, col_name, color = "#1B6B5F") {
      if (is.null(col_name) || col_name == "" || !col_name %in% names(data)) {
        return(
          plot_ly() |>
            layout(
              annotations = list(list(
                text = "Select an indicator",
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
                text = "Insufficient data",
                showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5
              )),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      # Create friendly label
      x_label <- gsub("_pct$", " (%)", col_name)
      x_label <- gsub("_per_month$", " (per month)", x_label)
      x_label <- gsub("_", " ", x_label)
      x_label <- gsub("IC\\.FRM\\.", "", x_label)
      x_label <- tools::toTitleCase(x_label)

      # Calculate density
      dens <- density(values, na.rm = TRUE)

      # Calculate statistics
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
          hovertemplate = paste0(x_label, ": %{x:.1f}<extra></extra>")
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
          xaxis = list(title = x_label, titlefont = list(size = 10)),
          yaxis = list(title = "Density", titlefont = list(size = 10)),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center", font = list(size = 8)),
          margin = list(l = 40, r = 10, t = 10, b = 60),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    }

    # Helper to create stats summary (mobile optimized)
    create_stats_summary_mobile <- function(data, col_name) {
      if (is.null(col_name) || col_name == "" || !col_name %in% names(data)) {
        return(NULL)
      }

      values <- data[[col_name]]
      values <- values[!is.na(values)]

      if (length(values) < 3) return(NULL)

      tags$div(
        class = "small text-color-gray",
        style = "font-size: 11px; display: flex; flex-wrap: wrap; gap: 8px;",
        tags$span(tags$strong("N: "), length(values)),
        tags$span(tags$strong("Min: "), round(min(values), 1)),
        tags$span(tags$strong("Max: "), round(max(values), 1)),
        tags$span(tags$strong("SD: "), round(sd(values, na.rm = TRUE), 1))
      )
    }

    # Density plots
    output$density_plot_1_mobile <- renderPlotly({
      req(filtered_data(), input$density_var_1_mobile)
      create_density_plot_mobile(filtered_data(), input$density_var_1_mobile, "#1B6B5F")
    })

    output$density_plot_2_mobile <- renderPlotly({
      req(filtered_data(), input$density_var_2_mobile)
      create_density_plot_mobile(filtered_data(), input$density_var_2_mobile, "#9c27b0")
    })

    # Stats summaries
    output$density_stats_1_mobile <- renderUI({
      req(filtered_data(), input$density_var_1_mobile)
      create_stats_summary_mobile(filtered_data(), input$density_var_1_mobile)
    })

    output$density_stats_2_mobile <- renderUI({
      req(filtered_data(), input$density_var_2_mobile)
      create_stats_summary_mobile(filtered_data(), input$density_var_2_mobile)
    })

    # Benchmark Chart
    output$benchmark_chart_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      indicator <- input$benchmark_indicator_mobile %||% "power_outages_per_month"

      country_data <- data |>
        group_by(country) |>
        summarise(value = mean(.data[[indicator]], na.rm = TRUE), .groups = "drop") |>
        filter(!is.na(value)) |>
        arrange(desc(value)) |>
        head(20)

      plot_ly(
        country_data,
        y = ~reorder(country, value),
        x = ~value,
        type = "bar",
        orientation = "h",
        marker = list(color = "#1B6B5F")
      ) |>
        layout(
          xaxis = list(title = gsub("_", " ", indicator)),
          yaxis = list(title = "", tickfont = list(size = 10)),
          margin = list(l = 100, r = 20, t = 10, b = 40)
        ) |>
        config(displayModeBar = FALSE)
    })

    # Regional averages chart
    output$regional_avg_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      indicator <- input$benchmark_indicator_mobile %||% "power_outages_per_month"

      regional_data <- data |>
        group_by(region) |>
        summarise(value = mean(.data[[indicator]], na.rm = TRUE), .groups = "drop") |>
        filter(!is.na(value), !is.na(region)) |>
        arrange(desc(value))

      plot_ly(
        regional_data,
        x = ~region,
        y = ~value,
        type = "bar",
        marker = list(color = "#F49B7A")
      ) |>
        layout(
          xaxis = list(title = "", tickangle = 45, tickfont = list(size = 10)),
          yaxis = list(title = gsub("_", " ", indicator)),
          margin = list(b = 80, t = 10)
        ) |>
        config(displayModeBar = FALSE)
    })

    # Country KPIs
    output$country_kpis_mobile <- renderUI({
      req(wbes_data(), input$country_select_mobile)
      data <- wbes_data()$latest |>
        filter(country == input$country_select_mobile)

      if (nrow(data) == 0) return(tags$p("No data available"))

      tags$div(
        class = "display-flex flex-wrap",
        style = "gap: 10px;",
        f7Chip(label = paste("Firms:", format(nrow(data), big.mark = ",")), status = "green"),
        f7Chip(label = paste("Region:", first(data$region)), status = "blue"),
        f7Chip(label = paste("Income:", first(data$income)), status = "orange")
      )
    })

    # Country Radar Chart
    output$country_radar_mobile <- renderPlotly({
      req(wbes_data(), input$country_select_mobile)
      data <- wbes_data()$latest |>
        filter(country == input$country_select_mobile)

      if (nrow(data) == 0) {
        return(plot_ly() |> layout(annotations = list(text = "No data", showarrow = FALSE)))
      }

      # Calculate key metrics
      metrics <- c(
        "Infrastructure" = 100 - min(mean(data$power_outages_per_month, na.rm = TRUE) * 5, 100),
        "Finance" = mean(data$firms_with_credit_line_pct, na.rm = TRUE),
        "Governance" = 100 - mean(data$bribery_incidence_pct, na.rm = TRUE),
        "Workforce" = mean(data$pct_female_top_manager, na.rm = TRUE) * 2,
        "Performance" = mean(data$capacity_utilization_pct, na.rm = TRUE)
      )

      metrics[is.na(metrics)] <- 50

      plot_ly(
        type = "scatterpolar",
        r = as.numeric(metrics),
        theta = names(metrics),
        fill = "toself",
        fillcolor = "rgba(27, 107, 95, 0.3)",
        line = list(color = "#1B6B5F")
      ) |>
        layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 100))
          ),
          showlegend = FALSE,
          margin = list(l = 40, r = 40, t = 40, b = 40)
        ) |>
        config(displayModeBar = FALSE)
    })

    # Country Profile - Infrastructure accordion content
    output$country_infrastructure_mobile <- renderUI({
      req(wbes_data(), input$country_select_mobile)
      data <- wbes_data()$latest |>
        filter(country == input$country_select_mobile)

      if (nrow(data) == 0) return(tags$p("No data available", class = "text-color-gray"))

      avg_outages <- round(mean(data$power_outages_per_month, na.rm = TRUE), 1)
      avg_generator <- if ("pct_firms_with_generator" %in% names(data)) {
        round(mean(data$pct_firms_with_generator, na.rm = TRUE), 1)
      } else { NA }
      avg_water <- if ("water_insufficiency_pct" %in% names(data)) {
        round(mean(data$water_insufficiency_pct, na.rm = TRUE), 1)
      } else { NA }

      tags$div(
        style = "padding: 10px 0;",
        tags$div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          tags$div(
            tags$span("Power Outages", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(avg_outages, "/month"), style = "color: #dc3545; margin: 5px 0;")
          ),
          if (!is.na(avg_generator)) tags$div(
            tags$span("Generator Use", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(avg_generator, "%"), style = "color: #1B6B5F; margin: 5px 0;")
          ),
          if (!is.na(avg_water)) tags$div(
            tags$span("Water Issues", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(avg_water, "%"), style = "color: #F49B7A; margin: 5px 0;")
          )
        )
      )
    })

    # Country Profile - Finance accordion content
    output$country_finance_mobile <- renderUI({
      req(wbes_data(), input$country_select_mobile)
      data <- wbes_data()$latest |>
        filter(country == input$country_select_mobile)

      if (nrow(data) == 0) return(tags$p("No data available", class = "text-color-gray"))

      credit_access <- round(mean(data$firms_with_credit_line_pct, na.rm = TRUE), 1)
      bank_account <- if ("firms_with_bank_account_pct" %in% names(data)) {
        round(mean(data$firms_with_bank_account_pct, na.rm = TRUE), 1)
      } else { NA }
      collateral <- if ("pct_collateral_required" %in% names(data)) {
        round(mean(data$pct_collateral_required, na.rm = TRUE), 1)
      } else { NA }

      tags$div(
        style = "padding: 10px 0;",
        tags$div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          tags$div(
            tags$span("Credit Access", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(credit_access, "%"), style = "color: #1B6B5F; margin: 5px 0;")
          ),
          if (!is.na(bank_account)) tags$div(
            tags$span("Bank Account", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(bank_account, "%"), style = "color: #17a2b8; margin: 5px 0;")
          ),
          if (!is.na(collateral)) tags$div(
            tags$span("Collateral Required", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(collateral, "%"), style = "color: #F49B7A; margin: 5px 0;")
          )
        )
      )
    })

    # Country Profile - Governance accordion content
    output$country_governance_mobile <- renderUI({
      req(wbes_data(), input$country_select_mobile)
      data <- wbes_data()$latest |>
        filter(country == input$country_select_mobile)

      if (nrow(data) == 0) return(tags$p("No data available", class = "text-color-gray"))

      bribery <- round(mean(data$bribery_incidence_pct, na.rm = TRUE), 1)
      mgmt_time_regs <- if ("mgmt_time_on_regulations_pct" %in% names(data)) {
        round(mean(data$mgmt_time_on_regulations_pct, na.rm = TRUE), 1)
      } else { NA }
      informal_competition <- if ("informal_competition_pct" %in% names(data)) {
        round(mean(data$informal_competition_pct, na.rm = TRUE), 1)
      } else { NA }

      tags$div(
        style = "padding: 10px 0;",
        tags$div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          tags$div(
            tags$span("Bribery Incidence", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(bribery, "%"), style = "color: #dc3545; margin: 5px 0;")
          ),
          if (!is.na(mgmt_time_regs)) tags$div(
            tags$span("Mgmt Time on Regs", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(mgmt_time_regs, "%"), style = "color: #F49B7A; margin: 5px 0;")
          ),
          if (!is.na(informal_competition)) tags$div(
            tags$span("Informal Competition", style = "font-size: 12px; color: #666;"),
            tags$h4(paste0(informal_competition, "%"), style = "color: #6C757D; margin: 5px 0;")
          )
        )
      )
    })

    # Domain summaries
    output$domain_infrastructure_mobile <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      avg_outages <- round(mean(data$power_outages_per_month, na.rm = TRUE), 1)
      avg_generator <- round(mean(data$pct_firms_with_generator, na.rm = TRUE), 1)

      tags$div(
        f7Card(
          tags$div(
            style = "display: flex; justify-content: space-between;",
            tags$div(
              tags$strong("Avg. Outages/Month"),
              tags$h3(avg_outages, style = "color: #dc3545;")
            ),
            tags$div(
              tags$strong("Firms with Generator"),
              tags$h3(paste0(avg_generator, "%"), style = "color: #1B6B5F;")
            )
          )
        ),
        plotlyOutput(ns("infra_chart_mobile"), height = "200px")
      )
    })

    output$infra_chart_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      regional <- data |>
        group_by(region) |>
        summarise(outages = mean(power_outages_per_month, na.rm = TRUE), .groups = "drop") |>
        filter(!is.na(region))

      plot_ly(regional, x = ~region, y = ~outages, type = "bar",
              marker = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = "", tickangle = 45, tickfont = list(size = 9)),
          yaxis = list(title = "Outages/Month"),
          margin = list(b = 80)
        ) |>
        config(displayModeBar = FALSE)
    })

    output$domain_finance_mobile <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      avg_credit <- round(mean(data$firms_with_credit_line_pct, na.rm = TRUE), 1)

      tags$div(
        f7Card(
          tags$div(
            style = "text-align: center;",
            tags$strong("Firms with Credit Access"),
            tags$h2(paste0(avg_credit, "%"), style = "color: #1B6B5F;")
          )
        )
      )
    })

    output$domain_corruption_mobile <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      avg_bribery <- round(mean(data$bribery_incidence_pct, na.rm = TRUE), 1)

      tags$div(
        f7Card(
          tags$div(
            style = "text-align: center;",
            tags$strong("Bribery Incidence"),
            tags$h2(paste0(avg_bribery, "%"), style = "color: #dc3545;")
          )
        )
      )
    })

    output$domain_workforce_mobile <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      avg_female <- round(mean(data$pct_female_top_manager, na.rm = TRUE), 1)

      tags$div(
        f7Card(
          tags$div(
            style = "text-align: center;",
            tags$strong("Female Top Managers"),
            tags$h2(paste0(avg_female, "%"), style = "color: #F49B7A;")
          )
        )
      )
    })

    output$domain_performance_mobile <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      avg_capacity <- round(mean(data$capacity_utilization_pct, na.rm = TRUE), 1)
      avg_exports <- round(mean(data$pct_direct_exports, na.rm = TRUE), 1)

      tags$div(
        f7Card(
          tags$div(
            style = "display: flex; justify-content: space-between;",
            tags$div(
              tags$strong("Capacity Utilization"),
              tags$h3(paste0(avg_capacity, "%"), style = "color: #1B6B5F;")
            ),
            tags$div(
              tags$strong("Export Intensity"),
              tags$h3(paste0(avg_exports, "%"), style = "color: #F49B7A;")
            )
          )
        )
      )
    })

    output$domain_crime_mobile <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      if ("security_costs_pct" %in% names(data)) {
        avg_security <- round(mean(data$security_costs_pct, na.rm = TRUE), 1)
      } else {
        avg_security <- "N/A"
      }

      tags$div(
        f7Card(
          tags$div(
            style = "text-align: center;",
            tags$strong("Average Security Costs"),
            tags$h2(paste0(if(is.numeric(avg_security)) paste0(avg_security, "%") else avg_security),
                    style = "color: #dc3545;")
          )
        )
      )
    })

    # Active filters display
    output$active_filters_display <- renderUI({
      filters <- list()

      if (!is.null(input$mobile_region_filter) && input$mobile_region_filter != "all") {
        filters <- c(filters, list(f7Chip(label = paste("Region:", input$mobile_region_filter),
                                          status = "blue", outline = TRUE)))
      }
      if (!is.null(input$mobile_sector_filter) && input$mobile_sector_filter != "all") {
        filters <- c(filters, list(f7Chip(label = paste("Sector:", input$mobile_sector_filter),
                                          status = "green", outline = TRUE)))
      }
      if (!is.null(input$mobile_size_filter) && input$mobile_size_filter != "all") {
        filters <- c(filters, list(f7Chip(label = paste("Size:", input$mobile_size_filter),
                                          status = "orange", outline = TRUE)))
      }
      if (!is.null(input$mobile_income_filter) && input$mobile_income_filter != "all") {
        filters <- c(filters, list(f7Chip(label = paste("Income:", input$mobile_income_filter),
                                          status = "purple", outline = TRUE)))
      }

      if (length(filters) == 0) {
        tags$p("No filters applied", class = "text-color-gray")
      } else {
        tags$div(
          tags$strong("Active Filters:"),
          tags$div(style = "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 10px;", filters)
        )
      }
    })

    # Reset filters
    observeEvent(input$reset_filters_mobile, {
      shiny::updateSelectInput(session, "mobile_region_filter", selected = "all")
      shiny::updateSelectInput(session, "mobile_sector_filter", selected = "all")
      shiny::updateSelectInput(session, "mobile_size_filter", selected = "all")
      shiny::updateSelectInput(session, "mobile_income_filter", selected = "all")
      shiny::updateSelectInput(session, "mobile_year_filter", selected = "latest")
    })

    # Return mobile filter state for potential sync with desktop
    reactive({
      list(
        region = input$mobile_region_filter,
        sector = input$mobile_sector_filter,
        firm_size = input$mobile_size_filter,
        income = input$mobile_income_filter,
        year = input$mobile_year_filter
      )
    })
  })
}
