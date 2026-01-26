# app/view/mod_mobile_ui.R
# Mobile UI Module using shinyMobile (Framework7)
# Provides mobile-optimized interface for the WBES Dashboard

box::use(
  shiny[moduleServer, NS, reactive, reactiveVal, req, tags, HTML, icon, div, h2, h3, h4, p, span,
        fluidRow, column, selectInput, actionButton, observeEvent, renderUI, uiOutput,
        updateSelectInput, downloadButton, renderText, textOutput, invalidateLater, isolate, observe],
  shinyMobile[f7Page, f7TabLayout, f7Navbar, f7Tabs, f7Tab, f7Card, f7Block,
              f7List, f7ListItem, f7Select, f7Button, f7Accordion,
              f7AccordionItem, f7Icon, f7Chip],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, config, add_trace],
  leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, addCircleMarkers, setView, colorNumeric, addLegend, labelFormat],
  dplyr[filter, arrange, desc, mutate, summarise, group_by, n, first, any_of, across],
  rlang[`%||%`],
  stats[na.omit, setNames, density, median, sd, reorder, var, dnorm, dlnorm, dgamma, dexp, dbeta,
        pnorm, plnorm, pgamma, pexp, pbeta, ks.test],
  scales[rescale],
  utils[head],
  MASS[fitdistr],
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
            # Use standard selectInput instead of f7Select for better compatibility
            selectInput(
              inputId = ns("map_indicator_mobile"),
              label = "Select Indicator",
              choices = c(
                "Power Outages" = "power_outages_per_month",
                "Access to Credit" = "firms_with_credit_line_pct",
                "Bribery Incidence" = "bribery_incidence_pct",
                "Capacity Utilization" = "capacity_utilization_pct"
              ),
              width = "100%"
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
          f7Block(
            strong = TRUE,
            inset = TRUE,
            tags$h4("Distribution Analysis", style = "color: #1B6B5F; margin-bottom: 5px;"),
            tags$p("Explore distribution of key business metrics", style = "font-size: 12px; color: #666;")
          ),

          # Distribution Plot 1 with navigation
          f7Card(
            title = NULL,
            # Navigation controls row
            tags$div(
              style = "display: flex; align-items: center; gap: 4px; margin-bottom: 10px;",
              actionButton(ns("prev_1_mobile"), label = NULL, icon = icon("chevron-left"),
                          class = "btn-sm", style = "width: 32px; padding: 4px 6px; flex-shrink: 0;"),
              tags$div(style = "flex: 1; min-width: 0;",
                selectInput(
                  ns("density_var_1_mobile"),
                  label = NULL,
                  choices = c("Loading..." = ""),
                  width = "100%"
                )
              ),
              actionButton(ns("next_1_mobile"), label = NULL, icon = icon("chevron-right"),
                          class = "btn-sm", style = "width: 32px; padding: 4px 6px; flex-shrink: 0;"),
              actionButton(ns("auto_scroll_1_mobile"), label = NULL, icon = icon("play"),
                          class = "btn-sm btn-outline-primary", style = "width: 32px; padding: 4px 6px; flex-shrink: 0;", title = "Auto-scroll"),
              actionButton(ns("stop_scroll_1_mobile"), label = NULL, icon = icon("stop"),
                          class = "btn-sm btn-outline-secondary", style = "width: 32px; padding: 4px 6px; display: none; flex-shrink: 0;", title = "Stop")
            ),
            plotlyOutput(ns("density_plot_1_mobile"), height = "200px"),
            uiOutput(ns("density_stats_1_mobile")),
            # Distribution fit table
            tags$div(
              style = "margin-top: 10px;",
              tags$h6(icon("table"), " Best Fit", style = "color: #1B6B5F; font-size: 12px;"),
              uiOutput(ns("dist_fit_table_1_mobile"))
            )
          ),

          # Distribution Plot 2 with navigation
          f7Card(
            title = NULL,
            # Navigation controls row
            tags$div(
              style = "display: flex; align-items: center; gap: 4px; margin-bottom: 10px;",
              actionButton(ns("prev_2_mobile"), label = NULL, icon = icon("chevron-left"),
                          class = "btn-sm", style = "width: 32px; padding: 4px 6px; flex-shrink: 0;"),
              tags$div(style = "flex: 1; min-width: 0;",
                selectInput(
                  ns("density_var_2_mobile"),
                  label = NULL,
                  choices = c("Loading..." = ""),
                  width = "100%"
                )
              ),
              actionButton(ns("next_2_mobile"), label = NULL, icon = icon("chevron-right"),
                          class = "btn-sm", style = "width: 32px; padding: 4px 6px; flex-shrink: 0;"),
              actionButton(ns("auto_scroll_2_mobile"), label = NULL, icon = icon("play"),
                          class = "btn-sm btn-outline-primary", style = "width: 32px; padding: 4px 6px; flex-shrink: 0;", title = "Auto-scroll"),
              actionButton(ns("stop_scroll_2_mobile"), label = NULL, icon = icon("stop"),
                          class = "btn-sm btn-outline-secondary", style = "width: 32px; padding: 4px 6px; display: none; flex-shrink: 0;", title = "Stop")
            ),
            plotlyOutput(ns("density_plot_2_mobile"), height = "200px"),
            uiOutput(ns("density_stats_2_mobile")),
            # Distribution fit table
            tags$div(
              style = "margin-top: 10px;",
              tags$h6(icon("table"), " Best Fit", style = "color: #1B6B5F; font-size: 12px;"),
              uiOutput(ns("dist_fit_table_2_mobile"))
            )
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
                "Female Workers" = "female_workers_pct",
                "Female Ownership" = "female_ownership_pct"
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

    # ============================================================
    # World Map
    # ============================================================
    output$world_map_mobile <- renderLeaflet({
      req(filtered_data())
      data <- filtered_data()
      indicator <- input$map_indicator_mobile %||% "power_outages_per_month"

      # Check if lat/lng columns exist
      has_coords <- "lat" %in% names(data) && "lng" %in% names(data)

      if (!has_coords) {
        return(
          leaflet() |>
            addTiles() |>
            setView(lng = 20, lat = 10, zoom = 1)
        )
      }

      # Handle lon/lng naming
      if ("lng" %in% names(data) && !"lon" %in% names(data)) {
        data$lon <- data$lng
      }

      data <- data[!is.na(data$lat) & !is.na(data$lon), ]

      if (indicator %in% names(data)) {
        data <- data[!is.na(data[[indicator]]), ]
      }

      if (nrow(data) > 0 && indicator %in% names(data)) {
        pal <- colorNumeric(
          palette = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026"),
          domain = data[[indicator]],
          na.color = "#808080"
        )

        size_values <- data[[indicator]]
        size_values[is.na(size_values)] <- min(size_values, na.rm = TRUE)
        data$marker_size <- rescale(size_values, to = c(4, 14))

        leaflet(data) |>
          addTiles() |>
          setView(lng = 20, lat = 10, zoom = 1) |>
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            radius = ~marker_size,
            color = ~pal(get(indicator)),
            fillColor = ~pal(get(indicator)),
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 1,
            popup = ~paste0("<strong>", country, "</strong><br>", round(get(indicator), 1))
          )
      } else {
        leaflet() |>
          addTiles() |>
          setView(lng = 20, lat = 10, zoom = 1)
      }
    })

    # ============================================================
    # Obstacles Chart
    # ============================================================
    output$obstacles_chart_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      obstacles <- data.frame(obstacle = character(), pct = numeric(), stringsAsFactors = FALSE)

      if ("IC.FRM.FINA.ZS" %in% names(data)) {
        obstacles <- rbind(obstacles, data.frame(obstacle = "Finance", pct = mean(data$IC.FRM.FINA.ZS, na.rm = TRUE)))
      }
      if ("IC.FRM.ELEC.ZS" %in% names(data)) {
        obstacles <- rbind(obstacles, data.frame(obstacle = "Electricity", pct = mean(data$IC.FRM.ELEC.ZS, na.rm = TRUE)))
      }
      if ("IC.FRM.CORR.ZS" %in% names(data)) {
        obstacles <- rbind(obstacles, data.frame(obstacle = "Corruption", pct = mean(data$IC.FRM.CORR.ZS, na.rm = TRUE)))
      }
      if ("IC.FRM.CRIM.ZS" %in% names(data)) {
        obstacles <- rbind(obstacles, data.frame(obstacle = "Crime", pct = mean(data$IC.FRM.CRIM.ZS, na.rm = TRUE)))
      }

      obstacles <- obstacles[!is.na(obstacles$pct), ]

      if (nrow(obstacles) > 0) {
        obstacles <- arrange(obstacles, pct)
        obstacles$obstacle <- factor(obstacles$obstacle, levels = obstacles$obstacle)

        plot_ly(obstacles, y = ~obstacle, x = ~pct, type = "bar", orientation = "h",
                marker = list(color = "#1B6B5F")) |>
          layout(
            xaxis = list(title = "% of Firms", ticksuffix = "%"),
            yaxis = list(title = ""),
            margin = list(l = 80, r = 10, t = 10, b = 40)
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(annotations = list(text = "No data", showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))
      }
    })

    # ============================================================
    # Regional Comparison
    # ============================================================
    output$regional_comparison_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      if (!"region" %in% names(data)) {
        return(plot_ly() |> layout(annotations = list(text = "No regional data", showarrow = FALSE)))
      }

      regional <- data |>
        filter(!is.na(region)) |>
        group_by(region) |>
        summarise(
          power_outages = mean(power_outages_per_month, na.rm = TRUE),
          credit_access = mean(firms_with_credit_line_pct, na.rm = TRUE),
          bribery = mean(bribery_incidence_pct, na.rm = TRUE),
          .groups = "drop"
        )

      if (nrow(regional) > 0) {
        plot_ly(regional) |>
          add_trace(x = ~region, y = ~power_outages, type = "bar", name = "Outages", marker = list(color = "#1B6B5F")) |>
          add_trace(x = ~region, y = ~credit_access, type = "bar", name = "Credit %", marker = list(color = "#F49B7A")) |>
          add_trace(x = ~region, y = ~bribery, type = "bar", name = "Bribery %", marker = list(color = "#6C757D")) |>
          layout(
            barmode = "group",
            xaxis = list(title = "", tickangle = 45, tickfont = list(size = 8)),
            yaxis = list(title = ""),
            legend = list(orientation = "h", y = -0.35, font = list(size = 8)),
            margin = list(b = 80, t = 10)
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |> layout(annotations = list(text = "No data", showarrow = FALSE))
      }
    })

    # ============================================================
    # Infrastructure Gauge
    # ============================================================
    output$infrastructure_gauge_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      infra_score <- 50
      if ("power_outages_per_month" %in% names(data)) {
        avg_outages <- mean(data$power_outages_per_month, na.rm = TRUE)
        infra_score <- max(0, min(100, 100 - (avg_outages * 10)))
      }

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = round(infra_score, 1),
        title = list(text = "Infrastructure", font = list(size = 11)),
        gauge = list(
          axis = list(range = list(0, 100), tickfont = list(size = 8)),
          bar = list(color = "#1B6B5F"),
          steps = list(
            list(range = c(0, 40), color = "#ffebee"),
            list(range = c(40, 70), color = "#fff3e0"),
            list(range = c(70, 100), color = "#e8f5e9")
          )
        )
      ) |>
        layout(margin = list(t = 30, b = 10, l = 20, r = 20)) |>
        config(displayModeBar = FALSE)
    })

    # ============================================================
    # Finance Gauge
    # ============================================================
    output$finance_gauge_mobile <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      finance_score <- 50
      if ("firms_with_credit_line_pct" %in% names(data)) {
        finance_score <- mean(data$firms_with_credit_line_pct, na.rm = TRUE)
      }

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = round(finance_score, 1),
        title = list(text = "Credit Access", font = list(size = 11)),
        gauge = list(
          axis = list(range = list(0, 100), tickfont = list(size = 8)),
          bar = list(color = "#F49B7A"),
          steps = list(
            list(range = c(0, 30), color = "#ffebee"),
            list(range = c(30, 60), color = "#fff3e0"),
            list(range = c(60, 100), color = "#e8f5e9")
          )
        )
      ) |>
        layout(margin = list(t = 30, b = 10, l = 20, r = 20)) |>
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

    # Update dropdown choices when base data loads (only once)
    observeEvent(wbes_data(), {
      req(wbes_data()$latest)
      data <- wbes_data()$latest

      # Get numeric columns
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      exclude_cols <- c("lat", "lng", "lon", "year", "sample_size", "firms_count", "marker_size")
      numeric_cols <- setdiff(numeric_cols, exclude_cols)

      if (length(numeric_cols) > 0) {
        # Create named vector with friendly labels
        labels <- sapply(numeric_cols, function(col) {
          label <- gsub("_pct$", " (%)", col)
          label <- gsub("_per_month$", " (per month)", label)
          label <- gsub("_", " ", label)
          label <- gsub("IC\\.FRM\\.", "", label)
          label <- tools::toTitleCase(label)
          label
        })
        choices <- setNames(numeric_cols, labels)

        defaults <- c(
          if ("female_workers_pct" %in% numeric_cols) "female_workers_pct" else if ("IC.FRM.FEMW.ZS" %in% numeric_cols) "IC.FRM.FEMW.ZS" else numeric_cols[1],
          if ("capacity_utilization_pct" %in% numeric_cols) "capacity_utilization_pct" else if ("IC.FRM.CAPU.ZS" %in% numeric_cols) "IC.FRM.CAPU.ZS" else numeric_cols[min(2, length(numeric_cols))]
        )

        shiny::updateSelectInput(session, "density_var_1_mobile", choices = choices, selected = defaults[1])
        shiny::updateSelectInput(session, "density_var_2_mobile", choices = choices, selected = defaults[2])
      }
    }, ignoreInit = FALSE, once = TRUE)

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

    # ============================================================
    # Navigation Controls for Density Plots
    # ============================================================

    # Reactive values to track auto-scroll state
    auto_scroll_active_1_mobile <- reactiveVal(FALSE)
    auto_scroll_active_2_mobile <- reactiveVal(FALSE)

    # Helper function to get next/previous index
    get_nav_index_mobile <- function(current, choices, direction = "next") {
      if (length(choices) == 0) return(NULL)
      current_idx <- which(choices == current)
      if (length(current_idx) == 0) current_idx <- 1

      if (direction == "next") {
        new_idx <- if (current_idx >= length(choices)) 1 else current_idx + 1
      } else {
        new_idx <- if (current_idx <= 1) length(choices) else current_idx - 1
      }
      choices[new_idx]
    }

    # Plot 1 Navigation
    observeEvent(input$prev_1_mobile, {
      auto_scroll_active_1_mobile(FALSE)
      choices <- available_density_vars_mobile()
      if (length(choices) > 0) {
        new_val <- get_nav_index_mobile(input$density_var_1_mobile, choices, "prev")
        shiny::updateSelectInput(session, "density_var_1_mobile", selected = new_val)
      }
    })

    observeEvent(input$next_1_mobile, {
      auto_scroll_active_1_mobile(FALSE)
      choices <- available_density_vars_mobile()
      if (length(choices) > 0) {
        new_val <- get_nav_index_mobile(input$density_var_1_mobile, choices, "next")
        shiny::updateSelectInput(session, "density_var_1_mobile", selected = new_val)
      }
    })

    observeEvent(input$auto_scroll_1_mobile, {
      auto_scroll_active_1_mobile(TRUE)
    })

    observeEvent(input$stop_scroll_1_mobile, {
      auto_scroll_active_1_mobile(FALSE)
    })

    # Auto-scroll observer for plot 1
    observe({
      if (auto_scroll_active_1_mobile()) {
        invalidateLater(4000)
        choices <- isolate(available_density_vars_mobile())
        if (length(choices) > 0) {
          current <- isolate(input$density_var_1_mobile)
          new_val <- get_nav_index_mobile(current, choices, "next")
          shiny::updateSelectInput(session, "density_var_1_mobile", selected = new_val)
        }
      }
    })

    # Plot 2 Navigation
    observeEvent(input$prev_2_mobile, {
      auto_scroll_active_2_mobile(FALSE)
      choices <- available_density_vars_mobile()
      if (length(choices) > 0) {
        new_val <- get_nav_index_mobile(input$density_var_2_mobile, choices, "prev")
        shiny::updateSelectInput(session, "density_var_2_mobile", selected = new_val)
      }
    })

    observeEvent(input$next_2_mobile, {
      auto_scroll_active_2_mobile(FALSE)
      choices <- available_density_vars_mobile()
      if (length(choices) > 0) {
        new_val <- get_nav_index_mobile(input$density_var_2_mobile, choices, "next")
        shiny::updateSelectInput(session, "density_var_2_mobile", selected = new_val)
      }
    })

    observeEvent(input$auto_scroll_2_mobile, {
      auto_scroll_active_2_mobile(TRUE)
    })

    observeEvent(input$stop_scroll_2_mobile, {
      auto_scroll_active_2_mobile(FALSE)
    })

    # Auto-scroll observer for plot 2
    observe({
      if (auto_scroll_active_2_mobile()) {
        invalidateLater(4000)
        choices <- isolate(available_density_vars_mobile())
        if (length(choices) > 0) {
          current <- isolate(input$density_var_2_mobile)
          new_val <- get_nav_index_mobile(current, choices, "next")
          shiny::updateSelectInput(session, "density_var_2_mobile", selected = new_val)
        }
      }
    })

    # ============================================================
    # Distribution Fitting (Mobile - Simplified)
    # ============================================================

    # Helper function to fit distributions (mobile - simplified table)
    fit_distributions_mobile <- function(data, col_name) {
      if (is.null(col_name) || col_name == "" || !col_name %in% names(data)) {
        return(NULL)
      }

      values <- data[[col_name]]
      values <- values[!is.na(values) & is.finite(values)]

      if (length(values) < 10) {
        return(tags$p(style = "font-size: 10px; color: #999;", "Need 10+ observations"))
      }

      results <- list()

      # Normal distribution
      tryCatch({
        fit_norm <- fitdistr(values, "normal")
        ks_norm <- ks.test(values, "pnorm", mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"])
        results$Normal <- list(dist = "Normal", aic = 2 * 2 - 2 * fit_norm$loglik, p = round(ks_norm$p.value, 3))
      }, error = function(e) NULL)

      # Log-normal (positive values only)
      if (all(values > 0)) {
        tryCatch({
          fit_lnorm <- fitdistr(values, "lognormal")
          ks_lnorm <- ks.test(values, "plnorm", meanlog = fit_lnorm$estimate["meanlog"], sdlog = fit_lnorm$estimate["sdlog"])
          results$Lognormal <- list(dist = "Log-normal", aic = 2 * 2 - 2 * fit_lnorm$loglik, p = round(ks_lnorm$p.value, 3))
        }, error = function(e) NULL)

        tryCatch({
          fit_gamma <- fitdistr(values, "gamma")
          ks_gamma <- ks.test(values, "pgamma", shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"])
          results$Gamma <- list(dist = "Gamma", aic = 2 * 2 - 2 * fit_gamma$loglik, p = round(ks_gamma$p.value, 3))
        }, error = function(e) NULL)
      }

      # Beta (for 0-100 range)
      if (all(values >= 0) && all(values <= 100)) {
        scaled <- pmax(0.001, pmin(0.999, values / 100))
        tryCatch({
          m <- mean(scaled); v <- var(scaled)
          if (v > 1e-10 && v < m * (1 - m)) {
            a <- m * ((m * (1 - m) / v) - 1)
            b <- (1 - m) * ((m * (1 - m) / v) - 1)
            if (a > 0.01 && b > 0.01) {
              fit_beta <- fitdistr(scaled, "beta", start = list(shape1 = a, shape2 = b))
              ks_beta <- tryCatch(ks.test(scaled, "pbeta", shape1 = fit_beta$estimate["shape1"], shape2 = fit_beta$estimate["shape2"]), error = function(e) NULL)
              if (!is.null(ks_beta)) {
                results$Beta <- list(dist = "Beta", aic = 2 * 2 - 2 * fit_beta$loglik, p = round(ks_beta$p.value, 3))
              }
            }
          }
        }, error = function(e) NULL)
      }

      if (length(results) == 0) {
        return(tags$p(style = "font-size: 10px; color: #999;", "Could not fit distributions"))
      }

      # Sort by significance then AIC
      df <- do.call(rbind, lapply(results, function(r) data.frame(dist = r$dist, aic = r$aic, p = r$p, stringsAsFactors = FALSE)))
      df$sig <- !is.na(df$p) & df$p >= 0.05
      df <- df[order(-df$sig, df$aic), ]

      # Show only top 2 results for mobile
      df <- head(df, 2)

      tags$div(
        style = "font-size: 10px;",
        lapply(1:nrow(df), function(i) {
          row <- df[i, ]
          p_color <- if (!is.na(row$p) && row$p >= 0.05) "#28a745" else "#dc3545"
          bg <- if (i == 1 && row$sig) "#e3f2fd" else "transparent"
          tags$div(
            style = paste0("padding: 3px 5px; background: ", bg, "; margin-bottom: 2px; border-radius: 3px;"),
            tags$span(tags$strong(row$dist), style = "margin-right: 8px;"),
            tags$span(paste0("AIC: ", round(row$aic, 0)), style = "margin-right: 8px; color: #666;"),
            tags$span(paste0("p: ", if(is.na(row$p)) "N/A" else row$p), style = paste0("color: ", p_color, ";"))
          )
        })
      )
    }

    # Distribution fit table outputs
    output$dist_fit_table_1_mobile <- renderUI({
      req(filtered_data(), input$density_var_1_mobile)
      fit_distributions_mobile(filtered_data(), input$density_var_1_mobile)
    })

    output$dist_fit_table_2_mobile <- renderUI({
      req(filtered_data(), input$density_var_2_mobile)
      fit_distributions_mobile(filtered_data(), input$density_var_2_mobile)
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
        "Workforce" = mean(data$female_workers_pct, na.rm = TRUE),
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
      avg_generator <- if ("firms_with_generator_pct" %in% names(data)) {
        round(mean(data$firms_with_generator_pct, na.rm = TRUE), 1)
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
      avg_generator <- if ("firms_with_generator_pct" %in% names(data)) {
        round(mean(data$firms_with_generator_pct, na.rm = TRUE), 1)
      } else { NA }

      tags$div(
        f7Card(
          tags$div(
            style = "display: flex; justify-content: space-between;",
            tags$div(
              tags$strong("Avg. Outages/Month"),
              tags$h3(avg_outages, style = "color: #dc3545;")
            ),
            if (!is.na(avg_generator)) tags$div(
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

      avg_female <- if ("female_workers_pct" %in% names(data)) {
        round(mean(data$female_workers_pct, na.rm = TRUE), 1)
      } else { NA }

      if (is.na(avg_female)) {
        return(tags$p("Female workforce data not available", class = "text-color-gray"))
      }

      tags$div(
        f7Card(
          tags$div(
            style = "text-align: center;",
            tags$strong("Female Workers"),
            tags$h2(paste0(avg_female, "%"), style = "color: #F49B7A;")
          )
        )
      )
    })

    output$domain_performance_mobile <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      avg_capacity <- if ("capacity_utilization_pct" %in% names(data)) {
        round(mean(data$capacity_utilization_pct, na.rm = TRUE), 1)
      } else { NA }

      tags$div(
        f7Card(
          tags$div(
            style = "text-align: center;",
            tags$strong("Capacity Utilization"),
            tags$h2(if (!is.na(avg_capacity)) paste0(avg_capacity, "%") else "N/A",
                    style = "color: #1B6B5F;")
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
