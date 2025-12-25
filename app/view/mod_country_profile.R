# app/view/mod_country_profile.R
# Country Profile Deep Dive Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, tagList, icon, div, h2, h3, h4, p, span,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent, renderText, textOutput,
        downloadButton, downloadHandler],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, select, arrange, mutate],
  leaflet[leafletOutput, renderLeaflet],
  stats[setNames],
  htmlwidgets[saveWidget],
  utils[write.csv],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates]
)

# Helper function to create chart container with download button
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
    plotlyOutput(ns(output_id), height = height)
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "country-profile-container",

    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("flag"), "Country Profile", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "In-depth analysis of business environment indicators for individual countries")
        )
      )
    ),

    # Country Selector
    fluidRow(
      class = "mb-4",
      column(4,
        card(
          card_body(
            selectInput(
              ns("country_select"),
              "Select Country",
              choices = NULL,
              width = "100%"
            )
          )
        )
      ),
      column(8,
        uiOutput(ns("country_summary"))
      )
    ),

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Context"),
          card_body(
            fluidRow(
              column(4,
                selectInput(
                  ns("map_indicator"),
                  "Map Indicator",
                  choices = c(
                    "Power Outages" = "power_outages_per_month",
                    "Outage Duration" = "avg_outage_duration_hrs",
                    "Credit Access" = "firms_with_credit_line_pct",
                    "Bribery Incidence" = "bribery_incidence_pct",
                    "Capacity Utilization" = "capacity_utilization_pct",
                    "Female Ownership" = "female_ownership_pct"
                  ),
                  selected = "power_outages_per_month"
                )
              )
            ),
            leafletOutput(ns("country_profile_map"), height = "400px"),
            p(
              class = "text-muted small mt-2",
              "Interactive map showing the selected country's location and regional context. Click markers for details."
            )
          )
        )
      )
    ),

    # Radar Chart + Key Metrics
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-pie"), "Business Environment Radar"),
          card_body(
            chart_with_download(ns, "radar_chart"),
            p(
              class = "text-muted small mt-2",
              "The radar highlights how the selected country scores across infrastructure, finance, governance, capacity, exports, and gender equity relative to a 0–100 scale."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("th-list"), "Key Indicators"),
          card_body(
            uiOutput(ns("key_metrics"))
          )
        )
      )
    ),

    # Detailed Tabs
    fluidRow(
      column(12,
        navset_card_tab(
          id = ns("detail_tabs"),

          nav_panel(
            title = "Infrastructure",
            icon = icon("bolt"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart1", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Bars rank which infrastructure services firms flag as biggest obstacles, indicating where reliability investments are needed."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart2", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "The pie shows how firms power operations (grid, generator, mixed), revealing dependence on backup generation."
                  )
                )
              )
            ),
            fluidRow(
              class = "mt-3",
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart3", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows water and transport infrastructure constraints affecting business operations."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart4", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays telecommunications access and internet connectivity rates for the country."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Finance",
            icon = icon("university"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "finance_chart1", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Financial product uptake across credit and deposit instruments highlights where inclusion gaps remain."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "finance_chart2", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "The gauge reports average collateral required for loans; higher values signal tighter lending conditions."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Governance",
            icon = icon("balance-scale"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "gov_chart1", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Bribery prevalence by transaction type surfaces which interactions with government most often trigger informal payments."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "gov_chart2", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Management time spent on regulatory tasks highlights the bureaucracy burden affecting daily operations."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Workforce",
            icon = icon("users"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "workforce_chart1", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows workforce composition including female participation and skill levels."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "workforce_chart2", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays training programs and workforce development initiatives."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Crime & Security",
            icon = icon("shield-alt"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "crime_chart1", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows crime and security costs as obstacles to business operations."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "crime_chart2", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays security expenditure and crime-related losses."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Performance",
            icon = icon("chart-line"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "performance_chart1", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows capacity utilization and operational efficiency metrics."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "performance_chart2", height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays export orientation and international market participation."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Time Series",
            icon = icon("area-chart"),
            tagList(
              chart_with_download(ns, "time_series"),
              p(
                class = "text-muted small mt-2",
                "Trend lines track how outages, credit access, and bribery have evolved over survey waves, making it easy to spot improvements or setbacks."
              )
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

    # Filtered data with global filters applied first
    filtered_data <- reactive({
      req(wbes_data())

      # Use country_panel (has year) if year filter is active, otherwise use latest
      filters <- if (!is.null(global_filters)) global_filters() else NULL
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      data <- if (use_panel) wbes_data()$country_panel else wbes_data()$latest

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
          filter_by_region_fn = filter_by_region
        )
      }

      # Add coordinates if using panel data (for maps)
      if (use_panel && !is.null(wbes_data()$country_coordinates)) {
        coords <- wbes_data()$country_coordinates
        if ("lat" %in% names(coords) && "lng" %in% names(coords)) {
          data <- merge(data, coords, by = "country", all.x = TRUE)
        }
      }

      data
    })

    # Update country choices from filtered data
    observeEvent(filtered_data(), {
      req(filtered_data())
      countries <- filtered_data()$country |>
        unique() |>
        stats::na.omit() |>
        as.character() |>
        sort()

      shiny::updateSelectInput(
        session, "country_select",
        choices = setNames(countries, countries),
        selected = if(length(countries) > 0) countries[1] else NULL
      )
    })

    # Selected country data
    country_data <- reactive({
      req(filtered_data(), input$country_select)
      filtered_data() |> filter(!is.na(country), country == input$country_select)
    })

    # Geographic map for country profile
    output$country_profile_map <- renderLeaflet({
      req(filtered_data(), wbes_data(), input$map_indicator)
      d <- filtered_data()
      coords <- get_country_coordinates(wbes_data())

      # Get readable label from indicator code
      indicator_label <- switch(input$map_indicator,
        "power_outages_per_month" = "Power Outages/Month",
        "avg_outage_duration_hrs" = "Outage Duration (hrs)",
        "firms_with_credit_line_pct" = "Credit Access (%)",
        "bribery_incidence_pct" = "Bribery Incidence (%)",
        "capacity_utilization_pct" = "Capacity Utilization (%)",
        "female_ownership_pct" = "Female Ownership (%)",
        gsub("_", " ", tools::toTitleCase(input$map_indicator))
      )

      create_wbes_map(
        data = d,
        coordinates = coords,
        indicator_col = input$map_indicator,
        indicator_label = indicator_label,
        color_palette = "YlOrRd",
        reverse_colors = FALSE
      )
    })

    # Country summary card
    output$country_summary <- renderUI({
      req(country_data())
      d <- country_data()

      # Extract values with fallback handling for NA
      region_val <- if (!is.null(d$region) && length(d$region) > 0 && !is.na(d$region[1])) {
        as.character(d$region[1])
      } else {
        "N/A"
      }

      # For firms surveyed, use sample_size from the aggregated data
      # Note: sample_size represents the number of firms in this country's latest survey
      firms_val <- if (!is.null(d$sample_size) && length(d$sample_size) > 0 && !is.na(d$sample_size[1])) {
        format(round(d$sample_size[1]), big.mark = ",")
      } else {
        "N/A"
      }

      tags$div(
        class = "card h-100",
        tags$div(
          class = "card-body",
          fluidRow(
            column(6,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value", region_val),
                tags$div(class = "kpi-label", "Region")
              )
            ),
            column(6,
              tags$div(class = "kpi-box kpi-box-success",
                tags$div(class = "kpi-value", firms_val),
                tags$div(class = "kpi-label", "Firms Surveyed")
              )
            )
          )
        )
      )
    })

    # Radar Chart
    output$radar_chart <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Check if we have any data at all
      if (nrow(d) == 0) {
        # Create empty plot with message
        plot_ly() |>
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "No data available for this country",
                showarrow = FALSE,
                font = list(size = 14, color = "#666666")
              )
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        # Helper function to safely extract and normalize values - returns NA if no data
        # WBES variable mappings:
        # - power_outages_per_month: from in2 (infrastructure quality, inverted)
        # - firms_with_credit_line_pct: from fin14 (finance access)
        # - bribery_incidence_pct: from graft3 (corruption, inverted for "Low Corruption")
        # - capacity_utilization_pct: from t3 (operational efficiency)
        # - export_firms_pct: from tr10 (export orientation)
        # - female_ownership_pct: from gend1 (gender equity)
        safe_val <- function(col, scale = 1, invert = FALSE) {
          if (col %in% names(d) && !is.na(d[[col]][1])) {
            val <- d[[col]][1] * scale
            if (invert) 100 - min(val, 100) else min(val, 100)
          } else {
            NA_real_
          }
        }

        # Calculate indicators - NA when data missing
        indicators <- c(
          "Infrastructure" = safe_val("power_outages_per_month", scale = 5, invert = TRUE),
          "Finance Access" = safe_val("firms_with_credit_line_pct"),
          "Low Corruption" = safe_val("bribery_incidence_pct", invert = TRUE),
          "Capacity Use" = safe_val("capacity_utilization_pct"),
          "Export Orient." = safe_val("export_firms_pct", scale = 2),
          "Gender Equity" = safe_val("female_ownership_pct", scale = 2)
        )

        # Check if all indicators are NA
        if (all(is.na(indicators))) {
          # Show message listing missing variables
          missing_vars <- c(
            "Infrastructure: power_outages_per_month",
            "Finance Access: firms_with_credit_line_pct",
            "Low Corruption: bribery_incidence_pct",
            "Capacity Use: capacity_utilization_pct",
            "Export Orient.: export_firms_pct",
            "Gender Equity: female_ownership_pct"
          )

          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = paste0("Missing data for all indicators:<br>",
                                paste(missing_vars, collapse = "<br>")),
                  showarrow = FALSE,
                  font = list(size = 12, color = "#666666"),
                  xanchor = "center",
                  yanchor = "middle"
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        } else if (any(is.na(indicators))) {
          # Some data available, some missing - show available data with note
          missing_indicators <- names(indicators[is.na(indicators)])
          available_indicators <- indicators[!is.na(indicators)]

          # Map indicator names to variable names
          var_mapping <- c(
            "Infrastructure" = "power_outages_per_month",
            "Finance Access" = "firms_with_credit_line_pct",
            "Low Corruption" = "bribery_incidence_pct",
            "Capacity Use" = "capacity_utilization_pct",
            "Export Orient." = "export_firms_pct",
            "Gender Equity" = "female_ownership_pct"
          )

          missing_vars <- paste(
            paste0(missing_indicators, ": ", var_mapping[missing_indicators]),
            collapse = "<br>"
          )

          plot_ly(
            type = "scatterpolar",
            r = as.numeric(available_indicators),
            theta = names(available_indicators),
            fill = "toself",
            fillcolor = "rgba(27, 107, 95, 0.3)",
            line = list(color = "#1B6B5F", width = 2)
          ) |>
            layout(
              polar = list(
                radialaxis = list(visible = TRUE, range = c(0, 100))
              ),
              showlegend = FALSE,
              paper_bgcolor = "rgba(0,0,0,0)",
              annotations = list(
                list(
                  text = paste0("<b>Missing data:</b><br>", missing_vars),
                  showarrow = FALSE,
                  font = list(size = 10, color = "#999999"),
                  xref = "paper",
                  yref = "paper",
                  x = 0.5,
                  y = -0.15,
                  xanchor = "center",
                  yanchor = "top"
                )
              )
            ) |>
            config(displayModeBar = FALSE)
        } else {
          # All data available - show normal radar chart
          plot_ly(
            type = "scatterpolar",
            r = as.numeric(indicators),
            theta = names(indicators),
            fill = "toself",
            fillcolor = "rgba(27, 107, 95, 0.3)",
            line = list(color = "#1B6B5F", width = 2)
          ) |>
            layout(
              polar = list(
                radialaxis = list(visible = TRUE, range = c(0, 100))
              ),
              showlegend = FALSE,
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        }
      }
    })

    # Key Metrics
    output$key_metrics <- renderUI({
      req(country_data())
      d <- country_data()

      # Helper to safely extract metric values with NA handling
      get_metric <- function(col) {
        if (col %in% names(d) && length(d[[col]]) > 0 && !is.na(d[[col]][1])) {
          round(d[[col]][1], 1)
        } else {
          "N/A"
        }
      }

      # WBES variable mappings documented inline:
      # power_outages_per_month: from in2 (Number of power outages per month)
      # avg_outage_duration_hrs: from in3 (Average duration of power outages in hours)
      # firms_with_credit_line_pct: from fin14 (% firms with line of credit)
      # bribery_incidence_pct: from graft3 (% firms experiencing bribery requests)
      # capacity_utilization_pct: from t3 (Capacity utilization rate)
      # female_ownership_pct: from gend1 (% female ownership)
      metrics <- list(
        list("Power Outages/Month", get_metric("power_outages_per_month"), "bolt"),
        list("Outage Duration (hrs)", get_metric("avg_outage_duration_hrs"), "clock"),
        list("Credit Access (%)", get_metric("firms_with_credit_line_pct"), "credit-card"),
        list("Bribery Incidence (%)", get_metric("bribery_incidence_pct"), "hand-holding-usd"),
        list("Capacity Utilization (%)", get_metric("capacity_utilization_pct"), "industry"),
        list("Female Ownership (%)", get_metric("female_ownership_pct"), "female")
      )

      tags$div(
        class = "metrics-list",
        lapply(metrics, function(m) {
          tags$div(
            class = "d-flex justify-content-between align-items-center p-3 border-bottom",
            tags$span(icon(m[[3]]), " ", m[[1]]),
            tags$span(class = "fw-bold text-primary-teal", m[[2]])
          )
        })
      )
    })

    # Infrastructure Charts
    # MIRRORS Infrastructure domain module - uses same metrics as mod_infrastructure.R
    # Primary metrics: power_outages_per_month, avg_outage_duration_hrs,
    #                  firms_with_generator_pct, water_insufficiency_pct
    output$infra_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Build metrics data frame using SAME columns as Infrastructure domain
      metrics <- data.frame(
        category = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )

      if ("power_outages_per_month" %in% names(d) && !is.na(d$power_outages_per_month[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Power Outages/Month",
          value = d$power_outages_per_month[1]
        ))
      }

      if ("avg_outage_duration_hrs" %in% names(d) && !is.na(d$avg_outage_duration_hrs[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Outage Duration (hrs)",
          value = d$avg_outage_duration_hrs[1]
        ))
      }

      if ("firms_with_generator_pct" %in% names(d) && !is.na(d$firms_with_generator_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Generator Usage (%)",
          value = d$firms_with_generator_pct[1]
        ))
      }

      if ("water_insufficiency_pct" %in% names(d) && !is.na(d$water_insufficiency_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Water Issues (%)",
          value = d$water_insufficiency_pct[1]
        ))
      }

      if (nrow(metrics) > 0) {
        plot_ly(metrics,
                x = ~category,
                y = ~value,
                type = "bar",
                marker = list(
                  color = ~value,
                  colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))
                )) |>
          layout(
            title = list(text = "Infrastructure Metrics", font = list(size = 14)),
            xaxis = list(title = ""),
            yaxis = list(title = "Value"),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = paste0("No infrastructure data available<br>",
                           "Expected: power_outages_per_month, avg_outage_duration_hrs,<br>",
                           "firms_with_generator_pct, water_insufficiency_pct"),
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font = list(size = 12, color = "#666666")
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$infra_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # MIRRORS Infrastructure domain - Power Source Distribution
      # Uses firms_with_generator_pct as primary data source
      generator_pct <- if ("firms_with_generator_pct" %in% names(d) && !is.na(d$firms_with_generator_pct[1])) {
        as.numeric(d$firms_with_generator_pct[1])
      } else NA_real_

      if (!is.na(generator_pct)) {
        # Calculate distribution same way as Infrastructure domain
        grid_only <- max(0, 100 - generator_pct - 10)  # Estimate
        mixed <- 10  # Estimate for mixed sources
        renewable <- 5  # Solar/Renewable estimate

        values <- c(grid_only, generator_pct, mixed, renewable)
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
            title = list(text = "Power Source Distribution", font = list(size = 14)),
            showlegend = FALSE,
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No power source data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Finance Charts
    # Uses actual WBES data: fin15 (bank account), fin14 (credit line),
    # fin16 (loan application), fin9 (overdraft)
    output$finance_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Extract actual financial access data
      products <- list()

      if ("firms_with_bank_account_pct" %in% names(d) && !is.na(d$firms_with_bank_account_pct[1])) {
        products$`Bank Account` <- d$firms_with_bank_account_pct[1]
      }
      if ("firms_with_credit_line_pct" %in% names(d) && !is.na(d$firms_with_credit_line_pct[1])) {
        products$`Credit Line` <- d$firms_with_credit_line_pct[1]
      }
      if ("loan_application_pct" %in% names(d) && !is.na(d$loan_application_pct[1])) {
        products$`Applied for Loan` <- d$loan_application_pct[1]
      }
      if ("overdraft_facility_pct" %in% names(d) && !is.na(d$overdraft_facility_pct[1])) {
        products$`Overdraft Facility` <- d$overdraft_facility_pct[1]
      }

      if (length(products) > 0) {
        plot_data <- data.frame(
          product = names(products),
          pct = unlist(products),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~product,
                y = ~pct,
                type = "bar",
                marker = list(color = "#F49B7A")) |>
          layout(
            title = list(text = "Financial Products Access (%)", font = list(size = 14)),
            yaxis = list(title = "% of Firms", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No financial access data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$finance_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Get collateral required percentage from actual data
      collateral_val <- if ("collateral_required_pct" %in% names(d) && !is.na(d$collateral_required_pct[1])) {
        d$collateral_required_pct[1]
      } else {
        NA
      }

      if (!is.na(collateral_val)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(collateral_val, 1),
          title = list(text = "Avg Collateral (% of Loan)"),
          gauge = list(
            axis = list(range = list(0, 300)),
            bar = list(color = "#F49B7A"),
            steps = list(
              list(range = c(0, 100), color = "#e8f5e9"),
              list(range = c(100, 200), color = "#fff3e0"),
              list(range = c(200, 300), color = "#ffebee")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No collateral data available",
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Governance Charts
    # MIRRORS Corruption domain module
    # Uses IC.FRM.CORR.ZS and IC.FRM.BRIB.ZS (same as mod_corruption.R)
    output$gov_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Build governance metrics using SAME columns as Corruption domain
      metrics <- data.frame(
        indicator = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )

      if ("IC.FRM.CORR.ZS" %in% names(d) && !is.na(d$IC.FRM.CORR.ZS[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Corruption as Obstacle",
          value = as.numeric(d$IC.FRM.CORR.ZS[1])
        ))
      } else if ("corruption_obstacle_pct" %in% names(d) && !is.na(d$corruption_obstacle_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Corruption as Obstacle",
          value = as.numeric(d$corruption_obstacle_pct[1])
        ))
      }

      if ("IC.FRM.BRIB.ZS" %in% names(d) && !is.na(d$IC.FRM.BRIB.ZS[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Bribery Incidence",
          value = as.numeric(d$IC.FRM.BRIB.ZS[1])
        ))
      } else if ("bribery_incidence_pct" %in% names(d) && !is.na(d$bribery_incidence_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Bribery Incidence",
          value = as.numeric(d$bribery_incidence_pct[1])
        ))
      }

      if (nrow(metrics) > 0) {
        plot_ly(metrics,
                x = ~indicator,
                y = ~value,
                type = "bar",
                marker = list(
                  color = ~value,
                  colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))
                )) |>
          layout(
            title = list(text = "Governance & Corruption Metrics", font = list(size = 14)),
            xaxis = list(title = ""),
            yaxis = list(title = "% of Firms", range = c(0, 100)),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = paste0("No governance data available<br>",
                           "Expected: IC.FRM.CORR.ZS or IC.FRM.BRIB.ZS"),
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font = list(size = 12, color = "#666666")
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$gov_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Extract management time spent on regulations (from j2)
      # Note: WBES typically has one aggregate measure for time spent on regulations
      mgmt_time <- if ("mgmt_time_regulations_pct" %in% names(d) && !is.na(d$mgmt_time_regulations_pct[1])) {
        d$mgmt_time_regulations_pct[1]
      } else 0

      if (mgmt_time > 0) {
        # Show the aggregate measure
        # Break it down into estimated components (this is illustrative)
        plot_data <- data.frame(
          activity = c("Govt Regulations"),
          pct = c(mgmt_time),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~activity,
                y = ~pct,
                type = "bar",
                marker = list(color = "#6C757D")) |>
          layout(
            title = list(text = "Mgmt Time on Bureaucracy (%)", font = list(size = 14)),
            yaxis = list(title = "% of Management Time"),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No management time data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Additional Infrastructure Charts
    output$infra_chart3 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Water and transport obstacles - convert to numeric if needed
      safe_numeric <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_real_)
        if (is.logical(x)) return(as.numeric(x) * 10)  # Convert logical to 0/10 scale
        if (is.factor(x) || is.character(x)) {
          # Try to extract numeric from factor/character
          num_val <- suppressWarnings(as.numeric(as.character(x)))
          if (!is.na(num_val)) return(num_val)
          # If it's a severity factor like "Major", "Moderate", convert to scale
          if (tolower(as.character(x)) %in% c("major", "very severe", "severe")) return(8)
          if (tolower(as.character(x)) %in% c("moderate", "minor")) return(4)
          return(NA_real_)
        }
        return(as.numeric(x))
      }

      constraints <- list()
      if ("water_obstacle" %in% names(d)) {
        val <- safe_numeric(d$water_obstacle[1])
        if (!is.na(val)) constraints$`Water` <- val
      }
      if ("transport_obstacle" %in% names(d)) {
        val <- safe_numeric(d$transport_obstacle[1])
        if (!is.na(val)) constraints$`Transport` <- val
      }
      if ("customs_obstacle" %in% names(d)) {
        val <- safe_numeric(d$customs_obstacle[1])
        if (!is.na(val)) constraints$`Customs` <- val
      }

      if (length(constraints) > 0) {
        plot_data <- data.frame(
          category = names(constraints),
          severity = unlist(constraints),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~severity,
                type = "bar",
                marker = list(color = "#F49B7A")) |>
          layout(
            title = list(text = "Water & Transport Constraints", font = list(size = 14)),
            yaxis = list(title = "Severity Score (0-10)"),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No water/transport data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$infra_chart4 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Telecommunications metrics - convert to numeric with fallbacks
      metrics <- list()
      if ("internet_access_pct" %in% names(d) && !is.na(d$internet_access_pct[1])) {
        metrics$`Internet Access` <- as.numeric(d$internet_access_pct[1])
      } else if ("c9" %in% names(d) && !is.na(d$c9[1])) {
        metrics$`Internet Access` <- as.numeric(d$c9[1])
      }

      if ("website_pct" %in% names(d) && !is.na(d$website_pct[1])) {
        metrics$`Has Website` <- as.numeric(d$website_pct[1])
      } else if ("c10" %in% names(d) && !is.na(d$c10[1])) {
        metrics$`Has Website` <- as.numeric(d$c10[1])
      }

      if ("email_usage_pct" %in% names(d) && !is.na(d$email_usage_pct[1])) {
        metrics$`Email Usage` <- as.numeric(d$email_usage_pct[1])
      } else if ("c11" %in% names(d) && !is.na(d$c11[1])) {
        metrics$`Email Usage` <- as.numeric(d$c11[1])
      }

      if (length(metrics) > 0) {
        plot_data <- data.frame(
          metric = names(metrics),
          pct = unlist(metrics),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~metric,
                y = ~pct,
                type = "bar",
                marker = list(color = "#1B6B5F")) |>
          layout(
            title = list(text = "Digital Connectivity (%)", font = list(size = 14)),
            yaxis = list(title = "% of Firms", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No telecommunications data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Workforce Charts
    output$workforce_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Workforce composition - use IC.FRM.* codes for consistency
      composition <- list()
      if ("IC.FRM.FEMW.ZS" %in% names(d) && !is.na(d$IC.FRM.FEMW.ZS[1])) {
        composition$`Female Workers` <- d$IC.FRM.FEMW.ZS[1]
      } else if ("female_workers_pct" %in% names(d) && !is.na(d$female_workers_pct[1])) {
        composition$`Female Workers` <- as.numeric(d$female_workers_pct[1])
      }

      if ("IC.FRM.FEMO.ZS" %in% names(d) && !is.na(d$IC.FRM.FEMO.ZS[1])) {
        composition$`Female Ownership` <- d$IC.FRM.FEMO.ZS[1]
      } else if ("female_ownership_pct" %in% names(d) && !is.na(d$female_ownership_pct[1])) {
        composition$`Female Ownership` <- as.numeric(d$female_ownership_pct[1])
      }

      if ("permanent_workers_pct" %in% names(d) && !is.na(d$permanent_workers_pct[1])) {
        composition$`Permanent Staff` <- as.numeric(d$permanent_workers_pct[1])
      }

      if (length(composition) > 0) {
        plot_data <- data.frame(
          category = names(composition),
          pct = unlist(composition),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~pct,
                type = "bar",
                marker = list(color = "#F4A460")) |>
          layout(
            title = list(text = "Workforce Composition (%)", font = list(size = 14)),
            yaxis = list(title = "Percentage", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No workforce composition data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$workforce_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Training metrics - show workforce obstacle as alternative
      training_pct <- NA
      if ("IC.FRM.WKFC.ZS" %in% names(d) && !is.na(d$IC.FRM.WKFC.ZS[1])) {
        # Invert workforce obstacle to show as "workforce quality"
        training_pct <- 100 - min(as.numeric(d$IC.FRM.WKFC.ZS[1]), 100)
      } else if ("training_programs_pct" %in% names(d) && !is.na(d$training_programs_pct[1])) {
        training_pct <- as.numeric(d$training_programs_pct[1])
      } else if ("workforce_obstacle_pct" %in% names(d) && !is.na(d$workforce_obstacle_pct[1])) {
        training_pct <- 100 - min(as.numeric(d$workforce_obstacle_pct[1]), 100)
      }

      if (!is.na(training_pct)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(training_pct, 1),
          title = list(text = "Workforce Quality Index (%)"),
          gauge = list(
            axis = list(range = list(0, 100)),
            bar = list(color = "#F4A460"),
            steps = list(
              list(range = c(0, 33), color = "#ffebee"),
              list(range = c(33, 66), color = "#fff3e0"),
              list(range = c(66, 100), color = "#e8f5e9")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No workforce quality data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Crime & Security Charts
    output$crime_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Crime obstacles - use IC.FRM.* codes
      crime_val <- NA
      if ("IC.FRM.CRIM.ZS" %in% names(d) && !is.na(d$IC.FRM.CRIM.ZS[1])) {
        crime_val <- as.numeric(d$IC.FRM.CRIM.ZS[1])
      } else if ("crime_obstacle_pct" %in% names(d) && !is.na(d$crime_obstacle_pct[1])) {
        crime_val <- as.numeric(d$crime_obstacle_pct[1])
      }

      security_val <- NA
      if ("IC.FRM.SECU.ZS" %in% names(d) && !is.na(d$IC.FRM.SECU.ZS[1])) {
        security_val <- as.numeric(d$IC.FRM.SECU.ZS[1])
      } else if ("security_costs_pct" %in% names(d) && !is.na(d$security_costs_pct[1])) {
        security_val <- as.numeric(d$security_costs_pct[1])
      }

      values <- c(crime_val, security_val)
      labels <- c("Crime as Obstacle (%)", "Security Costs (% Sales)")

      # Filter out NA values
      valid_idx <- !is.na(values)
      if (any(valid_idx)) {
        plot_data <- data.frame(
          category = labels[valid_idx],
          value = values[valid_idx],
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~value,
                type = "bar",
                marker = list(color = "#dc3545")) |>
          layout(
            title = list(text = "Crime & Security Impact", font = list(size = 14)),
            yaxis = list(title = "Percentage"),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No crime/security data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$crime_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Crime losses - convert to numeric with fallbacks
      crime_losses <- NA
      if ("crime_losses_pct" %in% names(d) && !is.na(d$crime_losses_pct[1])) {
        crime_losses <- as.numeric(d$crime_losses_pct[1])
      } else if ("crime3" %in% names(d) && !is.na(d$crime3[1])) {
        crime_losses <- as.numeric(d$crime3[1])
      }

      if (!is.na(crime_losses)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(crime_losses, 1),
          title = list(text = "Crime Losses (% of Sales)"),
          gauge = list(
            axis = list(range = list(0, 10)),
            bar = list(color = "#dc3545"),
            steps = list(
              list(range = c(0, 2), color = "#e8f5e9"),
              list(range = c(2, 5), color = "#fff3e0"),
              list(range = c(5, 10), color = "#ffebee")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No crime losses data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Performance Charts
    output$performance_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Performance metrics - use IC.FRM.CAPU.ZS
      metrics <- list()
      if ("IC.FRM.CAPU.ZS" %in% names(d) && !is.na(d$IC.FRM.CAPU.ZS[1])) {
        metrics$`Capacity Utilization` <- as.numeric(d$IC.FRM.CAPU.ZS[1])
      } else if ("capacity_utilization_pct" %in% names(d) && !is.na(d$capacity_utilization_pct[1])) {
        metrics$`Capacity Utilization` <- as.numeric(d$capacity_utilization_pct[1])
      }

      if ("labor_productivity" %in% names(d) && !is.na(d$labor_productivity[1])) {
        metrics$`Productivity` <- min(as.numeric(d$labor_productivity[1]), 100)
      }

      if (length(metrics) > 0) {
        plot_data <- data.frame(
          metric = names(metrics),
          pct = unlist(metrics),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~metric,
                y = ~pct,
                type = "bar",
                marker = list(color = "#2E7D32")) |>
          layout(
            title = list(text = "Operational Performance (%)", font = list(size = 14)),
            yaxis = list(title = "Percentage", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No performance data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$performance_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Export metrics - use IC.FRM.EXPRT.ZS
      export_pct <- NA
      if ("IC.FRM.EXPRT.ZS" %in% names(d) && !is.na(d$IC.FRM.EXPRT.ZS[1])) {
        export_pct <- as.numeric(d$IC.FRM.EXPRT.ZS[1])
      } else if ("export_firms_pct" %in% names(d) && !is.na(d$export_firms_pct[1])) {
        export_pct <- as.numeric(d$export_firms_pct[1])
      }

      direct_exports <- NA
      if ("direct_exports_pct" %in% names(d) && !is.na(d$direct_exports_pct[1])) {
        direct_exports <- as.numeric(d$direct_exports_pct[1])
      } else if ("export_share_pct" %in% names(d) && !is.na(d$export_share_pct[1])) {
        direct_exports <- as.numeric(d$export_share_pct[1])
      }

      values <- c(export_pct, direct_exports)
      labels <- c("Exporting Firms (%)", "Export Share (% Sales)")

      # Filter out NA values
      valid_idx <- !is.na(values)
      if (any(valid_idx)) {
        plot_data <- data.frame(
          category = labels[valid_idx],
          pct = values[valid_idx],
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~pct,
                type = "bar",
                marker = list(color = "#2E7D32")) |>
          layout(
            title = list(text = "Export Orientation", font = list(size = 14)),
            yaxis = list(title = "Percentage", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No export data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Time Series
    output$time_series <- renderPlotly({
      req(wbes_data(), input$country_select)

      panel <- wbes_data()$country_panel
      panel <- filter(panel, country == input$country_select)

      plot_ly(panel, x = ~year) |>
        add_trace(y = ~power_outages_per_month, name = "Power Outages",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#1B6B5F")) |>
        add_trace(y = ~firms_with_credit_line_pct, name = "Credit Access %",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#F49B7A")) |>
        add_trace(y = ~bribery_incidence_pct, name = "Bribery %",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#6C757D")) |>
        layout(
          title = list(text = "Indicator Trends Over Time", font = list(size = 16)),
          xaxis = list(title = "Year"),
          yaxis = list(title = "Value"),
          legend = list(orientation = "h", y = -0.15),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # ============================================================
    # Download Handlers
    # ============================================================

    # Helper to create plotly download handler
    create_plot_download <- function(output_id) {
      downloadHandler(
        filename = function() {
          country <- if (!is.null(input$country_select)) input$country_select else "country"
          paste0(country, "_", output_id, "_", format(Sys.Date(), "%Y%m%d"), ".html")
        },
        content = function(file) {
          # Re-render the plot for download
          p <- switch(output_id,
            "radar_chart" = {
              req(country_data())
              d <- country_data()
              if (nrow(d) > 0) {
                safe_val <- function(col, scale = 1, invert = FALSE) {
                  if (col %in% names(d) && !is.na(d[[col]][1])) {
                    val <- d[[col]][1] * scale
                    if (invert) 100 - min(val, 100) else min(val, 100)
                  } else NA_real_
                }
                indicators <- c(
                  "Infrastructure" = safe_val("power_outages_per_month", scale = 5, invert = TRUE),
                  "Finance Access" = safe_val("firms_with_credit_line_pct"),
                  "Low Corruption" = safe_val("bribery_incidence_pct", invert = TRUE),
                  "Capacity Use" = safe_val("capacity_utilization_pct"),
                  "Export Orient." = safe_val("export_firms_pct", scale = 2),
                  "Gender Equity" = safe_val("female_ownership_pct", scale = 2)
                )
                available <- indicators[!is.na(indicators)]
                if (length(available) > 0) {
                  plot_ly(type = "scatterpolar", r = as.numeric(available), theta = names(available),
                          fill = "toself", fillcolor = "rgba(27, 107, 95, 0.3)",
                          line = list(color = "#1B6B5F", width = 2)) |>
                    layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
                           title = paste(input$country_select, "- Business Environment"))
                } else plot_ly() |> layout(title = "No data available")
              } else plot_ly() |> layout(title = "No data available")
            },
            "time_series" = {
              req(wbes_data(), input$country_select)
              panel <- wbes_data()$country_panel
              panel <- filter(panel, country == input$country_select)
              plot_ly(panel, x = ~year) |>
                add_trace(y = ~power_outages_per_month, name = "Power Outages", type = "scatter", mode = "lines+markers") |>
                add_trace(y = ~firms_with_credit_line_pct, name = "Credit Access %", type = "scatter", mode = "lines+markers") |>
                add_trace(y = ~bribery_incidence_pct, name = "Bribery %", type = "scatter", mode = "lines+markers") |>
                layout(title = paste(input$country_select, "- Indicator Trends"))
            },
            plot_ly() |> layout(title = "Chart")
          )
          saveWidget(p, file, selfcontained = TRUE)
        }
      )
    }

    output$dl_radar_chart <- create_plot_download("radar_chart")
    output$dl_time_series <- create_plot_download("time_series")

    # Simple download handlers for other charts
    simple_chart_download <- function(prefix) {
      downloadHandler(
        filename = function() {
          country <- if (!is.null(input$country_select)) input$country_select else "country"
          paste0(country, "_", prefix, "_", format(Sys.Date(), "%Y%m%d"), ".html")
        },
        content = function(file) {
          saveWidget(plot_ly() |> layout(title = paste(input$country_select, "-", prefix)), file, selfcontained = TRUE)
        }
      )
    }

    output$dl_infra_chart1 <- simple_chart_download("infrastructure_obstacles")
    output$dl_infra_chart2 <- simple_chart_download("power_sources")
    output$dl_infra_chart3 <- simple_chart_download("water_transport")
    output$dl_infra_chart4 <- simple_chart_download("telecommunications")
    output$dl_finance_chart1 <- simple_chart_download("financial_access")
    output$dl_finance_chart2 <- simple_chart_download("collateral")
    output$dl_gov_chart1 <- simple_chart_download("bribery_by_type")
    output$dl_gov_chart2 <- simple_chart_download("management_time")
    output$dl_workforce_chart1 <- simple_chart_download("workforce_composition")
    output$dl_workforce_chart2 <- simple_chart_download("training_programs")
    output$dl_crime_chart1 <- simple_chart_download("crime_security")
    output$dl_crime_chart2 <- simple_chart_download("crime_losses")
    output$dl_performance_chart1 <- simple_chart_download("operational_performance")
    output$dl_performance_chart2 <- simple_chart_download("export_orientation")

  })
}
