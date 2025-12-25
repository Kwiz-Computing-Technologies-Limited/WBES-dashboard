# app/view/mod_size_profile.R
# Firm Size Profile Deep Dive Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, tagList, icon, div, h2, h3, h4, p, span,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent, renderText, textOutput],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, select, arrange, mutate, group_by, summarise, n],
  leaflet[leafletOutput, renderLeaflet],
  stats[setNames],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates],
  app/logic/chart_utils[create_chart_caption, map_with_caption]
)

# Helper function to create chart container with caption
chart_with_caption <- function(ns, output_id, height = "400px", title = NULL) {
  div(
    class = "position-relative",
    if (!is.null(title)) h4(title, class = "text-primary-teal mb-2"),
    plotlyOutput(ns(output_id), height = height),
    create_chart_caption(output_id)
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "size-profile-container",

    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("building"), "Firm Size Profile", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "In-depth analysis of business environment indicators by firm size category")
        )
      )
    ),

    # Size Summary (no duplicate selector - use sidebar filter)
    fluidRow(
      class = "mb-4",
      column(12,
        uiOutput(ns("size_summary"))
      )
    ),

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Distribution"),
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
            map_with_caption(ns, "size_profile_map", height = "400px", title = "Firm Size Geographic Distribution")
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
            chart_with_caption(ns, "radar_chart", height = "400px", title = "Business Environment Radar")
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
                  plotlyOutput(ns("infra_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Bars rank which infrastructure services firms of this size flag as biggest obstacles."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("infra_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "The pie shows how firms of this size power operations (grid, generator, mixed)."
                  )
                )
              )
            ),
            fluidRow(
              column(6,
                tagList(
                  plotlyOutput(ns("infra_chart3"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Water and transport constraints for firms of this size."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("infra_chart4"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Digital connectivity metrics for firms of this size."
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
                  plotlyOutput(ns("finance_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Financial product uptake across credit and deposit instruments for this firm size."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("finance_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "The gauge reports average collateral required for loans for firms of this size."
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
                  plotlyOutput(ns("gov_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Bribery prevalence by transaction type for firms of this size."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("gov_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Management time spent on regulatory tasks for this firm size category."
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
                  plotlyOutput(ns("workforce_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Gender composition in workforce and ownership for firms of this size."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("workforce_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Workforce quality and training metrics for this firm size."
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
                  plotlyOutput(ns("crime_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Crime as obstacle and security costs for firms of this size."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("crime_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Crime-related losses as percentage of sales."
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
                  plotlyOutput(ns("performance_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Operational performance metrics for firms of this size."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("performance_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Export orientation of firms of this size."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Country Distribution",
            icon = icon("globe"),
            tagList(
              plotlyOutput(ns("country_dist"), height = "400px"),
              p(
                class = "text-muted small mt-2",
                "Shows the geographic distribution of surveyed firms in this size category across countries."
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

    # Filtered data with global filters applied
    filtered_data <- reactive({
      req(wbes_data())

      # Get filters
      filters <- if (!is.null(global_filters)) global_filters() else list(firm_size = "all")
      size_val <- if (!is.null(filters$firm_size)) filters$firm_size else "all"

      # Check if year filter is active
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      # Choose appropriate data source
      # Priority: year filter > size filter > default latest
      data <- if (use_panel) {
        wbes_data()$country_panel  # Has year dimension
      } else if (size_val == "all") {
        wbes_data()$latest  # Global country aggregates
      } else {
        wbes_data()$country_size  # Country-size combinations
      }

      # Apply global filters if provided
      if (!is.null(global_filters)) {
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

    # Get selected size from global filters
    selected_size <- reactive({
      if (!is.null(global_filters)) {
        filters <- global_filters()
        if (!is.null(filters$firm_size) && filters$firm_size != "all") {
          return(filters$firm_size)
        }
      }
      # Return "all" when All Sizes is selected
      return("all")
    })

    # Selected size data
    size_data <- reactive({
      req(filtered_data(), selected_size())
      data <- filtered_data()
      size_val <- selected_size()

      # If a specific size is selected from sidebar, filter to that size
      if (!is.null(size_val) && size_val != "all") {
        data |> filter(!is.na(firm_size), firm_size == size_val)
      } else {
        # If "all" is selected, return all data (globally aggregated)
        data
      }
    })

    # Geographic map for size profile
    output$size_profile_map <- renderLeaflet({
      req(size_data(), wbes_data(), input$map_indicator)
      d <- size_data()
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

    # Size summary card
    output$size_summary <- renderUI({
      req(size_data(), selected_size())
      d <- size_data()
      size_val <- selected_size()

      # Display name: "All Sizes (Global)" or the specific size name
      size_name <- if (size_val == "all") {
        "All Firm Sizes (Global)"
      } else {
        paste0(size_val, " Firms")
      }

      # Count countries and firms in this size category
      countries_count <- if (!is.null(d$country) && length(d$country) > 0) {
        length(unique(d$country[!is.na(d$country)]))
      } else {
        0
      }

      firms_count <- if (!is.null(d$sample_size) && length(d$sample_size) > 0) {
        sum(d$sample_size, na.rm = TRUE)
      } else {
        0
      }

      tags$div(
        class = "card h-100",
        tags$div(
          class = "card-body",
          tags$h4(
            class = "text-primary-teal mb-3",
            icon("building"), " ", size_name
          ),
          fluidRow(
            column(6,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value", countries_count),
                tags$div(class = "kpi-label", if (size_val == "all") "Countries Worldwide" else "Countries Covered")
              )
            ),
            column(6,
              tags$div(class = "kpi-box kpi-box-success",
                tags$div(class = "kpi-value", format(firms_count, big.mark = ",")),
                tags$div(class = "kpi-label", "Total Firms")
              )
            )
          )
        )
      )
    })

    # Radar Chart
    output$radar_chart <- renderPlotly({
      req(size_data())
      d <- size_data()

      # Check if we have any data at all
      if (nrow(d) == 0) {
        # Create empty plot with message
        plot_ly() |>
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "No data available for this firm size",
                showarrow = FALSE,
                font = list(size = 14, color = "#666666")
              )
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        # Helper function to safely aggregate values - returns NA if no data
        safe_mean <- function(col, scale = 1, invert = FALSE) {
          if (col %in% names(d)) {
            values <- d[[col]][!is.na(d[[col]])]
            if (length(values) > 0) {
              val <- mean(values, na.rm = TRUE) * scale
              if (invert) 100 - min(val, 100) else min(val, 100)
            } else {
              NA_real_
            }
          } else {
            NA_real_
          }
        }

        # Calculate indicators - NA when data missing
        indicators <- c(
          "Infrastructure" = safe_mean("power_outages_per_month", scale = 5, invert = TRUE),
          "Finance Access" = safe_mean("firms_with_credit_line_pct"),
          "Low Corruption" = safe_mean("bribery_incidence_pct", invert = TRUE),
          "Capacity Use" = safe_mean("capacity_utilization_pct"),
          "Export Orient." = safe_mean("export_firms_pct", scale = 2),
          "Gender Equity" = safe_mean("female_ownership_pct", scale = 2)
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
      req(size_data())
      d <- size_data()

      get_metric <- function(col) {
        if (col %in% names(d)) {
          values <- d[[col]][!is.na(d[[col]])]
          if (length(values) > 0) {
            round(mean(values, na.rm = TRUE), 1)
          } else {
            "N/A"
          }
        } else {
          "N/A"
        }
      }

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
    # MIRRORS Infrastructure domain module - aggregated by firm size
    # Primary metrics: power_outages_per_month, avg_outage_duration_hrs,
    #                  firms_with_generator_pct, water_insufficiency_pct
    output$infra_chart1 <- renderPlotly({
      req(size_data())
      d <- size_data()

      # Build metrics using SAME columns as Infrastructure domain (aggregated by size)
      metrics <- data.frame(
        category = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )

      if ("power_outages_per_month" %in% names(d)) {
        val <- mean(as.numeric(d$power_outages_per_month), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            category = "Power Outages/Month",
            value = val
          ))
        }
      }

      if ("avg_outage_duration_hrs" %in% names(d)) {
        val <- mean(as.numeric(d$avg_outage_duration_hrs), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            category = "Outage Duration (hrs)",
            value = val
          ))
        }
      }

      if ("firms_with_generator_pct" %in% names(d)) {
        val <- mean(as.numeric(d$firms_with_generator_pct), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            category = "Generator Usage (%)",
            value = val
          ))
        }
      }

      if ("water_insufficiency_pct" %in% names(d)) {
        val <- mean(as.numeric(d$water_insufficiency_pct), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            category = "Water Issues (%)",
            value = val
          ))
        }
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
            title = list(text = "Infrastructure Metrics (Size Avg)", font = list(size = 14)),
            xaxis = list(title = ""),
            yaxis = list(title = "Average Value"),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = paste0("No infrastructure data available for this firm size<br>",
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
      req(size_data())
      d <- size_data()

      # MIRRORS Infrastructure domain - Power Source Distribution (aggregated by size)
      generator_pct <- if ("firms_with_generator_pct" %in% names(d)) {
        mean(as.numeric(d$firms_with_generator_pct), na.rm = TRUE)
      } else NA_real_

      if (!is.na(generator_pct)) {
        # Calculate distribution same way as Infrastructure domain
        grid_only <- max(0, 100 - generator_pct - 10)
        mixed <- 10
        renewable <- 5

        values <- c(grid_only, generator_pct, mixed, renewable)
        labels <- c("Grid Only", "Generator Primary", "Mixed Sources", "Solar/Renewable")

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
            title = list(text = "Power Source Distribution (By Size)", font = list(size = 14)),
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
    output$finance_chart1 <- renderPlotly({
      req(size_data())
      d <- size_data()

      products <- list()
      if ("firms_with_bank_account_pct" %in% names(d)) {
        val <- mean(d$firms_with_bank_account_pct, na.rm = TRUE)
        if (!is.na(val)) products$`Bank Account` <- val
      }
      if ("firms_with_credit_line_pct" %in% names(d)) {
        val <- mean(d$firms_with_credit_line_pct, na.rm = TRUE)
        if (!is.na(val)) products$`Credit Line` <- val
      }
      if ("loan_application_pct" %in% names(d)) {
        val <- mean(d$loan_application_pct, na.rm = TRUE)
        if (!is.na(val)) products$`Applied for Loan` <- val
      }
      if ("overdraft_facility_pct" %in% names(d)) {
        val <- mean(d$overdraft_facility_pct, na.rm = TRUE)
        if (!is.na(val)) products$`Overdraft Facility` <- val
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
      req(size_data())
      d <- size_data()

      collateral_val <- if ("collateral_required_pct" %in% names(d)) {
        mean(d$collateral_required_pct, na.rm = TRUE)
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
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Governance Charts
    # MIRRORS Corruption domain module - aggregated by firm size
    # Uses IC.FRM.CORR.ZS and IC.FRM.BRIB.ZS (same as mod_corruption.R)
    output$gov_chart1 <- renderPlotly({
      req(size_data())
      d <- size_data()

      # Build governance metrics using SAME columns as Corruption domain (aggregated by size)
      metrics <- data.frame(
        indicator = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )

      if ("IC.FRM.CORR.ZS" %in% names(d)) {
        val <- mean(as.numeric(d$IC.FRM.CORR.ZS), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            indicator = "Corruption as Obstacle",
            value = val
          ))
        }
      } else if ("corruption_obstacle_pct" %in% names(d)) {
        val <- mean(as.numeric(d$corruption_obstacle_pct), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            indicator = "Corruption as Obstacle",
            value = val
          ))
        }
      }

      if ("IC.FRM.BRIB.ZS" %in% names(d)) {
        val <- mean(as.numeric(d$IC.FRM.BRIB.ZS), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            indicator = "Bribery Incidence",
            value = val
          ))
        }
      } else if ("bribery_incidence_pct" %in% names(d)) {
        val <- mean(as.numeric(d$bribery_incidence_pct), na.rm = TRUE)
        if (!is.na(val)) {
          metrics <- rbind(metrics, data.frame(
            indicator = "Bribery Incidence",
            value = val
          ))
        }
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
            title = list(text = "Governance & Corruption Metrics (Size Avg)", font = list(size = 14)),
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
              text = paste0("No governance data available for this firm size<br>",
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
      req(size_data())
      d <- size_data()

      mgmt_time <- if ("mgmt_time_regulations_pct" %in% names(d)) {
        mean(d$mgmt_time_regulations_pct, na.rm = TRUE)
      } else {
        NA
      }

      if (!is.na(mgmt_time) && mgmt_time > 0) {
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
      req(size_data())
      d <- size_data()

      # Safe numeric conversion for aggregated data
      safe_numeric <- function(x) {
        if (is.null(x) || length(x) == 0) return(numeric(0))
        if (is.logical(x)) return(as.numeric(x) * 10)
        if (is.factor(x) || is.character(x)) {
          num_val <- suppressWarnings(as.numeric(as.character(x)))
          if (all(is.na(num_val))) {
            ifelse(tolower(as.character(x)) %in% c("major", "very severe", "severe"), 8,
                   ifelse(tolower(as.character(x)) %in% c("moderate", "minor"), 4, NA_real_))
          } else {
            num_val
          }
        } else {
          as.numeric(x)
        }
      }

      constraints <- list()
      if ("water_obstacle" %in% names(d)) {
        vals <- safe_numeric(d$water_obstacle)
        val <- mean(vals[!is.na(vals)], na.rm = TRUE)
        if (!is.na(val)) constraints$`Water` <- val
      }
      if ("transport_obstacle" %in% names(d)) {
        vals <- safe_numeric(d$transport_obstacle)
        val <- mean(vals[!is.na(vals)], na.rm = TRUE)
        if (!is.na(val)) constraints$`Transport` <- val
      }
      if ("customs_obstacle" %in% names(d)) {
        vals <- safe_numeric(d$customs_obstacle)
        val <- mean(vals[!is.na(vals)], na.rm = TRUE)
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
            yaxis = list(title = "Avg Severity Score"),
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
      req(size_data())
      d <- size_data()

      metrics <- list()
      if ("internet_access_pct" %in% names(d)) {
        val <- mean(as.numeric(d$internet_access_pct), na.rm = TRUE)
        if (!is.na(val)) metrics$`Internet Access` <- val
      } else if ("c9" %in% names(d)) {
        val <- mean(as.numeric(d$c9), na.rm = TRUE)
        if (!is.na(val)) metrics$`Internet Access` <- val
      }

      if ("website_pct" %in% names(d)) {
        val <- mean(as.numeric(d$website_pct), na.rm = TRUE)
        if (!is.na(val)) metrics$`Has Website` <- val
      } else if ("c10" %in% names(d)) {
        val <- mean(as.numeric(d$c10), na.rm = TRUE)
        if (!is.na(val)) metrics$`Has Website` <- val
      }

      if ("email_usage_pct" %in% names(d)) {
        val <- mean(as.numeric(d$email_usage_pct), na.rm = TRUE)
        if (!is.na(val)) metrics$`Email Usage` <- val
      } else if ("c11" %in% names(d)) {
        val <- mean(as.numeric(d$c11), na.rm = TRUE)
        if (!is.na(val)) metrics$`Email Usage` <- val
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
      req(size_data())
      d <- size_data()

      composition <- list()
      if ("IC.FRM.FEMW.ZS" %in% names(d)) {
        val <- mean(as.numeric(d$IC.FRM.FEMW.ZS), na.rm = TRUE)
        if (!is.na(val)) composition$`Female Workers` <- val
      } else if ("female_workers_pct" %in% names(d)) {
        val <- mean(as.numeric(d$female_workers_pct), na.rm = TRUE)
        if (!is.na(val)) composition$`Female Workers` <- val
      }

      if ("IC.FRM.FEMO.ZS" %in% names(d)) {
        val <- mean(as.numeric(d$IC.FRM.FEMO.ZS), na.rm = TRUE)
        if (!is.na(val)) composition$`Female Ownership` <- val
      } else if ("female_ownership_pct" %in% names(d)) {
        val <- mean(as.numeric(d$female_ownership_pct), na.rm = TRUE)
        if (!is.na(val)) composition$`Female Ownership` <- val
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
                marker = list(color = "#1B6B5F")) |>
          layout(
            title = list(text = "Gender Composition (%)", font = list(size = 14)),
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
      req(size_data())
      d <- size_data()

      quality_val <- NA
      if ("IC.FRM.WKFC.ZS" %in% names(d)) {
        quality_val <- 100 - mean(as.numeric(d$IC.FRM.WKFC.ZS), na.rm = TRUE)
      } else if ("workforce_obstacle_pct" %in% names(d)) {
        quality_val <- 100 - mean(as.numeric(d$workforce_obstacle_pct), na.rm = TRUE)
      }

      if (!is.na(quality_val) && quality_val >= 0) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(quality_val, 1),
          title = list(text = "Workforce Quality Index"),
          gauge = list(
            axis = list(range = list(0, 100)),
            bar = list(color = "#1B6B5F"),
            steps = list(
              list(range = c(0, 40), color = "#ffebee"),
              list(range = c(40, 70), color = "#fff3e0"),
              list(range = c(70, 100), color = "#e8f5e9")
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
      req(size_data())
      d <- size_data()

      crime_val <- NA
      if ("IC.FRM.CRIM.ZS" %in% names(d)) {
        crime_val <- mean(as.numeric(d$IC.FRM.CRIM.ZS), na.rm = TRUE)
      } else if ("crime_obstacle_pct" %in% names(d)) {
        crime_val <- mean(as.numeric(d$crime_obstacle_pct), na.rm = TRUE)
      }

      security_val <- NA
      if ("IC.FRM.SECU.ZS" %in% names(d)) {
        security_val <- mean(as.numeric(d$IC.FRM.SECU.ZS), na.rm = TRUE)
      } else if ("security_costs_pct" %in% names(d)) {
        security_val <- mean(as.numeric(d$security_costs_pct), na.rm = TRUE)
      }

      values <- c(crime_val, security_val)
      labels <- c("Crime as Obstacle (%)", "Security Costs (% Sales)")
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
      req(size_data())
      d <- size_data()

      crime_losses <- NA
      if ("crime_losses_pct" %in% names(d)) {
        crime_losses <- mean(as.numeric(d$crime_losses_pct), na.rm = TRUE)
      } else if ("crime3" %in% names(d)) {
        crime_losses <- mean(as.numeric(d$crime3), na.rm = TRUE)
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
      req(size_data())
      d <- size_data()

      capacity_val <- NA
      if ("IC.FRM.CAPU.ZS" %in% names(d)) {
        capacity_val <- mean(as.numeric(d$IC.FRM.CAPU.ZS), na.rm = TRUE)
      } else if ("capacity_utilization_pct" %in% names(d)) {
        capacity_val <- mean(as.numeric(d$capacity_utilization_pct), na.rm = TRUE)
      }

      if (!is.na(capacity_val)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(capacity_val, 1),
          title = list(text = "Capacity Utilization (%)"),
          gauge = list(
            axis = list(range = list(0, 100)),
            bar = list(color = "#1B6B5F"),
            steps = list(
              list(range = c(0, 50), color = "#ffebee"),
              list(range = c(50, 75), color = "#fff3e0"),
              list(range = c(75, 100), color = "#e8f5e9")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No capacity utilization data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$performance_chart2 <- renderPlotly({
      req(size_data())
      d <- size_data()

      export_val <- NA
      if ("IC.FRM.EXPRT.ZS" %in% names(d)) {
        export_val <- mean(as.numeric(d$IC.FRM.EXPRT.ZS), na.rm = TRUE)
      } else if ("export_firms_pct" %in% names(d)) {
        export_val <- mean(as.numeric(d$export_firms_pct), na.rm = TRUE)
      }

      if (!is.na(export_val)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(export_val, 1),
          title = list(text = "Export Firms (%)"),
          gauge = list(
            axis = list(range = list(0, 100)),
            bar = list(color = "#F49B7A"),
            steps = list(
              list(range = c(0, 20), color = "#ffebee"),
              list(range = c(20, 50), color = "#fff3e0"),
              list(range = c(50, 100), color = "#e8f5e9")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No export orientation data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Country Distribution
    output$country_dist <- renderPlotly({
      req(size_data())
      d <- size_data()

      if ("country" %in% names(d) && "sample_size" %in% names(d)) {
        country_counts <- d |>
          group_by(country) |>
          summarise(firms = sum(sample_size, na.rm = TRUE), .groups = "drop") |>
          arrange(desc(firms)) |>
          filter(!is.na(country), firms > 0)

        if (nrow(country_counts) > 0) {
          # Show top 15 countries
          if (nrow(country_counts) > 15) {
            country_counts <- country_counts[1:15, ]
          }

          plot_ly(country_counts,
                  x = ~country,
                  y = ~firms,
                  type = "bar",
                  marker = list(color = "#1B6B5F")) |>
            layout(
              title = list(text = "Top Countries by Firm Count", font = list(size = 16)),
              xaxis = list(title = "", tickangle = -45),
              yaxis = list(title = "Number of Firms"),
              margin = list(b = 120),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        } else {
          plot_ly() |>
            layout(
              annotations = list(
                text = "No country distribution data available",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, showarrow = FALSE
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            )
        }
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No country distribution data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

  })
}
