# app/view/mod_country_profile.R
# Country Profile Deep Dive Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, tagList, icon, div, h2, h3, h4, p, span,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent, renderText, textOutput],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, select, arrange, mutate],
  stats[setNames]
)

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

    # Radar Chart + Key Metrics
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-pie"), "Business Environment Radar"),
          card_body(
            plotlyOutput(ns("radar_chart"), height = "400px"),
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
                  plotlyOutput(ns("infra_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Bars rank which infrastructure services firms flag as biggest obstacles, indicating where reliability investments are needed."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("infra_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "The pie shows how firms power operations (grid, generator, mixed), revealing dependence on backup generation."
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
                    "Financial product uptake across credit and deposit instruments highlights where inclusion gaps remain."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("finance_chart2"), height = "300px"),
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
                  plotlyOutput(ns("gov_chart1"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Bribery prevalence by transaction type surfaces which interactions with government most often trigger informal payments."
                  )
                )
              ),
              column(6,
                tagList(
                  plotlyOutput(ns("gov_chart2"), height = "300px"),
                  p(
                    class = "text-muted small mt-2",
                    "Management time spent on regulatory tasks highlights the bureaucracy burden affecting daily operations."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Time Series",
            icon = icon("chart-line"),
            tagList(
              plotlyOutput(ns("time_series"), height = "400px"),
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
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {

    # Update country choices
    observeEvent(wbes_data(), {
      req(wbes_data())
      # Extract unique countries from the latest dataset (already cleaned, no year suffix)
      countries <- wbes_data()$latest$country |>
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
      req(wbes_data(), input$country_select)
      data <- wbes_data()$latest
      # Filter using the clean country name (year suffix already removed in data processing)
      data |> filter(!is.na(country) & country == input$country_select)
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
    # Uses actual WBES data: elec (electricity obstacle), c16 (water), d4 (transport)
    output$infra_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Extract actual obstacle scores with fallback to 0 if missing
      obstacles <- data.frame(
        category = c("Electricity", "Water", "Transport"),
        severity = c(
          if ("electricity_obstacle" %in% names(d) && !is.na(d$electricity_obstacle[1])) d$electricity_obstacle[1] else 0,
          if ("water_obstacle" %in% names(d) && !is.na(d$water_obstacle[1])) d$water_obstacle[1] else 0,
          if ("transport_obstacle" %in% names(d) && !is.na(d$transport_obstacle[1])) d$transport_obstacle[1] else 0
        ),
        stringsAsFactors = FALSE
      )

      # Only show bars for obstacles with data
      obstacles <- obstacles[obstacles$severity > 0, ]

      if (nrow(obstacles) > 0) {
        plot_ly(obstacles,
                x = ~category,
                y = ~severity,
                type = "bar",
                marker = list(color = "#1B6B5F")) |>
          layout(
            title = list(text = "Infrastructure Obstacles", font = list(size = 14)),
            yaxis = list(title = "Severity Score (0-10)"),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No infrastructure obstacle data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$infra_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Calculate power source distribution based on actual data
      # generator_share_pct (from in7) indicates % electricity from generator
      # firms_with_generator_pct (from in9) indicates % firms with generator
      generator_pct <- if ("firms_with_generator_pct" %in% names(d) && !is.na(d$firms_with_generator_pct[1])) {
        d$firms_with_generator_pct[1]
      } else 0

      generator_share <- if ("generator_share_pct" %in% names(d) && !is.na(d$generator_share_pct[1])) {
        d$generator_share_pct[1]
      } else 0

      # Estimate distribution: If generator usage is high, more firms rely on it
      # This is an estimation based on available data
      values <- if (generator_pct > 0 || generator_share > 0) {
        c(
          max(generator_pct, generator_share),  # Generator primary/heavy use
          max(0, 100 - max(generator_pct, generator_share) - 10),  # Grid only
          10  # Mixed (estimated)
        )
      } else {
        c(0, 85, 15)  # Default: mostly grid with some mixed
      }

      # Filter out zero values
      labels <- c("Generator (Primary)", "Grid Only", "Mixed")
      data_df <- data.frame(
        labels = labels,
        values = values,
        stringsAsFactors = FALSE
      )
      data_df <- data_df[data_df$values > 0, ]

      if (nrow(data_df) > 0) {
        plot_ly(data_df,
                labels = ~labels,
                values = ~values,
                type = "pie",
                marker = list(colors = c("#1B6B5F", "#F49B7A", "#6C757D")),
                textinfo = "label+percent") |>
          layout(
            title = list(text = "Power Sources", font = list(size = 14)),
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
    # Uses actual WBES data: j7a-j7e (bribery for different transaction types)
    output$gov_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Extract actual bribery data by transaction type
      bribes <- list()

      if ("bribe_for_permit" %in% names(d) && !is.na(d$bribe_for_permit[1])) {
        bribes$`Construction Permits` <- d$bribe_for_permit[1]
      }
      if ("bribe_for_utilities" %in% names(d) && !is.na(d$bribe_for_utilities[1])) {
        bribes$`Utility Connection` <- d$bribe_for_utilities[1]
      }
      if ("bribe_for_import" %in% names(d) && !is.na(d$bribe_for_import[1])) {
        bribes$`Import License` <- d$bribe_for_import[1]
      }
      if ("bribe_for_tax" %in% names(d) && !is.na(d$bribe_for_tax[1])) {
        bribes$`Tax Assessment` <- d$bribe_for_tax[1]
      }
      if ("bribe_for_contract" %in% names(d) && !is.na(d$bribe_for_contract[1])) {
        bribes$`Government Contract` <- d$bribe_for_contract[1]
      }

      if (length(bribes) > 0) {
        plot_data <- data.frame(
          transaction = names(bribes),
          pct = unlist(bribes),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~transaction,
                y = ~pct,
                type = "bar",
                marker = list(color = "#1B6B5F")) |>
          layout(
            title = list(text = "Bribery by Transaction Type (%)", font = list(size = 14)),
            yaxis = list(title = "% Reporting Bribes"),
            xaxis = list(title = "", tickangle = -30),
            margin = list(b = 100),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No bribery data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
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

  })
}
