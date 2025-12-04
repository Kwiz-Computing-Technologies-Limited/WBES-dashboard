# app/view/mod_size_profile.R
# Firm Size Profile Deep Dive Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, tagList, icon, div, h2, h3, h4, p, span,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent, renderText, textOutput],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, select, arrange, mutate, group_by, summarise, n],
  stats[setNames]
)

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

    # Size Selector
    fluidRow(
      class = "mb-4",
      column(4,
        card(
          card_body(
            selectInput(
              ns("size_select"),
              "Select Firm Size Category",
              choices = NULL,
              width = "100%"
            )
          )
        )
      ),
      column(8,
        uiOutput(ns("size_summary"))
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
              "The radar highlights how firms in this size category score across infrastructure, finance, governance, capacity, exports, and gender equity relative to a 0–100 scale."
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
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {

    # Update size choices
    observeEvent(wbes_data(), {
      req(wbes_data())
      data <- wbes_data()$latest

      # Get unique firm sizes
      sizes <- data$firm_size |>
        unique() |>
        stats::na.omit() |>
        as.character() |>
        sort()

      shiny::updateSelectInput(
        session, "size_select",
        choices = setNames(sizes, sizes),
        selected = if(length(sizes) > 0) sizes[1] else NULL
      )
    })

    # Selected size data
    size_data <- reactive({
      req(wbes_data(), input$size_select)
      data <- wbes_data()$latest
      data |> filter(!is.na(firm_size) & firm_size == input$size_select)
    })

    # Size summary card
    output$size_summary <- renderUI({
      req(size_data())
      d <- size_data()

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
          fluidRow(
            column(6,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value", countries_count),
                tags$div(class = "kpi-label", "Countries Covered")
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

      # Helper function to safely aggregate values
      safe_mean <- function(col, default = 0, scale = 1, invert = FALSE) {
        if (col %in% names(d)) {
          values <- d[[col]][!is.na(d[[col]])]
          if (length(values) > 0) {
            val <- mean(values, na.rm = TRUE) * scale
            if (invert) 100 - min(val, 100) else min(val, 100)
          } else {
            default
          }
        } else {
          default
        }
      }

      indicators <- c(
        "Infrastructure" = safe_mean("power_outages_per_month", default = 50, scale = 5, invert = TRUE),
        "Finance Access" = safe_mean("firms_with_credit_line_pct", default = 30),
        "Low Corruption" = safe_mean("bribery_incidence_pct", default = 50, invert = TRUE),
        "Capacity Use" = safe_mean("capacity_utilization_pct", default = 50),
        "Export Orient." = safe_mean("export_firms_pct", default = 25, scale = 2),
        "Gender Equity" = safe_mean("female_ownership_pct", default = 25, scale = 2)
      )

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
    output$infra_chart1 <- renderPlotly({
      req(size_data())
      d <- size_data()

      obstacles <- data.frame(
        category = c("Electricity", "Water", "Transport"),
        severity = c(
          mean(d$electricity_obstacle, na.rm = TRUE),
          mean(d$water_obstacle, na.rm = TRUE),
          mean(d$transport_obstacle, na.rm = TRUE)
        ),
        stringsAsFactors = FALSE
      )
      obstacles <- obstacles[obstacles$severity > 0 & !is.na(obstacles$severity), ]

      if (nrow(obstacles) > 0) {
        plot_ly(obstacles,
                x = ~category,
                y = ~severity,
                type = "bar",
                marker = list(color = "#1B6B5F")) |>
          layout(
            title = list(text = "Infrastructure Obstacles", font = list(size = 14)),
            yaxis = list(title = "Avg Severity Score"),
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
      req(size_data())
      d <- size_data()

      generator_pct <- mean(d$firms_with_generator_pct, na.rm = TRUE)
      generator_share <- mean(d$generator_share_pct, na.rm = TRUE)

      values <- if (!is.na(generator_pct) || !is.na(generator_share)) {
        gen_val <- max(generator_pct, generator_share, na.rm = TRUE)
        c(gen_val, max(0, 100 - gen_val - 10), 10)
      } else {
        c(0, 85, 15)
      }

      labels <- c("Generator (Primary)", "Grid Only", "Mixed")
      data_df <- data.frame(labels = labels, values = values)
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
    output$gov_chart1 <- renderPlotly({
      req(size_data())
      d <- size_data()

      bribes <- list()
      if ("bribe_for_permit" %in% names(d)) {
        val <- mean(d$bribe_for_permit, na.rm = TRUE)
        if (!is.na(val)) bribes$`Construction Permits` <- val
      }
      if ("bribe_for_utilities" %in% names(d)) {
        val <- mean(d$bribe_for_utilities, na.rm = TRUE)
        if (!is.na(val)) bribes$`Utility Connection` <- val
      }
      if ("bribe_for_import" %in% names(d)) {
        val <- mean(d$bribe_for_import, na.rm = TRUE)
        if (!is.na(val)) bribes$`Import License` <- val
      }
      if ("bribe_for_tax" %in% names(d)) {
        val <- mean(d$bribe_for_tax, na.rm = TRUE)
        if (!is.na(val)) bribes$`Tax Assessment` <- val
      }
      if ("bribe_for_contract" %in% names(d)) {
        val <- mean(d$bribe_for_contract, na.rm = TRUE)
        if (!is.na(val)) bribes$`Government Contract` <- val
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
