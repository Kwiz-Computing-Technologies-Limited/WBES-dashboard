# app/view/mod_country_profile.R
# Country Profile Deep Dive Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, p, span,
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
      countries <- sort(wbes_data()$countries)
      shiny::updateSelectInput(
        session, "country_select",
        choices = setNames(countries, countries),
        selected = countries[1]
      )
    })
    
    # Selected country data
    country_data <- reactive({
      req(wbes_data(), input$country_select)
      data <- wbes_data()$latest
      filter(data, country == input$country_select)
    })
    
    # Country summary card
    output$country_summary <- renderUI({
      req(country_data())
      d <- country_data()
      
      tags$div(
        class = "card h-100",
        tags$div(
          class = "card-body",
          fluidRow(
            column(4,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value", d$region[1]),
                tags$div(class = "kpi-label", "Region")
              )
            ),
            column(4,
              tags$div(class = "kpi-box kpi-box-coral",
                tags$div(class = "kpi-value", d$income_group[1]),
                tags$div(class = "kpi-label", "Income Group")
              )
            ),
            column(4,
              tags$div(class = "kpi-box kpi-box-success",
                tags$div(class = "kpi-value", d$sample_size[1]),
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
      
      # Normalize to 0-100 scale
      indicators <- c(
        "Infrastructure" = 100 - min(d$power_outages_per_month * 5, 100),
        "Finance Access" = d$firms_with_credit_line_pct,
        "Low Corruption" = 100 - d$bribery_incidence_pct,
        "Capacity Use" = d$capacity_utilization_pct,
        "Export Orient." = d$export_firms_pct * 2,
        "Gender Equity" = d$female_ownership_pct * 2
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
      req(country_data())
      d <- country_data()
      
      metrics <- list(
        list("Power Outages/Month", round(d$power_outages_per_month, 1), "bolt"),
        list("Outage Duration (hrs)", round(d$avg_outage_duration_hrs, 1), "clock"),
        list("Credit Access (%)", round(d$firms_with_credit_line_pct, 1), "credit-card"),
        list("Bribery Incidence (%)", round(d$bribery_incidence_pct, 1), "hand-holding-usd"),
        list("Capacity Utilization (%)", round(d$capacity_utilization_pct, 1), "industry"),
        list("Female Ownership (%)", round(d$female_ownership_pct, 1), "female")
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
      plot_ly(
        x = c("Power", "Water", "Transport"),
        y = c(8.2, 4.5, 3.1),
        type = "bar",
        marker = list(color = "#1B6B5F")
      ) |>
        layout(
          title = list(text = "Infrastructure Obstacles", font = list(size = 14)),
          yaxis = list(title = "Severity Score"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    output$infra_chart2 <- renderPlotly({
      plot_ly(
        labels = c("Generator", "Grid Only", "Mixed"),
        values = c(45, 35, 20),
        type = "pie",
        marker = list(colors = c("#1B6B5F", "#F49B7A", "#6C757D"))
      ) |>
        layout(
          title = list(text = "Power Sources", font = list(size = 14)),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Finance Charts
    output$finance_chart1 <- renderPlotly({
      plot_ly(
        x = c("Bank Account", "Credit Line", "Loan", "Overdraft"),
        y = c(88, 32, 25, 18),
        type = "bar",
        marker = list(color = "#F49B7A")
      ) |>
        layout(
          title = list(text = "Financial Products Access (%)", font = list(size = 14)),
          yaxis = list(title = "% of Firms"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    output$finance_chart2 <- renderPlotly({
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = 185,
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
    })
    
    # Governance Charts
    output$gov_chart1 <- renderPlotly({
      plot_ly(
        x = c("Permits", "Utilities", "Customs", "Taxes", "Courts"),
        y = c(12, 18, 25, 15, 8),
        type = "bar",
        marker = list(color = "#1B6B5F")
      ) |>
        layout(
          title = list(text = "Bribery by Transaction Type (%)", font = list(size = 14)),
          yaxis = list(title = "% Reporting Bribes"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    output$gov_chart2 <- renderPlotly({
      plot_ly(
        x = c("Regulations", "Taxes", "Licenses", "Inspections"),
        y = c(15, 12, 8, 5),
        type = "bar",
        marker = list(color = "#6C757D")
      ) |>
        layout(
          title = list(text = "Mgmt Time on Bureaucracy (%)", font = list(size = 14)),
          yaxis = list(title = "% of Time"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
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
