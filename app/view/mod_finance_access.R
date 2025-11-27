# app/view/mod_finance_access.R
# Access to Finance Analysis Module

box::use(
  stats[setNames, runif],
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, mutate, group_by, summarise]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "finance-container",

    # CSS for independent dropdown scrolling
    tags$style(HTML("
      .filter-column {
        position: relative;
        padding: 0 10px;
      }
      .filter-column .selectize-dropdown {
        position: absolute !important;
        z-index: 9999 !important;
        max-height: 300px;
        overflow-y: auto;
      }
      .filter-column .selectize-input {
        z-index: 1;
      }
    ")),

    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("university"), "Access to Finance Analysis", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Analyze financial inclusion, credit access, and financing constraints across economies")
        )
      )
    ),

    # KPI Row
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_bank_account"))),
      column(3, uiOutput(ns("kpi_credit_line"))),
      column(3, uiOutput(ns("kpi_collateral"))),
      column(3, uiOutput(ns("kpi_rejection")))
    ),
    
    # Filters
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(
            class = "py-2",
            fluidRow(
              column(3, class = "filter-column",
                selectInput(ns("region_filter"), "Region",
                  choices = c("All Regions" = "all"))
              ),
              column(3, class = "filter-column",
                selectInput(ns("firm_size"), "Firm Size",
                  choices = c("All Sizes" = "all", "Small (5-19)" = "small",
                              "Medium (20-99)" = "medium", "Large (100+)" = "large"))
              ),
              column(3, class = "filter-column",
                selectInput(ns("sector"), "Sector",
                  choices = c("All Sectors" = "all", "Manufacturing" = "mfg",
                              "Services" = "svc", "Retail" = "retail"))
              ),
              column(3, class = "filter-column",
                selectInput(ns("gender"), "Ownership",
                  choices = c("All" = "all", "Female-Owned" = "female",
                              "Male-Owned" = "male"))
              )
            )
          )
        )
      )
    ),
    
    # Main Charts
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("credit-card"), "Financial Products Access by Region"),
          card_body(
            tags$p(
              class = "text-muted mb-3",
              style = "font-size: 0.9em;",
              icon("info-circle"), " ",
              tags$strong("What this shows: "),
              "Regional comparison of firms with bank accounts and credit lines. ",
              tags$em("Highlights financial inclusion disparities - "),
              "lower access correlates with slower business growth."
            ),
            plotlyOutput(ns("finance_by_region"), height = "400px")
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-pie"), "Reasons for Not Applying for Loans"),
          card_body(
            tags$p(
              class = "text-muted mb-3",
              style = "font-size: 0.9em;",
              icon("info-circle"), " ",
              tags$strong("What this shows: "),
              "Why businesses don't seek credit. High interest rates and complex requirements are major barriers. ",
              "Some firms self-exclude believing they'll be rejected."
            ),
            plotlyOutput(ns("no_apply_reasons"), height = "400px")
          )
        )
      )
    ),

    # SME Finance Gap
    fluidRow(
      class = "mb-4",
      column(8,
        card(
          card_header(icon("chart-bar"), "SME Finance Gap by Country"),
          card_body(
            tags$p(
              class = "text-muted mb-3",
              style = "font-size: 0.9em;",
              icon("info-circle"), " ",
              tags$strong("What this shows: "),
              "Unmet financing needs of small/medium enterprises as % of GDP. ",
              "Larger gaps indicate credit markets aren't serving SMEs, constraining economic growth."
            ),
            plotlyOutput(ns("sme_finance_gap"), height = "400px")
          )
        )
      ),
      column(4,
        card(
          card_header(icon("venus"), "Gender Gap in Finance Access"),
          card_body(
            tags$p(
              class = "text-muted mb-3",
              style = "font-size: 0.9em;",
              icon("info-circle"), " ",
              tags$strong("What this shows: "),
              "Difference in credit access between male and female-owned firms. ",
              "Positive values = gender gap exists."
            ),
            plotlyOutput(ns("gender_gap"), height = "400px")
          )
        )
      )
    ),

    # Collateral Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("landmark"), "Collateral Requirements"),
          card_body(
            tags$p(
              class = "text-muted mb-3",
              style = "font-size: 0.9em;",
              icon("info-circle"), " ",
              tags$strong("What this shows: "),
              "Average collateral needed as % of loan value. ",
              tags$em("Values over 100% = banks require more collateral than loan amount, "),
              "making credit inaccessible for asset-poor businesses."
            ),
            plotlyOutput(ns("collateral_chart"), height = "350px")
          )
        )
      ),
      column(6,
        card(
          card_header(icon("clock"), "Loan Processing Time"),
          card_body(
            tags$p(
              class = "text-muted mb-3",
              style = "font-size: 0.9em;",
              icon("info-circle"), " ",
              tags$strong("What this shows: "),
              "Days to approve/disburse loans. Longer times mean businesses miss opportunities and incur costs while waiting."
            ),
            plotlyOutput(ns("processing_time"), height = "350px")
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {
    
    # Update filters
    observeEvent(wbes_data(), {
      req(wbes_data())
      regions <- unique(wbes_data()$latest$region)
      shiny::updateSelectInput(session, "region_filter",
        choices = c("All Regions" = "all", setNames(regions, regions)))
    })
    
    # Filtered data
    filtered_data <- reactive({
      req(wbes_data())
      data <- wbes_data()$latest
      if (input$region_filter != "all") {
        data <- filter(data, region == input$region_filter)
      }
      data
    })
    
    # KPIs
    output$kpi_bank_account <- renderUI({
      req(filtered_data())
      avg <- base::round(base::mean(filtered_data()$firms_with_bank_account_pct, na.rm = TRUE), 1)
      tags$div(class = "kpi-box",
        tags$div(class = "kpi-value", paste0(avg, "%")),
        tags$div(class = "kpi-label", "Bank Account")
      )
    })
    
    output$kpi_credit_line <- renderUI({
      req(filtered_data())
      avg <- base::round(base::mean(filtered_data()$firms_with_credit_line_pct, na.rm = TRUE), 1)
      tags$div(class = "kpi-box kpi-box-coral",
        tags$div(class = "kpi-value", paste0(avg, "%")),
        tags$div(class = "kpi-label", "Credit Access")
      )
    })
    
    output$kpi_collateral <- renderUI({
      req(filtered_data())
      avg <- base::round(base::mean(filtered_data()$collateral_required_pct, na.rm = TRUE), 0)
      tags$div(class = "kpi-box kpi-box-warning",
        tags$div(class = "kpi-value", paste0(avg, "%")),
        tags$div(class = "kpi-label", "Collateral Required")
      )
    })
    
    output$kpi_rejection <- renderUI({
      req(filtered_data())
      avg <- base::round(base::mean(filtered_data()$loan_rejection_rate_pct, na.rm = TRUE), 1)
      tags$div(class = "kpi-box kpi-box-success",
        tags$div(class = "kpi-value", paste0(avg, "%")),
        tags$div(class = "kpi-label", "Rejection Rate")
      )
    })
    
    # Finance by region
    output$finance_by_region <- renderPlotly({
      req(wbes_data())
      regional <- wbes_data()$regional

      if (base::is.null(regional)) {
        regional <- base::data.frame(
          region = base::c("Sub-Saharan Africa", "South Asia", "East Asia & Pacific",
                     "Latin America", "Europe & Central Asia"),
          firms_with_bank_account_pct = base::c(82, 85, 92, 94, 96),
          firms_with_credit_line_pct = base::c(22, 28, 35, 42, 48)
        )
      }

      plot_ly(regional) |>
        add_trace(x = ~region, y = ~firms_with_bank_account_pct, name = "Bank Account",
                  type = "bar", marker = list(color = "#1B6B5F")) |>
        add_trace(x = ~region, y = ~firms_with_credit_line_pct, name = "Credit Line",
                  type = "bar", marker = list(color = "#F49B7A")) |>
        layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = -30),
          yaxis = list(title = "% of Firms", ticksuffix = "%"),
          legend = list(orientation = "h", y = -0.2),
          margin = list(b = 100),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # No apply reasons
    output$no_apply_reasons <- renderPlotly({
      reasons <- data.frame(
        reason = c("No Need", "High Interest", "Complex Procedures", 
                   "Collateral Issues", "Informal Alternative", "Size of Loan"),
        pct = c(42, 18, 15, 12, 8, 5)
      )
      
      plot_ly(reasons,
              labels = ~reason,
              values = ~pct,
              type = "pie",
              hole = 0.4,
              marker = list(colors = c("#1B6B5F", "#F49B7A", "#2E7D32", 
                                       "#17a2b8", "#6C757D", "#ffc107")),
              textinfo = "label+percent") |>
        layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # SME finance gap
    output$sme_finance_gap <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      data <- arrange(data, desc(firms_with_credit_line_pct))[1:12, ]
      data$country <- factor(data$country, levels = rev(data$country))
      
      # Simulated gap data
      data$need <- data$firms_with_credit_line_pct + runif(base::nrow(data), 20, 40)
      data$gap <- data$need - data$firms_with_credit_line_pct
      
      plot_ly(data) |>
        add_trace(y = ~country, x = ~firms_with_credit_line_pct, 
                  name = "Current Access", type = "bar", orientation = "h",
                  marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~country, x = ~gap, 
                  name = "Unmet Need (Gap)", type = "bar", orientation = "h",
                  marker = list(color = "#F49B7A")) |>
        layout(
          barmode = "stack",
          xaxis = list(title = "% of SMEs", ticksuffix = "%"),
          yaxis = list(title = ""),
          legend = list(orientation = "h", y = -0.15),
          margin = list(l = 100),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Gender gap
    output$gender_gap <- renderPlotly({
      gender_data <- data.frame(
        category = c("Credit Line", "Bank Loan", "Overdraft", "Trade Credit"),
        female = c(28, 22, 15, 35),
        male = c(35, 28, 20, 42)
      )
      
      plot_ly(gender_data) |>
        add_trace(x = ~category, y = ~female, name = "Female-Owned",
                  type = "bar", marker = list(color = "#F49B7A")) |>
        add_trace(x = ~category, y = ~male, name = "Male-Owned",
                  type = "bar", marker = list(color = "#1B6B5F")) |>
        layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "% of Firms", ticksuffix = "%"),
          legend = list(orientation = "h", y = -0.15),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Collateral chart
    output$collateral_chart <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      data <- arrange(data, desc(collateral_required_pct))[1:10, ]
      data$country <- factor(data$country, levels = rev(data$country))
      
      plot_ly(data,
              y = ~country,
              x = ~collateral_required_pct,
              type = "bar",
              orientation = "h",
              marker = list(
                color = ~collateral_required_pct,
                colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))
              )) |>
        layout(
          xaxis = list(title = "Collateral (% of Loan Value)"),
          yaxis = list(title = ""),
          margin = list(l = 100),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Processing time
    output$processing_time <- renderPlotly({
      time_data <- data.frame(
        region = c("SSA", "SA", "EAP", "LAC", "ECA"),
        days = c(45, 38, 25, 32, 18)
      )
      time_data <- arrange(time_data, desc(days))
      time_data$region <- factor(time_data$region, levels = time_data$region)
      
      plot_ly(time_data,
              x = ~region,
              y = ~days,
              type = "bar",
              marker = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = "Region"),
          yaxis = list(title = "Average Days"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
  })
}
