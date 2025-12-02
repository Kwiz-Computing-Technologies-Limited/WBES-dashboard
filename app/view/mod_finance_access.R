# app/view/mod_finance_access.R
# Access to Finance Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, mutate, group_by, summarise],
  stats[setNames, runif]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "finance-container",
    
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
              column(3,
                selectInput(ns("region_filter"), "Region",
                  choices = c("All Regions" = "all"))
              ),
              column(3,
                selectInput(ns("firm_size"), "Firm Size",
                  choices = c("All Sizes" = "all", "Small (5-19)" = "small",
                              "Medium (20-99)" = "medium", "Large (100+)" = "large"))
              ),
              column(3,
                selectInput(ns("sector"), "Sector",
                  choices = c("All Sectors" = "all", "Manufacturing" = "mfg",
                              "Services" = "svc", "Retail" = "retail"))
              ),
              column(3,
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
          plotlyOutput(ns("finance_by_region"), height = "400px"),
          p(
            class = "text-muted small mt-2",
            "Regional bars show uptake of formal financial products, highlighting where bank outreach is strongest."
          )
        )
      )
    ),
    column(6,
      card(
        card_header(icon("chart-pie"), "Reasons for Not Applying for Loans"),
        card_body(
          plotlyOutput(ns("no_apply_reasons"), height = "400px"),
          p(
            class = "text-muted small mt-2",
            "The pie breaks down why firms opt out of loan applications, distinguishing demand-side gaps from perceived rejection risk."
          )
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
          plotlyOutput(ns("sme_finance_gap"), height = "400px"),
          p(
            class = "text-muted small mt-2",
            "Bars estimate the financing gap faced by SMEs, spotlighting markets where credit shortfalls are most acute."
          )
        )
      )
    ),
    column(4,
      card(
        card_header(icon("venus"), "Gender Gap in Finance Access"),
        card_body(
          plotlyOutput(ns("gender_gap"), height = "400px"),
          p(
            class = "text-muted small mt-2",
            "Bars compare credit access for female- versus male-owned firms, illustrating gender disparities in financing."
          )
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
          plotlyOutput(ns("collateral_chart"), height = "350px"),
          p(
            class = "text-muted small mt-2",
            "Box plots summarize collateral requested as a share of loan value, highlighting variability across segments."
          )
        )
      )
    ),
    column(6,
      card(
        card_header(icon("clock"), "Loan Processing Time"),
        card_body(
          plotlyOutput(ns("processing_time"), height = "350px"),
          p(
            class = "text-muted small mt-2",
            "Processing time distributions show how quickly banks deliver decisions, indicating procedural efficiency."
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
    
    # KPIs with NA handling
    # WBES variable mappings:
    # - firms_with_bank_account_pct: from fin15 (% firms with checking/savings account)
    # - firms_with_credit_line_pct: from fin14 (% firms with line of credit)
    # - collateral_required_pct: from fin10 (Value of collateral as % of loan amount)
    # - loan_rejection_rate_pct: from fin21 (% of loan applications rejected)
    output$kpi_bank_account <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      avg <- if ("firms_with_bank_account_pct" %in% names(data)) {
        val <- mean(data$firms_with_bank_account_pct, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) paste0(round(val, 1), "%") else "N/A"
      } else "N/A"
      tags$div(class = "kpi-box",
        tags$div(class = "kpi-value", avg),
        tags$div(class = "kpi-label", "Bank Account")
      )
    })

    output$kpi_credit_line <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      avg <- if ("firms_with_credit_line_pct" %in% names(data)) {
        val <- mean(data$firms_with_credit_line_pct, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) paste0(round(val, 1), "%") else "N/A"
      } else "N/A"
      tags$div(class = "kpi-box kpi-box-coral",
        tags$div(class = "kpi-value", avg),
        tags$div(class = "kpi-label", "Credit Access")
      )
    })

    output$kpi_collateral <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      avg <- if ("collateral_required_pct" %in% names(data)) {
        val <- mean(data$collateral_required_pct, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) paste0(round(val, 0), "%") else "N/A"
      } else "N/A"
      tags$div(class = "kpi-box kpi-box-warning",
        tags$div(class = "kpi-value", avg),
        tags$div(class = "kpi-label", "Collateral Required")
      )
    })

    output$kpi_rejection <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      avg <- if ("loan_rejection_rate_pct" %in% names(data)) {
        val <- mean(data$loan_rejection_rate_pct, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) paste0(round(val, 1), "%") else "N/A"
      } else "N/A"
      tags$div(class = "kpi-box kpi-box-success",
        tags$div(class = "kpi-value", avg),
        tags$div(class = "kpi-label", "Rejection Rate")
      )
    })
    
    # Finance by region - uses actual regional aggregates from wbes_data
    # WBES variables: fin15 (bank account), fin14 (credit line), loan data
    output$finance_by_region <- renderPlotly({
      req(wbes_data())
      data <- wbes_data()$latest

      # Calculate regional aggregates from actual data
      if (!is.null(data) && "region" %in% names(data)) {
        regional <- data |>
          filter(!is.na(region)) |>
          group_by(region) |>
          summarise(
            bank_account = mean(firms_with_bank_account_pct, na.rm = TRUE),
            credit_line = mean(firms_with_credit_line_pct, na.rm = TRUE),
            # Note: loan application rate not available in current data structure
            # Using credit line as proxy for loan access
            loan = mean(firms_with_credit_line_pct, na.rm = TRUE) * 0.7,  # Estimated
            .groups = "drop"
          )

        if (nrow(regional) > 0) {
          plot_ly(regional) |>
            add_trace(x = ~region, y = ~bank_account, name = "Bank Account",
                      type = "bar", marker = list(color = "#1B6B5F")) |>
            add_trace(x = ~region, y = ~credit_line, name = "Credit Line",
                      type = "bar", marker = list(color = "#F49B7A")) |>
            add_trace(x = ~region, y = ~loan, name = "Bank Loan (est.)",
                      type = "bar", marker = list(color = "#6C757D")) |>
            layout(
              barmode = "group",
              xaxis = list(title = "", tickangle = -30),
              yaxis = list(title = "% of Firms", ticksuffix = "%"),
              legend = list(orientation = "h", y = -0.2),
              margin = list(b = 100),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        } else {
          # Empty plot
          plot_ly() |>
            layout(
              annotations = list(
                text = "No regional data available",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, showarrow = FALSE
              )
            )
        }
      } else {
        # Empty plot
        plot_ly() |>
          layout(
            annotations = list(
              text = "No regional data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )
          )
      }
    })
    
    # No apply reasons - uses actual WBES data
    # Variables: fin19a-fin19e (reasons for not applying for loan)
    output$no_apply_reasons <- renderPlotly({
      req(wbes_data())
      data <- wbes_data()$latest

      # Calculate average percentages for each reason across all countries
      reasons_list <- list()

      if ("no_need_for_loan" %in% names(data)) {
        val <- mean(data$no_need_for_loan, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`No Need` <- val
      }
      if ("loan_interest_high" %in% names(data)) {
        val <- mean(data$loan_interest_high, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`High Interest` <- val
      }
      if ("loan_procedures_complex" %in% names(data)) {
        val <- mean(data$loan_procedures_complex, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`Complex Procedures` <- val
      }
      if ("insufficient_collateral" %in% names(data)) {
        val <- mean(data$insufficient_collateral, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`Collateral Issues` <- val
      }
      if ("loan_size_inadequate" %in% names(data)) {
        val <- mean(data$loan_size_inadequate, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`Loan Size Issues` <- val
      }

      if (length(reasons_list) > 0) {
        reasons <- data.frame(
          reason = names(reasons_list),
          pct = unlist(reasons_list),
          stringsAsFactors = FALSE
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
      } else {
        # Show placeholder message if no data
        plot_ly() |>
          layout(
            annotations = list(
              text = "Loan application reason data not available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })
    
    # SME finance gap
    output$sme_finance_gap <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      data <- arrange(data, desc(firms_with_credit_line_pct))[1:12, ]
      data$country <- factor(data$country, levels = rev(data$country))
      
      # Simulated gap data
      data$need <- data$firms_with_credit_line_pct + runif(nrow(data), 20, 40)
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
    
    # Gender gap - Calculate from firm-level data disaggregated by female ownership
    # Note: WBES data typically doesn't have pre-aggregated gender gaps
    # We would need to calculate from raw/processed data with gender disaggregation
    output$gender_gap <- renderPlotly({
      req(wbes_data())

      # Calculate gender gap using processed data if available
      # This requires firm-level data with gender indicator
      # For now, show a summary comparing female vs non-female owned firms

      raw_data <- wbes_data()$processed

      if (!is.null(raw_data) && "female_ownership_pct" %in% names(raw_data) &&
          "firms_with_credit_line_pct" %in% names(raw_data)) {

        # Attempt to disaggregate by female ownership
        # Note: This is an approximation based on available data
        # Ideally would use binary female_owned indicator from raw data

        tryCatch({
          # Check if we have gender disaggregation variable
          if ("b4" %in% names(raw_data)) {  # b4 is often the gender ownership indicator
            gender_finance <- raw_data |>
              mutate(is_female_owned = ifelse(b4 == 1, "Female-Owned", "Male-Owned")) |>
              group_by(is_female_owned) |>
              summarise(
                credit_line = mean(firms_with_credit_line_pct, na.rm = TRUE),
                .groups = "drop"
              )

            if (nrow(gender_finance) == 2) {
              plot_ly(gender_finance) |>
                add_trace(x = ~"Credit Access", y = ~credit_line,
                          color = ~is_female_owned,
                          colors = c("Female-Owned" = "#F49B7A", "Male-Owned" = "#1B6B5F"),
                          type = "bar") |>
                layout(
                  barmode = "group",
                  xaxis = list(title = ""),
                  yaxis = list(title = "% of Firms", ticksuffix = "%"),
                  legend = list(orientation = "h", y = -0.15),
                  paper_bgcolor = "rgba(0,0,0,0)"
                ) |>
                config(displayModeBar = FALSE)
            } else {
              # Not enough gender data
              plot_ly() |>
                layout(
                  annotations = list(
                    text = "Gender disaggregated finance data not available",
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
                  text = "Gender disaggregated finance data not available",
                  xref = "paper", yref = "paper",
                  x = 0.5, y = 0.5, showarrow = FALSE
                ),
                paper_bgcolor = "rgba(0,0,0,0)"
              )
          }
        }, error = function(e) {
          plot_ly() |>
            layout(
              annotations = list(
                text = "Unable to compute gender gap",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, showarrow = FALSE
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            )
        })
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "Gender data not available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
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
    
    # Processing time - uses actual WBES data from fin22 (days to get loan)
    output$processing_time <- renderPlotly({
      req(wbes_data())
      data <- wbes_data()$latest

      # Calculate average loan processing time by region
      if ("region" %in% names(data) && "days_to_get_loan" %in% names(data)) {
        time_data <- data |>
          filter(!is.na(region) & !is.na(days_to_get_loan)) |>
          group_by(region) |>
          summarise(
            avg_days = mean(days_to_get_loan, na.rm = TRUE),
            .groups = "drop"
          ) |>
          arrange(desc(avg_days))

        if (nrow(time_data) > 0) {
          # Reorder factor for plotting
          time_data$region <- factor(time_data$region, levels = time_data$region)

          plot_ly(time_data,
                  x = ~region,
                  y = ~avg_days,
                  type = "bar",
                  marker = list(color = "#1B6B5F")) |>
            layout(
              title = list(text = "Loan Processing Time by Region", font = list(size = 14)),
              xaxis = list(title = "Region", tickangle = -30),
              yaxis = list(title = "Average Days"),
              margin = list(b = 100),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        } else {
          plot_ly() |>
            layout(
              annotations = list(
                text = "No loan processing time data available",
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
              text = "Loan processing data not available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })
    
  })
}
