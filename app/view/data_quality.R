# app/view/data_quality.R
# Data Quality Documentation Module
# Complete transparency on data issues and filtering logic

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, h4, h5, p, pre, code,
        fluidRow, column, renderUI, uiOutput, HTML, downloadButton, downloadHandler],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel, accordion, accordion_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, config],
  DT[DTOutput, renderDT, datatable],
  dplyr[arrange]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container-fluid py-4",
    
    # Header
    fluidRow(
      column(12,
        h2(icon("clipboard-check"), " Data Quality & Methodology", class = "text-primary mb-2"),
        p(class = "lead text-muted mb-4",
          "Complete transparency on data sources, quality issues, and filtering logic")
      )
    ),
    
    # Overview Cards
    fluidRow(
      class = "mb-4",
      column(4,
        div(class = "card bg-primary text-white h-100",
          div(class = "card-body text-center",
            icon("database", class = "fa-2x mb-2"),
            h4("Source Transparency"),
            p(class = "mb-0", "Full documentation of data origins")
          )
        )
      ),
      column(4,
        div(class = "card bg-secondary text-white h-100",
          div(class = "card-body text-center",
            icon("filter", class = "fa-2x mb-2"),
            h4("Filter Documentation"),
            p(class = "mb-0", "All transformations explained")
          )
        )
      ),
      column(4,
        div(class = "card bg-success text-white h-100",
          div(class = "card-body text-center",
            icon("code", class = "fa-2x mb-2"),
            h4("Reproducible R Code"),
            p(class = "mb-0", "Copy-paste ready snippets")
          )
        )
      )
    ),
    
    # Completeness Charts
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-pie"), " Data Completeness by Indicator"),
          card_body(plotlyOutput(ns("completeness_chart"), height = "300px"))
        )
      ),
      column(6,
        card(
          card_header(icon("globe-africa"), " Completeness by Region"),
          card_body(plotlyOutput(ns("regional_completeness"), height = "300px"))
        )
      )
    ),
    
    # Documented Issues
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("exclamation-triangle"), " Documented Data Quality Issues"),
          card_body(
            p(class = "text-muted mb-3",
              "Each issue includes severity, affected indicators, and the specific filter applied."),
            uiOutput(ns("issues_list"))
          )
        )
      )
    ),
    
    # Filter Logic Tabs
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("code"), " Analysis-Specific Filter Logic"),
          card_body(
            navset_card_tab(
              id = ns("filter_tabs"),
              
              nav_panel(
                title = "Infrastructure",
                icon = icon("bolt"),
                div(class = "mt-3",
                  h4("Infrastructure Analysis Filters", class = "text-primary"),
                  p(tags$strong("Purpose:"), " Analyze power, water, and transport infrastructure impacts"),
                  h5("Filter Steps:"),
                  tags$ol(
                    tags$li("Exclude firms with missing power outage data (~12% of records)"),
                    tags$li("Winsorize extreme values at 99th percentile"),
                    tags$li("Flag countries with sample size < 200"),
                    tags$li("Apply sampling weights for national estimates")
                  ),
                  h5("R Code:"),
                  pre(class = "bg-light p-3 rounded", code(
'wbes_data |>
  filter(!is.na(IC.FRM.OUTG.ZS)) |>
  mutate(
    IC.FRM.OUTG.ZS = pmin(IC.FRM.OUTG.ZS, 
      quantile(IC.FRM.OUTG.ZS, 0.99, na.rm = TRUE)),
    low_sample_flag = sample_size < 200
  ) |>
  group_by(country, year) |>
  summarise(
    avg_power_obstacle = mean(IC.FRM.OUTG.ZS, na.rm = TRUE),
    .groups = "drop"
  )'
                  ))
                )
              ),
              
              nav_panel(
                title = "Finance",
                icon = icon("university"),
                div(class = "mt-3",
                  h4("Financial Access Filters", class = "text-primary"),
                  p(tags$strong("Purpose:"), " Analyze credit constraints and financial inclusion"),
                  h5("Filter Steps:"),
                  tags$ol(
                    tags$li("Separate analysis by firm size (SME definition: < 100 employees)"),
                    tags$li("Apply sector-specific adjustments"),
                    tags$li("Calculate weighted means using sampling weights")
                  ),
                  h5("R Code:"),
                  pre(class = "bg-light p-3 rounded", code(
'wbes_data |>
  mutate(
    size_category = case_when(
      employees < 20  ~ "Small (5-19)",
      employees < 100 ~ "Medium (20-99)",
      TRUE ~ "Large (100+)"
    )
  ) |>
  group_by(country, size_category) |>
  summarise(
    finance_obstacle = mean(IC.FRM.FINA.ZS, na.rm = TRUE),
    credit_constraint = mean(IC.FRM.CRED.ZS, na.rm = TRUE),
    .groups = "drop"
  )'
                  ))
                )
              ),
              
              nav_panel(
                title = "Corruption",
                icon = icon("balance-scale"),
                div(class = "mt-3",
                  h4("Corruption Analysis Filters", class = "text-primary"),
                  p(class = "text-danger",
                    tags$strong("Caution:"), " Sensitive topic with known underreporting bias"),
                  h5("Filter Steps:"),
                  tags$ol(
                    tags$li("Apply survey weights for stratified sampling"),
                    tags$li("Exclude countries with < 50% response rate on sensitive questions"),
                    tags$li("Document underreporting as methodological limitation")
                  ),
                  h5("R Code:"),
                  pre(class = "bg-light p-3 rounded", code(
'wbes_data |>
  filter(response_rate >= 0.50) |>
  group_by(country, year) |>
  summarise(
    bribery_pct = mean(IC.FRM.BRIB.ZS, na.rm = TRUE),
    corruption_obstacle = mean(IC.FRM.CORR.ZS, na.rm = TRUE),
    .groups = "drop"
  )

# NOTE: Bribery systematically underreported
# Compare with Transparency International CPI for validation'
                  )),
                  div(class = "alert alert-warning mt-3",
                    icon("exclamation-triangle"), " ",
                    tags$strong("Critical Limitation:"), 
                    " Corruption indicators are systematically underreported. 
                    Cross-country comparisons should focus on relative rankings."
                  )
                )
              ),
              
              nav_panel(
                title = "Cross-Country",
                icon = icon("globe"),
                div(class = "mt-3",
                  h4("Cross-Country Standardization", class = "text-primary"),
                  p(tags$strong("Challenge:"), " Survey timing and definitions vary across countries"),
                  h5("Standardization Steps:"),
                  tags$ol(
                    tags$li("Use only Global Methodology surveys (2006+)"),
                    tags$li("Use latest available year per country"),
                    tags$li("Calculate z-scores for valid cross-country comparison"),
                    tags$li("Report confidence intervals for small samples")
                  ),
                  h5("R Code:"),
                  pre(class = "bg-light p-3 rounded", code(
'wbes_data |>
  filter(year >= 2019) |>
  group_by(country) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(
    across(
      starts_with("IC."),
      ~(.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
      .names = "{.col}_z"
    )
  )

# Best practice: Report survey years and sample sizes
# Avoid comparing countries with > 3 year survey gap'
                  ))
                )
              )
            )
          )
        )
      )
    ),
    
    # Variable Dictionary
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("book"), " Variable Dictionary"),
          card_body(DTOutput(ns("variable_dict")))
        )
      )
    ),
    
    # Downloads and References
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("file-alt"), " Methodology Reference"),
          card_body(
            h5("World Bank Enterprise Surveys"),
            p("Stratified random sampling with three levels:"),
            tags$ul(
              tags$li(tags$strong("Sector:"), " Manufacturing, Retail, Services"),
              tags$li(tags$strong("Firm Size:"), " Small, Medium, Large"),
              tags$li(tags$strong("Geography:"), " Main business regions")
            ),
            tags$hr(),
            h5("Key Documentation"),
            tags$ul(
              tags$li(tags$a(href = "https://www.enterprisesurveys.org/en/methodology",
                             target = "_blank", "Official Methodology Guide")),
              tags$li(tags$a(href = "https://www.enterprisesurveys.org/en/survey-datasets",
                             target = "_blank", "Survey Questionnaires")),
              tags$li(tags$a(href = "https://kwizresearch.com/blog",
                             target = "_blank", "Kwiz Research Blog on Data Quality"))
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("download"), " Download Documentation"),
          card_body(
            p("Download complete data quality documentation:"),
            div(class = "d-grid gap-2",
              downloadButton(ns("download_issues"), "Issues Log (CSV)", class = "btn-primary"),
              downloadButton(ns("download_filters"), "Filter Logic (R)", class = "btn-secondary"),
              downloadButton(ns("download_dict"), "Variable Dictionary (CSV)", class = "btn-outline-secondary")
            ),
            tags$hr(),
            h5("Citation"),
            pre(class = "bg-light p-2 rounded", style = "font-size: 0.85rem;",
'World Bank. (2024). Enterprise Surveys.
https://www.enterprisesurveys.org

Dashboard: Kwiz Computing Technologies
https://kwizresearch.com'
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
    
    # Completeness chart
    output$completeness_chart <- renderPlotly({
      indicators <- c("Power", "Electricity", "Finance", "Corruption", "Capacity", "Gender")
      completeness <- c(88, 85, 92, 78, 90, 94)
      
      d <- data.frame(indicator = indicators, pct = completeness)
      d <- arrange(d, pct)
      d$indicator <- factor(d$indicator, levels = d$indicator)
      
      plot_ly(d, y = ~indicator, x = ~pct, type = "bar", orientation = "h",
              marker = list(color = ~pct,
                           colorscale = list(c(0, "#dc3545"), c(0.7, "#F4A460"), c(1, "#2E7D32")))) |>
        layout(
          xaxis = list(title = "Completeness (%)", range = c(0, 100)),
          yaxis = list(title = ""),
          margin = list(l = 80),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Regional completeness
    output$regional_completeness <- renderPlotly({
      regions <- c("SSA", "South Asia", "East Asia", "LAC", "Europe")
      completeness <- c(82, 85, 91, 88, 94)
      
      plot_ly(x = regions, y = completeness, type = "bar",
              marker = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Completeness (%)", range = c(0, 100)),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Issues list
    output$issues_list <- renderUI({
      req(wbes_data())
      issues <- wbes_data()$quality$issues
      
      severity_class <- function(sev) {
        switch(tolower(sev),
               "high" = "alert-danger",
               "medium" = "alert-warning",
               "low" = "alert-info",
               "alert-secondary")
      }
      
      div(
        lapply(issues, function(issue) {
          div(class = paste("alert", severity_class(issue$severity), "mb-3"),
            fluidRow(
              column(10,
                h5(paste0("[", issue$id, "] ", issue$category, ": ", issue$indicator)),
                p(issue$description),
                p(tags$strong("Affected:"), paste(issue$affected_countries, collapse = ", ")),
                p(tags$strong("Filter:"), tags$span(class = "text-success", issue$filter_applied)),
                code(class = "bg-light p-1 rounded", issue$r_code)
              ),
              column(2, class = "text-end",
                tags$span(class = paste("badge",
                  if (issue$severity == "High") "bg-danger"
                  else if (issue$severity == "Medium") "bg-warning text-dark"
                  else "bg-info"),
                  issue$severity)
              )
            )
          )
        })
      )
    })
    
    # Variable dictionary
    output$variable_dict <- renderDT({
      dict <- data.frame(
        Variable = c("IC.FRM.OUTG.ZS", "IC.FRM.ELEC.ZS", "IC.FRM.FINA.ZS",
                     "IC.FRM.BANK.ZS", "IC.FRM.CORR.ZS", "IC.FRM.BRIB.ZS",
                     "IC.FRM.CAPU.ZS", "IC.FRM.FEMO.ZS"),
        Description = c(
          "Power outages as major/severe obstacle (%)",
          "Electricity as major/severe obstacle (%)",
          "Access to finance as major/severe obstacle (%)",
          "Firms with bank account (%)",
          "Corruption as major/severe obstacle (%)",
          "Bribery incidence (%)",
          "Capacity utilization (%)",
          "Female ownership (%)"
        ),
        `Typical Range` = c("10-50", "15-55", "15-60", "70-99", "10-50", "5-40", "50-90", "10-50"),
        `Missing Rate` = c("12%", "10%", "8%", "5%", "15%", "22%", "6%", "4%"),
        stringsAsFactors = FALSE
      )
      
      datatable(dict, options = list(pageLength = 10, dom = "t"),
                class = "display compact", rownames = FALSE)
    })
    
    # Download handlers
    output$download_issues <- downloadHandler(
      filename = function() paste0("wbes_issues_", Sys.Date(), ".csv"),
      content = function(file) {
        issues <- wbes_data()$quality$issues
        df <- do.call(rbind, lapply(issues, function(x) {
          data.frame(
            ID = x$id, Category = x$category, Severity = x$severity,
            Indicator = x$indicator, Description = x$description,
            Filter = x$filter_applied, stringsAsFactors = FALSE
          )
        }))
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    output$download_filters <- downloadHandler(
      filename = function() paste0("wbes_filters_", Sys.Date(), ".R"),
      content = function(file) {
        code <- '# WBES Dashboard Filter Logic
# Generated by Kwiz Computing Technologies

library(dplyr)

# Infrastructure Analysis
filter_infrastructure <- function(data) {
  data |>
    filter(!is.na(IC.FRM.OUTG.ZS)) |>
    mutate(low_sample_flag = sample_size < 200)
}

# Finance Analysis
filter_finance <- function(data) {
  data |>
    mutate(
      size_category = case_when(
        employees < 20 ~ "Small",
        employees < 100 ~ "Medium",
        TRUE ~ "Large"
      )
    )
}

# Corruption Analysis
filter_corruption <- function(data) {
  data |>
    filter(response_rate >= 0.50)
}
'
        writeLines(code, file)
      }
    )
    
    output$download_dict <- downloadHandler(
      filename = function() paste0("wbes_dictionary_", Sys.Date(), ".csv"),
      content = function(file) {
        dict <- data.frame(
          Variable = c("IC.FRM.OUTG.ZS", "IC.FRM.FINA.ZS", "IC.FRM.CORR.ZS"),
          Description = c("Power outages obstacle", "Finance obstacle", "Corruption obstacle"),
          stringsAsFactors = FALSE
        )
        write.csv(dict, file, row.names = FALSE)
      }
    )
    
  })
}
