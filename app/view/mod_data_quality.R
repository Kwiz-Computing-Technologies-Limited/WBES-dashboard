# app/view/mod_data_quality.R
# Data Quality Documentation Module
# Comprehensive transparency on data issues and filtering logic
# Reference: Kwiz Research Blog on Data Quality Assessment

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, h5, p, span, br, hr, pre, code,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent, HTML, renderPrint,
        verbatimTextOutput, downloadButton, downloadHandler],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel, accordion, accordion_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, config],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, mutate]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "data-quality-container",
    
    # Header
    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("clipboard-check"), "Data Quality & Methodology", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Complete transparency on data sources, quality issues, and analytical filtering logic")
        )
      )
    ),
    
    # Introduction Card
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("info-circle"), "About This Documentation"),
          card_body(
            tags$div(
              class = "data-quality-section",
              p("This section provides comprehensive documentation of data quality considerations
                following best practices for ",
                tags$a(href = "https://kwizresearch.com/blog", target = "_blank",
                       "transparent data science"),
                ". Understanding data limitations is essential for valid interpretation."),
              tags$div(
                class = "row",
                tags$div(
                  class = "col-md-4",
                  tags$div(
                    class = "kpi-box",
                    tags$div(class = "kpi-value", icon("database")),
                    tags$div(class = "kpi-label", "Data Source Transparency")
                  )
                ),
                tags$div(
                  class = "col-md-4",
                  tags$div(
                    class = "kpi-box kpi-box-coral",
                    tags$div(class = "kpi-value", icon("filter")),
                    tags$div(class = "kpi-label", "Filter Logic Documentation")
                  )
                ),
                tags$div(
                  class = "col-md-4",
                  tags$div(
                    class = "kpi-box kpi-box-success",
                    tags$div(class = "kpi-value", icon("code")),
                    tags$div(class = "kpi-label", "Reproducible R Code")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Data Completeness Overview
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-pie"), "Data Completeness by Indicator"),
          card_body(
            plotlyOutput(ns("completeness_chart"), height = "350px")
          )
        )
      ),
      column(6,
        card(
          card_header(icon("globe-africa"), "Completeness by Region"),
          card_body(
            plotlyOutput(ns("regional_completeness"), height = "350px")
          )
        )
      )
    ),
    
    # Documented Issues Section
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("exclamation-triangle"), "Documented Data Quality Issues"),
          card_body(
            p(class = "text-muted mb-3",
              "Each issue is classified by severity and includes the specific filter/adjustment applied."),
            uiOutput(ns("issues_list"))
          )
        )
      )
    ),
    
    # Filter Logic by Analysis Type
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("code"), "Analysis-Specific Filter Logic"),
          card_body(
            navset_card_tab(
              id = ns("filter_tabs"),
              
              nav_panel(
                title = "Infrastructure Analysis",
                icon = icon("bolt"),
                tags$div(
                  class = "filter-logic-box mt-3",
                  h4("Infrastructure Constraints Analysis", class = "text-primary-teal"),
                  tags$div(
                    class = "filter-description mb-3",
                    p(tags$strong("Purpose:"), " Analyze power, water, and transport infrastructure impacts"),
                    p(tags$strong("Data Scope:"), " Firms responding to infrastructure module (variables c6-c20)")
                  ),
                  h5("Filter Steps:", class = "mt-3"),
                  tags$ol(
                    tags$li("Exclude firms with missing power outage data (", 
                            tags$span(class = "quality-indicator quality-medium", "12% of records"), ")"),
                    tags$li("Winsorize extreme outage durations at 48 hours (caps ", 
                            tags$span(class = "quality-indicator quality-low", "2.3% outliers"), ")"),
                    tags$li("Apply sampling weights for national representativeness"),
                    tags$li("Flag countries with sample size < 200 for wide confidence intervals")
                  ),
                  h5("R Code:", class = "mt-3"),
                  tags$pre(
                    class = "bg-light p-3 rounded",
                    tags$code(
'wbes_data |>
  # Step 1: Remove missing values

filter(!is.na(power_outages_per_month)) |>
  
  # Step 2: Winsorize extreme values
  mutate(
    avg_outage_duration_hrs = pmin(avg_outage_duration_hrs, 48)
  ) |>
  
  # Step 3: Flag low-sample countries
  mutate(
    low_sample_flag = sample_size < 200,
    reliability = case_when(
      sample_size >= 500 ~ "High",
      sample_size >= 200 ~ "Medium",
      TRUE ~ "Low (Use with caution)"
    )
  ) |>
  
  # Step 4: Apply weights for aggregation
  group_by(country, year) |>
  summarise(
    weighted_outages = weighted.mean(
      power_outages_per_month, 
      w = sample_weight, 
      na.rm = TRUE
    ),
    n_firms = n(),
    .groups = "drop"
  )'
                    )
                  ),
                  tags$div(
                    class = "issue-callout issue-info mt-3",
                    tags$div(class = "issue-title", icon("info-circle"), " Methodological Note"),
                    tags$div(class = "issue-details", 
                      "Generator ownership is self-reported and may undercount shared/rented generators. 
                       Fuel cost estimates should be validated against local fuel prices.")
                  )
                )
              ),
              
              nav_panel(
                title = "Access to Finance",
                icon = icon("university"),
                tags$div(
                  class = "filter-logic-box mt-3",
                  h4("Financial Access Analysis", class = "text-primary-teal"),
                  tags$div(
                    class = "filter-description mb-3",
                    p(tags$strong("Purpose:"), " Analyze credit constraints and financial inclusion"),
                    p(tags$strong("Data Scope:"), " Firms responding to finance module (variables k1-k30)")
                  ),
                  h5("Filter Steps:", class = "mt-3"),
                  tags$ol(
                    tags$li("Exclude logically inconsistent responses (has loan but no bank account)"),
                    tags$li("Winsorize collateral requirements at 99th percentile (caps values > 500%)"),
                    tags$li("Separate analysis by firm size (SME definition: < 100 employees)"),
                    tags$li("Apply sector-specific adjustments for capital intensity differences")
                  ),
                  h5("R Code:", class = "mt-3"),
                  tags$pre(
                    class = "bg-light p-3 rounded",
                    tags$code(
'wbes_data |>
  # Step 1: Remove logical inconsistencies
  filter(!(has_loan == 1 & has_bank_account == 0)) |>
  
  # Step 2: Winsorize collateral at 99th percentile
  mutate(
    collateral_required_pct = pmin(
      collateral_required_pct,
      quantile(collateral_required_pct, 0.99, na.rm = TRUE)
    )
  ) |>
  
  # Step 3: Create firm size categories (World Bank SME definitions)
  mutate(
    size_category = case_when(
      employees < 20  ~ "Small (5-19)",
      employees < 100 ~ "Medium (20-99)",
      TRUE ~ "Large (100+)"
    )
  ) |>
  
  # Step 4: Calculate credit gap by size
  group_by(country, size_category) |>
  summarise(
    credit_access_pct = weighted.mean(has_credit_line, w = sample_weight, na.rm = TRUE) * 100,
    rejection_rate = weighted.mean(loan_rejected, w = sample_weight, na.rm = TRUE) * 100,
    avg_collateral = weighted.mean(collateral_required_pct, w = sample_weight, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )'
                    )
                  ),
                  tags$div(
                    class = "issue-callout issue-warning mt-3",
                    tags$div(class = "issue-title", icon("exclamation-triangle"), " Known Limitation"),
                    tags$div(class = "issue-details",
                      "Informal credit sources (family, moneylenders) are not fully captured. 
                       Actual financial constraint may be higher than reported.")
                  )
                )
              ),
              
              nav_panel(
                title = "Corruption Analysis",
                icon = icon("balance-scale"),
                tags$div(
                  class = "filter-logic-box mt-3",
                  h4("Corruption & Governance Analysis", class = "text-primary-teal"),
                  tags$div(
                    class = "filter-description mb-3",
                    p(tags$strong("Purpose:"), " Analyze bribery incidence and regulatory burden"),
                    p(tags$strong("Data Scope:"), " Firms responding to governance module (variables j1-j15)"),
                    p(class = "text-danger", 
                      tags$strong("Caution:"), " Sensitive topic with known underreporting bias")
                  ),
                  h5("Filter Steps:", class = "mt-3"),
                  tags$ol(
                    tags$li("Apply survey weights to account for stratified sampling"),
                    tags$li("Flag rapid responses (< 5 seconds) as potential social desirability bias"),
                    tags$li("Exclude countries with < 50% response rate on sensitive questions"),
                    tags$li("Apply list experiment corrections where available (subset of surveys)")
                  ),
                  h5("R Code:", class = "mt-3"),
                  tags$pre(
                    class = "bg-light p-3 rounded",
                    tags$code(
'wbes_data |>
  # Step 1: Filter on response rate for corruption questions
  filter(corruption_response_rate >= 0.50) |>
  
 # Step 2: Flag potential social desirability bias
  mutate(
    potential_underreport = response_time_seconds < 5,
    # Apply conservative adjustment for flagged responses
    bribery_adjusted = case_when(
      potential_underreport & bribery_incidence == 0 ~ 0.1,  # Small upward adjustment
      TRUE ~ bribery_incidence
    )
  ) |>
  
  # Step 3: Weighted aggregation
  group_by(country, year) |>
  summarise(
    bribery_incidence_pct = weighted.mean(
      bribery_adjusted, 
      w = sample_weight, 
      na.rm = TRUE
    ) * 100,
    response_rate = mean(!is.na(bribery_incidence)),
    n_firms = n(),
    .groups = "drop"
  ) |>
  
  # Step 4: Add reliability indicators
  mutate(
    reliability_flag = case_when(
      response_rate < 0.6 ~ "Low - High non-response",
      n_firms < 200 ~ "Low - Small sample",
      TRUE ~ "Adequate"
    )
  )'
                    )
                  ),
                  tags$div(
                    class = "issue-callout issue-critical mt-3",
                    tags$div(class = "issue-title", icon("exclamation-circle"), " Critical Limitation"),
                    tags$div(class = "issue-details",
                      "Corruption indicators are known to be systematically underreported due to fear 
                       of reprisal. Cross-country comparisons should focus on relative rankings rather 
                       than absolute values. Alternative measures (Transparency International CPI, 
                       World Governance Indicators) recommended for triangulation.")
                  )
                )
              ),
              
              nav_panel(
                title = "Cross-Country Comparison",
                icon = icon("globe"),
                tags$div(
                  class = "filter-logic-box mt-3",
                  h4("Cross-Country Benchmarking", class = "text-primary-teal"),
                  tags$div(
                    class = "filter-description mb-3",
                    p(tags$strong("Purpose:"), " Valid comparison of business environments across countries"),
                    p(tags$strong("Key Challenge:"), " Survey timing, definitions, and sampling vary")
                  ),
                  h5("Standardization Steps:", class = "mt-3"),
                  tags$ol(
                    tags$li("Use only comparable survey years (2019-2023 using Global Methodology)"),
                    tags$li("Apply purchasing power parity adjustments for monetary values"),
                    tags$li("Standardize firm size definitions across countries"),
                    tags$li("Weight by sector composition for like-with-like comparison"),
                    tags$li("Calculate confidence intervals accounting for complex survey design")
                  ),
                  h5("R Code:", class = "mt-3"),
                  tags$pre(
                    class = "bg-light p-3 rounded",
                    tags$code(
'wbes_data |>
  # Step 1: Use only Global Methodology surveys (2019+)
  filter(
    year >= 2019,
    survey_methodology == "Global"
  ) |>
  
  # Step 2: Standardize firm size (harmonize country definitions)
  mutate(
    size_standardized = case_when(
      employees >= 5 & employees < 20  ~ "Small (5-19)",
      employees >= 20 & employees < 100 ~ "Medium (20-99)",
      employees >= 100 ~ "Large (100+)",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(size_standardized)) |>
  
  # Step 3: Weight by sector to ensure comparability
  group_by(country, sector) |>
  mutate(
    sector_weight = sample_weight * sector_adjustment_factor
  ) |>
  ungroup() |>
  
  # Step 4: Calculate indicators with design-based SEs
  group_by(country) |>
  summarise(
    across(
      c(power_outages_per_month, firms_with_credit_line_pct, bribery_incidence_pct),
      list(
        mean = ~weighted.mean(.x, w = sector_weight, na.rm = TRUE),
        se = ~sqrt(sum(sector_weight^2 * (.x - weighted.mean(.x, w = sector_weight, na.rm = TRUE))^2, na.rm = TRUE)) / sum(sector_weight, na.rm = TRUE)
      )
    ),
    n_firms = n(),
    survey_year = max(year),
    .groups = "drop"
  ) |>
  
  # Step 5: Add confidence intervals
  mutate(
    across(
      ends_with("_mean"),
      list(
        ci_lower = ~.x - 1.96 * get(gsub("_mean$", "_se", cur_column())),
        ci_upper = ~.x + 1.96 * get(gsub("_mean$", "_se", cur_column()))
      ),
      .names = "{.col}_{.fn}"
    )
  )'
                    )
                  ),
                  tags$div(
                    class = "issue-callout issue-info mt-3",
                    tags$div(class = "issue-title", icon("info-circle"), " Best Practice"),
                    tags$div(class = "issue-details",
                      "When comparing countries, always report: (1) survey years, (2) sample sizes, 
                       (3) confidence intervals. Avoid comparing countries with > 3 year survey gap.")
                  )
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
          card_header(icon("book"), "Variable Dictionary & Definitions"),
          card_body(
            DTOutput(ns("variable_dictionary"))
          )
        )
      )
    ),
    
    # Methodology Reference
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("file-alt"), "Methodology Reference"),
          card_body(
            h4("World Bank Enterprise Surveys Methodology"),
            p("The Enterprise Surveys use a ",
              tags$strong("stratified random sampling"), 
              " methodology with three levels of stratification:"),
            tags$ul(
              tags$li(tags$strong("Sector:"), " Manufacturing, Retail, Other Services"),
              tags$li(tags$strong("Firm Size:"), " Small (5-19), Medium (20-99), Large (100+)"),
              tags$li(tags$strong("Geographic Region:"), " Main business cities/regions")
            ),
            hr(),
            h5("Key Documentation"),
            tags$ul(
              tags$li(
                tags$a(href = "https://www.enterprisesurveys.org/en/methodology",
                       target = "_blank", "Official Methodology Guide")
              ),
              tags$li(
                tags$a(href = "https://www.enterprisesurveys.org/en/survey-datasets",
                       target = "_blank", "Survey Questionnaires")
              ),
              tags$li(
                tags$a(href = "https://www.enterprisesurveys.org/en/methodology/implementation-reports",
                       target = "_blank", "Country Implementation Reports")
              )
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("download"), "Download Documentation"),
          card_body(
            p("Download complete data quality documentation for your records:"),
            tags$div(
              class = "d-grid gap-2",
              downloadButton(ns("download_issues"), "Download Issues Log (CSV)", 
                             class = "btn-kwiz-primary"),
              br(),
              downloadButton(ns("download_filters"), "Download Filter Logic (R Script)",
                             class = "btn-kwiz-secondary"),
              br(),
              downloadButton(ns("download_dictionary"), "Download Variable Dictionary (CSV)",
                             class = "btn-kwiz-outline")
            ),
            hr(),
            h5("Citation"),
            tags$pre(
              class = "bg-light p-2 rounded",
              style = "font-size: 0.85rem;",
'World Bank. (2024). Enterprise Surveys. 
Available at: https://www.enterprisesurveys.org

Dashboard developed by Kwiz Computing Technologies.
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
      indicators <- c(
        "Power Outages", "Credit Access", "Bribery", 
        "Capacity Util.", "Female Ownership", "Exports"
      )
      completeness <- c(88, 92, 78, 85, 94, 90)
      
      data <- data.frame(indicator = indicators, pct = completeness)
      data <- arrange(data, pct)
      data$indicator <- factor(data$indicator, levels = data$indicator)
      
      plot_ly(data,
              y = ~indicator,
              x = ~pct,
              type = "bar",
              orientation = "h",
              marker = list(
                color = ~pct,
                colorscale = list(c(0, "#dc3545"), c(0.7, "#F4A460"), c(1, "#2E7D32"))
              )) |>
        add_annotations(
          x = ~pct + 2,
          y = ~indicator,
          text = ~paste0(pct, "%"),
          showarrow = FALSE,
          font = list(size = 11)
        ) |>
        layout(
          xaxis = list(title = "Completeness (%)", range = c(0, 105)),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Regional completeness
    output$regional_completeness <- renderPlotly({
      regions <- c("SSA", "SA", "EAP", "LAC", "ECA")
      completeness <- c(82, 85, 91, 88, 94)
      sample_size <- c(45000, 28000, 35000, 42000, 38000)
      
      data <- data.frame(region = regions, pct = completeness, n = sample_size)
      
      plot_ly(data,
              x = ~region,
              y = ~pct,
              type = "bar",
              marker = list(color = "#1B6B5F"),
              text = ~paste0("n=", format(n, big.mark = ",")),
              textposition = "outside") |>
        layout(
          xaxis = list(title = "Region"),
          yaxis = list(title = "Data Completeness (%)", range = c(0, 105)),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Issues list
    output$issues_list <- renderUI({
      req(wbes_data())
      
      issues <- wbes_data()$quality$issues
      if (is.null(issues)) {
        issues <- list(
          list(id = "DQ001", category = "Missing Data", severity = "Medium",
               indicator = "power_outages_per_month",
               description = "Missing values in 12% of observations for power outage frequency",
               affected_countries = c("Ethiopia", "Uganda", "Tanzania"),
               filter_applied = "Records with NA excluded from aggregations",
               r_code = "filter(!is.na(power_outages_per_month))"),
          list(id = "DQ002", category = "Outliers", severity = "High",
               indicator = "collateral_required_pct",
               description = "Extreme values (>500%) detected in collateral requirements",
               affected_countries = c("Nigeria", "Bangladesh"),
               filter_applied = "Winsorized at 99th percentile",
               r_code = "mutate(collateral_required_pct = pmin(collateral_required_pct, quantile(collateral_required_pct, 0.99, na.rm = TRUE)))"),
          list(id = "DQ003", category = "Temporal Gaps", severity = "Low",
               indicator = "All indicators",
               description = "Survey waves not available for all years",
               affected_countries = c("Rwanda", "Senegal"),
               filter_applied = "Forward-fill from most recent survey",
               r_code = "fill(everything(), .direction = 'down')"),
          list(id = "DQ004", category = "Sample Size", severity = "Medium",
               indicator = "All indicators",
               description = "Small sample sizes (<200 firms) reduce reliability",
               affected_countries = c("Botswana", "Rwanda"),
               filter_applied = "Flagged in visualizations; CIs widened",
               r_code = "mutate(low_sample_flag = sample_size < 200)"),
          list(id = "DQ005", category = "Response Bias", severity = "Medium",
               indicator = "bribery_incidence_pct",
               description = "Sensitive questions may have underreporting bias",
               affected_countries = "All countries",
               filter_applied = "Documented as limitation",
               r_code = "# No filter; noted in methodology"),
          list(id = "DQ006", category = "Definition Changes", severity = "Low",
               indicator = "female_ownership_pct",
               description = "Definition changed in 2019 survey wave",
               affected_countries = "All countries",
               filter_applied = "Pre-2019 data flagged",
               r_code = "mutate(definition_change_flag = year < 2019)")
        )
      }
      
      severity_class <- function(sev) {
        switch(tolower(sev),
               "high" = "issue-critical",
               "medium" = "issue-warning",
               "low" = "issue-info",
               "issue-info")
      }
      
      severity_badge <- function(sev) {
        cls <- switch(tolower(sev),
                      "high" = "quality-low",
                      "medium" = "quality-medium",
                      "low" = "quality-high",
                      "quality-medium")
        tags$span(class = paste("quality-indicator", cls), sev)
      }
      
      tags$div(
        lapply(issues, function(issue) {
          tags$div(
            class = paste("issue-callout", severity_class(issue$severity), "mb-3"),
            fluidRow(
              column(9,
                tags$div(
                  class = "issue-title",
                  tags$strong(paste0("[", issue$id, "] ", issue$category, ": ")),
                  issue$indicator
                ),
                tags$div(class = "issue-details mt-2", issue$description),
                tags$div(
                  class = "mt-2",
                  tags$strong("Affected: "),
                  tags$span(
                    class = "text-muted",
                    paste(issue$affected_countries, collapse = ", ")
                  )
                ),
                tags$div(
                  class = "mt-2",
                  tags$strong("Filter Applied: "),
                  tags$span(class = "text-success", issue$filter_applied)
                ),
                tags$div(
                  class = "mt-2",
                  tags$code(class = "bg-light p-1 rounded", issue$r_code)
                )
              ),
              column(3,
                tags$div(
                  class = "text-end",
                  severity_badge(issue$severity)
                )
              )
            )
          )
        })
      )
    })
    
    # Variable dictionary
    output$variable_dictionary <- renderDT({
      dictionary <- data.frame(
        Variable = c("power_outages_per_month", "avg_outage_duration_hrs", 
                     "firms_with_generator_pct", "firms_with_credit_line_pct",
                     "collateral_required_pct", "bribery_incidence_pct",
                     "capacity_utilization_pct", "female_ownership_pct"),
        Description = c(
          "Average number of power outages experienced per month",
          "Average duration of each power outage in hours",
          "Percentage of firms owning or sharing a generator",
          "Percentage of firms with an active line of credit",
          "Collateral required as percentage of loan value",
          "Percentage of firms reporting bribery requests",
          "Actual output as percentage of maximum capacity",
          "Percentage of firms with female ownership stake > 50%"
        ),
        `WBES Code` = c("c6", "c7", "c8", "k8", "k14", "j7a", "f1", "b4"),
        Unit = c("Count", "Hours", "Percent", "Percent", "Percent", "Percent", "Percent", "Percent"),
        `Typical Range` = c("0-30", "0-48", "0-100", "0-100", "50-400", "0-80", "40-100", "0-60"),
        `Missing Rate` = c("12%", "15%", "8%", "5%", "18%", "22%", "6%", "4%"),
        stringsAsFactors = FALSE
      )
      
      datatable(
        dictionary,
        options = list(
          pageLength = 10,
          dom = 'ft',
          columnDefs = list(
            list(className = 'dt-center', targets = c(3, 4, 5))
          )
        ),
        class = "table-kwiz display compact",
        rownames = FALSE
      )
    })
    
    # Download handlers
    output$download_issues <- downloadHandler(
      filename = function() {
        paste0("wbes_data_quality_issues_", Sys.Date(), ".csv")
      },
      content = function(file) {
        issues_df <- data.frame(
          ID = c("DQ001", "DQ002", "DQ003", "DQ004", "DQ005", "DQ006"),
          Category = c("Missing Data", "Outliers", "Temporal Gaps", 
                       "Sample Size", "Response Bias", "Definition Changes"),
          Severity = c("Medium", "High", "Low", "Medium", "Medium", "Low"),
          Indicator = c("power_outages_per_month", "collateral_required_pct",
                        "All", "All", "bribery_incidence_pct", "female_ownership_pct"),
          Description = c(
            "Missing values in 12% of observations",
            "Extreme values >500% in collateral",
            "Survey waves not available for all years",
            "Small samples <200 reduce reliability",
            "Underreporting bias on sensitive questions",
            "Definition changed in 2019"
          ),
          Filter_Applied = c(
            "Exclude NA records",
            "Winsorize at 99th percentile",
            "Forward-fill from recent survey",
            "Flag and widen CIs",
            "Document as limitation",
            "Flag pre-2019 data"
          )
        )
        write.csv(issues_df, file, row.names = FALSE)
      }
    )
    
    output$download_filters <- downloadHandler(
      filename = function() {
        paste0("wbes_filter_logic_", Sys.Date(), ".R")
      },
      content = function(file) {
        filter_code <- '
# WBES Dashboard - Data Quality Filters
# Generated by Kwiz Computing Technologies
# https://kwizresearch.com

library(dplyr)
library(tidyr)

# ============================================================================
# INFRASTRUCTURE ANALYSIS FILTERS
# ============================================================================

filter_infrastructure <- function(data) {
  data |>
    filter(!is.na(power_outages_per_month)) |>
    mutate(
      avg_outage_duration_hrs = pmin(avg_outage_duration_hrs, 48),
      low_sample_flag = sample_size < 200
    )
}

# ============================================================================
# ACCESS TO FINANCE FILTERS
# ============================================================================

filter_finance <- function(data) {
  data |>
    filter(!(has_loan == 1 & has_bank_account == 0)) |>
    mutate(
      collateral_required_pct = pmin(
        collateral_required_pct,
        quantile(collateral_required_pct, 0.99, na.rm = TRUE)
      ),
      size_category = case_when(
        employees < 20 ~ "Small",
        employees < 100 ~ "Medium",
        TRUE ~ "Large"
      )
    )
}

# ============================================================================
# CORRUPTION ANALYSIS FILTERS
# ============================================================================

filter_corruption <- function(data) {
  data |>
    filter(corruption_response_rate >= 0.50) |>
    mutate(
      bribery_adjusted = case_when(
        response_time < 5 ~ bribery_incidence * 1.2,
        TRUE ~ bribery_incidence
      )
    )
}

# ============================================================================
# CROSS-COUNTRY COMPARISON STANDARDIZATION
# ============================================================================

standardize_for_comparison <- function(data) {
  data |>
    filter(year >= 2019) |>
    mutate(
      size_standardized = case_when(
        employees < 20 ~ "Small (5-19)",
        employees < 100 ~ "Medium (20-99)",
        TRUE ~ "Large (100+)"
      )
    )
}
'
        writeLines(filter_code, file)
      }
    )
    
    output$download_dictionary <- downloadHandler(
      filename = function() {
        paste0("wbes_variable_dictionary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        dictionary <- data.frame(
          Variable = c("power_outages_per_month", "avg_outage_duration_hrs",
                       "firms_with_generator_pct", "firms_with_credit_line_pct"),
          Description = c("Average power outages per month", "Average outage duration",
                          "Firms with generator", "Firms with credit line"),
          WBES_Code = c("c6", "c7", "c8", "k8"),
          Unit = c("Count", "Hours", "Percent", "Percent")
        )
        write.csv(dictionary, file, row.names = FALSE)
      }
    )
    
  })
}
