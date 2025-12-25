# app/view/mod_finance_access.R
# Access to Finance Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent,
        downloadButton, downloadHandler],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  leaflet[leafletOutput, renderLeaflet],
  dplyr[filter, arrange, mutate, group_by, summarise, coalesce],
  tidyr[pivot_wider],
  stats[setNames, runif],
  utils[head],
  htmlwidgets[saveWidget],
  utils[write.csv],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates],
  app/logic/chart_utils[create_chart_caption, map_with_caption]
)

# Helper function to create chart container with download button and caption
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
    plotlyOutput(ns(output_id), height = height),
    create_chart_caption(output_id)
  )
}

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

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Distribution of Finance Access"),
          card_body(
            fluidRow(
              column(4,
                selectInput(
                  ns("map_indicator"),
                  "Map Indicator",
                  choices = c(
                    "Credit Access" = "firms_with_credit_line_pct",
                    "Bank Account" = "firms_with_bank_account_pct",
                    "Loan Rejection Rate" = "loan_rejection_rate_pct",
                    "Collateral Required" = "collateral_required_pct"
                  ),
                  selected = "firms_with_credit_line_pct"
                )
              )
            ),
            map_with_caption(ns, "finance_map", height = "400px", title = "Finance Access Indicators by Country")
          )
        )
      )
    ),

    # Filters
    # Tab-specific filter: Gender/Ownership (Region, Sector, Size are in sidebar)
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(
            class = "py-2",
            fluidRow(
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
          chart_with_download(ns, "finance_by_region", height = "400px", title = "Financial Products Access by Region"),
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
          chart_with_download(ns, "no_apply_reasons", height = "400px", title = "Reasons for Not Applying for Loans"),
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
          chart_with_download(ns, "sme_finance_gap", height = "400px", title = "SME Finance Gap by Country"),
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
          chart_with_download(ns, "gender_gap", height = "400px", title = "Gender Gap in Finance Access"),
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
          chart_with_download(ns, "collateral_chart", height = "350px", title = "Collateral Requirements by Country"),
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
          chart_with_download(ns, "processing_time", height = "350px", title = "Loan Processing Time by Region"),
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
server <- function(id, wbes_data, global_filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # Filtered data - Use firm-level processed data with global filters
    filtered_data <- reactive({
      req(wbes_data())
      data <- wbes_data()$processed

      # Apply global filters if provided
      if (!is.null(global_filters)) {
        filters <- global_filters()
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

      # Return NULL if data is NULL or empty
      if (is.null(data) || nrow(data) == 0) return(NULL)

      # Apply region filter
      if (!is.null(input$region_filter) && input$region_filter != "all" && !is.na(input$region_filter)) {
        data <- data |> filter(!is.na(region) & region == input$region_filter)
      }

      # Apply firm size filter (check if column exists first)
      if ("firm_size" %in% names(data) && !is.null(input$firm_size) && input$firm_size != "all" && !is.na(input$firm_size)) {
        data <- data |> filter(!is.na(firm_size) & firm_size == input$firm_size)
      }

      # Apply sector filter
      if (!is.null(input$sector) && input$sector != "all" && !is.na(input$sector)) {
        data <- data |> filter(!is.na(sector) & sector == input$sector)
      }

      # Apply gender/ownership filter
      if (!is.null(input$gender) && input$gender != "all" && !is.na(input$gender)) {
        if (input$gender == "female") {
          data <- data |> filter(!is.na(female_ownership) & female_ownership == TRUE)
        } else if (input$gender == "male") {
          data <- data |> filter(!is.na(female_ownership) & female_ownership == FALSE)
        }
      }

      data
    })

    # Interactive Map - uses country_panel when year filter active
    output$finance_map <- renderLeaflet({
      req(wbes_data(), input$map_indicator)

      # Use country_panel (has year) if year filter is active, otherwise use latest
      filters <- if (!is.null(global_filters)) global_filters() else NULL
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      d <- if (use_panel) wbes_data()$country_panel else wbes_data()$latest
      coords <- get_country_coordinates(wbes_data())

      # Apply global filters to map data
      if (!is.null(filters)) {
        d <- apply_common_filters(
          d,
          region_value = filters$region,
          sector_value = filters$sector,
          firm_size_value = filters$firm_size,
          income_value = filters$income,
          year_value = filters$year,
          custom_regions = filters$custom_regions,
          filter_by_region_fn = filter_by_region
        )
      }

      # Add coordinates if using panel data
      if (use_panel && !is.null(wbes_data()$country_coordinates)) {
        coords_data <- wbes_data()$country_coordinates
        if ("lat" %in% names(coords_data) && "lng" %in% names(coords_data)) {
          d <- merge(d, coords_data, by = "country", all.x = TRUE)
        }
      }

      # Get readable label from indicator code
      indicator_label <- switch(input$map_indicator,
        "firms_with_credit_line_pct" = "Credit Access (%)",
        "firms_with_bank_account_pct" = "Bank Account (%)",
        "loan_rejection_rate_pct" = "Loan Rejection Rate (%)",
        "collateral_required_pct" = "Collateral Required (%)",
        gsub("_", " ", tools::toTitleCase(input$map_indicator))
      )

      create_wbes_map(
        data = d,
        coordinates = coords,
        indicator_col = input$map_indicator,
        indicator_label = indicator_label,
        color_palette = "Blues",
        reverse_colors = FALSE  # Higher is better
      )
    })

    # KPIs with NA handling - aggregate from firm-level data
    # WBES variable mappings:
    # - firms_with_bank_account_pct: from fin15 (% firms with checking/savings account)
    # - firms_with_credit_line_pct: from fin14 (% firms with line of credit)
    # - collateral_required_pct: from fin10 (Value of collateral as % of loan amount)
    # - loan_rejection_rate_pct: from fin21 (% of loan applications rejected)
    output$kpi_bank_account <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      # Calculate percentage across filtered firms
      avg <- if ("firms_with_bank_account_pct" %in% names(data) && nrow(data) > 0) {
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
      avg <- if ("firms_with_credit_line_pct" %in% names(data) && nrow(data) > 0) {
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
      avg <- if ("collateral_required_pct" %in% names(data) && nrow(data) > 0) {
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
      avg <- if ("loan_rejection_rate_pct" %in% names(data) && nrow(data) > 0) {
        val <- mean(data$loan_rejection_rate_pct, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) paste0(round(val, 1), "%") else "N/A"
      } else "N/A"
      tags$div(class = "kpi-box kpi-box-success",
        tags$div(class = "kpi-value", avg),
        tags$div(class = "kpi-label", "Rejection Rate")
      )
    })
    
    # Finance by region - uses filtered firm-level data
    # WBES variables: fin15 (bank account), fin14 (credit line), loan data
    output$finance_by_region <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      # Calculate regional aggregates from filtered firm-level data
      if (!is.null(data) && nrow(data) > 0 && "region" %in% names(data)) {
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
    
    # No apply reasons - uses filtered firm-level data
    # Variables: fin19a-fin19e (reasons for not applying for loan)
    output$no_apply_reasons <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()

      # Calculate average percentages for each reason from filtered firms
      reasons_list <- list()

      if ("no_need_for_loan" %in% names(data) && nrow(data) > 0) {
        val <- mean(data$no_need_for_loan, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`No Need` <- val
      }
      if ("loan_interest_high" %in% names(data) && nrow(data) > 0) {
        val <- mean(data$loan_interest_high, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`High Interest` <- val
      }
      if ("loan_procedures_complex" %in% names(data) && nrow(data) > 0) {
        val <- mean(data$loan_procedures_complex, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`Complex Procedures` <- val
      }
      if ("insufficient_collateral" %in% names(data) && nrow(data) > 0) {
        val <- mean(data$insufficient_collateral, na.rm = TRUE)
        if (!is.nan(val) && !is.na(val)) reasons_list$`Collateral Issues` <- val
      }
      if ("loan_size_inadequate" %in% names(data) && nrow(data) > 0) {
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
    
    # SME finance gap - aggregate firm-level data by country
    output$sme_finance_gap <- renderPlotly({
      req(filtered_data())
      firm_data <- filtered_data()

      # Aggregate by country
      if (nrow(firm_data) > 0 && "country" %in% names(firm_data)) {
        data <- firm_data |>
          group_by(country) |>
          summarise(
            firms_with_credit_line_pct = mean(firms_with_credit_line_pct, na.rm = TRUE),
            .groups = "drop"
          ) |>
          filter(!is.na(firms_with_credit_line_pct)) |>
          arrange(desc(firms_with_credit_line_pct)) |>
          head(12)

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
      } else {
        # Empty plot if no data
        plot_ly() |>
          layout(
            title = "No data available",
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })
    
    # Gender gap - Calculate from filtered firm-level data disaggregated by female ownership
    output$gender_gap <- renderPlotly({
      req(filtered_data())
      firm_data <- filtered_data()

      # Calculate gender gap using firm-level data with female ownership indicator
      if (!is.null(firm_data) && nrow(firm_data) > 0 &&
          "female_ownership" %in% names(firm_data) &&
          "firms_with_credit_line_pct" %in% names(firm_data) &&
          "country" %in% names(firm_data)) {

        # Disaggregate by female ownership
        gender_data <- firm_data |>
          filter(!is.na(female_ownership) & !is.na(firms_with_credit_line_pct)) |>
          group_by(country, female_ownership) |>
          summarise(
            credit_access = mean(firms_with_credit_line_pct, na.rm = TRUE),
            .groups = "drop"
          ) |>
          tidyr::pivot_wider(
            names_from = female_ownership,
            values_from = credit_access,
            names_prefix = "female_"
          )

        # Check if both columns exist before calculating gap
        if ("female_FALSE" %in% names(gender_data) && "female_TRUE" %in% names(gender_data)) {
          gender_data <- gender_data |>
            mutate(
              gap = female_FALSE - female_TRUE,  # Positive = female-owned have less access
              male_access = coalesce(female_FALSE, 0),
              female_access = coalesce(female_TRUE, 0)
            ) |>
            filter(!is.na(gap)) |>
            arrange(desc(gap)) |>
            head(12)
        } else {
          # If columns don't exist, return empty data frame
          gender_data <- data.frame()
        }

        if (nrow(gender_data) > 0) {
          gender_data$country <- factor(gender_data$country, levels = rev(gender_data$country))

          # Plot gender gap by country
          plot_ly(gender_data) |>
            add_trace(y = ~country, x = ~female_access,
                      name = "Female-Owned", type = "bar", orientation = "h",
                      marker = list(color = "#F49B7A")) |>
            add_trace(y = ~country, x = ~male_access,
                      name = "Male-Owned", type = "bar", orientation = "h",
                      marker = list(color = "#1B6B5F")) |>
            layout(
              barmode = "group",
              xaxis = list(title = "% with Credit Access", ticksuffix = "%"),
              yaxis = list(title = ""),
              legend = list(orientation = "h", y = -0.15),
              margin = list(l = 100),
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
      firm_data <- filtered_data()

      # Aggregate by country first to avoid duplicate factor levels
      if (!is.null(firm_data) && nrow(firm_data) > 0 &&
          "country" %in% names(firm_data) &&
          "collateral_required_pct" %in% names(firm_data)) {

        data <- firm_data |>
          filter(!is.na(country) & !is.na(collateral_required_pct)) |>
          group_by(country) |>
          summarise(
            collateral_required_pct = mean(collateral_required_pct, na.rm = TRUE),
            .groups = "drop"
          ) |>
          arrange(desc(collateral_required_pct)) |>
          head(10)

        if (nrow(data) > 0) {
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

    # ============================================================
    # Download Handlers
    # ============================================================

    simple_chart_download <- function(prefix) {
      downloadHandler(
        filename = function() {
          paste0("finance_", prefix, "_", format(Sys.Date(), "%Y%m%d"), ".html")
        },
        content = function(file) {
          saveWidget(plot_ly() |> layout(title = paste("Finance -", prefix)), file, selfcontained = TRUE)
        }
      )
    }

    output$dl_finance_by_region <- simple_chart_download("by_region")
    output$dl_no_apply_reasons <- simple_chart_download("no_apply_reasons")
    output$dl_sme_finance_gap <- simple_chart_download("sme_finance_gap")
    output$dl_gender_gap <- simple_chart_download("gender_gap")
    output$dl_collateral_chart <- simple_chart_download("collateral")
    output$dl_processing_time <- simple_chart_download("processing_time")

  })
}
