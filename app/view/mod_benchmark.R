# app/view/mod_benchmark.R
# Cross-Country Benchmarking Module with Domain Sub-menus

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, p,
        fluidRow, column, selectInput, selectizeInput, renderUI, uiOutput,
        observeEvent, actionButton, HTML, downloadButton, downloadHandler, tagList,
        withProgress, incProgress],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config, subplot],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, mutate, desc, group_by, summarise, n, across, any_of, bind_rows, left_join],
  leaflet[leafletOutput, renderLeaflet],
  stats[setNames, reorder],
  htmlwidgets[saveWidget],
  utils[write.csv],
  rlang[sym],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates],
  app/logic/scatter_utils[create_scatter_with_trend],
  app/logic/chart_utils[create_chart_caption, map_with_caption, generate_chart_id],
  app/logic/wb_integration[
    get_wb_country_context,
    get_wb_context_from_cache,
    map_wbes_countries_to_iso3,
    format_wb_indicator,
    get_wb_indicator_label,
    get_wb_databank_indicators
  ]
)

# Define indicator domains with their indicators
DOMAINS <- list(
  infrastructure = list(
    name = "Infrastructure",
    icon = "bolt",
    indicators = c(
      "Power Outages (per month)" = "power_outages_per_month",
      "Avg Outage Duration (hrs)" = "avg_outage_duration_hrs",
      "Firms with Generator (%)" = "firms_with_generator_pct",
      "Water Insufficiency (%)" = "water_insufficiency_pct"
    )
  ),
  finance = list(
    name = "Access to Finance",
    icon = "university",
    indicators = c(
      "Firms with Credit Line (%)" = "firms_with_credit_line_pct",
      "Firms with Bank Account (%)" = "firms_with_bank_account_pct",
      "Loan Rejection Rate (%)" = "loan_rejection_rate_pct",
      "Collateral Required (%)" = "collateral_required_pct"
    )
  ),
  governance = list(
    name = "Governance & Corruption",
    icon = "balance-scale",
    indicators = c(
      "Bribery Incidence (%)" = "IC.FRM.BRIB.ZS",
      "Corruption Obstacle (%)" = "IC.FRM.CORR.ZS",
      "Mgmt Time on Regulations (%)" = "mgmt_time_regulations_pct"
    )
  ),
  workforce = list(
    name = "Workforce & Gender",
    icon = "users",
    indicators = c(
      "Female Ownership (%)" = "female_ownership_pct",
      "Female Workers (%)" = "female_workers_pct",
      "Workforce Obstacle (%)" = "workforce_obstacle_pct"
    )
  ),
  performance = list(
    name = "Performance & Trade",
    icon = "chart-line",
    indicators = c(
      "Capacity Utilization (%)" = "capacity_utilization_pct",
      "Export Firms (%)" = "export_firms_pct",
      "Export Share (%)" = "export_share_pct",
      "Annual Sales Growth (%)" = "annual_sales_growth_pct"
    )
  ),
  crime = list(
    name = "Crime & Security",
    icon = "shield-alt",
    indicators = c(
      "Crime Obstacle (%)" = "crime_obstacle_pct",
      "Security Costs (%)" = "security_costs_pct"
    )
  )
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

# Helper function for table with download
table_with_download <- function(ns, output_id, title = NULL) {
  div(
    class = "position-relative",
    if (!is.null(title)) h4(title, class = "text-primary-teal mb-2"),
    div(
      class = "mb-2",
      downloadButton(
        ns(paste0("dl_", output_id)),
        label = "Download CSV",
        icon = icon("download"),
        class = "btn-sm btn-outline-secondary"
      )
    ),
    DTOutput(ns(output_id))
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "benchmark-container",

    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("chart-bar"), "Cross-Country Benchmarking", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Compare business environment indicators across multiple countries and regions")
        )
      )
    ),

    # Selection Panel
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("sliders-h"), "Comparison Settings"),
          card_body(
            fluidRow(
              column(4,
                selectizeInput(
                  ns("countries_compare"),
                  "Select Countries to Compare",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(maxItems = 10, placeholder = "Choose countries...")
                )
              ),
              column(2,
                selectInput(
                  ns("group_dimension"),
                  "Group By (3rd Dim)",
                  choices = c(
                    "None" = "none",
                    "Region" = "region",
                    "Firm Size" = "firm_size",
                    "Income Group" = "income"
                  ),
                  selected = "none"
                )
              ),
              column(2,
                selectInput(
                  ns("sort_order"),
                  "Sort Order",
                  choices = c("Descending" = "desc", "Ascending" = "asc")
                )
              ),
              column(2,
                selectInput(
                  ns("chart_type"),
                  "Chart Type",
                  choices = c("Bar Chart" = "bar", "Radar Chart" = "radar", "Heatmap" = "heatmap")
                )
              )
            )
          )
        )
      )
    ),

    # Domain Tabs
    fluidRow(
      column(12,
        navset_card_tab(
          id = ns("domain_tabs"),
          # Overview Tab
          nav_panel(
            title = tags$span(icon("th-large"), " Overview"),
            value = "overview",
            fluidRow(
              class = "mt-3",
              column(12,
                h4("Cross-Domain Comparison", class = "text-primary-teal mb-3"),
                p(class = "text-muted", "Summary of key indicators across all domains for selected countries")
              )
            ),
            fluidRow(
              column(12, chart_with_download(ns, "overview_heatmap", height = "500px", title = "Multi-Indicator Comparison Heatmap"))
            ),
            fluidRow(class = "mt-4", column(12, table_with_download(ns, "overview_table")))
          ),

          # Infrastructure Tab
          nav_panel(
            title = tags$span(icon("bolt"), " Infrastructure"),
            value = "infrastructure",
            fluidRow(class = "mt-3 mb-3",
              column(3, uiOutput(ns("infra_kpi_outages"))),
              column(3, uiOutput(ns("infra_kpi_duration"))),
              column(3, uiOutput(ns("infra_kpi_generator"))),
              column(3, uiOutput(ns("infra_kpi_water")))
            ),
            fluidRow(
              column(8, chart_with_download(ns, "infra_comparison", title = "Infrastructure Comparison")),
              column(4, chart_with_download(ns, "infra_radar", title = "Infrastructure Radar"))
            ),
            fluidRow(class = "mt-3",
              column(4, selectInput(ns("infra_map_indicator"), "Map Indicator",
                choices = c("Power Outages/Month" = "power_outages_per_month",
                            "Outage Duration (hrs)" = "avg_outage_duration_hrs",
                            "Firms with Generator (%)" = "firms_with_generator_pct",
                            "Water Insufficiency (%)" = "water_insufficiency_pct"))),
              column(12, map_with_caption(ns, "infra_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "infra_outage_impact", height = "350px", title = "Outage Impact Analysis")),
              column(6, chart_with_download(ns, "infra_generator_correlation", height = "350px", title = "Generator Correlation"))
            ),
            fluidRow(class = "mt-3", column(12, card(card_header(icon("lightbulb"), " Infrastructure Insights"), card_body(uiOutput(ns("infra_insights"))))))
          ),

          # Finance Tab
          nav_panel(
            title = tags$span(icon("university"), " Finance"),
            value = "finance",
            fluidRow(class = "mt-3 mb-3",
              column(3, uiOutput(ns("finance_kpi_credit"))),
              column(3, uiOutput(ns("finance_kpi_bank"))),
              column(3, uiOutput(ns("finance_kpi_rejection"))),
              column(3, uiOutput(ns("finance_kpi_collateral")))
            ),
            fluidRow(
              column(8, chart_with_download(ns, "finance_comparison", title = "Finance Access Comparison")),
              column(4, chart_with_download(ns, "finance_radar", title = "Finance Access Radar"))
            ),
            fluidRow(class = "mt-3",
              column(4, selectInput(ns("finance_map_indicator"), "Map Indicator",
                choices = c("Credit Access (%)" = "firms_with_credit_line_pct",
                            "Bank Account (%)" = "firms_with_bank_account_pct",
                            "Loan Rejection (%)" = "loan_rejection_rate_pct",
                            "Collateral Required (%)" = "collateral_required_pct"))),
              column(12, map_with_caption(ns, "finance_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "finance_access_gap", height = "350px", title = "Finance Access Gap")),
              column(6, chart_with_download(ns, "finance_collateral_burden", height = "350px", title = "Collateral Burden"))
            ),
            fluidRow(class = "mt-3", column(12, card(card_header(icon("lightbulb"), " Finance Insights"), card_body(uiOutput(ns("finance_insights"))))))
          ),

          # Governance Tab
          nav_panel(
            title = tags$span(icon("balance-scale"), " Governance"),
            value = "governance",
            fluidRow(class = "mt-3 mb-3",
              column(4, uiOutput(ns("governance_kpi_bribery"))),
              column(4, uiOutput(ns("governance_kpi_corruption"))),
              column(4, uiOutput(ns("governance_kpi_regulations")))
            ),
            fluidRow(
              column(8, chart_with_download(ns, "governance_comparison", title = "Governance Comparison")),
              column(4, chart_with_download(ns, "governance_radar", title = "Governance Radar"))
            ),
            fluidRow(class = "mt-3",
              column(4, selectInput(ns("governance_map_indicator"), "Map Indicator",
                choices = c("Bribery Incidence (%)" = "bribery_incidence_pct",
                            "Corruption Obstacle (%)" = "corruption_obstacle_pct",
                            "Mgmt Time on Regulations (%)" = "mgmt_time_regulations_pct"))),
              column(12, map_with_caption(ns, "governance_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "governance_bribery_vs_corruption", height = "350px", title = "Bribery vs Corruption")),
              column(6, chart_with_download(ns, "governance_regulatory_burden", height = "350px", title = "Regulatory Burden"))
            ),
            fluidRow(class = "mt-3", column(12, card(card_header(icon("lightbulb"), " Governance Insights"), card_body(uiOutput(ns("governance_insights"))))))
          ),

          # Workforce Tab
          nav_panel(
            title = tags$span(icon("users"), " Workforce"),
            value = "workforce",
            fluidRow(class = "mt-3 mb-3",
              column(4, uiOutput(ns("workforce_kpi_female_own"))),
              column(4, uiOutput(ns("workforce_kpi_female_work"))),
              column(4, uiOutput(ns("workforce_kpi_obstacle")))
            ),
            fluidRow(
              column(8, chart_with_download(ns, "workforce_comparison", title = "Workforce Comparison")),
              column(4, chart_with_download(ns, "workforce_radar", title = "Workforce Radar"))
            ),
            fluidRow(class = "mt-3",
              column(4, selectInput(ns("workforce_map_indicator"), "Map Indicator",
                choices = c("Female Ownership (%)" = "female_ownership_pct",
                            "Female Workers (%)" = "female_workers_pct",
                            "Workforce Obstacle (%)" = "workforce_obstacle_pct"))),
              column(12, map_with_caption(ns, "workforce_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "workforce_gender_gap", height = "350px", title = "Gender Gap Analysis")),
              column(6, chart_with_download(ns, "workforce_obstacle_correlation", height = "350px", title = "Workforce Obstacle Correlation"))
            ),
            fluidRow(class = "mt-3", column(12, card(card_header(icon("lightbulb"), " Workforce Insights"), card_body(uiOutput(ns("workforce_insights"))))))
          ),

          # Performance Tab
          nav_panel(
            title = tags$span(icon("chart-line"), " Performance"),
            value = "performance",
            fluidRow(class = "mt-3 mb-3",
              column(3, uiOutput(ns("performance_kpi_capacity"))),
              column(3, uiOutput(ns("performance_kpi_export_firms"))),
              column(3, uiOutput(ns("performance_kpi_export_share"))),
              column(3, uiOutput(ns("performance_kpi_growth")))
            ),
            fluidRow(
              column(8, chart_with_download(ns, "performance_comparison", title = "Performance Comparison")),
              column(4, chart_with_download(ns, "performance_radar", title = "Performance Radar"))
            ),
            fluidRow(class = "mt-3",
              column(4, selectInput(ns("performance_map_indicator"), "Map Indicator",
                choices = c("Capacity Utilization (%)" = "capacity_utilization_pct",
                            "Export Firms (%)" = "export_firms_pct",
                            "Export Share (%)" = "export_share_pct",
                            "Sales Growth (%)" = "annual_sales_growth_pct"))),
              column(12, map_with_caption(ns, "performance_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "performance_capacity_vs_exports", height = "350px", title = "Capacity vs Exports")),
              column(6, chart_with_download(ns, "performance_growth_distribution", height = "350px", title = "Growth Distribution"))
            ),
            fluidRow(class = "mt-3", column(12, card(card_header(icon("lightbulb"), " Performance Insights"), card_body(uiOutput(ns("performance_insights"))))))
          ),

          # Crime Tab
          nav_panel(
            title = tags$span(icon("shield-alt"), " Crime"),
            value = "crime",
            fluidRow(class = "mt-3 mb-3",
              column(6, uiOutput(ns("crime_kpi_obstacle"))),
              column(6, uiOutput(ns("crime_kpi_security")))
            ),
            fluidRow(
              column(8, chart_with_download(ns, "crime_comparison", title = "Crime Comparison")),
              column(4, chart_with_download(ns, "crime_radar", title = "Crime & Security Radar"))
            ),
            fluidRow(class = "mt-3",
              column(4, selectInput(ns("crime_map_indicator"), "Map Indicator",
                choices = c("Crime Obstacle (%)" = "crime_obstacle_pct",
                            "Security Costs (% Sales)" = "security_costs_pct"))),
              column(12, map_with_caption(ns, "crime_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "crime_vs_security_cost", height = "350px", title = "Crime vs Security Cost")),
              column(6, chart_with_download(ns, "crime_impact_performance", height = "350px", title = "Crime Impact on Performance"))
            ),
            fluidRow(class = "mt-3", column(12, card(card_header(icon("lightbulb"), " Crime Insights"), card_body(uiOutput(ns("crime_insights"))))))
          ),

          # Macro Context Tab - World Bank Databank Integration
          nav_panel(
            title = tags$span(icon("globe"), " Macro Context"),
            value = "macro_context",
            tagList(
              fluidRow(class = "mt-3",
                column(12,
                  tags$div(
                    class = "alert alert-info mb-3",
                    icon("info-circle"),
                    " World Bank Databank macro indicators provide national-level context for comparing firm-level WBES metrics across countries."
                  )
                )
              ),
              # Macro KPIs Row
              fluidRow(class = "mb-3",
                column(3, uiOutput(ns("macro_kpi_gdp"))),
                column(3, uiOutput(ns("macro_kpi_growth"))),
                column(3, uiOutput(ns("macro_kpi_inflation"))),
                column(3, uiOutput(ns("macro_kpi_exports")))
              ),
              # Economic Comparison Chart
              fluidRow(
                column(12,
                  card(
                    card_header(icon("chart-bar"), "Macroeconomic Comparison"),
                    card_body(
                      chart_with_download(ns, "macro_economic_comparison", height = "400px", title = "GDP, Growth, and Inflation by Country")
                    )
                  )
                )
              ),
              # Infrastructure and Governance Row
              fluidRow(class = "mt-4",
                column(6,
                  card(
                    card_header(icon("plug"), "National Infrastructure"),
                    card_body(
                      chart_with_download(ns, "macro_infrastructure_chart", height = "350px", title = "Electricity & Internet Access"),
                      p(class = "text-muted small mt-2", "National-level access rates from World Bank complement firm-level WBES infrastructure data.")
                    )
                  )
                ),
                column(6,
                  card(
                    card_header(icon("landmark"), "Governance Quality (WGI)"),
                    card_body(
                      chart_with_download(ns, "macro_governance_chart", height = "350px", title = "Worldwide Governance Indicators"),
                      p(class = "text-muted small mt-2", "WGI scores range from -2.5 (weak) to +2.5 (strong governance).")
                    )
                  )
                )
              ),
              # WBES vs WB Comparison Row
              fluidRow(class = "mt-4",
                column(12,
                  card(
                    card_header(icon("exchange-alt"), "Firm-Level (WBES) vs National (WB) Comparison"),
                    card_body(
                      chart_with_download(ns, "wbes_vs_wb_comparison", height = "400px", title = "WBES Perceptions vs World Bank Data"),
                      p(class = "text-muted small mt-2",
                        "Compares firm-reported metrics (WBES) with national statistics (World Bank). ",
                        "Gaps may indicate differences between firm experience and national averages.")
                    )
                  )
                )
              ),
              # Labor and Trade Row
              fluidRow(class = "mt-4",
                column(6,
                  card(
                    card_header(icon("users"), "Labor Market"),
                    card_body(
                      chart_with_download(ns, "macro_labor_chart", height = "300px", title = "Labor Force Participation & Unemployment")
                    )
                  )
                ),
                column(6,
                  card(
                    card_header(icon("ship"), "Trade & Investment"),
                    card_body(
                      chart_with_download(ns, "macro_trade_chart", height = "300px", title = "Exports, Imports & FDI")
                    )
                  )
                )
              ),
              # Data Table
              fluidRow(class = "mt-4",
                column(12,
                  card(
                    card_header(icon("table"), "Full Macro Data Comparison"),
                    card_body(
                      table_with_download(ns, "macro_data_table")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data, global_filters = NULL, wb_prefetched_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # Filtered data with global filters applied
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
          custom_sectors = filters$custom_sectors,
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

      shiny::updateSelectizeInput(
        session, "countries_compare",
        choices = setNames(countries, countries),
        selected = countries[1:min(5, length(countries))]
      )
    })

    # Data filtered by selected countries
    comparison_data <- reactive({
      req(filtered_data(), input$countries_compare)
      data <- filtered_data()
      data <- filter(data, country %in% input$countries_compare)
      data
    })

    # Get the current grouping dimension
    group_dim <- reactive({
      if (is.null(input$group_dimension)) "none" else input$group_dimension
    })

    # Helper function to aggregate data by grouping dimension
    aggregate_by_group <- function(data, indicators) {
      req(data)
      dim <- group_dim()

      if (dim == "none") {
        # No grouping - aggregate by country
        result <- data |>
          group_by(country) |>
          summarise(across(any_of(indicators), ~mean(.x, na.rm = TRUE)), .groups = "drop")
        result$group_label <- result$country
        return(result)
      }

      # Check if dimension column exists
      if (!dim %in% names(data)) {
        # Fall back to country grouping if dimension not available
        result <- data |>
          group_by(country) |>
          summarise(across(any_of(indicators), ~mean(.x, na.rm = TRUE)), .groups = "drop")
        result$group_label <- result$country
        return(result)
      }

      # Group by the selected dimension
      result <- data |>
        group_by(!!rlang::sym(dim)) |>
        summarise(across(any_of(indicators), ~mean(.x, na.rm = TRUE)), .groups = "drop")
      result$group_label <- as.character(result[[dim]])
      result
    }

    # ==============================================================================
    # OVERVIEW TAB OUTPUTS
    # ==============================================================================

    # Overview heatmap
    output$overview_heatmap <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      key_indicators <- c(
        "power_outages_per_month", "firms_with_credit_line_pct", "IC.FRM.BRIB.ZS",
        "female_ownership_pct", "capacity_utilization_pct", "crime_obstacle_pct"
      )
      indicator_labels <- c("Power Outages", "Credit Access", "Bribery", "Female Own", "Capacity", "Crime")

      available <- intersect(key_indicators, names(data))
      if (length(available) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data available", showarrow = FALSE))))
      }

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, available)
      groups <- unique(agg_data$group_label)

      # Create matrix for heatmap
      z_matrix <- sapply(available, function(ind) {
        sapply(groups, function(g) {
          val <- agg_data[agg_data$group_label == g, ind, drop = TRUE]
          if (length(val) > 0) {
            mean_val <- mean(val, na.rm = TRUE)
            if (!is.na(mean_val)) mean_val else NA
          } else {
            NA
          }
        })
      })

      # Match labels to available indicators
      label_indices <- match(available, key_indicators)
      labels_subset <- indicator_labels[label_indices]

      plot_ly(
        x = labels_subset,
        y = groups,
        z = z_matrix,
        type = "heatmap",
        colorscale = list(c(0, "#f7fbff"), c(0.5, "#6baed6"), c(1, "#08306b")),
        hovertemplate = "%{y}<br>%{x}: %{z:.1f}<extra></extra>"
      ) |>
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Overview table
    output$overview_table <- renderDT({
      req(comparison_data())
      data <- comparison_data()

      dim <- group_dim()
      indicator_cols <- c("power_outages_per_month", "firms_with_credit_line_pct",
                          "IC.FRM.BRIB.ZS", "capacity_utilization_pct",
                          "female_ownership_pct", "export_firms_pct")

      if (dim == "none") {
        # Show by country
        display_cols <- c("country", "region", "income", indicator_cols)
        data <- select(data, any_of(display_cols))
      } else {
        # Aggregate by grouping dimension
        agg_data <- aggregate_by_group(data, indicator_cols)
        data <- agg_data
        # Rename group_label to dimension name
        if (dim == "region") {
          names(data)[names(data) == "group_label"] <- "Region"
        } else if (dim == "firm_size") {
          names(data)[names(data) == "group_label"] <- "Firm Size"
        } else if (dim == "income") {
          names(data)[names(data) == "group_label"] <- "Income Group"
        }
        # Remove the original grouping column if it exists
        if (dim %in% names(data)) {
          data <- data[, !names(data) %in% dim, drop = FALSE]
        }
      }

      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        class = "table-kwiz display compact",
        rownames = FALSE
      )
    })

    # ==============================================================================
    # INFRASTRUCTURE TAB OUTPUTS
    # ==============================================================================

    output$infra_kpi_outages <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$power_outages_per_month, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", round(val, 1)),
          p(class = "text-muted small mb-0", "Avg Power Outages/Month")
        )
      )
    })

    output$infra_kpi_duration <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$avg_outage_duration_hrs, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", round(val, 1)),
          p(class = "text-muted small mb-0", "Avg Outage Duration (hrs)")
        )
      )
    })

    output$infra_kpi_generator <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$firms_with_generator_pct, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
          p(class = "text-muted small mb-0", "Firms with Generator")
        )
      )
    })

    output$infra_kpi_water <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$water_insufficiency_pct, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
          p(class = "text-muted small mb-0", "Water Insufficiency")
        )
      )
    })

    output$infra_comparison <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("power_outages_per_month", "avg_outage_duration_hrs",
                      "firms_with_generator_pct", "water_insufficiency_pct")
      labels <- c("Power Outages", "Outage Duration", "Generator %", "Water Insufficiency %")

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, indicators)
      groups <- unique(agg_data$group_label)

      traces <- list()
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]

        values <- sapply(indicators, function(ind) {
          val <- group_data[[ind]]
          if (length(val) > 0) mean(val, na.rm = TRUE) else NA
        })

        traces[[i]] <- list(
          x = labels,
          y = values,
          name = group,
          type = "bar"
        )
      }

      p <- plot_ly()
      for (i in seq_along(groups)) {
        p <- add_trace(p, data = traces[[i]], x = ~x, y = ~y, name = traces[[i]]$name, type = "bar")
      }
      p |>
        layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Value"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    output$infra_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("power_outages_per_month", "avg_outage_duration_hrs", "firms_with_generator_pct", "water_insufficiency_pct")
      labels <- c("Power Outages", "Outage Duration", "Generator %", "Water %")

      available_indicators <- intersect(indicators, names(data))
      if (length(available_indicators) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data available", showarrow = FALSE))))
      }

      available_labels <- labels[match(available_indicators, indicators)]

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, available_indicators)
      groups <- unique(agg_data$group_label)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      # Calculate max value for range
      all_vals <- unlist(lapply(available_indicators, function(ind) agg_data[[ind]]))
      max_val <- max(all_vals, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10

      p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(available_indicators, function(ind) {
          val <- mean(group_data[[ind]], na.rm = TRUE)
          if (is.na(val)) 0 else val
        })
        values <- c(values, values[1])
        theta <- c(available_labels, available_labels[1])

        p <- p |> add_trace(
          r = values,
          theta = theta,
          name = group,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
        )
      }

      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
        margin = list(b = 70),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> config(displayModeBar = FALSE)
    })

    output$infra_map <- renderLeaflet({
      req(comparison_data(), wbes_data())
      data <- comparison_data()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$infra_map_indicator)) input$infra_map_indicator else "power_outages_per_month"
      palette_info <- switch(indicator,
        "power_outages_per_month" = list(palette = "Reds", label = "Power Outages/Month", reverse = TRUE),
        "avg_outage_duration_hrs" = list(palette = "YlOrRd", label = "Outage Duration (hrs)", reverse = TRUE),
        "firms_with_generator_pct" = list(palette = "Oranges", label = "Firms with Generator (%)", reverse = FALSE),
        "water_insufficiency_pct" = list(palette = "Blues", label = "Water Insufficiency (%)", reverse = TRUE),
        list(palette = "Reds", label = indicator, reverse = FALSE)
      )

      create_wbes_map(
        data = data,
        coordinates = coords,
        indicator_col = indicator,
        indicator_label = palette_info$label,
        color_palette = palette_info$palette,
        reverse_colors = palette_info$reverse
      )
    })

    output$infra_outage_impact <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      create_scatter_with_trend(
        data = data,
        x_col = "power_outages_per_month",
        y_col = "capacity_utilization_pct",
        x_label = "Power Outages/Month",
        y_label = "Capacity Utilization %",
        title = "Power Outages vs Capacity"
      )
    })

    output$infra_generator_correlation <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      create_scatter_with_trend(
        data = data,
        x_col = "power_outages_per_month",
        y_col = "firms_with_generator_pct",
        x_label = "Power Outages/Month",
        y_label = "Firms with Generator %",
        title = "Outages vs Generator Adoption"
      )
    })

    output$infra_insights <- renderUI({
      req(comparison_data())
      HTML("<ul><li>Higher power outages correlate with lower capacity utilization</li><li>Countries with frequent outages show higher generator adoption rates</li></ul>")
    })

    # ==============================================================================
    # FINANCE TAB OUTPUTS
    # ==============================================================================

    output$finance_kpi_credit <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$firms_with_credit_line_pct, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
          p(class = "text-muted small mb-0", "Firms with Credit")
        )
      )
    })

    output$finance_kpi_bank <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$firms_with_bank_account_pct, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
          p(class = "text-muted small mb-0", "Firms with Bank Account")
        )
      )
    })

    output$finance_kpi_rejection <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$loan_rejection_rate_pct, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
          p(class = "text-muted small mb-0", "Loan Rejection Rate")
        )
      )
    })

    output$finance_kpi_collateral <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$collateral_required_pct, na.rm = TRUE)
      card(
        card_body(
          class = "text-center",
          h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
          p(class = "text-muted small mb-0", "Collateral Required")
        )
      )
    })

    output$finance_comparison <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("firms_with_credit_line_pct", "firms_with_bank_account_pct",
                      "loan_rejection_rate_pct", "collateral_required_pct")
      labels <- c("Credit Access %", "Bank Account %", "Loan Rejection %", "Collateral %")

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, indicators)
      groups <- unique(agg_data$group_label)

      traces <- list()
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]

        values <- sapply(indicators, function(ind) {
          val <- group_data[[ind]]
          if (length(val) > 0) mean(val, na.rm = TRUE) else NA
        })

        traces[[i]] <- list(x = labels, y = values, name = group, type = "bar")
      }

      p <- plot_ly()
      for (i in seq_along(groups)) {
        p <- add_trace(p, data = traces[[i]], x = ~x, y = ~y, name = traces[[i]]$name, type = "bar")
      }
      p |>
        layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Value"), paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    output$finance_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("firms_with_credit_line_pct", "firms_with_bank_account_pct", "loan_rejection_rate_pct", "collateral_required_pct")
      labels <- c("Credit %", "Bank Account %", "Rejection %", "Collateral %")

      available_indicators <- intersect(indicators, names(data))
      if (length(available_indicators) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data available", showarrow = FALSE))))
      }

      available_labels <- labels[match(available_indicators, indicators)]

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, available_indicators)
      groups <- unique(agg_data$group_label)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      all_vals <- unlist(lapply(available_indicators, function(ind) agg_data[[ind]]))
      max_val <- max(all_vals, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10

      p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(available_indicators, function(ind) {
          val <- mean(group_data[[ind]], na.rm = TRUE)
          if (is.na(val)) 0 else val
        })
        values <- c(values, values[1])
        theta <- c(available_labels, available_labels[1])

        p <- p |> add_trace(
          r = values,
          theta = theta,
          name = group,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
        )
      }

      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
        margin = list(b = 70),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> config(displayModeBar = FALSE)
    })

    output$finance_map <- renderLeaflet({
      req(comparison_data(), wbes_data())
      data <- comparison_data()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$finance_map_indicator)) input$finance_map_indicator else "firms_with_credit_line_pct"
      palette_info <- switch(indicator,
        "firms_with_credit_line_pct" = list(palette = "Greens", label = "Credit Access (%)", reverse = FALSE),
        "firms_with_bank_account_pct" = list(palette = "Blues", label = "Bank Account (%)", reverse = FALSE),
        "loan_rejection_rate_pct" = list(palette = "Reds", label = "Loan Rejection (%)", reverse = TRUE),
        "collateral_required_pct" = list(palette = "YlOrRd", label = "Collateral Required (%)", reverse = TRUE),
        list(palette = "Blues", label = indicator, reverse = FALSE)
      )

      create_wbes_map(data = data, coordinates = coords, indicator_col = indicator,
                      indicator_label = palette_info$label, color_palette = palette_info$palette, reverse_colors = palette_info$reverse)
    })

    output$finance_access_gap <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      create_scatter_with_trend(data, "firms_with_credit_line_pct", "capacity_utilization_pct",
                                "Credit Access %", "Capacity Utilization %", "Credit vs Performance")
    })

    output$finance_collateral_burden <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      create_scatter_with_trend(data, "collateral_required_pct", "loan_rejection_rate_pct",
                                "Collateral Required %", "Loan Rejection %", "Collateral vs Rejection")
    })

    output$finance_insights <- renderUI({
      HTML("<ul><li>Better credit access correlates with higher capacity utilization</li><li>Higher collateral requirements often lead to higher rejection rates</li></ul>")
    })

    # ==============================================================================
    # GOVERNANCE TAB OUTPUTS
    # ==============================================================================

    output$governance_kpi_bribery <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$IC.FRM.BRIB.ZS, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Bribery Incidence")))
    })

    output$governance_kpi_corruption <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$IC.FRM.CORR.ZS, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Corruption Obstacle")))
    })

    output$governance_kpi_regulations <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$mgmt_time_regulations_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Mgmt Time on Regulations")))
    })

    output$governance_comparison <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- c("IC.FRM.BRIB.ZS", "IC.FRM.CORR.ZS", "mgmt_time_regulations_pct")
      labels <- c("Bribery %", "Corruption %", "Regulations %")

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, indicators)
      groups <- unique(agg_data$group_label)

      traces <- list()
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(indicators, function(ind) {
          val <- group_data[[ind]]
          if (length(val) > 0) mean(val, na.rm = TRUE) else NA
        })
        traces[[i]] <- list(x = labels, y = values, name = group, type = "bar")
      }

      p <- plot_ly()
      for (i in seq_along(groups)) {
        p <- add_trace(p, data = traces[[i]], x = ~x, y = ~y, name = traces[[i]]$name, type = "bar")
      }
      p |>
        layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Value"), paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    output$governance_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("IC.FRM.BRIB.ZS", "IC.FRM.CORR.ZS", "mgmt_time_regulations_pct")
      labels <- c("Bribery %", "Corruption %", "Regulations %")

      available_indicators <- intersect(indicators, names(data))
      if (length(available_indicators) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data available", showarrow = FALSE))))
      }

      available_labels <- labels[match(available_indicators, indicators)]

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, available_indicators)
      groups <- unique(agg_data$group_label)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      all_vals <- unlist(lapply(available_indicators, function(ind) agg_data[[ind]]))
      max_val <- max(all_vals, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10

      p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(available_indicators, function(ind) {
          val <- mean(group_data[[ind]], na.rm = TRUE)
          if (is.na(val)) 0 else val
        })
        values <- c(values, values[1])
        theta <- c(available_labels, available_labels[1])

        p <- p |> add_trace(
          r = values,
          theta = theta,
          name = group,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
        )
      }

      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
        margin = list(b = 70),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> config(displayModeBar = FALSE)
    })

    output$governance_map <- renderLeaflet({
      req(comparison_data(), wbes_data())
      data <- comparison_data()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$governance_map_indicator)) input$governance_map_indicator else "bribery_incidence_pct"
      palette_info <- switch(indicator,
        "bribery_incidence_pct" = list(palette = "Reds", label = "Bribery Incidence (%)", reverse = TRUE, col = "IC.FRM.BRIB.ZS"),
        "corruption_obstacle_pct" = list(palette = "YlOrRd", label = "Corruption Obstacle (%)", reverse = TRUE, col = "IC.FRM.CORR.ZS"),
        "mgmt_time_regulations_pct" = list(palette = "Oranges", label = "Mgmt Time on Regulations (%)", reverse = TRUE, col = "mgmt_time_regulations_pct"),
        list(palette = "Reds", label = indicator, reverse = TRUE, col = indicator)
      )

      # Try the mapped column first, fallback to indicator name
      col_to_use <- if (palette_info$col %in% names(data)) palette_info$col else indicator
      create_wbes_map(data, coords, col_to_use, palette_info$label, palette_info$palette, palette_info$reverse)
    })

    output$governance_bribery_vs_corruption <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "IC.FRM.CORR.ZS", "IC.FRM.BRIB.ZS",
                                "Corruption %", "Bribery %", "Corruption vs Bribery")
    })

    output$governance_regulatory_burden <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "mgmt_time_regulations_pct", "capacity_utilization_pct",
                                "Mgmt Time on Regulations %", "Capacity %", "Regulations vs Performance")
    })

    output$governance_insights <- renderUI({
      HTML("<ul><li>Corruption and bribery tend to correlate strongly</li><li>Higher regulatory burden may reduce capacity utilization</li></ul>")
    })

    # ==============================================================================
    # WORKFORCE TAB OUTPUTS
    # ==============================================================================

    output$workforce_kpi_female_own <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$female_ownership_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Female Ownership")))
    })

    output$workforce_kpi_female_work <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$female_workers_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Female Workers")))
    })

    output$workforce_kpi_obstacle <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$workforce_obstacle_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Workforce Obstacle")))
    })

    output$workforce_comparison <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- c("female_ownership_pct", "female_workers_pct", "workforce_obstacle_pct")
      labels <- c("Female Own %", "Female Workers %", "Obstacle %")

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, indicators)
      groups <- unique(agg_data$group_label)

      traces <- list()
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(indicators, function(ind) {
          val <- group_data[[ind]]
          if (length(val) > 0) mean(val, na.rm = TRUE) else NA
        })
        traces[[i]] <- list(x = labels, y = values, name = group, type = "bar")
      }

      p <- plot_ly()
      for (i in seq_along(groups)) {
        p <- add_trace(p, data = traces[[i]], x = ~x, y = ~y, name = traces[[i]]$name, type = "bar")
      }
      p |>
        layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Value"), paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    output$workforce_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("female_ownership_pct", "female_workers_pct", "workforce_obstacle_pct")
      labels <- c("Female Own %", "Female Workers %", "Obstacle %")

      available_indicators <- intersect(indicators, names(data))
      if (length(available_indicators) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data available", showarrow = FALSE))))
      }

      available_labels <- labels[match(available_indicators, indicators)]

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, available_indicators)
      groups <- unique(agg_data$group_label)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      all_vals <- unlist(lapply(available_indicators, function(ind) agg_data[[ind]]))
      max_val <- max(all_vals, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10

      p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(available_indicators, function(ind) {
          val <- mean(group_data[[ind]], na.rm = TRUE)
          if (is.na(val)) 0 else val
        })
        values <- c(values, values[1])
        theta <- c(available_labels, available_labels[1])

        p <- p |> add_trace(
          r = values,
          theta = theta,
          name = group,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
        )
      }

      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
        margin = list(b = 70),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> config(displayModeBar = FALSE)
    })

    output$workforce_map <- renderLeaflet({
      req(comparison_data(), wbes_data())
      data <- comparison_data()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$workforce_map_indicator)) input$workforce_map_indicator else "female_ownership_pct"
      palette_info <- switch(indicator,
        "female_ownership_pct" = list(palette = "Purples", label = "Female Ownership (%)", reverse = FALSE),
        "female_workers_pct" = list(palette = "PuRd", label = "Female Workers (%)", reverse = FALSE),
        "workforce_obstacle_pct" = list(palette = "YlOrRd", label = "Workforce Obstacle (%)", reverse = TRUE),
        list(palette = "Purples", label = indicator, reverse = FALSE)
      )

      create_wbes_map(data, coords, indicator, palette_info$label, palette_info$palette, palette_info$reverse)
    })

    output$workforce_gender_gap <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "female_ownership_pct", "female_workers_pct",
                                "Female Ownership %", "Female Workers %", "Ownership vs Workers")
    })

    output$workforce_obstacle_correlation <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "workforce_obstacle_pct", "capacity_utilization_pct",
                                "Workforce Obstacle %", "Capacity %", "Workforce Issues vs Performance")
    })

    output$workforce_insights <- renderUI({
      HTML("<ul><li>Female ownership correlates with higher female worker participation</li><li>Workforce obstacles negatively impact capacity utilization</li></ul>")
    })

    # ==============================================================================
    # PERFORMANCE TAB OUTPUTS
    # ==============================================================================

    output$performance_kpi_capacity <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$capacity_utilization_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Capacity Utilization")))
    })

    output$performance_kpi_export_firms <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$export_firms_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Export Firms")))
    })

    output$performance_kpi_export_share <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$export_share_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Export Share")))
    })

    output$performance_kpi_growth <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$annual_sales_growth_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Sales Growth")))
    })

    output$performance_comparison <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- c("capacity_utilization_pct", "export_firms_pct", "export_share_pct", "annual_sales_growth_pct")
      labels <- c("Capacity %", "Export Firms %", "Export Share %", "Growth %")

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, indicators)
      groups <- unique(agg_data$group_label)

      traces <- list()
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(indicators, function(ind) {
          val <- group_data[[ind]]
          if (length(val) > 0) mean(val, na.rm = TRUE) else NA
        })
        traces[[i]] <- list(x = labels, y = values, name = group, type = "bar")
      }

      p <- plot_ly()
      for (i in seq_along(groups)) {
        p <- add_trace(p, data = traces[[i]], x = ~x, y = ~y, name = traces[[i]]$name, type = "bar")
      }
      p |>
        layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Value"), paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    output$performance_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("capacity_utilization_pct", "export_firms_pct", "export_share_pct", "annual_sales_growth_pct")
      labels <- c("Capacity %", "Export Firms %", "Export Share %", "Growth %")

      available_indicators <- intersect(indicators, names(data))
      if (length(available_indicators) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data available", showarrow = FALSE))))
      }

      available_labels <- labels[match(available_indicators, indicators)]

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, available_indicators)
      groups <- unique(agg_data$group_label)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      all_vals <- unlist(lapply(available_indicators, function(ind) agg_data[[ind]]))
      max_val <- max(all_vals, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10

      p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(available_indicators, function(ind) {
          val <- mean(group_data[[ind]], na.rm = TRUE)
          if (is.na(val)) 0 else val
        })
        values <- c(values, values[1])
        theta <- c(available_labels, available_labels[1])

        p <- p |> add_trace(
          r = values,
          theta = theta,
          name = group,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
        )
      }

      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
        margin = list(b = 70),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> config(displayModeBar = FALSE)
    })

    output$performance_map <- renderLeaflet({
      req(comparison_data(), wbes_data())
      data <- comparison_data()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$performance_map_indicator)) input$performance_map_indicator else "capacity_utilization_pct"
      palette_info <- switch(indicator,
        "capacity_utilization_pct" = list(palette = "Greens", label = "Capacity Utilization (%)", reverse = FALSE),
        "export_firms_pct" = list(palette = "Blues", label = "Export Firms (%)", reverse = FALSE),
        "export_share_pct" = list(palette = "BuGn", label = "Export Share (%)", reverse = FALSE),
        "annual_sales_growth_pct" = list(palette = "YlGn", label = "Sales Growth (%)", reverse = FALSE),
        list(palette = "Greens", label = indicator, reverse = FALSE)
      )

      create_wbes_map(data, coords, indicator, palette_info$label, palette_info$palette, palette_info$reverse)
    })

    output$performance_capacity_vs_exports <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "capacity_utilization_pct", "export_firms_pct",
                                "Capacity %", "Export Firms %", "Capacity vs Exports")
    })

    output$performance_growth_distribution <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "export_share_pct", "annual_sales_growth_pct",
                                "Export Share %", "Sales Growth %", "Exports vs Growth")
    })

    output$performance_insights <- renderUI({
      HTML("<ul><li>Higher capacity utilization correlates with export activity</li><li>Export-oriented firms show stronger sales growth</li></ul>")
    })

    # ==============================================================================
    # CRIME TAB OUTPUTS
    # ==============================================================================

    output$crime_kpi_obstacle <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$crime_obstacle_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Crime Obstacle")))
    })

    output$crime_kpi_security <- renderUI({
      req(comparison_data())
      val <- mean(comparison_data()$security_costs_pct, na.rm = TRUE)
      card(card_body(class = "text-center", h4(class = "text-primary-teal mb-0", paste0(round(val, 1), "%")),
                     p(class = "text-muted small mb-0", "Security Costs")))
    })

    output$crime_comparison <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- c("crime_obstacle_pct", "security_costs_pct")
      labels <- c("Crime Obstacle %", "Security Costs %")

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, indicators)
      groups <- unique(agg_data$group_label)

      traces <- list()
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(indicators, function(ind) {
          val <- group_data[[ind]]
          if (length(val) > 0) mean(val, na.rm = TRUE) else NA
        })
        traces[[i]] <- list(x = labels, y = values, name = group, type = "bar")
      }

      p <- plot_ly()
      for (i in seq_along(groups)) {
        p <- add_trace(p, data = traces[[i]], x = ~x, y = ~y, name = traces[[i]]$name, type = "bar")
      }
      p |>
        layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Value"), paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    output$crime_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      indicators <- c("crime_obstacle_pct", "security_costs_pct")
      labels <- c("Crime Obstacle %", "Security Costs %")

      available_indicators <- intersect(indicators, names(data))
      if (length(available_indicators) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data available", showarrow = FALSE))))
      }

      available_labels <- labels[match(available_indicators, indicators)]

      # Use grouping dimension
      agg_data <- aggregate_by_group(data, available_indicators)
      groups <- unique(agg_data$group_label)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      all_vals <- unlist(lapply(available_indicators, function(ind) agg_data[[ind]]))
      max_val <- max(all_vals, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10

      p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
      for (i in seq_along(groups)) {
        group <- groups[i]
        group_data <- agg_data[agg_data$group_label == group, ]
        values <- sapply(available_indicators, function(ind) {
          val <- mean(group_data[[ind]], na.rm = TRUE)
          if (is.na(val)) 0 else val
        })
        values <- c(values, values[1])
        theta <- c(available_labels, available_labels[1])

        p <- p |> add_trace(
          r = values,
          theta = theta,
          name = group,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
        )
      }

      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
        margin = list(b = 70),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> config(displayModeBar = FALSE)
    })

    output$crime_map <- renderLeaflet({
      req(comparison_data(), wbes_data())
      data <- comparison_data()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$crime_map_indicator)) input$crime_map_indicator else "crime_obstacle_pct"
      palette_info <- switch(indicator,
        "crime_obstacle_pct" = list(palette = "Reds", label = "Crime Obstacle (%)", reverse = TRUE),
        "security_costs_pct" = list(palette = "Oranges", label = "Security Costs (% Sales)", reverse = TRUE),
        list(palette = "Reds", label = indicator, reverse = TRUE)
      )

      create_wbes_map(data, coords, indicator, palette_info$label, palette_info$palette, palette_info$reverse)
    })

    output$crime_vs_security_cost <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "crime_obstacle_pct", "security_costs_pct",
                                "Crime Obstacle %", "Security Costs %", "Crime vs Security Costs")
    })

    output$crime_impact_performance <- renderPlotly({
      req(comparison_data())
      create_scatter_with_trend(comparison_data(), "crime_obstacle_pct", "capacity_utilization_pct",
                                "Crime Obstacle %", "Capacity %", "Crime vs Performance")
    })

    output$crime_insights <- renderUI({
      HTML("<ul><li>Higher crime obstacles lead to increased security costs</li><li>Crime negatively impacts capacity utilization</li></ul>")
    })

    # ==============================================================================
    # MACRO CONTEXT TAB OUTPUTS - World Bank Databank Integration
    # ==============================================================================

    # Reactive to get WB data for all selected countries
    # Uses prefetched cache if available, with fallback to live API
    wb_comparison_data <- reactive({
      req(input$countries_compare)
      countries <- input$countries_compare

      # Get survey year from filtered data if available
      filters <- if (!is.null(global_filters)) global_filters() else NULL
      target_year <- if (!is.null(filters$year) && length(filters$year) > 0 &&
                         !all(filters$year %in% c("all", NA))) {
        as.integer(filters$year[1])
      } else {
        NULL
      }

      # Map countries to ISO3 codes
      country_mapping <- tryCatch({
        map_wbes_countries_to_iso3(countries)
      }, error = function(e) {
        return(NULL)
      })

      if (is.null(country_mapping) || nrow(country_mapping) == 0) {
        return(NULL)
      }

      # Filter out countries without ISO3 codes
      valid_mapping <- country_mapping[!is.na(country_mapping$iso3c), ]
      if (nrow(valid_mapping) == 0) {
        return(NULL)
      }

      # Get prefetched data if available
      prefetched <- if (!is.null(wb_prefetched_data)) wb_prefetched_data() else NULL

      # Limit countries to prevent excessive API calls if not using cache
      max_countries <- if (!is.null(prefetched)) nrow(valid_mapping) else min(nrow(valid_mapping), 5)

      wb_results <- withProgress(
        message = if (!is.null(prefetched)) "Loading cached WB data..." else "Loading World Bank data...",
        value = 0,
        {
          results <- lapply(seq_len(max_countries), function(i) {
            incProgress(1 / max_countries, detail = valid_mapping$country[i])

            country_name <- valid_mapping$country[i]
            iso3_code <- valid_mapping$iso3c[i]

            tryCatch({
              # Use cached data if available
              wb_data <- if (!is.null(prefetched)) {
                get_wb_context_from_cache(iso3_code, prefetched, target_year = target_year,
                                           fallback_to_api = TRUE)
              } else {
                get_wb_country_context(iso3_code, target_year = target_year)
              }

              if (!is.null(wb_data)) {
                wb_data$country <- country_name
              }
              wb_data
            }, error = function(e) {
              NULL
            })
          })
          results
        }
      )

      # Filter out NULL results
      wb_results <- wb_results[!sapply(wb_results, is.null)]

      if (length(wb_results) == 0) {
        return(NULL)
      }

      wb_results
    })

    # Helper to extract values from WB data list
    extract_wb_values <- function(wb_list, indicator_code) {
      if (is.null(wb_list)) return(NULL)

      data.frame(
        country = sapply(wb_list, function(x) x$country),
        value = sapply(wb_list, function(x) {
          if (!is.null(x[[indicator_code]]) && !is.na(x[[indicator_code]])) {
            x[[indicator_code]]
          } else {
            NA_real_
          }
        }),
        stringsAsFactors = FALSE
      )
    }

    # Macro KPIs - Show loading state while WB data is being fetched
    output$macro_kpi_gdp <- renderUI({
      # Check if countries are selected first
      if (is.null(input$countries_compare) || length(input$countries_compare) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", "-"),
          p(class = "text-muted small mb-0", "Select countries")
        )))
      }

      # Check if we're still in initial WB data prefetch
      prefetched <- if (!is.null(wb_prefetched_data)) wb_prefetched_data() else NULL
      loading_msg <- if (is.null(prefetched)) "Initializing WB data..." else "Loading cached data..."

      wb_data <- wb_comparison_data()
      if (is.null(wb_data) || length(wb_data) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", icon("spinner", class = "fa-spin")),
          p(class = "text-muted small mb-0", loading_msg)
        )))
      }

      vals <- sapply(wb_data, function(x) x[["NY.GDP.PCAP.CD"]])
      avg_val <- mean(vals, na.rm = TRUE)

      card(card_body(class = "text-center",
        h4(class = "text-primary-teal mb-0",
           if (!is.na(avg_val)) paste0("$", format(round(avg_val), big.mark = ",")) else "N/A"),
        p(class = "text-muted small mb-0", "Avg GDP per Capita")
      ))
    })

    output$macro_kpi_growth <- renderUI({
      if (is.null(input$countries_compare) || length(input$countries_compare) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", "-"),
          p(class = "text-muted small mb-0", "Select countries")
        )))
      }

      wb_data <- wb_comparison_data()
      if (is.null(wb_data) || length(wb_data) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", icon("spinner", class = "fa-spin")),
          p(class = "text-muted small mb-0", "Loading...")
        )))
      }

      vals <- sapply(wb_data, function(x) x[["NY.GDP.PCAP.KD.ZG"]])
      avg_val <- mean(vals, na.rm = TRUE)

      card(card_body(class = "text-center",
        h4(class = if (!is.na(avg_val) && avg_val >= 0) "text-success mb-0" else "text-danger mb-0",
           if (!is.na(avg_val)) paste0(round(avg_val, 1), "%") else "N/A"),
        p(class = "text-muted small mb-0", "Avg GDP Growth")
      ))
    })

    output$macro_kpi_inflation <- renderUI({
      if (is.null(input$countries_compare) || length(input$countries_compare) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", "-"),
          p(class = "text-muted small mb-0", "Select countries")
        )))
      }

      wb_data <- wb_comparison_data()
      if (is.null(wb_data) || length(wb_data) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", icon("spinner", class = "fa-spin")),
          p(class = "text-muted small mb-0", "Loading...")
        )))
      }

      vals <- sapply(wb_data, function(x) x[["FP.CPI.TOTL.ZG"]])
      avg_val <- mean(vals, na.rm = TRUE)

      card(card_body(class = "text-center",
        h4(class = if (!is.na(avg_val) && avg_val <= 5) "text-success mb-0" else "text-warning mb-0",
           if (!is.na(avg_val)) paste0(round(avg_val, 1), "%") else "N/A"),
        p(class = "text-muted small mb-0", "Avg Inflation")
      ))
    })

    output$macro_kpi_exports <- renderUI({
      if (is.null(input$countries_compare) || length(input$countries_compare) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", "-"),
          p(class = "text-muted small mb-0", "Select countries")
        )))
      }

      wb_data <- wb_comparison_data()
      if (is.null(wb_data) || length(wb_data) == 0) {
        return(card(card_body(class = "text-center",
          h4(class = "text-muted mb-0", icon("spinner", class = "fa-spin")),
          p(class = "text-muted small mb-0", "Loading...")
        )))
      }

      vals <- sapply(wb_data, function(x) x[["NE.EXP.GNFS.ZS"]])
      avg_val <- mean(vals, na.rm = TRUE)

      card(card_body(class = "text-center",
        h4(class = "text-primary-teal mb-0",
           if (!is.na(avg_val)) paste0(round(avg_val, 1), "%") else "N/A"),
        p(class = "text-muted small mb-0", "Avg Exports (% GDP)")
      ))
    })

    # Helper to prepare WB data for charting (sorting and data frame conversion)
    prepare_wb_chart_data <- function(wb_data, indicator_codes, indicator_labels = NULL) {
      if (is.null(wb_data) || length(wb_data) == 0) return(NULL)

      # Build data frame
      chart_df <- data.frame(
        country = sapply(wb_data, function(x) x$country),
        stringsAsFactors = FALSE
      )

      for (i in seq_along(indicator_codes)) {
        code <- indicator_codes[i]
        label <- if (!is.null(indicator_labels)) indicator_labels[i] else code
        chart_df[[label]] <- sapply(wb_data, function(x) {
          val <- x[[code]]
          if (!is.null(val) && !is.na(val)) val else NA_real_
        })
      }

      # Apply sort order based on first indicator
      sort_col <- if (!is.null(indicator_labels)) indicator_labels[1] else indicator_codes[1]
      sort_dir <- if (!is.null(input$sort_order) && input$sort_order == "asc") FALSE else TRUE

      if (sort_col %in% names(chart_df) && any(!is.na(chart_df[[sort_col]]))) {
        sort_idx <- order(chart_df[[sort_col]], decreasing = sort_dir, na.last = TRUE)
        chart_df <- chart_df[sort_idx, ]
      }

      chart_df
    }

    # Macro Economic Comparison Chart
    output$macro_economic_comparison <- renderPlotly({
      wb_data <- wb_comparison_data()
      chart_type <- if (!is.null(input$chart_type)) input$chart_type else "bar"

      if (is.null(wb_data) || length(wb_data) == 0) {
        return(plot_ly() |>
          layout(annotations = list(list(
            text = "World Bank data loading... If this persists, the WB API may be unavailable.",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 14)
          )), paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE))
      }

      # Prepare data with sorting
      indicator_codes <- c("NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG", "FP.CPI.TOTL.ZG")
      indicator_labels <- c("GDP/Capita ($100s)", "GDP Growth %", "Inflation %")
      chart_df <- prepare_wb_chart_data(wb_data, indicator_codes, indicator_labels)

      if (is.null(chart_df)) {
        return(plot_ly() |> layout(annotations = list(list(
          text = "No data available", showarrow = FALSE
        ))) |> config(displayModeBar = FALSE))
      }

      # Scale GDP for display
      chart_df[["GDP/Capita ($100s)"]] <- chart_df[["GDP/Capita ($100s)"]] / 100

      colors <- c("#1B6B5F", "#2E7D32", "#F49B7A")

      if (chart_type == "radar") {
        # Radar chart - normalize values to 0-100 for comparability
        radar_labels <- indicator_labels
        p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")

        for (i in seq_len(nrow(chart_df))) {
          values <- as.numeric(chart_df[i, indicator_labels])
          # Normalize: GDP/100 max ~200, Growth max ~15, Inflation max ~30
          norm_vals <- c(
            min(values[1] / 2, 100),  # GDP/100 scaled to ~100 max
            min((values[2] + 10) * 4, 100),  # Growth from -10 to +15 -> 0-100
            min(values[3] * 2, 100)  # Inflation 0-50 -> 0-100
          )
          norm_vals <- c(norm_vals, norm_vals[1])
          theta <- c(radar_labels, radar_labels[1])

          p <- p |> add_trace(
            r = norm_vals, theta = theta, name = chart_df$country[i],
            line = list(color = colors[((i - 1) %% length(colors)) + 1]),
            fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
          )
        }
        p |> layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          showlegend = TRUE,
          legend = list(orientation = "v", x = 1.02, y = 0.5),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |> config(displayModeBar = FALSE)

      } else if (chart_type == "heatmap") {
        # Heatmap
        z_matrix <- as.matrix(chart_df[, indicator_labels])
        plot_ly(
          x = indicator_labels, y = chart_df$country, z = z_matrix,
          type = "heatmap",
          colorscale = list(c(0, "#f8f9fa"), c(1, "#1B6B5F")),
          hovertemplate = "%{y}<br>%{x}: %{z:.2f}<extra></extra>"
        ) |>
          layout(
            xaxis = list(title = "", tickangle = -45),
            yaxis = list(title = "", autorange = "reversed"),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)

      } else {
        # Bar chart (default)
        plot_ly() |>
          add_trace(x = chart_df$country, y = chart_df[["GDP/Capita ($100s)"]],
                    name = "GDP/Capita ($100s)", type = "bar", marker = list(color = colors[1])) |>
          add_trace(x = chart_df$country, y = chart_df[["GDP Growth %"]],
                    name = "GDP Growth %", type = "bar", marker = list(color = colors[2])) |>
          add_trace(x = chart_df$country, y = chart_df[["Inflation %"]],
                    name = "Inflation %", type = "bar", marker = list(color = colors[3])) |>
          layout(
            barmode = "group",
            xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = chart_df$country),
            yaxis = list(title = "Value"),
            legend = list(orientation = "h", y = -0.2),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      }
    })

    # Macro Infrastructure Chart
    output$macro_infrastructure_chart <- renderPlotly({
      wb_data <- wb_comparison_data()
      chart_type <- if (!is.null(input$chart_type)) input$chart_type else "bar"

      if (is.null(wb_data) || length(wb_data) == 0) {
        return(plot_ly() |>
          layout(annotations = list(list(
            text = "Infrastructure data not available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
          )), paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE))
      }

      indicator_codes <- c("EG.ELC.ACCS.ZS", "IT.NET.USER.ZS")
      indicator_labels <- c("Electricity Access %", "Internet Users %")
      chart_df <- prepare_wb_chart_data(wb_data, indicator_codes, indicator_labels)

      if (is.null(chart_df)) {
        return(plot_ly() |> layout(annotations = list(list(
          text = "No data available", showarrow = FALSE
        ))) |> config(displayModeBar = FALSE))
      }

      colors <- c("#F4A460", "#17a2b8")

      if (chart_type == "radar") {
        p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
        for (i in seq_len(nrow(chart_df))) {
          values <- as.numeric(chart_df[i, indicator_labels])
          values <- c(values, values[1])
          theta <- c(indicator_labels, indicator_labels[1])
          p <- p |> add_trace(
            r = values, theta = theta, name = chart_df$country[i],
            line = list(color = colors[((i - 1) %% length(colors)) + 1]),
            fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
          )
        }
        p |> layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          showlegend = TRUE, legend = list(orientation = "v", x = 1.02, y = 0.5),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |> config(displayModeBar = FALSE)

      } else if (chart_type == "heatmap") {
        z_matrix <- as.matrix(chart_df[, indicator_labels])
        plot_ly(
          x = indicator_labels, y = chart_df$country, z = z_matrix,
          type = "heatmap",
          colorscale = list(c(0, "#f8f9fa"), c(1, "#F4A460")),
          zmin = 0, zmax = 100,
          hovertemplate = "%{y}<br>%{x}: %{z:.1f}%<extra></extra>"
        ) |>
          layout(
            xaxis = list(title = "", tickangle = -45),
            yaxis = list(title = "", autorange = "reversed"),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)

      } else {
        plot_ly() |>
          add_trace(x = chart_df$country, y = chart_df[["Electricity Access %"]],
                    name = "Electricity Access %", type = "bar", marker = list(color = colors[1])) |>
          add_trace(x = chart_df$country, y = chart_df[["Internet Users %"]],
                    name = "Internet Users %", type = "bar", marker = list(color = colors[2])) |>
          layout(
            barmode = "group",
            xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = chart_df$country),
            yaxis = list(title = "Percentage", range = c(0, 100)),
            legend = list(orientation = "h", y = -0.3),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      }
    })

    # Macro Governance Chart (WGI)
    output$macro_governance_chart <- renderPlotly({
      wb_data <- wb_comparison_data()
      chart_type <- if (!is.null(input$chart_type)) input$chart_type else "bar"

      if (is.null(wb_data) || length(wb_data) == 0) {
        return(plot_ly() |>
          layout(annotations = list(list(
            text = "Governance data not available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
          )), paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE))
      }

      # WGI indicators
      wgi_codes <- c("GE.EST", "RQ.EST", "RL.EST", "CC.EST")
      wgi_labels <- c("Gov't Effectiveness", "Regulatory Quality", "Rule of Law", "Control of Corruption")
      chart_df <- prepare_wb_chart_data(wb_data, wgi_codes, wgi_labels)

      if (is.null(chart_df)) {
        return(plot_ly() |> layout(annotations = list(list(
          text = "No data available", showarrow = FALSE
        ))) |> config(displayModeBar = FALSE))
      }

      colors <- c("#1B6B5F", "#2E7D32", "#17a2b8", "#F49B7A")

      if (chart_type == "radar") {
        # WGI ranges from -2.5 to 2.5, normalize to 0-100
        p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
        for (i in seq_len(nrow(chart_df))) {
          values <- as.numeric(chart_df[i, wgi_labels])
          norm_vals <- (values + 2.5) / 5 * 100  # Normalize to 0-100
          norm_vals <- c(norm_vals, norm_vals[1])
          theta <- c(wgi_labels, wgi_labels[1])
          p <- p |> add_trace(
            r = norm_vals, theta = theta, name = chart_df$country[i],
            line = list(color = colors[((i - 1) %% length(colors)) + 1]),
            fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
          )
        }
        p |> layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          showlegend = TRUE, legend = list(orientation = "v", x = 1.02, y = 0.5),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |> config(displayModeBar = FALSE)

      } else if (chart_type == "bar") {
        plot_ly() |>
          add_trace(x = chart_df$country, y = chart_df[["Gov't Effectiveness"]],
                    name = "Gov't Effectiveness", type = "bar", marker = list(color = colors[1])) |>
          add_trace(x = chart_df$country, y = chart_df[["Regulatory Quality"]],
                    name = "Regulatory Quality", type = "bar", marker = list(color = colors[2])) |>
          add_trace(x = chart_df$country, y = chart_df[["Rule of Law"]],
                    name = "Rule of Law", type = "bar", marker = list(color = colors[3])) |>
          add_trace(x = chart_df$country, y = chart_df[["Control of Corruption"]],
                    name = "Control of Corruption", type = "bar", marker = list(color = colors[4])) |>
          layout(
            barmode = "group",
            xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = chart_df$country),
            yaxis = list(title = "WGI Score", range = c(-2.5, 2.5)),
            legend = list(orientation = "h", y = -0.3),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)

      } else {
        # Heatmap (default for governance - best visualization for this data)
        z_matrix <- as.matrix(chart_df[, wgi_labels])
        plot_ly(
          x = wgi_labels, y = chart_df$country, z = z_matrix,
          type = "heatmap",
          colorscale = list(c(0, "#dc3545"), c(0.5, "#f8f9fa"), c(1, "#2E7D32")),
          zmin = -2.5, zmax = 2.5,
          hovertemplate = "%{y}<br>%{x}: %{z:.2f}<extra></extra>"
        ) |>
          layout(
            xaxis = list(title = "", tickangle = -45),
            yaxis = list(title = "", autorange = "reversed"),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      }
    })

    # WBES vs WB Comparison Chart
    output$wbes_vs_wb_comparison <- renderPlotly({
      req(comparison_data())
      wbes_data_df <- comparison_data()
      wb_data <- wb_comparison_data()

      if (is.null(wb_data) || length(wb_data) == 0 || nrow(wbes_data_df) == 0) {
        return(plot_ly() |>
          layout(annotations = list(list(
            text = "Comparison data not available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
          )), paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE))
      }

      countries <- sapply(wb_data, function(x) x$country)

      # Build comparison data
      comparison_rows <- lapply(countries, function(country) {
        # Get WBES data for country
        wbes_country <- wbes_data_df[wbes_data_df$country == country, ]
        wb_country <- wb_data[sapply(wb_data, function(x) x$country == country)][[1]]

        if (nrow(wbes_country) == 0 || is.null(wb_country)) {
          return(NULL)
        }

        # Compare electricity: WBES generator usage vs WB electricity access
        wbes_power_reliable <- if ("firms_with_generator_pct" %in% names(wbes_country) &&
                                   !is.na(wbes_country$firms_with_generator_pct[1])) {
          100 - wbes_country$firms_with_generator_pct[1]  # Invert: no generator = reliable
        } else NA

        wb_electricity <- if (!is.null(wb_country[["EG.ELC.ACCS.ZS"]]) &&
                              !is.na(wb_country[["EG.ELC.ACCS.ZS"]])) {
          wb_country[["EG.ELC.ACCS.ZS"]]
        } else NA

        # Compare corruption: WBES bribery vs WB control of corruption
        wbes_low_corruption <- if ("IC.FRM.BRIB.ZS" %in% names(wbes_country) &&
                                   !is.na(wbes_country$IC.FRM.BRIB.ZS[1])) {
          100 - wbes_country$IC.FRM.BRIB.ZS[1]  # Invert: low bribery = low corruption
        } else NA

        wb_corruption <- if (!is.null(wb_country[["CC.EST"]]) &&
                             !is.na(wb_country[["CC.EST"]])) {
          ((wb_country[["CC.EST"]] + 2.5) / 5) * 100  # Normalize to 0-100
        } else NA

        list(
          country = country,
          wbes_power = wbes_power_reliable,
          wb_power = wb_electricity,
          wbes_corruption = wbes_low_corruption,
          wb_corruption = wb_corruption
        )
      })

      comparison_rows <- comparison_rows[!sapply(comparison_rows, is.null)]

      if (length(comparison_rows) == 0) {
        return(plot_ly() |>
          layout(annotations = list(list(
            text = "No comparable data points",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
          )), paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE))
      }

      # Create grouped bar chart
      countries <- sapply(comparison_rows, function(x) x$country)

      plot_ly() |>
        add_trace(
          x = countries,
          y = sapply(comparison_rows, function(x) x$wbes_power),
          name = "Reliable Power (WBES)",
          type = "bar",
          marker = list(color = "#1B6B5F")
        ) |>
        add_trace(
          x = countries,
          y = sapply(comparison_rows, function(x) x$wb_power),
          name = "Electricity Access (WB)",
          type = "bar",
          marker = list(color = "#F4A460")
        ) |>
        add_trace(
          x = countries,
          y = sapply(comparison_rows, function(x) x$wbes_corruption),
          name = "Low Corruption (WBES)",
          type = "bar",
          marker = list(color = "#2E7D32")
        ) |>
        add_trace(
          x = countries,
          y = sapply(comparison_rows, function(x) x$wb_corruption),
          name = "Control of Corruption (WB)",
          type = "bar",
          marker = list(color = "#F49B7A")
        ) |>
        layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Score (0-100)", range = c(0, 100)),
          legend = list(orientation = "h", y = -0.25),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Macro Labor Chart
    output$macro_labor_chart <- renderPlotly({
      wb_data <- wb_comparison_data()
      chart_type <- if (!is.null(input$chart_type)) input$chart_type else "bar"

      if (is.null(wb_data) || length(wb_data) == 0) {
        return(plot_ly() |>
          layout(annotations = list(list(
            text = "Labor data not available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
          )), paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE))
      }

      indicator_codes <- c("SL.TLF.CACT.ZS", "SL.TLF.CACT.FE.ZS", "SL.UEM.TOTL.ZS")
      indicator_labels <- c("Labor Force %", "Female Labor %", "Unemployment %")
      chart_df <- prepare_wb_chart_data(wb_data, indicator_codes, indicator_labels)

      if (is.null(chart_df)) {
        return(plot_ly() |> layout(annotations = list(list(
          text = "No data available", showarrow = FALSE
        ))) |> config(displayModeBar = FALSE))
      }

      colors <- c("#1B6B5F", "#9C27B0", "#dc3545")

      if (chart_type == "radar") {
        p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
        for (i in seq_len(nrow(chart_df))) {
          values <- as.numeric(chart_df[i, indicator_labels])
          values <- c(values, values[1])
          theta <- c(indicator_labels, indicator_labels[1])
          p <- p |> add_trace(
            r = values, theta = theta, name = chart_df$country[i],
            line = list(color = colors[((i - 1) %% length(colors)) + 1]),
            fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
          )
        }
        p |> layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          showlegend = TRUE, legend = list(orientation = "v", x = 1.02, y = 0.5),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |> config(displayModeBar = FALSE)

      } else if (chart_type == "heatmap") {
        z_matrix <- as.matrix(chart_df[, indicator_labels])
        plot_ly(
          x = indicator_labels, y = chart_df$country, z = z_matrix,
          type = "heatmap",
          colorscale = list(c(0, "#f8f9fa"), c(1, "#1B6B5F")),
          zmin = 0, zmax = 100,
          hovertemplate = "%{y}<br>%{x}: %{z:.1f}%<extra></extra>"
        ) |>
          layout(
            xaxis = list(title = "", tickangle = -45),
            yaxis = list(title = "", autorange = "reversed"),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)

      } else {
        plot_ly() |>
          add_trace(x = chart_df$country, y = chart_df[["Labor Force %"]],
                    name = "Labor Force Participation %", type = "bar", marker = list(color = colors[1])) |>
          add_trace(x = chart_df$country, y = chart_df[["Female Labor %"]],
                    name = "Female Labor Force %", type = "bar", marker = list(color = colors[2])) |>
          add_trace(x = chart_df$country, y = chart_df[["Unemployment %"]],
                    name = "Unemployment %", type = "bar", marker = list(color = colors[3])) |>
          layout(
            barmode = "group",
            xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = chart_df$country),
            yaxis = list(title = "Percentage"),
            legend = list(orientation = "h", y = -0.3),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      }
    })

    # Macro Trade Chart
    output$macro_trade_chart <- renderPlotly({
      wb_data <- wb_comparison_data()
      chart_type <- if (!is.null(input$chart_type)) input$chart_type else "bar"

      if (is.null(wb_data) || length(wb_data) == 0) {
        return(plot_ly() |>
          layout(annotations = list(list(
            text = "Trade data not available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
          )), paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE))
      }

      indicator_codes <- c("NE.EXP.GNFS.ZS", "NE.IMP.GNFS.ZS")
      indicator_labels <- c("Exports (% GDP)", "Imports (% GDP)")
      chart_df <- prepare_wb_chart_data(wb_data, indicator_codes, indicator_labels)

      if (is.null(chart_df)) {
        return(plot_ly() |> layout(annotations = list(list(
          text = "No data available", showarrow = FALSE
        ))) |> config(displayModeBar = FALSE))
      }

      colors <- c("#2E7D32", "#F49B7A")

      if (chart_type == "radar") {
        p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
        max_val <- max(unlist(chart_df[, indicator_labels]), na.rm = TRUE)
        range_max <- ceiling(max_val * 1.1 / 10) * 10
        for (i in seq_len(nrow(chart_df))) {
          values <- as.numeric(chart_df[i, indicator_labels])
          values <- c(values, values[1])
          theta <- c(indicator_labels, indicator_labels[1])
          p <- p |> add_trace(
            r = values, theta = theta, name = chart_df$country[i],
            line = list(color = colors[((i - 1) %% length(colors)) + 1]),
            fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
          )
        }
        p |> layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
          showlegend = TRUE, legend = list(orientation = "v", x = 1.02, y = 0.5),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |> config(displayModeBar = FALSE)

      } else if (chart_type == "heatmap") {
        z_matrix <- as.matrix(chart_df[, indicator_labels])
        plot_ly(
          x = indicator_labels, y = chart_df$country, z = z_matrix,
          type = "heatmap",
          colorscale = list(c(0, "#f8f9fa"), c(1, "#2E7D32")),
          hovertemplate = "%{y}<br>%{x}: %{z:.1f}%<extra></extra>"
        ) |>
          layout(
            xaxis = list(title = "", tickangle = -45),
            yaxis = list(title = "", autorange = "reversed"),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)

      } else {
        plot_ly() |>
          add_trace(x = chart_df$country, y = chart_df[["Exports (% GDP)"]],
                    name = "Exports (% GDP)", type = "bar", marker = list(color = colors[1])) |>
          add_trace(x = chart_df$country, y = chart_df[["Imports (% GDP)"]],
                    name = "Imports (% GDP)", type = "bar", marker = list(color = colors[2])) |>
          layout(
            barmode = "group",
            xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = chart_df$country),
            yaxis = list(title = "% of GDP"),
            legend = list(orientation = "h", y = -0.3),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      }
    })

    # Macro Data Table
    output$macro_data_table <- renderDT({
      wb_data <- wb_comparison_data()

      if (is.null(wb_data) || length(wb_data) == 0) {
        return(datatable(data.frame(Message = "No World Bank data available"),
                        options = list(dom = 't'), rownames = FALSE))
      }

      # Build data frame from WB results
      indicators <- c("NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG", "FP.CPI.TOTL.ZG",
                     "NE.EXP.GNFS.ZS", "EG.ELC.ACCS.ZS", "IT.NET.USER.ZS",
                     "GE.EST", "RQ.EST", "RL.EST", "CC.EST",
                     "SL.UEM.TOTL.ZS", "SL.TLF.CACT.ZS")

      table_data <- do.call(bind_rows, lapply(wb_data, function(x) {
        row <- data.frame(Country = x$country, stringsAsFactors = FALSE)
        for (ind in indicators) {
          val <- x[[ind]]
          row[[get_wb_indicator_label(ind)]] <- if (!is.null(val) && !is.na(val)) {
            round(val, 2)
          } else {
            NA
          }
        }
        row
      }))

      # Apply sort order based on GDP per capita
      sort_dir <- if (!is.null(input$sort_order) && input$sort_order == "asc") FALSE else TRUE
      if ("GDP per Capita" %in% names(table_data) && any(!is.na(table_data[["GDP per Capita"]]))) {
        table_data <- table_data[order(table_data[["GDP per Capita"]], decreasing = sort_dir, na.last = TRUE), ]
      }

      datatable(
        table_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        class = "table-kwiz display compact",
        rownames = FALSE
      )
    })

    # Download handler for macro data table
    output$dl_macro_data_table <- downloadHandler(
      filename = function() { paste0("macro_comparison_", Sys.Date(), ".csv") },
      content = function(file) {
        wb_data <- wb_comparison_data()
        if (!is.null(wb_data) && length(wb_data) > 0) {
          indicators <- c("NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG", "FP.CPI.TOTL.ZG",
                         "NE.EXP.GNFS.ZS", "EG.ELC.ACCS.ZS", "IT.NET.USER.ZS",
                         "GE.EST", "RQ.EST", "RL.EST", "CC.EST")

          table_data <- do.call(bind_rows, lapply(wb_data, function(x) {
            row <- data.frame(Country = x$country, stringsAsFactors = FALSE)
            for (ind in indicators) {
              val <- x[[ind]]
              row[[get_wb_indicator_label(ind)]] <- if (!is.null(val) && !is.na(val)) round(val, 2) else NA
            }
            row
          }))
          write.csv(table_data, file, row.names = FALSE)
        } else {
          write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
        }
      }
    )

    # ==============================================================================
    # DOWNLOAD HANDLERS (Placeholders)
    # ==============================================================================

    # Overview downloads
    output$dl_overview_heatmap <- downloadHandler(
      filename = function() { paste0("overview_heatmap_", Sys.Date(), ".html") },
      content = function(file) { htmlwidgets::saveWidget(plotly::as_widget(output$overview_heatmap), file) }
    )
    output$dl_overview_table <- downloadHandler(
      filename = function() { paste0("overview_table_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(comparison_data(), file, row.names = FALSE) }
    )

    # Map downloads - using htmlwidgets to save leaflet maps
    output$dl_infra_map <- downloadHandler(
      filename = function() { paste0("infrastructure_map_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = "power_outages_per_month",
          indicator_label = "Power Outages/Month",
          color_palette = "Reds",
          reverse_colors = TRUE
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_finance_map <- downloadHandler(
      filename = function() { paste0("finance_map_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = "firms_with_credit_line_pct",
          indicator_label = "Credit Access %",
          color_palette = "Blues",
          reverse_colors = FALSE
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_governance_map <- downloadHandler(
      filename = function() { paste0("governance_map_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = "IC.FRM.BRIB.ZS",
          indicator_label = "Bribery Incidence %",
          color_palette = "Reds",
          reverse_colors = TRUE
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_workforce_map <- downloadHandler(
      filename = function() { paste0("workforce_map_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = "female_ownership_pct",
          indicator_label = "Female Ownership %",
          color_palette = "Purples",
          reverse_colors = FALSE
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_performance_map <- downloadHandler(
      filename = function() { paste0("performance_map_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = "capacity_utilization_pct",
          indicator_label = "Capacity Utilization %",
          color_palette = "Greens",
          reverse_colors = FALSE
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_crime_map <- downloadHandler(
      filename = function() { paste0("crime_map_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = "crime_obstacle_pct",
          indicator_label = "Crime Obstacle %",
          color_palette = "Reds",
          reverse_colors = TRUE
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

  })
}
