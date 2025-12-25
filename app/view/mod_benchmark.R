# app/view/mod_benchmark.R
# Cross-Country Benchmarking Module with Domain Sub-menus

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, p,
        fluidRow, column, selectInput, selectizeInput, renderUI, uiOutput,
        observeEvent, actionButton, HTML, downloadButton, downloadHandler],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config, subplot],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, mutate, desc, group_by, summarise, n, across, any_of],
  leaflet[leafletOutput, renderLeaflet],
  stats[setNames, reorder],
  htmlwidgets[saveWidget],
  utils[write.csv],
  rlang[sym],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates],
  app/logic/scatter_utils[create_scatter_with_trend],
  app/logic/chart_utils[create_chart_caption, map_with_caption, generate_chart_id]
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
