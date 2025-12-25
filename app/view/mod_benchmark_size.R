# app/view/mod_benchmark_size.R
# Cross-Size Benchmarking Module with Domain Sub-menus

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, p,
        fluidRow, column, selectInput, selectizeInput, renderUI, uiOutput,
        observeEvent, actionButton, tabsetPanel, tabPanel, HTML,
        downloadButton, downloadHandler],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config, subplot],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, mutate, desc, group_by, summarise, n, across, any_of],
  leaflet[leafletOutput, renderLeaflet],
  stats[setNames, reorder],
  htmlwidgets[saveWidget],
  utils[write.csv],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map_3d, get_country_coordinates],
  app/logic/scatter_utils[create_scatter_with_trend],
  app/logic/chart_utils[create_chart_caption, map_with_caption, generate_chart_id]
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

# Define indicator domains
DOMAINS <- list(
  infrastructure = list(
    name = "Infrastructure",
    icon = "bolt",
    indicators = c(
      "Power Outages (per month)" = "power_outages_per_month",
      "Avg Outage Duration (hrs)" = "avg_outage_duration_hrs",
      "Firms with Generator (%)" = "firms_with_generator_pct",
      "Electricity Obstacle" = "electricity_obstacle"
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
      "Bribery Incidence (%)" = "bribery_incidence_pct",
      "Corruption Obstacle (%)" = "corruption_obstacle_pct",
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

#' @export
ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "benchmark-container",

    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("building"), "Cross-Size Benchmarking", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Compare business environment indicators across firm size categories by domain")
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
                  ns("sizes_compare"),
                  "Select Firm Sizes to Compare",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(placeholder = "Choose firm sizes...")
                )
              ),
              column(2,
                selectInput(
                  ns("group_dimension"),
                  "Group By (3rd Dim)",
                  choices = c(
                    "None" = "none",
                    "Region" = "region",
                    "Sector" = "sector",
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
              ),
              column(2,
                actionButton(ns("compare_btn"), "Update Comparison",
                             icon = icon("sync"),
                             class = "btn-kwiz-primary w-100 mt-4")
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
                p(class = "text-muted", "Summary of key indicators across all domains for selected firm sizes")
              )
            ),
            fluidRow(
              column(12, chart_with_download(ns, "overview_heatmap", height = "500px"))
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
              column(3, uiOutput(ns("infra_kpi_obstacle")))
            ),
            fluidRow(
              column(8, chart_with_download(ns, "infra_comparison")),
              column(4, chart_with_download(ns, "infra_radar"))
            ),
            fluidRow(
              class = "mt-3",
              column(4, selectInput(ns("infra_map_indicator"), "Map Indicator",
                choices = c("Power Outages/Month" = "power_outages_per_month", "Outage Duration (hrs)" = "avg_outage_duration_hrs", "Generator Usage (%)" = "firms_with_generator_pct"))),
              column(12, map_with_caption(ns, "infra_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "infra_outage_impact", height = "350px")),
              column(6, chart_with_download(ns, "infra_generator_correlation", height = "350px"))
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
              column(8, chart_with_download(ns, "finance_comparison")),
              column(4, chart_with_download(ns, "finance_radar"))
            ),
            fluidRow(
              class = "mt-3",
              column(4, selectInput(ns("finance_map_indicator"), "Map Indicator",
                choices = c("Credit Access (%)" = "firms_with_credit_line_pct", "Bank Account (%)" = "firms_with_bank_account_pct", "Collateral Required (%)" = "collateral_required_pct"))),
              column(12, map_with_caption(ns, "finance_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "finance_access_gap", height = "350px")),
              column(6, chart_with_download(ns, "finance_collateral_burden", height = "350px"))
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
              column(8, chart_with_download(ns, "governance_comparison")),
              column(4, chart_with_download(ns, "governance_radar"))
            ),
            fluidRow(
              class = "mt-3",
              column(4, selectInput(ns("governance_map_indicator"), "Map Indicator",
                choices = c("Bribery Incidence (%)" = "bribery_incidence_pct", "Corruption Obstacle (%)" = "corruption_obstacle_pct"))),
              column(12, map_with_caption(ns, "governance_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "governance_bribery_vs_corruption", height = "350px")),
              column(6, chart_with_download(ns, "governance_regulatory_burden", height = "350px"))
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
              column(8, chart_with_download(ns, "workforce_comparison")),
              column(4, chart_with_download(ns, "workforce_radar"))
            ),
            fluidRow(
              class = "mt-3",
              column(4, selectInput(ns("workforce_map_indicator"), "Map Indicator",
                choices = c("Female Ownership (%)" = "female_ownership_pct", "Female Workers (%)" = "female_workers_pct"))),
              column(12, map_with_caption(ns, "workforce_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "workforce_gender_gap", height = "350px")),
              column(6, chart_with_download(ns, "workforce_obstacle_correlation", height = "350px"))
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
              column(8, chart_with_download(ns, "performance_comparison")),
              column(4, chart_with_download(ns, "performance_radar"))
            ),
            fluidRow(
              class = "mt-3",
              column(4, selectInput(ns("performance_map_indicator"), "Map Indicator",
                choices = c("Capacity Utilization (%)" = "capacity_utilization_pct", "Export Firms (%)" = "export_firms_pct"))),
              column(12, map_with_caption(ns, "performance_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "performance_capacity_vs_exports", height = "350px")),
              column(6, chart_with_download(ns, "performance_growth_distribution", height = "350px"))
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
              column(8, chart_with_download(ns, "crime_comparison")),
              column(4, chart_with_download(ns, "crime_radar"))
            ),
            fluidRow(
              class = "mt-3",
              column(4, selectInput(ns("crime_map_indicator"), "Map Indicator",
                choices = c("Crime Obstacle (%)" = "crime_obstacle_pct", "Security Costs (% Sales)" = "security_costs_pct"))),
              column(12, map_with_caption(ns, "crime_map"))
            ),
            fluidRow(class = "mt-3",
              column(6, chart_with_download(ns, "crime_vs_security_cost", height = "350px")),
              column(6, chart_with_download(ns, "crime_impact_performance", height = "350px"))
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

    # Filtered data (EXCEPT firm_size filter)
    filtered_data <- reactive({
      req(wbes_data())

      # Use country_panel (has year) if year filter is active, otherwise use latest
      filters <- if (!is.null(global_filters)) global_filters() else NULL
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      data <- if (use_panel) wbes_data()$country_panel else wbes_data()$latest

      if (!is.null(filters)) {
        data <- apply_common_filters(
          data,
          region_value = filters$region,
          sector_value = filters$sector,
          firm_size_value = "all",
          income_value = filters$income,
          year_value = filters$year,
          custom_regions = filters$custom_regions,
          filter_by_region_fn = filter_by_region
        )
      }

      # Add coordinates if using panel data
      if (use_panel && !is.null(wbes_data()$country_coordinates)) {
        coords <- wbes_data()$country_coordinates
        if ("lat" %in% names(coords) && "lng" %in% names(coords)) {
          data <- merge(data, coords, by = "country", all.x = TRUE)
        }
      }

      data
    })

    # Data filtered by selected sizes
    selected_size_data <- reactive({
      req(filtered_data(), input$sizes_compare)
      data <- filtered_data()
      selected <- input$sizes_compare

      if (length(selected) > 0) {
        data <- data |> filter(firm_size %in% selected)
      }

      data
    })

    # Update size choices
    observeEvent(filtered_data(), {
      req(filtered_data())
      data <- filtered_data()

      sizes <- data$firm_size |>
        unique() |>
        stats::na.omit() |>
        as.character() |>
        sort()

      current_selection <- input$sizes_compare
      valid_selection <- current_selection[current_selection %in% sizes]

      if (length(valid_selection) == 0) {
        valid_selection <- sizes
      }

      shiny::updateSelectizeInput(
        session, "sizes_compare",
        choices = setNames(sizes, sizes),
        selected = valid_selection
      )
    })

    # Aggregate size data
    size_aggregated <- reactive({
      req(filtered_data())
      data <- filtered_data()

      group_dim <- if (!is.null(input$group_dimension) && input$group_dimension != "none") {
        input$group_dimension
      } else {
        NULL
      }

      # Use unname to preserve column names in across()
      all_indicators <- unname(unlist(lapply(DOMAINS, function(d) d$indicators)))

      if (!is.null(group_dim) && group_dim %in% names(data)) {
        agg <- data |>
          filter(!is.na(firm_size), !is.na(.data[[group_dim]])) |>
          group_by(firm_size, .data[[group_dim]]) |>
          summarise(
            countries_count = length(unique(country[!is.na(country)])),
            firms_count = sum(sample_size, na.rm = TRUE),
            across(any_of(all_indicators), ~mean(.x, na.rm = TRUE)),
            .groups = "drop"
          )
        names(agg)[names(agg) == group_dim] <- "group_value"
        agg$group_dimension <- group_dim
      } else {
        agg <- data |>
          filter(!is.na(firm_size)) |>
          group_by(firm_size) |>
          summarise(
            countries_count = length(unique(country[!is.na(country)])),
            firms_count = sum(sample_size, na.rm = TRUE),
            across(any_of(all_indicators), ~mean(.x, na.rm = TRUE)),
            .groups = "drop"
          )
        agg$group_value <- NA
        agg$group_dimension <- NA
      }

      agg
    })

    # Comparison data - reactive to selection changes
    comparison_data <- reactive({
      req(size_aggregated())
      data <- size_aggregated()

      # Get selected sizes (may be NULL initially)
      selected <- input$sizes_compare

      if (is.null(selected) || length(selected) == 0) {
        # Return all sizes by default
        available_sizes <- unique(data$firm_size)
        selected <- available_sizes[1:min(3, length(available_sizes))]
      }

      filter(data, firm_size %in% selected)
    })

    # Helper: create domain chart
    create_domain_chart <- function(data, domain_key, chart_type = "bar") {
      domain <- DOMAINS[[domain_key]]
      indicators <- domain$indicators

      # Filter to available indicators - use as.character to get just the column names
      indicator_cols <- as.character(indicators)
      available <- intersect(indicator_cols, names(data))

      if (length(available) == 0) {
        return(plot_ly() |> layout(annotations = list(list(
          text = paste("No", domain$name, "data available"),
          showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5
        ))))
      }

      # Get display names for available indicators
      indicator_names <- names(indicators)[match(available, indicator_cols)]
      group_dim <- input$group_dimension
      has_grouping <- !is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data) && !all(is.na(data$group_value))

      if (chart_type == "bar") {
        if (has_grouping) {
          # Get unique group values for consistent coloring
          group_values <- unique(data$group_value[!is.na(data$group_value)])
          colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

          # Create grouped bar chart for each indicator
          plots <- lapply(seq_along(available), function(i) {
            ind <- available[i]
            ind_name <- indicator_names[i]

            p <- plot_ly()
            for (j in seq_along(group_values)) {
              gv <- group_values[j]
              subset_data <- data[data$group_value == gv, ]
              y_vals <- subset_data[[ind]]

              p <- p |> add_trace(
                x = subset_data$firm_size,
                y = y_vals,
                type = "bar",
                name = as.character(gv),
                legendgroup = as.character(gv),
                showlegend = (i == 1),  # Only show legend on first chart
                marker = list(color = colors[((j - 1) %% length(colors)) + 1]),
                hovertemplate = paste0(ind_name, " (", gv, "): %{y:.1f}<extra></extra>")
              )
            }
            p |> layout(
              xaxis = list(title = ind_name, tickangle = -45, automargin = TRUE),
              yaxis = list(title = "", automargin = TRUE)
            )
          })

          subplot(plots, nrows = 1, shareY = FALSE, titleX = TRUE, margin = 0.05) |>
            layout(
              title = list(text = paste(domain$name, "Indicators by", tools::toTitleCase(gsub("_", " ", group_dim))), font = list(size = 14)),
              barmode = "group",
              showlegend = TRUE,
              legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)", title = list(text = tools::toTitleCase(gsub("_", " ", group_dim)))),
              margin = list(l = 60, r = 120, t = 40, b = 120),
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        } else {
          # No grouping - simple bar chart
          plots <- lapply(seq_along(available), function(i) {
            ind <- available[i]
            ind_name <- indicator_names[i]
            y_vals <- data[[ind]]

            plot_ly(data, x = ~firm_size, y = y_vals, type = "bar",
                    marker = list(color = "#1B6B5F"), name = ind_name,
                    hovertemplate = paste0(ind_name, ": %{y:.1f}<extra></extra>"))
          })

          subplot(plots, nrows = 1, shareY = FALSE, titleX = TRUE) |>
            layout(
              title = list(text = paste(domain$name, "Indicators"), font = list(size = 14)),
              barmode = "group",
              showlegend = TRUE,
              legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
              margin = list(l = 60, r = 120, t = 40, b = 120),
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        }
      } else if (chart_type == "heatmap") {
        # Heatmap chart - matrix of sizes vs indicators
        create_domain_heatmap(data, available, indicator_names, domain$name)
      } else {
        # Radar chart - faceted by size
        create_faceted_radar(data, available, indicator_names, domain$name)
      }
    }

    # Helper: radar chart (legacy - kept for compatibility)
    create_radar_chart <- function(data, indicators, indicator_names) {
      sizes <- unique(data$firm_size)

      # Collect all values for dynamic range calculation
      all_values <- c()
      plot_data <- lapply(sizes, function(s) {
        size_data <- data[data$firm_size == s, ]
        values <- sapply(indicators, function(ind) {
          val <- size_data[[ind]][1]
          if (is.na(val)) 0 else val
        })
        all_values <<- c(all_values, values)
        values <- c(values, values[1])
        list(size = s, values = values)
      })

      # Calculate dynamic range with 10% padding
      max_val <- max(all_values, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10  # Round up to nearest 10 with 10% padding

      p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers', fill = 'toself')
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      for (i in seq_along(plot_data)) {
        p <- p |> add_trace(
          r = plot_data[[i]]$values,
          theta = c(indicator_names, indicator_names[1]),
          name = plot_data[[i]]$size,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33")
        )
      }

      p |>
        layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, range_max))),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center"),
          margin = list(b = 80),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    }

    # Helper function to create faceted radar charts (one per size, with nested grouping if 3rd dim selected)
    create_faceted_radar <- function(data, indicators, indicator_names, domain_name) {
      group_dim <- input$group_dimension
      has_grouping <- !is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data) && !all(is.na(data$group_value))

      sizes <- unique(data$firm_size)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")
      n_sizes <- length(sizes)

      # Calculate dynamic range from all indicator values
      all_values <- unlist(lapply(indicators, function(ind) data[[ind]]))
      max_val <- max(all_values, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10  # Round up to nearest 10 with 10% padding

      # Calculate grid layout
      n_cols <- min(3, n_sizes)
      n_rows <- ceiling(n_sizes / n_cols)

      # Calculate domain positions for each radar
      get_domain <- function(idx, total, n_cols) {
        row <- (idx - 1) %/% n_cols
        col <- (idx - 1) %% n_cols
        n_rows <- ceiling(total / n_cols)

        x_size <- 0.9 / n_cols
        y_size <- 0.85 / n_rows
        x_gap <- 0.1 / (n_cols + 1)
        y_gap <- 0.1 / (n_rows + 1)

        x_start <- x_gap + col * (x_size + x_gap)
        y_start <- 1 - (row + 1) * (y_size + y_gap)

        list(x = c(x_start, x_start + x_size), y = c(y_start, y_start + y_size))
      }

      p <- plot_ly()

      # Build layout with multiple polar axes
      layout_args <- list(
        title = list(text = paste(domain_name, "- Radar Comparison"), font = list(size = 14)),
        paper_bgcolor = "rgba(0,0,0,0)",
        showlegend = has_grouping,
        legend = list(orientation = "v", x = 1.02, y = 0.5, yanchor = "middle", bgcolor = "rgba(255,255,255,0.8)"),
        margin = list(l = 40, r = 120, t = 40, b = 40)
      )

      annotations <- list()

      for (i in seq_along(sizes)) {
        s <- sizes[i]
        polar_name <- if (i == 1) "polar" else paste0("polar", i)
        domain <- get_domain(i, n_sizes, n_cols)

        # Add polar axis to layout with dynamic range
        layout_args[[polar_name]] <- list(
          domain = domain,
          radialaxis = list(visible = TRUE, range = c(0, range_max), tickfont = list(size = 8)),
          angularaxis = list(tickfont = list(size = 7))
        )

        # Add annotation (title) for this radar
        annotations[[length(annotations) + 1]] <- list(
          text = s,
          x = mean(domain$x),
          y = domain$y[2] + 0.03,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 10, weight = "bold", color = colors[((i - 1) %% length(colors)) + 1])
        )

        if (has_grouping) {
          # Nested faceting: show each group value within this size
          group_values <- unique(data$group_value[!is.na(data$group_value)])
          for (j in seq_along(group_values)) {
            gv <- group_values[j]
            subset_data <- data[data$firm_size == s & data$group_value == gv, ]

            if (nrow(subset_data) > 0) {
              values <- sapply(indicators, function(ind) {
                val <- mean(subset_data[[ind]], na.rm = TRUE)
                if (is.na(val)) 0 else val
              })
              values <- c(values, values[1])
              theta <- c(indicator_names, indicator_names[1])

              p <- p |> add_trace(
                type = 'scatterpolar',
                mode = 'lines+markers',
                fill = 'toself',
                r = values,
                theta = theta,
                name = as.character(gv),
                legendgroup = as.character(gv),
                showlegend = (i == 1),
                line = list(color = colors[((j - 1) %% length(colors)) + 1]),
                fillcolor = paste0(colors[((j - 1) %% length(colors)) + 1], "22"),
                marker = list(size = 4),
                subplot = polar_name
              )
            }
          }
        } else {
          # No grouping: single radar per size
          size_data <- data[data$firm_size == s, ]
          values <- sapply(indicators, function(ind) {
            val <- mean(size_data[[ind]], na.rm = TRUE)
            if (is.na(val)) 0 else val
          })
          values <- c(values, values[1])
          theta <- c(indicator_names, indicator_names[1])

          p <- p |> add_trace(
            type = 'scatterpolar',
            mode = 'lines+markers',
            fill = 'toself',
            r = values,
            theta = theta,
            name = s,
            line = list(color = colors[((i - 1) %% length(colors)) + 1]),
            fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33"),
            marker = list(size = 5),
            showlegend = FALSE,
            subplot = polar_name
          )
        }
      }

      layout_args$annotations <- annotations

      # Apply layout using do.call
      p <- do.call(layout, c(list(p), layout_args))
      p |> config(displayModeBar = FALSE)
    }

    # Helper function to create domain heatmap (faceted by 3rd dimension if selected)
    create_domain_heatmap <- function(data, indicators, indicator_names, domain_name) {
      group_dim <- input$group_dimension
      has_grouping <- !is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data) && !all(is.na(data$group_value))

      sizes <- unique(data$firm_size)

      if (has_grouping) {
        # Faceted heatmaps - one per group value
        group_values <- unique(data$group_value[!is.na(data$group_value)])
        n_groups <- length(group_values)
        n_cols <- min(3, n_groups)
        n_rows <- ceiling(n_groups / n_cols)

        # Calculate domain positions for each heatmap
        get_domain <- function(idx, total, n_cols) {
          row <- (idx - 1) %/% n_cols
          col <- (idx - 1) %% n_cols
          n_rows <- ceiling(total / n_cols)

          x_size <- 0.88 / n_cols
          y_size <- 0.82 / n_rows
          x_gap <- 0.12 / (n_cols + 1)
          y_gap <- 0.12 / (n_rows + 1)

          x_start <- x_gap + col * (x_size + x_gap)
          y_start <- 1 - (row + 1) * (y_size + y_gap)

          list(x = c(x_start, x_start + x_size), y = c(y_start, y_start + y_size))
        }

        # Find global min/max for consistent color scale
        all_vals <- unlist(lapply(group_values, function(gv) {
          subset_data <- data[data$group_value == gv, ]
          unlist(lapply(indicators, function(ind) {
            mean(subset_data[[ind]], na.rm = TRUE)
          }))
        }))
        z_min <- min(all_vals, na.rm = TRUE)
        z_max <- max(all_vals, na.rm = TRUE)

        p <- plot_ly()
        annotations <- list()

        for (i in seq_along(group_values)) {
          gv <- group_values[i]
          subset_data <- data[data$group_value == gv, ]
          domain <- get_domain(i, n_groups, n_cols)

          # Create matrix for this group
          z_matrix <- sapply(indicators, function(ind) {
            sapply(sizes, function(s) {
              val <- subset_data[subset_data$firm_size == s, ind, drop = TRUE]
              if (length(val) > 0) {
                mean_val <- mean(val, na.rm = TRUE)
                if (!is.na(mean_val)) mean_val else NA
              } else {
                NA
              }
            })
          })

          # Axis names for this subplot
          xaxis_name <- if (i == 1) "xaxis" else paste0("xaxis", i)
          yaxis_name <- if (i == 1) "yaxis" else paste0("yaxis", i)
          xref <- if (i == 1) "x" else paste0("x", i)
          yref <- if (i == 1) "y" else paste0("y", i)

          p <- p |> add_trace(
            type = "heatmap",
            x = indicator_names,
            y = sizes,
            z = z_matrix,
            colorscale = list(c(0, "#f7fbff"), c(0.5, "#6baed6"), c(1, "#08306b")),
            zmin = z_min,
            zmax = z_max,
            showscale = (i == 1),
            hovertemplate = paste0(gv, "<br>%{y}<br>%{x}: %{z:.1f}<extra></extra>"),
            xaxis = xref,
            yaxis = yref
          )

          # Add annotation (title) for this heatmap
          annotations[[length(annotations) + 1]] <- list(
            text = paste0("<b>", gv, "</b>"),
            x = mean(domain$x),
            y = domain$y[2] + 0.03,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 11)
          )
        }

        # Build layout with multiple axes
        layout_args <- list(
          title = list(text = paste(domain_name, "by", tools::toTitleCase(gsub("_", " ", group_dim))), font = list(size = 14)),
          paper_bgcolor = "rgba(0,0,0,0)",
          annotations = annotations
        )

        for (i in seq_along(group_values)) {
          domain <- get_domain(i, n_groups, n_cols)
          xaxis_name <- if (i == 1) "xaxis" else paste0("xaxis", i)
          yaxis_name <- if (i == 1) "yaxis" else paste0("yaxis", i)

          layout_args[[xaxis_name]] <- list(
            domain = domain$x,
            anchor = if (i == 1) "y" else paste0("y", i),
            tickangle = -45,
            tickfont = list(size = 8)
          )
          layout_args[[yaxis_name]] <- list(
            domain = domain$y,
            anchor = if (i == 1) "x" else paste0("x", i),
            tickfont = list(size = 9)
          )
        }

        p <- do.call(layout, c(list(p), layout_args))
        p |> config(displayModeBar = FALSE)

      } else {
        # Single heatmap - no grouping
        z_matrix <- sapply(indicators, function(ind) {
          sapply(sizes, function(s) {
            val <- data[data$firm_size == s, ind, drop = TRUE]
            if (length(val) > 0) {
              mean_val <- mean(val, na.rm = TRUE)
              if (!is.na(mean_val)) mean_val else NA
            } else {
              NA
            }
          })
        })

        plot_ly(
          x = indicator_names,
          y = sizes,
          z = z_matrix,
          type = "heatmap",
          colorscale = list(c(0, "#f7fbff"), c(0.5, "#6baed6"), c(1, "#08306b")),
          hovertemplate = "%{y}<br>%{x}: %{z:.1f}<extra></extra>"
        ) |>
          layout(
            title = list(text = paste(domain_name, "- Heatmap"), font = list(size = 14)),
            xaxis = list(title = "", tickangle = -45),
            yaxis = list(title = "", automargin = TRUE),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      }
    }

    # Overview heatmap
    output$overview_heatmap <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      key_indicators <- c(
        "power_outages_per_month", "firms_with_credit_line_pct", "bribery_incidence_pct",
        "female_ownership_pct", "capacity_utilization_pct", "crime_obstacle_pct"
      )
      indicator_labels <- c("Power Outages", "Credit Access", "Bribery", "Female Own", "Capacity", "Crime")

      available <- intersect(key_indicators, names(data))
      if (length(available) == 0) {
        return(plot_ly() |> layout(annotations = list(list(text = "No data", showarrow = FALSE))))
      }

      sizes <- unique(data$firm_size)
      # Create matrix for heatmap - aggregate if grouped data
      z_matrix <- sapply(available, function(ind) {
        sapply(sizes, function(s) {
          val <- data[data$firm_size == s, ind, drop = TRUE]
          # Use mean for aggregation when multiple values exist (grouped data)
          if (length(val) > 0) {
            mean_val <- mean(val, na.rm = TRUE)
            if (!is.na(mean_val)) mean_val else NA
          } else {
            NA
          }
        })
      })

      plot_ly(
        x = indicator_labels[key_indicators %in% available],
        y = sizes,
        z = z_matrix,
        type = "heatmap",
        colorscale = list(c(0, "#1B6B5F"), c(0.5, "#F4A460"), c(1, "#dc3545")),
        hovertemplate = "%{y}<br>%{x}: %{z:.1f}<extra></extra>"
      ) |>
        layout(
          title = "Key Indicators by Firm Size",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "", automargin = TRUE),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Overview table
    output$overview_table <- renderDT({
      req(comparison_data())
      data <- comparison_data()

      key_cols <- c("firm_size", "countries_count", "power_outages_per_month",
                    "firms_with_credit_line_pct", "bribery_incidence_pct",
                    "female_ownership_pct", "capacity_utilization_pct")

      display_data <- select(data, any_of(key_cols))

      datatable(
        display_data,
        options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
        class = "table-kwiz display compact",
        rownames = FALSE,
        colnames = c("Firm Size", "Countries", "Power Outages", "Credit %", "Bribery %", "Female %", "Capacity %")[1:ncol(display_data)]
      )
    })

    # Domain charts
    output$infra_comparison <- renderPlotly({ req(comparison_data()); create_domain_chart(comparison_data(), "infrastructure", input$chart_type) })
    output$infra_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- DOMAINS$infrastructure$indicators
      available <- intersect(indicators, names(data))
      indicator_names <- names(DOMAINS$infrastructure$indicators)[DOMAINS$infrastructure$indicators %in% available]
      if (length(available) > 0) create_radar_chart(data, available, indicator_names)
      else plot_ly() |> layout(annotations = list(list(text = "No data", showarrow = FALSE)))
    })
    output$infra_map <- renderLeaflet({
      req(selected_size_data(), wbes_data())
      indicator <- if (!is.null(input$infra_map_indicator)) input$infra_map_indicator else "power_outages_per_month"
      palette_info <- switch(indicator,
        "power_outages_per_month" = list(palette = "YlOrRd", label = "Power Outages/Month"),
        "avg_outage_duration_hrs" = list(palette = "YlOrRd", label = "Outage Duration (hrs)"),
        "firms_with_generator_pct" = list(palette = "Oranges", label = "Generator Usage (%)"),
        list(palette = "YlOrRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = indicator, size_col = indicator,
                         color_label = palette_info$label, size_label = palette_info$label, color_palette = palette_info$palette)
    })

    output$finance_comparison <- renderPlotly({ req(comparison_data()); create_domain_chart(comparison_data(), "finance", input$chart_type) })
    output$finance_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- DOMAINS$finance$indicators
      available <- intersect(indicators, names(data))
      indicator_names <- names(DOMAINS$finance$indicators)[DOMAINS$finance$indicators %in% available]
      if (length(available) > 0) create_radar_chart(data, available, indicator_names)
      else plot_ly() |> layout(annotations = list(list(text = "No data", showarrow = FALSE)))
    })
    output$finance_map <- renderLeaflet({
      req(selected_size_data(), wbes_data())
      indicator <- if (!is.null(input$finance_map_indicator)) input$finance_map_indicator else "firms_with_credit_line_pct"
      palette_info <- switch(indicator,
        "firms_with_credit_line_pct" = list(palette = "YlGn", label = "Credit Access (%)"),
        "firms_with_bank_account_pct" = list(palette = "Blues", label = "Bank Account (%)"),
        "collateral_required_pct" = list(palette = "YlOrRd", label = "Collateral Required (%)"),
        list(palette = "YlGn", label = indicator)
      )
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = indicator, size_col = indicator,
                         color_label = palette_info$label, size_label = palette_info$label, color_palette = palette_info$palette)
    })

    output$governance_comparison <- renderPlotly({ req(comparison_data()); create_domain_chart(comparison_data(), "governance", input$chart_type) })
    output$governance_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- DOMAINS$governance$indicators
      available <- intersect(indicators, names(data))
      indicator_names <- names(DOMAINS$governance$indicators)[DOMAINS$governance$indicators %in% available]
      if (length(available) > 0) create_radar_chart(data, available, indicator_names)
      else plot_ly() |> layout(annotations = list(list(text = "No data", showarrow = FALSE)))
    })
    output$governance_map <- renderLeaflet({
      req(selected_size_data(), wbes_data())
      indicator <- if (!is.null(input$governance_map_indicator)) input$governance_map_indicator else "bribery_incidence_pct"
      palette_info <- switch(indicator,
        "bribery_incidence_pct" = list(palette = "YlOrRd", label = "Bribery Incidence (%)"),
        "corruption_obstacle_pct" = list(palette = "Reds", label = "Corruption Obstacle (%)"),
        list(palette = "YlOrRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = indicator, size_col = indicator,
                         color_label = palette_info$label, size_label = palette_info$label, color_palette = palette_info$palette)
    })

    output$workforce_comparison <- renderPlotly({ req(comparison_data()); create_domain_chart(comparison_data(), "workforce", input$chart_type) })
    output$workforce_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- DOMAINS$workforce$indicators
      available <- intersect(indicators, names(data))
      indicator_names <- names(DOMAINS$workforce$indicators)[DOMAINS$workforce$indicators %in% available]
      if (length(available) > 0) create_radar_chart(data, available, indicator_names)
      else plot_ly() |> layout(annotations = list(list(text = "No data", showarrow = FALSE)))
    })
    output$workforce_map <- renderLeaflet({
      req(selected_size_data(), wbes_data())
      indicator <- if (!is.null(input$workforce_map_indicator)) input$workforce_map_indicator else "female_ownership_pct"
      palette_info <- switch(indicator,
        "female_ownership_pct" = list(palette = "PuRd", label = "Female Ownership (%)"),
        "female_workers_pct" = list(palette = "Purples", label = "Female Workers (%)"),
        list(palette = "PuRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = indicator, size_col = indicator,
                         color_label = palette_info$label, size_label = palette_info$label, color_palette = palette_info$palette)
    })

    output$performance_comparison <- renderPlotly({ req(comparison_data()); create_domain_chart(comparison_data(), "performance", input$chart_type) })
    output$performance_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- DOMAINS$performance$indicators
      available <- intersect(indicators, names(data))
      indicator_names <- names(DOMAINS$performance$indicators)[DOMAINS$performance$indicators %in% available]
      if (length(available) > 0) create_radar_chart(data, available, indicator_names)
      else plot_ly() |> layout(annotations = list(list(text = "No data", showarrow = FALSE)))
    })
    output$performance_map <- renderLeaflet({
      req(selected_size_data(), wbes_data())
      indicator <- if (!is.null(input$performance_map_indicator)) input$performance_map_indicator else "capacity_utilization_pct"
      palette_info <- switch(indicator,
        "capacity_utilization_pct" = list(palette = "YlGn", label = "Capacity Utilization (%)"),
        "export_firms_pct" = list(palette = "Blues", label = "Export Firms (%)"),
        list(palette = "YlGn", label = indicator)
      )
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = indicator, size_col = indicator,
                         color_label = palette_info$label, size_label = palette_info$label, color_palette = palette_info$palette)
    })

    output$crime_comparison <- renderPlotly({ req(comparison_data()); create_domain_chart(comparison_data(), "crime", input$chart_type) })
    output$crime_radar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicators <- DOMAINS$crime$indicators
      available <- intersect(indicators, names(data))
      indicator_names <- names(DOMAINS$crime$indicators)[DOMAINS$crime$indicators %in% available]
      if (length(available) > 0) create_radar_chart(data, available, indicator_names)
      else plot_ly() |> layout(annotations = list(list(text = "No data", showarrow = FALSE)))
    })
    output$crime_map <- renderLeaflet({
      req(selected_size_data(), wbes_data())
      indicator <- if (!is.null(input$crime_map_indicator)) input$crime_map_indicator else "crime_obstacle_pct"
      palette_info <- switch(indicator,
        "crime_obstacle_pct" = list(palette = "YlOrRd", label = "Crime Obstacle (%)"),
        "security_costs_pct" = list(palette = "Oranges", label = "Security Costs (% Sales)"),
        list(palette = "YlOrRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = indicator, size_col = indicator,
                         color_label = palette_info$label, size_label = palette_info$label, color_palette = palette_info$palette)
    })

    # ============================================================
    # KPIs, Analysis Charts, and Insights
    # ============================================================

    # Helper function for KPI card
    create_kpi_card <- function(value, label, bg_color = "primary") {
      div(class = paste0("card bg-", bg_color, " text-white h-100"),
        div(class = "card-body text-center py-2",
          h4(value, class = "mb-0"),
          p(label, class = "small mb-0")))
    }

    # --- INFRASTRUCTURE KPIs ---
    output$infra_kpi_outages <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$power_outages_per_month, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "/mo"), "Avg Power Outages", "danger")
    })
    output$infra_kpi_duration <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$avg_outage_duration_hrs, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, " hrs"), "Avg Duration", "warning")
    })
    output$infra_kpi_generator <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$firms_with_generator_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Firms w/ Generator", "info")
    })
    output$infra_kpi_obstacle <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$electricity_obstacle, na.rm = TRUE), 1)
      create_kpi_card(val, "Obstacle Score", "secondary")
    })

    # Infrastructure Analysis Charts
    output$infra_outage_impact <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("power_outages_per_month", "capacity_utilization_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "power_outages_per_month", y_col = "capacity_utilization_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Outage Impact on Capacity", x_label = "Power Outages/Month", y_label = "Capacity Utilization %"
      )
    })
    output$infra_generator_correlation <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("power_outages_per_month", "firms_with_generator_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "power_outages_per_month", y_col = "firms_with_generator_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Generator Adoption vs Outages", x_label = "Power Outages/Month", y_label = "Firms with Generator %"
      )
    })
    output$infra_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      worst_size <- data$firm_size[which.max(data$power_outages_per_month)]
      best_size <- data$firm_size[which.min(data$power_outages_per_month)]
      avg_gen <- round(mean(data$firms_with_generator_pct, na.rm = TRUE), 1)
      tags$ul(
        tags$li(paste0("Highest outages: ", worst_size, " firms")),
        tags$li(paste0("Lowest outages: ", best_size, " firms")),
        tags$li(paste0("Average generator adoption: ", avg_gen, "%")),
        tags$li("Larger firms tend to have better infrastructure resilience")
      )
    })

    # --- FINANCE KPIs ---
    output$finance_kpi_credit <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$firms_with_credit_line_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Credit Line Access", "success")
    })
    output$finance_kpi_bank <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$firms_with_bank_account_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Bank Account", "primary")
    })
    output$finance_kpi_rejection <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$loan_rejection_rate_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Loan Rejection", "danger")
    })
    output$finance_kpi_collateral <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$collateral_required_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Collateral Required", "warning")
    })

    # Finance Analysis Charts
    output$finance_access_gap <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("firms_with_credit_line_pct", "loan_rejection_rate_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "firms_with_credit_line_pct", y_col = "loan_rejection_rate_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Credit Access vs Loan Rejection", x_label = "Credit Line Access %", y_label = "Loan Rejection Rate %"
      )
    })
    output$finance_collateral_burden <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("collateral_required_pct", "firms_with_credit_line_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "collateral_required_pct", y_col = "firms_with_credit_line_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Collateral Burden vs Credit Access", x_label = "Collateral Required %", y_label = "Credit Line Access %"
      )
    })
    output$finance_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      best_credit <- data$firm_size[which.max(data$firms_with_credit_line_pct)]
      worst_rejection <- data$firm_size[which.max(data$loan_rejection_rate_pct)]
      avg_collateral <- round(mean(data$collateral_required_pct, na.rm = TRUE), 1)
      tags$ul(
        tags$li(paste0("Best credit access: ", best_credit, " firms")),
        tags$li(paste0("Highest rejection rate: ", worst_rejection, " firms")),
        tags$li(paste0("Average collateral requirement: ", avg_collateral, "%")),
        tags$li("Smaller firms face greater financial constraints")
      )
    })

    # --- GOVERNANCE KPIs ---
    output$governance_kpi_bribery <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$bribery_incidence_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Bribery Incidence", "danger")
    })
    output$governance_kpi_corruption <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$corruption_obstacle_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Corruption Obstacle", "warning")
    })
    output$governance_kpi_regulations <- renderUI({
      req(comparison_data())
      val <- if ("mgmt_time_regulations_pct" %in% names(comparison_data()) &&
                 any(!is.na(comparison_data()$mgmt_time_regulations_pct))) {
        round(mean(comparison_data()$mgmt_time_regulations_pct, na.rm = TRUE), 1)
      } else {
        "N/A"
      }
      create_kpi_card(if (is.numeric(val)) paste0(val, "%") else val, "Mgmt Time on Regs", "info")
    })

    # Governance Analysis Charts
    output$governance_bribery_vs_corruption <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("bribery_incidence_pct", "corruption_obstacle_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "bribery_incidence_pct", y_col = "corruption_obstacle_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Bribery vs Corruption Perception", x_label = "Bribery Incidence %", y_label = "Corruption Obstacle %"
      )
    })
    output$governance_regulatory_burden <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      # Check if mgmt_time_regulations_pct has valid data
      has_valid_regs <- "mgmt_time_regulations_pct" %in% names(data) &&
                        any(!is.na(data$mgmt_time_regulations_pct))
      if (!has_valid_regs) {
        create_scatter_with_trend(
          data = data, x_col = "bribery_incidence_pct", y_col = "capacity_utilization_pct",
          color_col = "firm_size", shape_col = shape_col,
          title = "Bribery Impact on Productivity", x_label = "Bribery Incidence %", y_label = "Capacity Utilization %"
        )
      } else {
        create_scatter_with_trend(
          data = data, x_col = "mgmt_time_regulations_pct", y_col = "corruption_obstacle_pct",
          color_col = "firm_size", shape_col = shape_col,
          title = "Regulatory Burden vs Corruption", x_label = "Mgmt Time on Regulations %", y_label = "Corruption Obstacle %"
        )
      }
    })
    output$governance_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      worst_bribery <- data$firm_size[which.max(data$bribery_incidence_pct)]
      best_governance <- data$firm_size[which.min(data$corruption_obstacle_pct)]
      avg_regs <- if ("mgmt_time_regulations_pct" %in% names(data) &&
                      any(!is.na(data$mgmt_time_regulations_pct))) {
        round(mean(data$mgmt_time_regulations_pct, na.rm = TRUE), 1)
      } else {
        NA
      }
      reg_text <- if (is.na(avg_regs)) {
        "Regulatory burden data not available"
      } else {
        paste0("Average regulatory burden: ", avg_regs, "% of mgmt time")
      }
      tags$ul(
        tags$li(paste0("Highest bribery incidence: ", worst_bribery, " firms")),
        tags$li(paste0("Best governance environment: ", best_governance, " firms")),
        tags$li(reg_text),
        tags$li("Governance challenges vary by firm size")
      )
    })

    # --- WORKFORCE KPIs ---
    output$workforce_kpi_female_own <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$female_ownership_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Female Ownership", "primary")
    })
    output$workforce_kpi_female_work <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$female_workers_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Female Workers", "success")
    })
    output$workforce_kpi_obstacle <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$workforce_obstacle_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Workforce Obstacle", "warning")
    })

    # Workforce Analysis Charts
    output$workforce_gender_gap <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("female_ownership_pct", "female_workers_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "female_ownership_pct", y_col = "female_workers_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Gender in Ownership vs Workforce", x_label = "Female Ownership %", y_label = "Female Workers %"
      )
    })
    output$workforce_obstacle_correlation <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("workforce_obstacle_pct", "capacity_utilization_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "workforce_obstacle_pct", y_col = "capacity_utilization_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Workforce Obstacle vs Capacity", x_label = "Workforce Obstacle %", y_label = "Capacity Utilization %"
      )
    })
    output$workforce_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      best_gender <- data$firm_size[which.max(data$female_ownership_pct)]
      worst_obstacle <- data$firm_size[which.max(data$workforce_obstacle_pct)]
      avg_female <- round(mean(data$female_workers_pct, na.rm = TRUE), 1)
      tags$ul(
        tags$li(paste0("Highest female ownership: ", best_gender, " firms")),
        tags$li(paste0("Greatest workforce challenges: ", worst_obstacle, " firms")),
        tags$li(paste0("Average female workforce: ", avg_female, "%")),
        tags$li("Gender diversity varies significantly by firm size")
      )
    })

    # --- PERFORMANCE KPIs ---
    output$performance_kpi_capacity <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$capacity_utilization_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Capacity Utilization", "success")
    })
    output$performance_kpi_export_firms <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$export_firms_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Exporting Firms", "primary")
    })
    output$performance_kpi_export_share <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$export_share_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Export Share", "info")
    })
    output$performance_kpi_growth <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$annual_sales_growth_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Sales Growth", "warning")
    })

    # Performance Analysis Charts
    output$performance_capacity_vs_exports <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("capacity_utilization_pct", "export_firms_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "capacity_utilization_pct", y_col = "export_firms_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Capacity vs Export Propensity", x_label = "Capacity Utilization %", y_label = "Exporting Firms %"
      )
    })
    output$performance_growth_distribution <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!"annual_sales_growth_pct" %in% names(data)) {
        return(plot_ly() |> layout(title = "Data not available"))
      }

      group_dim <- input$group_dimension
      has_grouping <- !is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data) && !all(is.na(data$group_value))

      if (has_grouping) {
        # Grouped bar chart with 3rd dimension
        group_values <- unique(data$group_value[!is.na(data$group_value)])
        colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

        p <- plot_ly()
        for (j in seq_along(group_values)) {
          gv <- group_values[j]
          subset_data <- data[data$group_value == gv, ]

          p <- p |> add_trace(
            x = subset_data$firm_size,
            y = subset_data$annual_sales_growth_pct,
            type = "bar",
            name = as.character(gv),
            text = ~paste0(round(subset_data$annual_sales_growth_pct, 1), "%"),
            textposition = "outside",
            marker = list(color = colors[((j - 1) %% length(colors)) + 1]),
            hovertemplate = paste0("Sales Growth (", gv, "): %{y:.1f}%<extra></extra>")
          )
        }
        p |> layout(
          title = list(text = paste("Sales Growth by Firm Size and", tools::toTitleCase(gsub("_", " ", group_dim))), font = list(size = 14)),
          barmode = "group",
          xaxis = list(title = "Firm Size"),
          yaxis = list(title = "Annual Sales Growth %"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.3, x = 0.5, xanchor = "center", title = list(text = tools::toTitleCase(gsub("_", " ", group_dim)))),
          margin = list(b = 100),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
          config(displayModeBar = FALSE)
      } else {
        # Simple bar chart without grouping
        plot_ly(data, x = ~firm_size, y = ~annual_sales_growth_pct, color = ~firm_size,
                type = "bar", text = ~paste0(round(annual_sales_growth_pct, 1), "%"),
                textposition = "outside", hoverinfo = "text+name") |>
          layout(title = "Sales Growth by Firm Size", xaxis = list(title = "Firm Size"),
                 yaxis = list(title = "Annual Sales Growth %"), showlegend = FALSE,
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      }
    })
    output$performance_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      best_capacity <- data$firm_size[which.max(data$capacity_utilization_pct)]
      best_export <- data$firm_size[which.max(data$export_firms_pct)]
      avg_growth <- round(mean(data$annual_sales_growth_pct, na.rm = TRUE), 1)
      tags$ul(
        tags$li(paste0("Highest capacity utilization: ", best_capacity, " firms")),
        tags$li(paste0("Most export-oriented: ", best_export, " firms")),
        tags$li(paste0("Average sales growth: ", avg_growth, "%")),
        tags$li("Larger firms show stronger export performance")
      )
    })

    # --- CRIME KPIs ---
    output$crime_kpi_obstacle <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$crime_obstacle_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Crime Obstacle", "danger")
    })
    output$crime_kpi_security <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$security_costs_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Security Costs", "warning")
    })

    # Crime Analysis Charts
    output$crime_vs_security_cost <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("crime_obstacle_pct", "security_costs_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "crime_obstacle_pct", y_col = "security_costs_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Crime Perception vs Security Investment", x_label = "Crime Obstacle %", y_label = "Security Costs %"
      )
    })
    output$crime_impact_performance <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      if (!all(c("crime_obstacle_pct", "capacity_utilization_pct") %in% names(data))) {
        return(plot_ly() |> layout(title = "Data not available"))
      }
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "crime_obstacle_pct", y_col = "capacity_utilization_pct",
        color_col = "firm_size", shape_col = shape_col,
        title = "Crime Impact on Performance", x_label = "Crime Obstacle %", y_label = "Capacity Utilization %"
      )
    })
    output$crime_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      worst_crime <- data$firm_size[which.max(data$crime_obstacle_pct)]
      highest_security <- data$firm_size[which.max(data$security_costs_pct)]
      avg_security <- round(mean(data$security_costs_pct, na.rm = TRUE), 1)
      tags$ul(
        tags$li(paste0("Highest crime obstacle: ", worst_crime, " firms")),
        tags$li(paste0("Highest security spending: ", highest_security, " firms")),
        tags$li(paste0("Average security costs: ", avg_security, "% of sales")),
        tags$li("Crime impacts smaller firms disproportionately")
      )
    })

    # ============================================================
    # Download Handlers
    # ============================================================

    # Helper to create plotly download handler
    create_plot_download <- function(plot_output_func, prefix) {
      downloadHandler(
        filename = function() {
          paste0(prefix, "_", format(Sys.Date(), "%Y%m%d"), ".html")
        },
        content = function(file) {
          p <- plot_output_func()
          saveWidget(p, file, selfcontained = TRUE)
        }
      )
    }

    # Helper to create CSV download handler
    create_csv_download <- function(data_func, prefix) {
      downloadHandler(
        filename = function() {
          paste0(prefix, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
          write.csv(data_func(), file, row.names = FALSE)
        }
      )
    }

    # Overview downloads
    output$dl_overview_heatmap <- create_plot_download(
      function() {
        req(comparison_data())
        data <- comparison_data()
        key_indicators <- c("power_outages_per_month", "firms_with_credit_line_pct", "bribery_incidence_pct",
                            "female_ownership_pct", "capacity_utilization_pct", "crime_obstacle_pct")
        indicator_labels <- c("Power Outages", "Credit Access", "Bribery", "Female Own", "Capacity", "Crime")
        available <- intersect(key_indicators, names(data))
        sizes <- unique(data$firm_size)
        z_matrix <- sapply(available, function(ind) {
          sapply(sizes, function(s) {
            val <- data[data$firm_size == s, ind, drop = TRUE]
            if (length(val) > 0) mean(val, na.rm = TRUE) else NA
          })
        })
        plot_ly(x = indicator_labels[key_indicators %in% available], y = sizes, z = z_matrix,
                type = "heatmap", colorscale = list(c(0, "#1B6B5F"), c(0.5, "#F4A460"), c(1, "#dc3545"))) |>
          layout(title = "Key Indicators by Firm Size")
      },
      "size_overview_heatmap"
    )

    output$dl_overview_table <- create_csv_download(
      function() {
        req(comparison_data())
        data <- comparison_data()
        key_cols <- c("firm_size", "countries_count", "power_outages_per_month",
                      "firms_with_credit_line_pct", "bribery_incidence_pct",
                      "female_ownership_pct", "capacity_utilization_pct")
        select(data, any_of(key_cols))
      },
      "size_overview_data"
    )

    # Infrastructure downloads
    output$dl_infra_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "infrastructure", input$chart_type) },
      "size_infrastructure_comparison"
    )
    output$dl_infra_radar <- create_plot_download(
      function() {
        data <- comparison_data()
        indicators <- DOMAINS$infrastructure$indicators
        available <- intersect(indicators, names(data))
        indicator_names <- names(DOMAINS$infrastructure$indicators)[DOMAINS$infrastructure$indicators %in% available]
        if (length(available) > 0) create_radar_chart(data, available, indicator_names)
        else plot_ly() |> layout(title = "No data available")
      },
      "size_infrastructure_radar"
    )

    # Finance downloads
    output$dl_finance_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "finance", input$chart_type) },
      "size_finance_comparison"
    )
    output$dl_finance_radar <- create_plot_download(
      function() {
        data <- comparison_data()
        indicators <- DOMAINS$finance$indicators
        available <- intersect(indicators, names(data))
        indicator_names <- names(DOMAINS$finance$indicators)[DOMAINS$finance$indicators %in% available]
        if (length(available) > 0) create_radar_chart(data, available, indicator_names)
        else plot_ly() |> layout(title = "No data available")
      },
      "size_finance_radar"
    )

    # Governance downloads
    output$dl_governance_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "governance", input$chart_type) },
      "size_governance_comparison"
    )
    output$dl_governance_radar <- create_plot_download(
      function() {
        data <- comparison_data()
        indicators <- DOMAINS$governance$indicators
        available <- intersect(indicators, names(data))
        indicator_names <- names(DOMAINS$governance$indicators)[DOMAINS$governance$indicators %in% available]
        if (length(available) > 0) create_radar_chart(data, available, indicator_names)
        else plot_ly() |> layout(title = "No data available")
      },
      "size_governance_radar"
    )

    # Workforce downloads
    output$dl_workforce_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "workforce", input$chart_type) },
      "size_workforce_comparison"
    )
    output$dl_workforce_radar <- create_plot_download(
      function() {
        data <- comparison_data()
        indicators <- DOMAINS$workforce$indicators
        available <- intersect(indicators, names(data))
        indicator_names <- names(DOMAINS$workforce$indicators)[DOMAINS$workforce$indicators %in% available]
        if (length(available) > 0) create_radar_chart(data, available, indicator_names)
        else plot_ly() |> layout(title = "No data available")
      },
      "size_workforce_radar"
    )

    # Performance downloads
    output$dl_performance_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "performance", input$chart_type) },
      "size_performance_comparison"
    )
    output$dl_performance_radar <- create_plot_download(
      function() {
        data <- comparison_data()
        indicators <- DOMAINS$performance$indicators
        available <- intersect(indicators, names(data))
        indicator_names <- names(DOMAINS$performance$indicators)[DOMAINS$performance$indicators %in% available]
        if (length(available) > 0) create_radar_chart(data, available, indicator_names)
        else plot_ly() |> layout(title = "No data available")
      },
      "size_performance_radar"
    )

    # Crime downloads
    output$dl_crime_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "crime", input$chart_type) },
      "size_crime_comparison"
    )
    output$dl_crime_radar <- create_plot_download(
      function() {
        data <- comparison_data()
        indicators <- DOMAINS$crime$indicators
        available <- intersect(indicators, names(data))
        indicator_names <- names(DOMAINS$crime$indicators)[DOMAINS$crime$indicators %in% available]
        if (length(available) > 0) create_radar_chart(data, available, indicator_names)
        else plot_ly() |> layout(title = "No data available")
      },
      "size_crime_radar"
    )

    # ============================================================
    # Analysis Chart Downloads
    # ============================================================

    # Infrastructure analysis downloads
    output$dl_infra_outage_impact <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~power_outages_per_month, y = ~capacity_utilization_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Outage Impact on Capacity", xaxis = list(title = "Power Outages/Month"),
                 yaxis = list(title = "Capacity Utilization %"))
      },
      "size_infra_outage_impact"
    )
    output$dl_infra_generator_correlation <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~power_outages_per_month, y = ~firms_with_generator_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Generator Adoption vs Outages", xaxis = list(title = "Power Outages/Month"),
                 yaxis = list(title = "Firms with Generator %"))
      },
      "size_infra_generator_correlation"
    )

    # Finance analysis downloads
    output$dl_finance_access_gap <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~firms_with_credit_line_pct, y = ~loan_rejection_rate_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Credit Access vs Loan Rejection", xaxis = list(title = "Credit Line Access %"),
                 yaxis = list(title = "Loan Rejection Rate %"))
      },
      "size_finance_access_gap"
    )
    output$dl_finance_collateral_burden <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~collateral_required_pct, y = ~firms_with_credit_line_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Collateral Burden vs Credit Access", xaxis = list(title = "Collateral Required %"),
                 yaxis = list(title = "Credit Line Access %"))
      },
      "size_finance_collateral_burden"
    )

    # Governance analysis downloads
    output$dl_governance_bribery_vs_corruption <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~bribery_incidence_pct, y = ~corruption_obstacle_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Bribery vs Corruption Perception", xaxis = list(title = "Bribery Incidence %"),
                 yaxis = list(title = "Corruption Obstacle %"))
      },
      "size_governance_bribery_corruption"
    )
    output$dl_governance_regulatory_burden <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~mgmt_time_regulations_pct, y = ~corruption_obstacle_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Regulatory Burden vs Corruption", xaxis = list(title = "Mgmt Time on Regulations %"),
                 yaxis = list(title = "Corruption Obstacle %"))
      },
      "size_governance_regulatory_burden"
    )

    # Workforce analysis downloads
    output$dl_workforce_gender_gap <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~female_ownership_pct, y = ~female_workers_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Gender in Ownership vs Workforce", xaxis = list(title = "Female Ownership %"),
                 yaxis = list(title = "Female Workers %"))
      },
      "size_workforce_gender_gap"
    )
    output$dl_workforce_obstacle_correlation <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~workforce_obstacle_pct, y = ~capacity_utilization_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Workforce Obstacle vs Capacity", xaxis = list(title = "Workforce Obstacle %"),
                 yaxis = list(title = "Capacity Utilization %"))
      },
      "size_workforce_obstacle_correlation"
    )

    # Performance analysis downloads
    output$dl_performance_capacity_vs_exports <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~capacity_utilization_pct, y = ~export_firms_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Capacity vs Export Propensity", xaxis = list(title = "Capacity Utilization %"),
                 yaxis = list(title = "Exporting Firms %"))
      },
      "size_performance_capacity_exports"
    )
    output$dl_performance_growth_distribution <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~firm_size, y = ~annual_sales_growth_pct, color = ~firm_size,
                type = "bar", textposition = "outside") |>
          layout(title = "Sales Growth by Firm Size", xaxis = list(title = "Firm Size"),
                 yaxis = list(title = "Annual Sales Growth %"), showlegend = FALSE)
      },
      "size_performance_growth_distribution"
    )

    # Crime analysis downloads
    output$dl_crime_vs_security_cost <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~crime_obstacle_pct, y = ~security_costs_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Crime Perception vs Security Investment", xaxis = list(title = "Crime Obstacle %"),
                 yaxis = list(title = "Security Costs %"))
      },
      "size_crime_security_cost"
    )
    output$dl_crime_impact_performance <- create_plot_download(
      function() {
        data <- comparison_data()
        plot_ly(data, x = ~crime_obstacle_pct, y = ~capacity_utilization_pct,
                color = ~firm_size, type = "scatter", mode = "markers",
                marker = list(size = 12)) |>
          layout(title = "Crime Impact on Performance", xaxis = list(title = "Crime Obstacle %"),
                 yaxis = list(title = "Capacity Utilization %"))
      },
      "size_crime_impact_performance"
    )

    # Map downloads
    output$dl_infra_map <- downloadHandler(
      filename = function() { paste0("infrastructure_map_size_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map_3d(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = input$infra_map_indicator,
          group_col = "firm_size",
          title = "Infrastructure Indicators by Firm Size"
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_finance_map <- downloadHandler(
      filename = function() { paste0("finance_map_size_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map_3d(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = input$finance_map_indicator,
          group_col = "firm_size",
          title = "Finance Indicators by Firm Size"
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_governance_map <- downloadHandler(
      filename = function() { paste0("governance_map_size_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map_3d(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = input$governance_map_indicator,
          group_col = "firm_size",
          title = "Governance Indicators by Firm Size"
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_workforce_map <- downloadHandler(
      filename = function() { paste0("workforce_map_size_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map_3d(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = input$workforce_map_indicator,
          group_col = "firm_size",
          title = "Workforce Indicators by Firm Size"
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_performance_map <- downloadHandler(
      filename = function() { paste0("performance_map_size_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map_3d(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = input$performance_map_indicator,
          group_col = "firm_size",
          title = "Performance Indicators by Firm Size"
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

    output$dl_crime_map <- downloadHandler(
      filename = function() { paste0("crime_map_size_", Sys.Date(), ".html") },
      content = function(file) {
        map <- create_wbes_map_3d(
          data = comparison_data(),
          coordinates = get_country_coordinates(wbes_data()),
          indicator_col = input$crime_map_indicator,
          group_col = "firm_size",
          title = "Crime Indicators by Firm Size"
        )
        htmlwidgets::saveWidget(map, file)
      }
    )

  })
}
