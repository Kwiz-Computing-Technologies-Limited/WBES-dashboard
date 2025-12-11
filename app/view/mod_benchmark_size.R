# app/view/mod_benchmark_size.R
# Cross-Size Benchmarking Module with Domain Sub-menus

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, h4, p,
        fluidRow, column, selectInput, selectizeInput, renderUI, uiOutput,
        observeEvent, actionButton, tabsetPanel, tabPanel, HTML],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config, subplot],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, mutate, desc, group_by, summarise, n, across, any_of],
  leaflet[leafletOutput, renderLeaflet],
  stats[setNames],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map_3d, get_country_coordinates]
)

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
              column(12, plotlyOutput(ns("overview_heatmap"), height = "500px"))
            ),
            fluidRow(class = "mt-4", column(12, DTOutput(ns("overview_table"))))
          ),

          # Infrastructure Tab
          nav_panel(
            title = tags$span(icon("bolt"), " Infrastructure"),
            value = "infrastructure",
            fluidRow(
              class = "mt-3",
              column(8, plotlyOutput(ns("infra_comparison"), height = "400px")),
              column(4, plotlyOutput(ns("infra_radar"), height = "400px"))
            ),
            fluidRow(class = "mt-3", column(12, leafletOutput(ns("infra_map"), height = "350px")))
          ),

          # Finance Tab
          nav_panel(
            title = tags$span(icon("university"), " Finance"),
            value = "finance",
            fluidRow(
              class = "mt-3",
              column(8, plotlyOutput(ns("finance_comparison"), height = "400px")),
              column(4, plotlyOutput(ns("finance_radar"), height = "400px"))
            ),
            fluidRow(class = "mt-3", column(12, leafletOutput(ns("finance_map"), height = "350px")))
          ),

          # Governance Tab
          nav_panel(
            title = tags$span(icon("balance-scale"), " Governance"),
            value = "governance",
            fluidRow(
              class = "mt-3",
              column(8, plotlyOutput(ns("governance_comparison"), height = "400px")),
              column(4, plotlyOutput(ns("governance_radar"), height = "400px"))
            ),
            fluidRow(class = "mt-3", column(12, leafletOutput(ns("governance_map"), height = "350px")))
          ),

          # Workforce Tab
          nav_panel(
            title = tags$span(icon("users"), " Workforce"),
            value = "workforce",
            fluidRow(
              class = "mt-3",
              column(8, plotlyOutput(ns("workforce_comparison"), height = "400px")),
              column(4, plotlyOutput(ns("workforce_radar"), height = "400px"))
            ),
            fluidRow(class = "mt-3", column(12, leafletOutput(ns("workforce_map"), height = "350px")))
          ),

          # Performance Tab
          nav_panel(
            title = tags$span(icon("chart-line"), " Performance"),
            value = "performance",
            fluidRow(
              class = "mt-3",
              column(8, plotlyOutput(ns("performance_comparison"), height = "400px")),
              column(4, plotlyOutput(ns("performance_radar"), height = "400px"))
            ),
            fluidRow(class = "mt-3", column(12, leafletOutput(ns("performance_map"), height = "350px")))
          ),

          # Crime Tab
          nav_panel(
            title = tags$span(icon("shield-alt"), " Crime"),
            value = "crime",
            fluidRow(
              class = "mt-3",
              column(8, plotlyOutput(ns("crime_comparison"), height = "400px")),
              column(4, plotlyOutput(ns("crime_radar"), height = "400px"))
            ),
            fluidRow(class = "mt-3", column(12, leafletOutput(ns("crime_map"), height = "350px")))
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
      data <- wbes_data()$latest

      if (!is.null(global_filters)) {
        filters <- global_filters()
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
              xaxis = list(title = ind_name, tickangle = -45),
              yaxis = list(title = "")
            )
          })

          subplot(plots, nrows = 1, shareY = FALSE, titleX = TRUE, margin = 0.05) |>
            layout(
              title = list(text = paste(domain$name, "Indicators by", tools::toTitleCase(gsub("_", " ", group_dim))), font = list(size = 14)),
              barmode = "group",
              showlegend = TRUE,
              legend = list(orientation = "h", y = -0.3, title = list(text = tools::toTitleCase(gsub("_", " ", group_dim)))),
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
              legend = list(orientation = "h", y = -0.2),
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
      plot_data <- lapply(sizes, function(s) {
        size_data <- data[data$firm_size == s, ]
        values <- sapply(indicators, function(ind) {
          val <- size_data[[ind]][1]
          if (is.na(val)) 0 else val
        })
        values <- c(values, values[1])
        list(size = s, values = values)
      })

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
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.1),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    }

    # Helper function to create faceted radar charts (one per size)
    create_faceted_radar <- function(data, indicators, indicator_names, domain_name) {
      sizes <- unique(data$firm_size)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      # Create individual radar charts for each size
      plots <- lapply(seq_along(sizes), function(i) {
        s <- sizes[i]
        size_data <- data[data$firm_size == s, ]

        values <- sapply(indicators, function(ind) {
          val <- mean(size_data[[ind]], na.rm = TRUE)
          if (is.na(val)) 0 else val
        })
        # Close the radar by repeating first value
        values <- c(values, values[1])
        theta <- c(indicator_names, indicator_names[1])

        plot_ly(
          type = 'scatterpolar',
          mode = 'lines+markers',
          fill = 'toself',
          r = values,
          theta = theta,
          name = s,
          line = list(color = colors[((i - 1) %% length(colors)) + 1]),
          fillcolor = paste0(colors[((i - 1) %% length(colors)) + 1], "33"),
          showlegend = FALSE
        ) |>
          layout(
            polar = list(
              radialaxis = list(visible = TRUE, range = c(0, 100)),
              angularaxis = list(tickfont = list(size = 8))
            ),
            annotations = list(list(
              text = s,
              x = 0.5, y = 1.15,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 11, color = colors[((i - 1) %% length(colors)) + 1])
            ))
          )
      })

      # Arrange in grid
      n_cols <- min(3, length(sizes))
      n_rows <- ceiling(length(sizes) / n_cols)

      subplot(plots, nrows = n_rows, margin = 0.08) |>
        layout(
          title = list(text = paste(domain_name, "- Radar Comparison"), font = list(size = 14)),
          paper_bgcolor = "rgba(0,0,0,0)",
          showlegend = FALSE
        ) |>
        config(displayModeBar = FALSE)
    }

    # Helper function to create domain heatmap
    create_domain_heatmap <- function(data, indicators, indicator_names, domain_name) {
      sizes <- unique(data$firm_size)

      # Create matrix for heatmap - sizes as rows, indicators as columns
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
          yaxis = list(title = ""),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
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
          yaxis = list(title = ""),
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
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = "power_outages_per_month", size_col = "power_outages_per_month",
                         color_label = "Power Outages", size_label = "Power Outages", color_palette = "YlOrRd")
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
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = "firms_with_credit_line_pct", size_col = "firms_with_credit_line_pct",
                         color_label = "Credit Access %", size_label = "Credit Access", color_palette = "YlGn")
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
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = "bribery_incidence_pct", size_col = "bribery_incidence_pct",
                         color_label = "Bribery %", size_label = "Bribery", color_palette = "YlOrRd")
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
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = "female_ownership_pct", size_col = "female_ownership_pct",
                         color_label = "Female Ownership %", size_label = "Female Ownership", color_palette = "PuRd")
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
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = "capacity_utilization_pct", size_col = "export_firms_pct",
                         color_label = "Capacity %", size_label = "Export Firms %", color_palette = "YlGn")
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
      create_wbes_map_3d(data = selected_size_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = "crime_obstacle_pct", size_col = "security_costs_pct",
                         color_label = "Crime Obstacle %", size_label = "Security Costs %", color_palette = "YlOrRd")
    })

  })
}
