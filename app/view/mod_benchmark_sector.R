# app/view/mod_benchmark_sector.R
# Cross-Sector Benchmarking Module with Domain Sub-menus

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
  app/logic/custom_sectors[filter_by_sector],
  app/logic/wbes_map[create_wbes_map_3d, get_country_coordinates],
  app/logic/scatter_utils[create_scatter_with_trend]
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

# Helper function to create chart container with download button
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
    plotlyOutput(ns(output_id), height = height)
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
          h2(icon("industry"), "Cross-Sector Benchmarking", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Compare business environment indicators across economic sectors by domain")
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
                  ns("sectors_compare"),
                  "Select Sectors to Compare",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(placeholder = "Choose sectors...")
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
                p(class = "text-muted", "Summary of key indicators across all domains for selected sectors")
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
              column(12, leafletOutput(ns("infra_map"), height = "350px"))
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
              column(12, leafletOutput(ns("finance_map"), height = "350px"))
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
              column(12, leafletOutput(ns("governance_map"), height = "350px"))
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
              column(12, leafletOutput(ns("workforce_map"), height = "350px"))
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
              column(12, leafletOutput(ns("performance_map"), height = "350px"))
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
              column(12, leafletOutput(ns("crime_map"), height = "350px"))
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

    # Filtered data (EXCEPT sector filter)
    filtered_data <- reactive({
      req(wbes_data())
      data <- wbes_data()$latest

      if (!is.null(global_filters)) {
        filters <- global_filters()
        data <- apply_common_filters(
          data,
          region_value = filters$region,
          sector_value = "all",
          firm_size_value = filters$firm_size,
          income_value = filters$income,
          year_value = filters$year,
          custom_regions = filters$custom_regions,
          filter_by_region_fn = filter_by_region
        )
      }

      data
    })

    # Data filtered by selected sectors
    selected_sector_data <- reactive({
      req(filtered_data(), input$sectors_compare)
      data <- filtered_data()
      selected <- input$sectors_compare

      custom_sectors <- if (!is.null(global_filters)) {
        filters <- global_filters()
        if (!is.null(filters$custom_sectors)) filters$custom_sectors else list()
      } else {
        list()
      }

      if (length(selected) > 0) {
        standard_selected <- selected[!grepl("^custom:", selected)]
        custom_selected <- gsub("^custom:", "", selected[grepl("^custom:", selected)])

        if (length(standard_selected) > 0) {
          standard_data <- data |> filter(sector %in% standard_selected)
        } else {
          standard_data <- data[0, ]
        }

        if (length(custom_selected) > 0) {
          custom_sector_list <- unlist(lapply(custom_selected, function(name) {
            if (!is.null(custom_sectors[[name]])) custom_sectors[[name]]$sectors else character(0)
          }))
          custom_data <- data |> filter(sector %in% custom_sector_list)
        } else {
          custom_data <- data[0, ]
        }

        data <- rbind(standard_data, custom_data)
      }

      data
    })

    # Update sector choices
    observeEvent(list(wbes_data(), global_filters()), {
      req(wbes_data())

      standard_sectors <- wbes_data()$sectors

      custom_sectors <- if (!is.null(global_filters)) {
        filters <- global_filters()
        if (!is.null(filters$custom_sectors)) filters$custom_sectors else list()
      } else {
        list()
      }

      if (length(custom_sectors) > 0) {
        custom_sector_names <- names(custom_sectors)
        all_sectors <- c(
          setNames(standard_sectors, paste0("   ", standard_sectors)),
          setNames(
            paste0("custom:", custom_sector_names),
            paste0("   [Custom] ", custom_sector_names)
          )
        )
      } else {
        all_sectors <- setNames(standard_sectors, standard_sectors)
      }

      shiny::updateSelectizeInput(
        session, "sectors_compare",
        choices = all_sectors,
        selected = standard_sectors[1:min(5, length(standard_sectors))]
      )
    }, ignoreNULL = FALSE)

    # Aggregate sector data
    sector_aggregated <- reactive({
      req(filtered_data())
      data <- filtered_data()

      group_dim <- if (!is.null(input$group_dimension) && input$group_dimension != "none") {
        input$group_dimension
      } else {
        NULL
      }

      custom_sectors <- if (!is.null(global_filters)) {
        filters <- global_filters()
        if (!is.null(filters$custom_sectors)) filters$custom_sectors else list()
      } else {
        list()
      }

      # Use unname to preserve column names in across()
      all_indicators <- unname(unlist(lapply(DOMAINS, function(d) d$indicators)))

      if (!is.null(group_dim) && group_dim %in% names(data)) {
        standard_agg <- data |>
          filter(!is.na(sector), !is.na(.data[[group_dim]])) |>
          group_by(sector, .data[[group_dim]]) |>
          summarise(
            countries_count = length(unique(country[!is.na(country)])),
            firms_count = sum(sample_size, na.rm = TRUE),
            across(any_of(all_indicators), ~mean(.x, na.rm = TRUE)),
            .groups = "drop"
          )
        names(standard_agg)[names(standard_agg) == group_dim] <- "group_value"
        standard_agg$group_dimension <- group_dim
      } else {
        standard_agg <- data |>
          filter(!is.na(sector)) |>
          group_by(sector) |>
          summarise(
            countries_count = length(unique(country[!is.na(country)])),
            firms_count = sum(sample_size, na.rm = TRUE),
            across(any_of(all_indicators), ~mean(.x, na.rm = TRUE)),
            .groups = "drop"
          )
        standard_agg$group_value <- NA
        standard_agg$group_dimension <- NA
      }

      # Custom sectors
      if (length(custom_sectors) > 0) {
        custom_agg_list <- lapply(names(custom_sectors), function(sector_name) {
          custom_sector <- custom_sectors[[sector_name]]
          sector_data <- data |> filter(sector %in% custom_sector$sectors)

          if (nrow(sector_data) > 0) {
            if (!is.null(group_dim) && group_dim %in% names(sector_data)) {
              agg <- sector_data |>
                filter(!is.na(.data[[group_dim]])) |>
                group_by(.data[[group_dim]]) |>
                summarise(
                  countries_count = length(unique(country[!is.na(country)])),
                  firms_count = sum(sample_size, na.rm = TRUE),
                  across(any_of(all_indicators), ~mean(.x, na.rm = TRUE)),
                  .groups = "drop"
                )
              names(agg)[names(agg) == group_dim] <- "group_value"
              agg$sector <- paste0("custom:", sector_name)
              agg$group_dimension <- group_dim
              agg
            } else {
              agg <- sector_data |>
                summarise(
                  countries_count = length(unique(country[!is.na(country)])),
                  firms_count = sum(sample_size, na.rm = TRUE),
                  across(any_of(all_indicators), ~mean(.x, na.rm = TRUE))
                )
              agg$sector <- paste0("custom:", sector_name)
              agg$group_value <- NA
              agg$group_dimension <- NA
              agg
            }
          } else {
            NULL
          }
        })

        custom_agg <- do.call(rbind, Filter(Negate(is.null), custom_agg_list))
        if (!is.null(custom_agg) && nrow(custom_agg) > 0) {
          standard_agg <- rbind(standard_agg, custom_agg)
        }
      }

      standard_agg
    })

    # Comparison data - reactive to selection changes
    comparison_data <- reactive({
      req(sector_aggregated())
      data <- sector_aggregated()

      # Get selected sectors (may be NULL initially)
      selected <- input$sectors_compare

      if (is.null(selected) || length(selected) == 0) {
        # Return first 5 sectors by default
        available_sectors <- unique(data$sector)
        selected <- available_sectors[1:min(5, length(available_sectors))]
      }

      filter(data, sector %in% selected)
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
                x = subset_data$sector,
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
              legend = list(orientation = "h", y = -0.45, x = 0.5, xanchor = "center", title = list(text = tools::toTitleCase(gsub("_", " ", group_dim)))),
              margin = list(b = 120),
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

            plot_ly(data, x = ~sector, y = y_vals, type = "bar",
                    marker = list(color = "#1B6B5F"), name = ind_name,
                    hovertemplate = paste0(ind_name, ": %{y:.1f}<extra></extra>"))
          })

          subplot(plots, nrows = 1, shareY = FALSE, titleX = TRUE) |>
            layout(
              title = list(text = paste(domain$name, "Indicators"), font = list(size = 14)),
              barmode = "group",
              showlegend = TRUE,
              legend = list(orientation = "h", y = -0.4, x = 0.5, xanchor = "center"),
              margin = list(b = 110),
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        }
      } else if (chart_type == "heatmap") {
        # Heatmap chart - matrix of sectors vs indicators
        create_domain_heatmap(data, available, indicator_names, domain$name)
      } else {
        # Radar chart - faceted by sector
        create_faceted_radar(data, available, indicator_names, domain$name)
      }
    }

    # Helper: radar chart (legacy - kept for compatibility)
    create_radar_chart <- function(data, indicators, indicator_names) {
      sectors <- unique(data$sector)

      # Collect all values for dynamic range calculation
      all_values <- c()
      plot_data <- lapply(sectors, function(s) {
        sector_data <- data[data$sector == s, ]
        values <- sapply(indicators, function(ind) {
          val <- sector_data[[ind]][1]
          if (is.na(val)) 0 else val
        })
        all_values <<- c(all_values, values)
        values <- c(values, values[1])
        list(sector = s, values = values)
      })

      # Calculate dynamic range with 10% padding
      max_val <- max(all_values, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10  # Round up to nearest 10 with 10% padding

      p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers', fill = 'toself')
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460", "#dc3545", "#9c27b0")

      for (i in seq_along(plot_data)) {
        p <- p |> add_trace(
          r = plot_data[[i]]$values,
          theta = c(indicator_names, indicator_names[1]),
          name = plot_data[[i]]$sector,
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

    # Helper function to create faceted radar charts (one per sector, with nested grouping if 3rd dim selected)
    create_faceted_radar <- function(data, indicators, indicator_names, domain_name) {
      group_dim <- input$group_dimension
      has_grouping <- !is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data) && !all(is.na(data$group_value))

      sectors <- unique(data$sector)
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460", "#dc3545", "#9c27b0")
      n_sectors <- length(sectors)

      # Calculate dynamic range from all indicator values
      all_values <- unlist(lapply(indicators, function(ind) data[[ind]]))
      max_val <- max(all_values, na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) max_val <- 100
      range_max <- ceiling(max_val * 1.1 / 10) * 10  # Round up to nearest 10 with 10% padding

      # Calculate grid layout (more columns for sectors)
      n_cols <- min(4, n_sectors)
      n_rows <- ceiling(n_sectors / n_cols)

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
        legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"),
        margin = list(b = 60)
      )

      annotations <- list()

      for (i in seq_along(sectors)) {
        s <- sectors[i]
        polar_name <- if (i == 1) "polar" else paste0("polar", i)
        domain <- get_domain(i, n_sectors, n_cols)

        # Add polar axis to layout with dynamic range
        layout_args[[polar_name]] <- list(
          domain = domain,
          radialaxis = list(visible = TRUE, range = c(0, range_max), tickfont = list(size = 7)),
          angularaxis = list(tickfont = list(size = 6))
        )

        # Add annotation (title) for this radar
        annotations[[length(annotations) + 1]] <- list(
          text = s,
          x = mean(domain$x),
          y = domain$y[2] + 0.02,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 9, weight = "bold", color = colors[((i - 1) %% length(colors)) + 1])
        )

        if (has_grouping) {
          # Nested faceting: show each group value within this sector
          group_values <- unique(data$group_value[!is.na(data$group_value)])
          for (j in seq_along(group_values)) {
            gv <- group_values[j]
            subset_data <- data[data$sector == s & data$group_value == gv, ]

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
                marker = list(size = 3),
                subplot = polar_name
              )
            }
          }
        } else {
          # No grouping: single radar per sector
          sector_data <- data[data$sector == s, ]
          values <- sapply(indicators, function(ind) {
            val <- mean(sector_data[[ind]], na.rm = TRUE)
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
            marker = list(size = 4),
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

      sectors <- unique(data$sector)

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
            sapply(sectors, function(s) {
              val <- subset_data[subset_data$sector == s, ind, drop = TRUE]
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
            y = sectors,
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
            tickfont = list(size = 7)
          )
          layout_args[[yaxis_name]] <- list(
            domain = domain$y,
            anchor = if (i == 1) "x" else paste0("x", i),
            tickfont = list(size = 8)
          )
        }

        p <- do.call(layout, c(list(p), layout_args))
        p |> config(displayModeBar = FALSE)

      } else {
        # Single heatmap - no grouping
        z_matrix <- sapply(indicators, function(ind) {
          sapply(sectors, function(s) {
            val <- data[data$sector == s, ind, drop = TRUE]
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
          y = sectors,
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

      sectors <- unique(data$sector)
      # Create matrix for heatmap - aggregate if grouped data
      z_matrix <- sapply(available, function(ind) {
        sapply(sectors, function(s) {
          val <- data[data$sector == s, ind, drop = TRUE]
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
        y = sectors,
        z = z_matrix,
        type = "heatmap",
        colorscale = list(c(0, "#1B6B5F"), c(0.5, "#F4A460"), c(1, "#dc3545")),
        hovertemplate = "%{y}<br>%{x}: %{z:.1f}<extra></extra>"
      ) |>
        layout(
          title = "Key Indicators by Sector",
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

      key_cols <- c("sector", "countries_count", "power_outages_per_month",
                    "firms_with_credit_line_pct", "bribery_incidence_pct",
                    "female_ownership_pct", "capacity_utilization_pct")

      display_data <- select(data, any_of(key_cols))

      datatable(
        display_data,
        options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
        class = "table-kwiz display compact",
        rownames = FALSE,
        colnames = c("Sector", "Countries", "Power Outages", "Credit %", "Bribery %", "Female %", "Capacity %")[1:ncol(display_data)]
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
      req(selected_sector_data(), wbes_data())
      indicator <- if (!is.null(input$infra_map_indicator)) input$infra_map_indicator else "power_outages_per_month"
      palette_info <- switch(indicator,
        "power_outages_per_month" = list(palette = "YlOrRd", label = "Power Outages/Month"),
        "avg_outage_duration_hrs" = list(palette = "YlOrRd", label = "Outage Duration (hrs)"),
        "firms_with_generator_pct" = list(palette = "Oranges", label = "Generator Usage (%)"),
        list(palette = "YlOrRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_sector_data(), coordinates = get_country_coordinates(wbes_data()),
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
      req(selected_sector_data(), wbes_data())
      indicator <- if (!is.null(input$finance_map_indicator)) input$finance_map_indicator else "firms_with_credit_line_pct"
      palette_info <- switch(indicator,
        "firms_with_credit_line_pct" = list(palette = "YlGn", label = "Credit Access (%)"),
        "firms_with_bank_account_pct" = list(palette = "Blues", label = "Bank Account (%)"),
        "collateral_required_pct" = list(palette = "YlOrRd", label = "Collateral Required (%)"),
        list(palette = "YlGn", label = indicator)
      )
      create_wbes_map_3d(data = selected_sector_data(), coordinates = get_country_coordinates(wbes_data()),
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
      req(selected_sector_data(), wbes_data())
      indicator <- if (!is.null(input$governance_map_indicator)) input$governance_map_indicator else "bribery_incidence_pct"
      palette_info <- switch(indicator,
        "bribery_incidence_pct" = list(palette = "YlOrRd", label = "Bribery Incidence (%)"),
        "corruption_obstacle_pct" = list(palette = "Reds", label = "Corruption Obstacle (%)"),
        list(palette = "YlOrRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_sector_data(), coordinates = get_country_coordinates(wbes_data()),
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
      req(selected_sector_data(), wbes_data())
      indicator <- if (!is.null(input$workforce_map_indicator)) input$workforce_map_indicator else "female_ownership_pct"
      palette_info <- switch(indicator,
        "female_ownership_pct" = list(palette = "PuRd", label = "Female Ownership (%)"),
        "female_workers_pct" = list(palette = "Purples", label = "Female Workers (%)"),
        list(palette = "PuRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_sector_data(), coordinates = get_country_coordinates(wbes_data()),
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
      req(selected_sector_data(), wbes_data())
      indicator <- if (!is.null(input$performance_map_indicator)) input$performance_map_indicator else "capacity_utilization_pct"
      palette_info <- switch(indicator,
        "capacity_utilization_pct" = list(palette = "YlGn", label = "Capacity Utilization (%)"),
        "export_firms_pct" = list(palette = "Blues", label = "Export Firms (%)"),
        list(palette = "YlGn", label = indicator)
      )
      create_wbes_map_3d(data = selected_sector_data(), coordinates = get_country_coordinates(wbes_data()),
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
      req(selected_sector_data(), wbes_data())
      indicator <- if (!is.null(input$crime_map_indicator)) input$crime_map_indicator else "crime_obstacle_pct"
      palette_info <- switch(indicator,
        "crime_obstacle_pct" = list(palette = "YlOrRd", label = "Crime Obstacle (%)"),
        "security_costs_pct" = list(palette = "Oranges", label = "Security Costs (% Sales)"),
        list(palette = "YlOrRd", label = indicator)
      )
      create_wbes_map_3d(data = selected_sector_data(), coordinates = get_country_coordinates(wbes_data()),
                         color_col = indicator, size_col = indicator,
                         color_label = palette_info$label, size_label = palette_info$label, color_palette = palette_info$palette)
    })

    # ============================================================
    # DOMAIN KPIs, ANALYSIS CHARTS & INSIGHTS
    # ============================================================

    # Helper function for KPI card
    create_kpi_card <- function(value, label, bg_color = "primary") {
      div(class = paste0("card bg-", bg_color, " text-white h-100"),
        div(class = "card-body text-center py-2",
          h4(value, class = "mb-0"),
          p(label, class = "small mb-0")))
    }

    # --- INFRASTRUCTURE ---
    output$infra_kpi_outages <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$power_outages_per_month, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "/mo"), "Power Outages", "danger")
    })
    output$infra_kpi_duration <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$avg_outage_duration_hrs, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, " hrs"), "Avg Duration", "warning")
    })
    output$infra_kpi_generator <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$firms_with_generator_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "With Generator", "info")
    })
    output$infra_kpi_obstacle <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$electricity_obstacle, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Elec. Obstacle", "secondary")
    })

    output$infra_outage_impact <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "power_outages_per_month", y_col = "capacity_utilization_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Outages vs Capacity Utilization", x_label = "Power Outages/Month", y_label = "Capacity Utilization (%)"
      )
    })

    output$infra_generator_correlation <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "power_outages_per_month", y_col = "firms_with_generator_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Outages vs Generator Adoption", x_label = "Power Outages/Month", y_label = "Firms with Generator (%)"
      )
    })

    output$infra_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      avg_outages <- round(mean(data$power_outages_per_month, na.rm = TRUE), 1)
      worst <- data$sector[which.max(data$power_outages_per_month)]
      best <- data$sector[which.min(data$power_outages_per_month)]
      tags$ul(
        tags$li(tags$strong("Average Outages: "), paste0(avg_outages, " per month across sectors")),
        tags$li(tags$strong("Most Affected: "), worst), tags$li(tags$strong("Best Infrastructure: "), best),
        tags$li(tags$strong("Key Finding: "), "Sectors with more outages show higher generator adoption")
      )
    })

    # --- FINANCE ---
    output$finance_kpi_credit <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$firms_with_credit_line_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Credit Access", "success")
    })
    output$finance_kpi_bank <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$firms_with_bank_account_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Bank Account", "info")
    })
    output$finance_kpi_rejection <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$loan_rejection_rate_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Loan Rejection", "danger")
    })
    output$finance_kpi_collateral <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$collateral_required_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Collateral Req.", "warning")
    })

    output$finance_access_gap <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "firms_with_bank_account_pct", y_col = "firms_with_credit_line_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Bank Account vs Credit Access", x_label = "Firms with Bank Account (%)", y_label = "Firms with Credit Line (%)"
      )
    })

    output$finance_collateral_burden <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "loan_rejection_rate_pct", y_col = "collateral_required_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Rejection Rate vs Collateral", x_label = "Loan Rejection Rate (%)", y_label = "Collateral Required (%)"
      )
    })

    output$finance_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      avg_credit <- round(mean(data$firms_with_credit_line_pct, na.rm = TRUE), 1)
      best_access <- data$sector[which.max(data$firms_with_credit_line_pct)]
      worst_rejection <- data$sector[which.max(data$loan_rejection_rate_pct)]
      tags$ul(
        tags$li(tags$strong("Average Credit Access: "), paste0(avg_credit, "% of firms have credit lines")),
        tags$li(tags$strong("Best Finance Access: "), best_access), tags$li(tags$strong("Highest Rejection: "), worst_rejection),
        tags$li(tags$strong("Key Finding: "), "Higher collateral requirements correlate with higher rejection rates")
      )
    })

    # --- GOVERNANCE ---
    output$governance_kpi_bribery <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$bribery_incidence_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Bribery Rate", "danger")
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

    output$governance_bribery_vs_corruption <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "bribery_incidence_pct", y_col = "corruption_obstacle_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Bribery vs Corruption Obstacle", x_label = "Bribery Incidence (%)", y_label = "Corruption as Obstacle (%)"
      )
    })

    output$governance_regulatory_burden <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      # Check if mgmt_time_regulations_pct has valid data
      if (!"mgmt_time_regulations_pct" %in% names(data) ||
          all(is.na(data$mgmt_time_regulations_pct))) {
        create_scatter_with_trend(
          data = data, x_col = "corruption_obstacle_pct", y_col = "capacity_utilization_pct",
          color_col = "sector", shape_col = shape_col,
          title = "Corruption Impact on Productivity", x_label = "Corruption as Obstacle (%)", y_label = "Capacity Utilization (%)"
        )
      } else {
        create_scatter_with_trend(
          data = data, x_col = "mgmt_time_regulations_pct", y_col = "capacity_utilization_pct",
          color_col = "sector", shape_col = shape_col,
          title = "Regulatory Burden vs Productivity", x_label = "Mgmt Time on Regulations (%)", y_label = "Capacity Utilization (%)"
        )
      }
    })

    output$governance_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      avg_bribery <- round(mean(data$bribery_incidence_pct, na.rm = TRUE), 1)
      worst <- data$sector[which.max(data$bribery_incidence_pct)]
      best <- data$sector[which.min(data$bribery_incidence_pct)]
      tags$ul(
        tags$li(tags$strong("Average Bribery: "), paste0(avg_bribery, "% incidence rate")),
        tags$li(tags$strong("Highest Risk: "), worst), tags$li(tags$strong("Best Governance: "), best),
        tags$li(tags$strong("Key Finding: "), "Bribery incidence strongly correlates with corruption as an obstacle")
      )
    })

    # --- WORKFORCE ---
    output$workforce_kpi_female_own <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$female_ownership_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Female Ownership", "primary")
    })
    output$workforce_kpi_female_work <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$female_workers_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Female Workers", "info")
    })
    output$workforce_kpi_obstacle <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$workforce_obstacle_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Workforce Obstacle", "warning")
    })

    output$workforce_gender_gap <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "female_workers_pct", y_col = "female_ownership_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Female Workers vs Ownership Gap", x_label = "Female Workers (%)", y_label = "Female Ownership (%)"
      )
    })

    output$workforce_obstacle_correlation <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "workforce_obstacle_pct", y_col = "capacity_utilization_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Workforce Challenges vs Productivity", x_label = "Workforce as Obstacle (%)", y_label = "Capacity Utilization (%)"
      )
    })

    output$workforce_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      avg_female_own <- round(mean(data$female_ownership_pct, na.rm = TRUE), 1)
      avg_female_work <- round(mean(data$female_workers_pct, na.rm = TRUE), 1)
      gap <- round(avg_female_work - avg_female_own, 1)
      best <- data$sector[which.max(data$female_ownership_pct)]
      tags$ul(
        tags$li(tags$strong("Female Ownership: "), paste0(avg_female_own, "% average")),
        tags$li(tags$strong("Female Workers: "), paste0(avg_female_work, "% average")),
        tags$li(tags$strong("Leadership Gap: "), paste0(gap, "% difference")),
        tags$li(tags$strong("Best Gender Inclusion: "), best)
      )
    })

    # --- PERFORMANCE ---
    output$performance_kpi_capacity <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$capacity_utilization_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Capacity Util.", "success")
    })
    output$performance_kpi_export_firms <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$export_firms_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Export Firms", "info")
    })
    output$performance_kpi_export_share <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$export_share_pct, na.rm = TRUE), 1)
      create_kpi_card(paste0(val, "%"), "Export Share", "primary")
    })
    output$performance_kpi_growth <- renderUI({
      req(comparison_data())
      val <- round(mean(comparison_data()$annual_sales_growth_pct, na.rm = TRUE), 1)
      color <- if (!is.na(val) && val > 0) "success" else "danger"
      create_kpi_card(paste0(val, "%"), "Sales Growth", color)
    })

    output$performance_capacity_vs_exports <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "capacity_utilization_pct", y_col = "export_firms_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Capacity vs Export Activity", x_label = "Capacity Utilization (%)", y_label = "Export Firms (%)"
      )
    })

    output$performance_growth_distribution <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      data <- data[order(data$annual_sales_growth_pct, decreasing = TRUE), ]
      plot_ly(data, x = ~reorder(sector, annual_sales_growth_pct), y = ~annual_sales_growth_pct, type = "bar",
              marker = list(color = ~annual_sales_growth_pct, colorscale = list(c(0, "#dc3545"), c(0.5, "#ffc107"), c(1, "#28a745")))) |>
        layout(title = list(text = "Sales Growth by Sector", font = list(size = 14)),
               xaxis = list(title = "", tickangle = -45), yaxis = list(title = "Annual Sales Growth (%)"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |> config(displayModeBar = FALSE)
    })

    output$performance_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      avg_capacity <- round(mean(data$capacity_utilization_pct, na.rm = TRUE), 1)
      avg_exports <- round(mean(data$export_firms_pct, na.rm = TRUE), 1)
      best <- data$sector[which.max(data$capacity_utilization_pct)]
      top_exporter <- data$sector[which.max(data$export_firms_pct)]
      tags$ul(
        tags$li(tags$strong("Avg Capacity: "), paste0(avg_capacity, "% utilization")),
        tags$li(tags$strong("Export Participation: "), paste0(avg_exports, "% of firms")),
        tags$li(tags$strong("Top Performer: "), best), tags$li(tags$strong("Export Leader: "), top_exporter)
      )
    })

    # --- CRIME ---
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

    output$crime_vs_security_cost <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "crime_obstacle_pct", y_col = "security_costs_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Crime Obstacle vs Security Spending", x_label = "Crime as Obstacle (%)", y_label = "Security Costs (% of Sales)"
      )
    })

    output$crime_impact_performance <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      group_dim <- input$group_dimension
      shape_col <- if (!is.null(group_dim) && group_dim != "none" && "group_value" %in% names(data)) "group_value" else NULL
      create_scatter_with_trend(
        data = data, x_col = "crime_obstacle_pct", y_col = "capacity_utilization_pct",
        color_col = "sector", shape_col = shape_col,
        title = "Crime Impact on Productivity", x_label = "Crime as Obstacle (%)", y_label = "Capacity Utilization (%)"
      )
    })

    output$crime_insights <- renderUI({
      req(comparison_data())
      data <- comparison_data()
      avg_crime <- round(mean(data$crime_obstacle_pct, na.rm = TRUE), 1)
      avg_security <- round(mean(data$security_costs_pct, na.rm = TRUE), 1)
      worst <- data$sector[which.max(data$crime_obstacle_pct)]
      safest <- data$sector[which.min(data$crime_obstacle_pct)]
      tags$ul(
        tags$li(tags$strong("Avg Crime Concern: "), paste0(avg_crime, "% of firms")),
        tags$li(tags$strong("Security Spending: "), paste0(avg_security, "% of sales")),
        tags$li(tags$strong("Highest Risk: "), worst), tags$li(tags$strong("Safest Sector: "), safest)
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
        sectors <- unique(data$sector)
        z_matrix <- sapply(available, function(ind) {
          sapply(sectors, function(s) {
            val <- data[data$sector == s, ind, drop = TRUE]
            if (length(val) > 0) mean(val, na.rm = TRUE) else NA
          })
        })
        plot_ly(x = indicator_labels[key_indicators %in% available], y = sectors, z = z_matrix,
                type = "heatmap", colorscale = list(c(0, "#1B6B5F"), c(0.5, "#F4A460"), c(1, "#dc3545"))) |>
          layout(title = "Key Indicators by Sector")
      },
      "sector_overview_heatmap"
    )

    output$dl_overview_table <- create_csv_download(
      function() {
        req(comparison_data())
        data <- comparison_data()
        key_cols <- c("sector", "countries_count", "power_outages_per_month",
                      "firms_with_credit_line_pct", "bribery_incidence_pct",
                      "female_ownership_pct", "capacity_utilization_pct")
        select(data, any_of(key_cols))
      },
      "sector_overview_data"
    )

    # Infrastructure downloads
    output$dl_infra_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "infrastructure", input$chart_type) },
      "sector_infrastructure_comparison"
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
      "sector_infrastructure_radar"
    )

    # Finance downloads
    output$dl_finance_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "finance", input$chart_type) },
      "sector_finance_comparison"
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
      "sector_finance_radar"
    )

    # Governance downloads
    output$dl_governance_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "governance", input$chart_type) },
      "sector_governance_comparison"
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
      "sector_governance_radar"
    )

    # Workforce downloads
    output$dl_workforce_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "workforce", input$chart_type) },
      "sector_workforce_comparison"
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
      "sector_workforce_radar"
    )

    # Performance downloads
    output$dl_performance_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "performance", input$chart_type) },
      "sector_performance_comparison"
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
      "sector_performance_radar"
    )

    # Crime downloads
    output$dl_crime_comparison <- create_plot_download(
      function() { create_domain_chart(comparison_data(), "crime", input$chart_type) },
      "sector_crime_comparison"
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
      "sector_crime_radar"
    )

  })
}
