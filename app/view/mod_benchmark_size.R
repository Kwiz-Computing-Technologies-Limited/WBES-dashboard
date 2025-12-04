# app/view/mod_benchmark_size.R
# Cross-Size Benchmarking Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, p,
        fluidRow, column, selectInput, selectizeInput, renderUI, uiOutput,
        observeEvent, actionButton],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config, subplot],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, mutate, desc, group_by, summarise],
  stats[setNames]
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
            "Compare business environment indicators across firm size categories")
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
              column(5,
                selectizeInput(
                  ns("sizes_compare"),
                  "Select Firm Size Categories",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(placeholder = "Choose size categories...")
                )
              ),
              column(3,
                selectInput(
                  ns("indicator_select"),
                  "Primary Indicator",
                  choices = c(
                    "Power Outages (per month)" = "power_outages_per_month",
                    "Access to Credit (%)" = "firms_with_credit_line_pct",
                    "Bribery Incidence (%)" = "bribery_incidence_pct",
                    "Capacity Utilization (%)" = "capacity_utilization_pct",
                    "Female Ownership (%)" = "female_ownership_pct",
                    "Export Firms (%)" = "export_firms_pct"
                  )
                )
              ),
              column(2,
                selectInput(
                  ns("sort_order"),
                  "Sort By",
                  choices = c("Ascending" = "asc", "Descending" = "desc")
                )
              ),
              column(2,
                tags$div(
                  class = "d-flex align-items-end h-100 pb-3",
                  actionButton(ns("compare_btn"), "Compare",
                               icon = icon("exchange-alt"),
                               class = "btn-kwiz-primary w-100")
                )
              )
            )
          )
        )
      )
    ),

    # Main Comparison Chart
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("chart-bar"), "Firm Size Comparison"),
          card_body(
            plotlyOutput(ns("comparison_bar"), height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Bars compare the selected indicator across firm size categories; the sort option controls whether leaders or laggards appear first."
            )
          )
        )
      )
    ),

    # Secondary Comparisons
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("globe"), "Geographic Coverage"),
          card_body(
            plotlyOutput(ns("country_coverage"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Shows how many countries have data for each firm size category."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-line"), "Correlation Analysis"),
          card_body(
            fluidRow(
              column(6,
                selectInput(ns("scatter_x"), "X-Axis",
                  choices = c(
                    "Power Outages" = "power_outages_per_month",
                    "Credit Access" = "firms_with_credit_line_pct"
                  ), selected = "power_outages_per_month")
              ),
              column(6,
                selectInput(ns("scatter_y"), "Y-Axis",
                  choices = c(
                    "Capacity Utilization" = "capacity_utilization_pct",
                    "Sales Growth" = "annual_sales_growth_pct"
                  ), selected = "capacity_utilization_pct")
              )
            ),
            plotlyOutput(ns("scatter_plot"), height = "280px"),
            p(
              class = "text-muted small mt-2",
              "Each point represents a firm size category; the scatter reveals correlations between the selected drivers."
            )
          )
        )
      )
    ),

    # Data Table
    fluidRow(
      column(12,
        card(
          card_header(icon("table"), "Detailed Comparison Table"),
          card_body(
            DTOutput(ns("comparison_table"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {

    # Update size choices
    observeEvent(wbes_data(), {
      req(wbes_data())
      data <- wbes_data()$latest

      sizes <- data$firm_size |>
        unique() |>
        stats::na.omit() |>
        as.character() |>
        sort()

      shiny::updateSelectizeInput(
        session, "sizes_compare",
        choices = setNames(sizes, sizes),
        selected = sizes[1:min(length(sizes), length(sizes))]
      )
    })

    # Aggregate size data
    size_aggregated <- reactive({
      req(wbes_data())
      data <- wbes_data()$latest

      # Aggregate by firm size
      data |>
        filter(!is.na(firm_size)) |>
        group_by(firm_size) |>
        summarise(
          countries_count = length(unique(country[!is.na(country)])),
          firms_count = sum(sample_size, na.rm = TRUE),
          power_outages_per_month = mean(power_outages_per_month, na.rm = TRUE),
          firms_with_credit_line_pct = mean(firms_with_credit_line_pct, na.rm = TRUE),
          bribery_incidence_pct = mean(bribery_incidence_pct, na.rm = TRUE),
          capacity_utilization_pct = mean(capacity_utilization_pct, na.rm = TRUE),
          female_ownership_pct = mean(female_ownership_pct, na.rm = TRUE),
          export_firms_pct = mean(export_firms_pct, na.rm = TRUE),
          annual_sales_growth_pct = mean(annual_sales_growth_pct, na.rm = TRUE),
          .groups = "drop"
        )
    })

    # Comparison data
    comparison_data <- reactive({
      req(size_aggregated(), input$sizes_compare)
      data <- size_aggregated()
      data <- filter(data, firm_size %in% input$sizes_compare)

      if (is.null(data) || !input$indicator_select %in% names(data)) return(NULL)

      if (input$sort_order == "desc") {
        data <- arrange(data, desc(.data[[input$indicator_select]]))
      } else {
        data <- arrange(data, .data[[input$indicator_select]])
      }

      data
    }) |> shiny::bindEvent(input$compare_btn, ignoreNULL = FALSE)

    # Main comparison bar chart
    output$comparison_bar <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      indicator <- input$indicator_select

      data$firm_size <- factor(data$firm_size, levels = data$firm_size)

      plot_ly(data,
              x = ~firm_size,
              y = ~get(indicator),
              type = "bar",
              marker = list(color = "#1B6B5F"),
              hovertemplate = "%{x}<br>%{y:.1f}<extra></extra>") |>
        layout(
          xaxis = list(title = "Firm Size", tickangle = -45),
          yaxis = list(title = gsub("_", " ", tools::toTitleCase(indicator))),
          margin = list(b = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Country coverage chart
    output$country_coverage <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()

      plot_ly(data,
              x = ~firm_size,
              y = ~countries_count,
              type = "bar",
              marker = list(color = "#F49B7A"),
              hovertemplate = "%{x}<br>Countries: %{y}<extra></extra>") |>
        layout(
          xaxis = list(title = "Firm Size", tickangle = -45),
          yaxis = list(title = "Number of Countries"),
          margin = list(b = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Scatter plot
    output$scatter_plot <- renderPlotly({
      req(size_aggregated())
      data <- size_aggregated()

      plot_ly(data,
              x = ~get(input$scatter_x),
              y = ~get(input$scatter_y),
              type = "scatter",
              mode = "markers",
              marker = list(size = 12, color = "#1B6B5F", opacity = 0.7),
              text = ~firm_size,
              hovertemplate = "%{text}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>") |>
        layout(
          xaxis = list(title = gsub("_", " ", input$scatter_x)),
          yaxis = list(title = gsub("_", " ", input$scatter_y)),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Comparison table
    output$comparison_table <- renderDT({
      req(comparison_data())
      data <- comparison_data()

      display_cols <- c("firm_size", "countries_count", "firms_count",
                        "power_outages_per_month", "firms_with_credit_line_pct",
                        "bribery_incidence_pct", "capacity_utilization_pct",
                        "female_ownership_pct", "export_firms_pct")

      data <- select(data, any_of(display_cols))

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
        rownames = FALSE,
        colnames = c("Firm Size", "Countries", "Firms", "Power Outages/Month",
                     "Credit Access %", "Bribery %", "Capacity Util %",
                     "Female Ownership %", "Export Firms %")
      )
    })

  })
}
