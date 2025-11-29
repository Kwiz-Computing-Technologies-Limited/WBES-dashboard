# app/view/mod_benchmark.R
# Cross-Country Benchmarking Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, p,
        fluidRow, column, selectInput, selectizeInput, renderUI, uiOutput,
        observeEvent, actionButton],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config, subplot],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, mutate, desc],
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
              column(5,
                selectizeInput(
                  ns("countries_compare"),
                  "Select Countries (max 10)",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(maxItems = 10, placeholder = "Choose countries...")
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
          card_header(icon("chart-bar"), "Country Comparison"),
          card_body(
            plotlyOutput(ns("comparison_bar"), height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Bars compare the selected indicator across chosen countries; the sort option controls whether leaders or laggards appear first."
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
          card_header(icon("chart-pie"), "Regional Distribution"),
          card_body(
            plotlyOutput(ns("regional_pie"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "The pie shows how the comparison sample is distributed by region so cross-country averages can be interpreted in context."
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
              "Each point represents a country; the scatter reveals correlations between the selected drivers, helping spot trade-offs or synergies."
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
    
    # Update country choices
    observeEvent(wbes_data(), {
      req(wbes_data())
      countries <- sort(wbes_data()$countries)
      shiny::updateSelectizeInput(
        session, "countries_compare",
        choices = setNames(countries, countries),
        selected = countries[1:min(5, length(countries))]
      )
    })
    
    # Comparison data
    comparison_data <- reactive({
      req(wbes_data(), input$countries_compare)
      data <- wbes_data()$latest
      data <- filter(data, country %in% input$countries_compare)
      
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
      
      # Color by region
      colors <- c(
        "Sub-Saharan Africa" = "#1B6B5F",
        "South Asia" = "#F49B7A",
        "East Asia & Pacific" = "#2E7D32",
        "Latin America & Caribbean" = "#17a2b8",
        "Europe & Central Asia" = "#6C757D"
      )
      
      data$color <- colors[data$region]
      data$country <- factor(data$country, levels = data$country)
      
      plot_ly(data,
              x = ~country,
              y = ~get(indicator),
              type = "bar",
              color = ~region,
              colors = colors,
              hovertemplate = "%{x}<br>%{y:.1f}<extra>%{fullData.name}</extra>") |>
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = gsub("_", " ", tools::toTitleCase(indicator))),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Regional distribution pie
    output$regional_pie <- renderPlotly({
      req(comparison_data())
      data <- comparison_data()
      
      regional_counts <- as.data.frame(table(data$region))
      names(regional_counts) <- c("region", "count")
      
      plot_ly(regional_counts,
              labels = ~region,
              values = ~count,
              type = "pie",
              textinfo = "label+percent",
              marker = list(colors = c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D"))) |>
        layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Scatter plot
    output$scatter_plot <- renderPlotly({
      req(wbes_data())
      data <- wbes_data()$latest
      
      plot_ly(data,
              x = ~get(input$scatter_x),
              y = ~get(input$scatter_y),
              type = "scatter",
              mode = "markers",
              color = ~region,
              colors = c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D"),
              text = ~country,
              hovertemplate = "%{text}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
              marker = list(size = 10, opacity = 0.7)) |>
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
      
      display_cols <- c("country", "region", "income_group",
                        "power_outages_per_month", "firms_with_credit_line_pct",
                        "bribery_incidence_pct", "capacity_utilization_pct",
                        "female_ownership_pct", "data_quality_score")
      
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
        rownames = FALSE
      )
    })
    
  })
}
