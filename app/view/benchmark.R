# app/view/benchmark.R
# Cross-Country Benchmarking Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, p,
        fluidRow, column, selectInput, selectizeInput, actionButton, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  DT[DTOutput, renderDT, datatable],
  dplyr[filter, select, arrange, desc, any_of],
  stats[setNames, lm, predict]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container-fluid py-4",
    
    fluidRow(
      column(12, h2(icon("chart-bar"), " Cross-Country Benchmarking", class = "text-primary mb-4"))
    ),
    
    # Selection Panel
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("sliders-h"), " Comparison Settings"),
          card_body(
            fluidRow(
              column(5, selectizeInput(ns("countries"), "Select Countries (max 10)",
                choices = NULL, multiple = TRUE, options = list(maxItems = 10))),
              column(3, selectInput(ns("indicator"), "Primary Indicator",
                choices = c(
                  "Power Outages" = "IC.FRM.OUTG.ZS",
                  "Finance Obstacle" = "IC.FRM.FINA.ZS",
                  "Corruption" = "IC.FRM.CORR.ZS",
                  "Capacity Utilization" = "IC.FRM.CAPU.ZS"
                ))),
              column(2, selectInput(ns("sort"), "Sort", choices = c("Ascending" = "asc", "Descending" = "desc"))),
              column(2, actionButton(ns("compare"), "Compare", icon = icon("exchange-alt"),
                class = "btn-primary w-100 mt-4"))
            )
          )
        )
      )
    ),
    
    # Main Chart
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("chart-bar"), " Country Comparison"),
          card_body(plotlyOutput(ns("comparison_bar"), height = "450px"))
        )
      )
    ),
    
    # Scatter and Table
    fluidRow(
      column(6,
        card(
          card_header(icon("project-diagram"), " Correlation Analysis"),
          card_body(plotlyOutput(ns("scatter_plot"), height = "350px"))
        )
      ),
      column(6,
        card(
          card_header(icon("table"), " Data Table"),
          card_body(DTOutput(ns("comparison_table")))
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(wbes_data(), {
      req(wbes_data())
      countries <- sort(wbes_data()$countries)
      # Pre-select some African countries
      selected <- intersect(c("Kenya", "Nigeria", "South Africa", "Ghana", "Ethiopia"), countries)
      if (length(selected) == 0) selected <- countries[1:min(5, length(countries))]
      
      shiny::updateSelectizeInput(session, "countries",
        choices = setNames(countries, countries),
        selected = selected)
    })
    
    # Comparison data
    comparison_data <- reactive({
      req(wbes_data(), input$countries)
      d <- filter(wbes_data()$latest, country %in% input$countries)
      
      if (input$sort == "desc") {
        d <- arrange(d, desc(.data[[input$indicator]]))
      } else {
        d <- arrange(d, .data[[input$indicator]])
      }
      d
    }) |> shiny::bindEvent(input$compare, ignoreNULL = FALSE)
    
    # Bar chart
    output$comparison_bar <- renderPlotly({
      req(comparison_data())
      d <- comparison_data()
      indicator <- input$indicator
      
      d$country <- factor(d$country, levels = d$country)
      
      colors <- c(
        "Sub-Saharan Africa" = "#1B6B5F",
        "South Asia" = "#F49B7A",
        "East Asia & Pacific" = "#2E7D32",
        "Latin America & Caribbean" = "#17a2b8",
        "Europe & Central Asia" = "#6C757D"
      )
      
      plot_ly(d, x = ~country, y = ~get(indicator), type = "bar",
              color = ~region, colors = colors) |>
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = gsub("IC.FRM.|.ZS", "", indicator)),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 120),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Scatter plot
    output$scatter_plot <- renderPlotly({
      req(wbes_data())
      d <- wbes_data()$latest

      # Filter data for trend line (remove NAs)
      d_trend <- d |>
        filter(!is.na(IC.FRM.OUTG.ZS) & !is.na(IC.FRM.CAPU.ZS))

      # Fit linear model for trend line
      fit <- NULL
      if (nrow(d_trend) > 2) {
        fit <- lm(IC.FRM.CAPU.ZS ~ IC.FRM.OUTG.ZS, data = d_trend)
        d_trend$predicted <- predict(fit, newdata = d_trend)
      }

      p <- plot_ly(d, x = ~IC.FRM.OUTG.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              name = "Countries",
              color = ~region, text = ~country,
              marker = list(size = 10, opacity = 0.7))

      # Add trend line if model exists
      if (!is.null(fit) && nrow(d_trend) > 2) {
        p <- p |>
          add_trace(data = d_trend, x = ~IC.FRM.OUTG.ZS, y = ~predicted,
                   type = "scatter", mode = "lines",
                   name = "Trend Line",
                   line = list(color = "#1B6B5F", width = 2, dash = "dash"),
                   hoverinfo = "skip",
                   showlegend = TRUE,
                   inherit = FALSE)
      }

      p |>
        layout(
          xaxis = list(title = "Power Outages Obstacle (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Data table
    output$comparison_table <- renderDT({
      req(comparison_data())
      
      d <- comparison_data() |>
        select(any_of(c("country", "region", "income_group",
                        "IC.FRM.OUTG.ZS", "IC.FRM.FINA.ZS",
                        "IC.FRM.CORR.ZS", "IC.FRM.CAPU.ZS")))
      
      names(d) <- c("Country", "Region", "Income", "Power", "Finance", "Corruption", "Capacity")
      
      datatable(d, options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
                class = "display compact", rownames = FALSE)
    })
    
  })
}
