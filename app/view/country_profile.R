# app/view/country_profile.R
# Country Profile Deep Dive Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h4, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, config],
  dplyr[filter, arrange, mutate]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(
      column(12,
        h2(icon("flag"), " Country Profile", class = "text-primary mb-4")
      )
    ),

    # Country Selector
    fluidRow(
      class = "mb-4",
      column(4,
        card(
          card_body(
            selectInput(ns("country"), "Select Country", choices = NULL, width = "100%")
          )
        )
      ),
      column(8, uiOutput(ns("country_summary")))
    ),

    # Radar and Metrics
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-pie"), " Business Environment Radar"),
          card_body(plotlyOutput(ns("radar_chart"), height = "400px"))
        )
      ),
      column(6,
        card(
          card_header(icon("th-list"), " Key Indicators"),
          card_body(uiOutput(ns("key_metrics")))
        )
      )
    ),

    # Time Series
    fluidRow(
      column(12,
        card(
          card_header(icon("chart-line"), " Indicator Trends"),
          card_body(plotlyOutput(ns("time_series"), height = "350px"))
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
      countries <- sort(unique(wbes_data()$country))
      shiny::updateSelectInput(session, "country",
        choices = setNames(countries, countries),
        selected = if ("Kenya" %in% countries) "Kenya" else countries[1])
    })

    # Selected country data
    country_data <- reactive({
      req(wbes_data(), input$country)
      filter(wbes_data()$latest, country == input$country)
    })

    # Country summary
    output$country_summary <- renderUI({
      req(country_data())
      d <- country_data()

      div(class = "card h-100",
        div(class = "card-body",
          fluidRow(
            column(4, div(class = "text-center",
              h4(d$region[1], class = "text-primary"),
              p(class = "text-muted mb-0", "Region")
            )),
            column(4, div(class = "text-center",
              h4(d$income_group[1], class = "text-secondary"),
              p(class = "text-muted mb-0", "Income Group")
            )),
            column(4, div(class = "text-center",
              h4(d$sample_size[1], class = "text-success"),
              p(class = "text-muted mb-0", "Firms Surveyed")
            ))
          )
        )
      )
    })

    # Radar Chart
    output$radar_chart <- renderPlotly({
      req(country_data())
      d <- country_data()

      indicators <- c(
        "Infrastructure" = 100 - min(d$IC.FRM.INFRA.ZS, 100),
        "Finance Access" = 100 - d$IC.FRM.FINA.ZS,
        "Low Corruption" = 100 - d$IC.FRM.CORR.ZS,
        "Capacity Use" = d$IC.FRM.CAPU.ZS,
        "Exports" = d$IC.FRM.EXPRT.ZS * 2,
        "Gender Equity" = d$IC.FRM.FEMO.ZS * 2
      )

      plot_ly(
        type = "scatterpolar",
        r = as.numeric(indicators),
        theta = names(indicators),
        fill = "toself",
        fillcolor = "rgba(27, 107, 95, 0.3)",
        line = list(color = "#1B6B5F")
      ) |>
        layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Key Metrics
    output$key_metrics <- renderUI({
      req(country_data())
      d <- country_data()

      metrics <- list(
        list("Power Outages Obstacle", round(d$IC.FRM.OUTG.ZS, 1), "%", "bolt"),
        list("Electricity Obstacle", round(d$IC.FRM.ELEC.ZS, 1), "%", "plug"),
        list("Finance Obstacle", round(d$IC.FRM.FINA.ZS, 1), "%", "credit-card"),
        list("Bribery Incidence", round(d$IC.FRM.BRIB.ZS, 1), "%", "hand-holding-usd"),
        list("Capacity Utilization", round(d$IC.FRM.CAPU.ZS, 1), "%", "industry"),
        list("Female Ownership", round(d$IC.FRM.FEMO.ZS, 1), "%", "venus")
      )

      div(
        class = "list-group",
        lapply(metrics, function(m) {
          div(class = "list-group-item d-flex justify-content-between align-items-center",
            tags$span(icon(m[[4]]), " ", m[[1]]),
            tags$span(class = "badge bg-primary rounded-pill", paste0(m[[2]], m[[3]]))
          )
        })
      )
    })

    # Time Series
    output$time_series <- renderPlotly({
      req(wbes_data(), input$country)

      panel <- wbes_data()$raw
      panel <- filter(panel, country == input$country)

      if (nrow(panel) == 0) return(NULL)

      plot_ly(panel, x = ~year) |>
        add_trace(y = ~IC.FRM.OUTG.ZS, name = "Power Outages",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#1B6B5F")) |>
        add_trace(y = ~IC.FRM.FINA.ZS, name = "Finance Obstacle",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#F49B7A")) |>
        add_trace(y = ~IC.FRM.CORR.ZS, name = "Corruption",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#6C757D")) |>
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "% of Firms"),
          legend = list(orientation = "h", y = -0.15),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

  })
}
