# app/view/infrastructure.R
# Infrastructure Constraints Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, desc, mutate, group_by, summarise]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container-fluid py-4",
    
    fluidRow(column(12, h2(icon("bolt"), " Infrastructure Constraints", class = "text-primary mb-4"))),
    
    # KPIs
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_power"))),
      column(3, uiOutput(ns("kpi_elec"))),
      column(3, uiOutput(ns("kpi_infra"))),
      column(3, uiOutput(ns("kpi_crime")))
    ),
    
    # Filter
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(class = "py-2",
            fluidRow(
              column(4, selectInput(ns("region"), "Region", choices = c("All" = "all"))),
              column(4, selectInput(ns("indicator"), "Indicator",
                choices = c("Power Outages" = "IC.FRM.OUTG.ZS",
                           "Electricity" = "IC.FRM.ELEC.ZS",
                           "Infrastructure" = "IC.FRM.INFRA.ZS"))),
              column(4, selectInput(ns("income"), "Income Group", choices = c("All" = "all")))
            )
          )
        )
      )
    ),
    
    # Charts
    fluidRow(
      class = "mb-4",
      column(8,
        card(
          card_header(icon("chart-bar"), " Infrastructure by Country"),
          card_body(plotlyOutput(ns("bar_chart"), height = "450px"))
        )
      ),
      column(4,
        card(
          card_header(icon("chart-pie"), " Regional Distribution"),
          card_body(plotlyOutput(ns("pie_chart"), height = "450px"))
        )
      )
    ),
    
    # Correlation
    fluidRow(
      column(6,
        card(
          card_header(icon("project-diagram"), " Power vs. Productivity"),
          card_body(plotlyOutput(ns("scatter"), height = "350px"))
        )
      ),
      column(6,
        card(
          card_header(icon("th"), " Regional Heatmap"),
          card_body(plotlyOutput(ns("heatmap"), height = "350px"))
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
      d <- wbes_data()$latest
      regions <- c("All" = "all", setNames(unique(d$region), unique(d$region)))
      incomes <- c("All" = "all", setNames(unique(d$income_group), unique(d$income_group)))
      shiny::updateSelectInput(session, "region", choices = regions)
      shiny::updateSelectInput(session, "income", choices = incomes)
    })
    
    filtered <- reactive({
      req(wbes_data())
      d <- wbes_data()$latest
      if (input$region != "all") d <- filter(d, region == input$region)
      if (input$income != "all") d <- filter(d, income_group == input$income)
      d
    })
    
    # KPIs
    output$kpi_power <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.OUTG.ZS, na.rm = TRUE), 1)
      div(class = "card bg-primary text-white h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Power Outages")))
    })
    
    output$kpi_elec <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.ELEC.ZS, na.rm = TRUE), 1)
      div(class = "card bg-secondary text-white h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Electricity Obstacle")))
    })
    
    output$kpi_infra <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.INFRA.ZS, na.rm = TRUE), 1)
      div(class = "card bg-warning text-dark h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Infrastructure")))
    })
    
    output$kpi_crime <- renderUI({
      req(filtered())
      d <- filtered()
      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d)) return(NULL)
      val <- round(mean(d$IC.FRM.CRIM.ZS, na.rm = TRUE), 1)
      div(class = "card bg-danger text-white h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Crime Obstacle")))
    })
    
    # Bar chart
    output$bar_chart <- renderPlotly({
      req(filtered())
      d <- filtered()
      indicator <- input$indicator
      d <- arrange(d, desc(.data[[indicator]]))[1:15, ]
      d$country <- factor(d$country, levels = rev(d$country))
      
      plot_ly(d, y = ~country, x = ~get(indicator), type = "bar",
              orientation = "h",
              marker = list(color = ~get(indicator),
                           colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545")))) |>
        layout(
          xaxis = list(title = "% of Firms"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Pie chart
    output$pie_chart <- renderPlotly({
      req(wbes_data())
      regional <- wbes_data()$regional
      if (is.null(regional)) return(NULL)
      
      plot_ly(regional, labels = ~region, values = ~IC.FRM.OUTG.ZS,
              type = "pie", hole = 0.4,
              marker = list(colors = c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D"))) |>
        layout(showlegend = TRUE, paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })
    
    # Scatter
    output$scatter <- renderPlotly({
      req(filtered())
      d <- filtered()
      
      plot_ly(d, x = ~IC.FRM.OUTG.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 12, color = ~IC.FRM.OUTG.ZS,
                           colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")),
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Power Outages (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Heatmap
    output$heatmap <- renderPlotly({
      regions <- c("Sub-Saharan Africa", "South Asia", "East Asia", "Latin America", "Europe")
      indicators <- c("Power", "Electricity", "Infrastructure", "Crime")
      z <- matrix(c(35, 42, 28, 25, 28, 35, 22, 18, 18, 22, 15, 15, 22, 28, 18, 20, 12, 15, 12, 12),
                  nrow = 5, byrow = TRUE)
      
      plot_ly(x = indicators, y = regions, z = z, type = "heatmap",
              colorscale = list(c(0, "#e8f5e9"), c(0.5, "#fff3e0"), c(1, "#ffebee"))) |>
        layout(paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })
    
  })
}
