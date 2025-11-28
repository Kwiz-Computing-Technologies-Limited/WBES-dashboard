# app/view/workforce.R
# Workforce & Gender Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, p, strong,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select],
  stats[setNames],
  utils[head]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(column(12, h2(icon("users"), " Workforce & Gender Inclusion", class = "text-primary mb-4"))),

    # KPIs
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_workforce"))),
      column(3, uiOutput(ns("kpi_female_workers"))),
      column(3, uiOutput(ns("kpi_female_ownership"))),
      column(3, uiOutput(ns("kpi_gender_gap")))
    ),

    # Filters
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(class = "py-2",
            fluidRow(
              column(3, selectInput(ns("region"), "Region", choices = c("All" = "all"))),
              column(3, selectInput(ns("indicator"), "Indicator",
                choices = c("Workforce Obstacle" = "IC.FRM.WKFC.ZS",
                           "Female Workers" = "IC.FRM.FEMW.ZS",
                           "Female Ownership" = "IC.FRM.FEMO.ZS"))),
              column(3, selectInput(ns("income"), "Income Group", choices = c("All" = "all"))),
              column(3, selectInput(ns("sort"), "Sort By",
                choices = c("Highest First" = "desc", "Lowest First" = "asc")))
            )
          )
        )
      )
    ),

    # Main Charts
    fluidRow(
      class = "mb-4",
      column(8,
        card(
          card_header(icon("chart-bar"), " Workforce Indicators by Country"),
          card_body(plotlyOutput(ns("bar_chart"), height = "450px"))
        )
      ),
      column(4,
        card(
          card_header(icon("venus-mars"), " Gender Balance Overview"),
          card_body(plotlyOutput(ns("gender_overview"), height = "450px"))
        )
      )
    ),

    # Analysis Charts
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-line"), " Female Participation Trends"),
          card_body(plotlyOutput(ns("participation_chart"), height = "350px"))
        )
      ),
      column(6,
        card(
          card_header(icon("project-diagram"), " Workforce vs. Productivity"),
          card_body(plotlyOutput(ns("scatter_productivity"), height = "350px"))
        )
      )
    ),

    # Gender Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("layer-group"), " Female Ownership by Region"),
          card_body(plotlyOutput(ns("regional_gender"), height = "350px"))
        )
      ),
      column(6,
        card(
          card_header(icon("chart-area"), " Gender Gap Analysis"),
          card_body(plotlyOutput(ns("gender_gap_chart"), height = "350px"))
        )
      )
    ),

    # Income Group Comparison
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("coins"), " Workforce Challenge by Income"),
          card_body(plotlyOutput(ns("income_comparison"), height = "350px"))
        )
      ),
      column(6,
        card(
          card_header(icon("graduation-cap"), " Skills & Gender Correlation"),
          card_body(plotlyOutput(ns("skills_correlation"), height = "350px"))
        )
      )
    ),

    # Insights
    fluidRow(
      column(12,
        card(
          card_header(icon("lightbulb"), " Key Insights"),
          card_body(uiOutput(ns("insights")))
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {

    # Update filters
    observeEvent(wbes_data(), {
      req(wbes_data())
      d <- wbes_data()$latest
      regions <- c("All" = "all", setNames(unique(d$region), unique(d$region)))
      incomes <- c("All" = "all", setNames(unique(d$income_group), unique(d$income_group)))
      shiny::updateSelectInput(session, "region", choices = regions)
      shiny::updateSelectInput(session, "income", choices = incomes)
    })

    # Filtered data
    filtered <- reactive({
      req(wbes_data())
      d <- wbes_data()$latest
      if (input$region != "all") d <- filter(d, region == input$region)
      if (input$income != "all") d <- filter(d, income_group == input$income)
      d
    })

    # KPIs
    output$kpi_workforce <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.WKFC.ZS, na.rm = TRUE), 1)
      div(class = "card bg-primary text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Workforce as Obstacle")))
    })

    output$kpi_female_workers <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.FEMW.ZS, na.rm = TRUE), 1)
      div(class = "card bg-info text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Female Workers (Avg)")))
    })

    output$kpi_female_ownership <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)
      div(class = "card bg-secondary text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Female Ownership")))
    })

    output$kpi_gender_gap <- renderUI({
      req(filtered())
      d <- filtered()
      # Gender gap: difference between worker participation and ownership
      gap <- round(mean(d$IC.FRM.FEMW.ZS - d$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)
      color <- if(gap > 15) "warning" else "success"
      div(class = paste0("card bg-", color, " text-white h-100"),
        div(class = "card-body text-center",
          h2(paste0(gap, "%")),
          p("Gender Leadership Gap")))
    })

    # Bar chart
    output$bar_chart <- renderPlotly({
      req(filtered())
      d <- filtered()
      indicator <- input$indicator

      if (input$sort == "desc") {
        d <- arrange(d, desc(.data[[indicator]]))
      } else {
        d <- arrange(d, .data[[indicator]])
      }

      d <- head(d, 20)
      d$country <- factor(d$country, levels = rev(d$country))

      plot_ly(d, y = ~country, x = ~get(indicator), type = "bar",
              orientation = "h",
              marker = list(color = ~get(indicator),
                           colorscale = list(c(0, "#1B6B5F"), c(0.5, "#17a2b8"), c(1, "#F49B7A"))),
              text = ~paste0(country, ": ", round(get(indicator), 1), "%"),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = "Percentage (%)"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Gender overview
    output$gender_overview <- renderPlotly({
      req(filtered())
      d <- filtered()

      avg_workers <- round(mean(d$IC.FRM.FEMW.ZS, na.rm = TRUE), 1)
      avg_ownership <- round(mean(d$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)

      data <- data.frame(
        category = c("Female Workers", "Female Ownership"),
        value = c(avg_workers, avg_ownership),
        color = c("#1B6B5F", "#F49B7A")
      )

      plot_ly(data, x = ~category, y = ~value, type = "bar",
              marker = list(color = ~color),
              text = ~paste0(value, "%"),
              textposition = "outside") |>
        layout(
          yaxis = list(title = "Percentage (%)"),
          xaxis = list(title = ""),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Participation chart
    output$participation_chart <- renderPlotly({
      req(wbes_data())
      regional <- wbes_data()$regional
      if (is.null(regional)) return(NULL)

      plot_ly(regional, x = ~IC.FRM.FEMW.ZS, y = ~IC.FRM.FEMO.ZS,
              type = "scatter", mode = "markers+text",
              text = ~region,
              textposition = "top center",
              marker = list(size = 15, color = "#1B6B5F", opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Female Workers (%)"),
          yaxis = list(title = "Female Ownership (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Scatter - Workforce vs Productivity
    output$scatter_productivity <- renderPlotly({
      req(filtered())
      d <- filtered()

      plot_ly(d, x = ~IC.FRM.WKFC.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 10,
                           color = ~IC.FRM.FEMW.ZS,
                           colorscale = list(c(0, "#dc3545"), c(0.5, "#F4A460"), c(1, "#2E7D32")),
                           colorbar = list(title = "Female<br>Workers (%)"),
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Workforce as Obstacle (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Regional gender
    output$regional_gender <- renderPlotly({
      req(wbes_data())
      regional <- wbes_data()$regional
      if (is.null(regional)) return(NULL)

      plot_ly(regional) |>
        add_trace(y = ~region, x = ~IC.FRM.FEMW.ZS, type = "bar",
                 orientation = "h", name = "Female Workers",
                 marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~region, x = ~IC.FRM.FEMO.ZS, type = "bar",
                 orientation = "h", name = "Female Ownership",
                 marker = list(color = "#F49B7A")) |>
        layout(
          barmode = "group",
          xaxis = list(title = "Percentage (%)"),
          yaxis = list(title = ""),
          legend = list(x = 0.7, y = 0.1),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Gender gap chart
    output$gender_gap_chart <- renderPlotly({
      req(filtered())
      d <- filtered() |>
        mutate(gender_gap = IC.FRM.FEMW.ZS - IC.FRM.FEMO.ZS) |>
        arrange(desc(gender_gap)) |>
        head(15)

      d$country <- factor(d$country, levels = rev(d$country))

      plot_ly(d, y = ~country, x = ~gender_gap, type = "bar",
              orientation = "h",
              marker = list(color = ~gender_gap,
                           colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")))) |>
        layout(
          xaxis = list(title = "Gap: Workers - Ownership (%)"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Income comparison
    output$income_comparison <- renderPlotly({
      req(filtered())
      d <- filtered()

      plot_ly(d, y = ~IC.FRM.WKFC.ZS, x = ~income_group, type = "box",
              marker = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Workforce Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Skills correlation
    output$skills_correlation <- renderPlotly({
      req(filtered())
      d <- filtered()

      plot_ly(d, x = ~IC.FRM.FEMO.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 10,
                           color = ~region,
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Female Ownership (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Insights
    output$insights <- renderUI({
      req(filtered())
      d <- filtered()

      avg_workforce <- round(mean(d$IC.FRM.WKFC.ZS, na.rm = TRUE), 1)
      avg_female_workers <- round(mean(d$IC.FRM.FEMW.ZS, na.rm = TRUE), 1)
      avg_female_ownership <- round(mean(d$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)
      gender_gap <- round(avg_female_workers - avg_female_ownership, 1)

      best_gender <- d |> arrange(desc(IC.FRM.FEMO.ZS)) |> head(1)
      worst_workforce <- d |> arrange(desc(IC.FRM.WKFC.ZS)) |> head(1)

      div(
        tags$ul(
          tags$li(tags$strong("Workforce Challenge: "),
                 paste0(avg_workforce, "% of firms report workforce as a major obstacle")),
          tags$li(tags$strong("Female Participation: "),
                 paste0(avg_female_workers, "% female workers on average")),
          tags$li(tags$strong("Female Leadership: "),
                 paste0(avg_female_ownership, "% of firms have female ownership")),
          tags$li(tags$strong("Gender Leadership Gap: "),
                 paste0(gender_gap, "% gap between workforce and ownership participation")),
          tags$li(tags$strong("Best Gender Inclusion: "),
                 paste0(best_gender$country, " (", round(best_gender$IC.FRM.FEMO.ZS, 1), "% female ownership)")),
          tags$li(tags$strong("Highest Workforce Challenge: "),
                 paste0(worst_workforce$country, " (", round(worst_workforce$IC.FRM.WKFC.ZS, 1), "%)"))
        )
      )
    })

  })
}
