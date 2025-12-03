# app/view/crime.R
# Crime & Security Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, p, strong,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select, case_when, n],
  stats[setNames, reorder],
  utils[head]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(column(12, h2(icon("shield-alt"), " Crime & Security", class = "text-primary mb-4"))),

    # KPIs
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_crime"))),
      column(3, uiOutput(ns("kpi_security_cost"))),
      column(3, uiOutput(ns("kpi_high_risk"))),
      column(3, uiOutput(ns("kpi_security_index")))
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
                choices = c("Crime as Obstacle" = "IC.FRM.CRIM.ZS",
                           "Security Costs" = "IC.FRM.SECU.ZS"))),
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
          card_header(icon("chart-bar"), " Security Indicators by Country"),
          card_body(
            plotlyOutput(ns("bar_chart"), height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Bars benchmark crime-related obstacles or security costs by country, surfacing the highest-risk environments."
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("globe-africa"), " Regional Security Overview"),
          card_body(
            plotlyOutput(ns("regional_chart"), height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Regional averages reveal how security pressures vary across geographies, contextualizing country performance."
            )
          )
        )
      )
    ),

    # Analysis Charts
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("coins"), " Security Cost Analysis"),
          card_body(
            plotlyOutput(ns("cost_analysis"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Bars translate security spending into percentage of sales, indicating the financial burden of crime mitigation."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("project-diagram"), " Crime vs. Business Performance"),
          card_body(
            plotlyOutput(ns("crime_performance"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Scatter points show how crime obstacles relate to operational performance, signaling whether insecurity dampens output."
            )
          )
        )
      )
    ),

    # Security Environment
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("layer-group"), " Security by Income Group"),
          card_body(
            plotlyOutput(ns("income_security"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Box plots summarize crime and security costs across income tiers, highlighting risk dispersion."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-area"), " Crime Impact Matrix"),
          card_body(
            plotlyOutput(ns("impact_matrix"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Matrix cells combine crime prevalence and severity to pinpoint the most disruptive contexts."
            )
          )
        )
      )
    ),

    # Comparative Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("balance-scale"), " Crime vs. Corruption"),
          card_body(
            plotlyOutput(ns("crime_corruption"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Scatter compares security risks with corruption incidence to see how governance and crime challenges overlap."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-pie"), " Risk Distribution"),
          card_body(
            plotlyOutput(ns("risk_distribution"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "The pie segments firms by reported risk level, providing a quick view of how pervasive security threats are."
            )
          )
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
      # Filter out NA values from region and income_group
      regions_vec <- unique(d$region) |> stats::na.omit() |> as.character() |> sort()
      incomes_vec <- unique(d$income_group) |> stats::na.omit() |> as.character() |> sort()
      regions <- c("All" = "all", setNames(regions_vec, regions_vec))
      incomes <- c("All" = "all", setNames(incomes_vec, incomes_vec))
      shiny::updateSelectInput(session, "region", choices = regions)
      shiny::updateSelectInput(session, "income", choices = incomes)
    })

    # Filtered data
    filtered <- reactive({
      req(wbes_data())
      d <- wbes_data()$latest
      if (input$region != "all" && !is.na(input$region)) {
        d <- d |> filter(!is.na(region) & region == input$region)
      }
      if (input$income != "all" && !is.na(input$income)) {
        d <- d |> filter(!is.na(income_group) & income_group == input$income)
      }
      d
    })

    # KPIs
    output$kpi_crime <- renderUI({
      req(filtered())
      d <- filtered()
      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d)) return(NULL)
      val <- round(mean(d$IC.FRM.CRIM.ZS, na.rm = TRUE), 1)
      div(class = "card bg-danger text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Crime as Major Obstacle")))
    })

    output$kpi_security_cost <- renderUI({
      req(filtered())
      d <- filtered()
      if (is.null(d) || !"IC.FRM.SECU.ZS" %in% names(d)) return(NULL)
      val <- round(mean(d$IC.FRM.SECU.ZS, na.rm = TRUE), 2)
      div(class = "card bg-warning text-dark h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Avg Security Costs (% Sales)")))
    })

    output$kpi_high_risk <- renderUI({
      req(filtered())
      d <- filtered()
      high_risk <- sum(d$IC.FRM.CRIM.ZS > 30, na.rm = TRUE)
      total <- sum(!is.na(d$IC.FRM.CRIM.ZS))
      pct <- round(high_risk / total * 100, 1)
      div(class = "card bg-dark text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(pct, "%")),
          p("High Risk Countries")))
    })

    output$kpi_security_index <- renderUI({
      req(filtered())
      d <- filtered()
      # Security index: weighted average of crime and security costs
      sec_index <- round(mean((d$IC.FRM.CRIM.ZS * 0.7 + d$IC.FRM.SECU.ZS * 10 * 0.3), na.rm = TRUE), 1)
      div(class = "card bg-info text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(sec_index, "%")),
          p("Security Risk Index")))
    })

    # Bar chart
    output$bar_chart <- renderPlotly({
      req(filtered())
      d <- filtered()
      indicator <- input$indicator

      if (is.null(d) || !indicator %in% names(d)) return(NULL)

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
                           colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))),
              text = ~paste0(country, ": ", round(get(indicator), 2), if(indicator == "IC.FRM.SECU.ZS") "%" else "%"),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = if(indicator == "IC.FRM.SECU.ZS") "% of Sales" else "% of Firms"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Regional chart
    output$regional_chart <- renderPlotly({
      req(wbes_data())
      regional <- wbes_data()$regional
      if (is.null(regional)) return(NULL)

      regional <- regional |>
        mutate(
          security_index = (IC.FRM.CRIM.ZS * 0.7 + IC.FRM.SECU.ZS * 10 * 0.3)
        ) |>
        arrange(desc(security_index))

      plot_ly(regional, x = ~security_index, y = ~reorder(region, security_index),
              type = "bar", orientation = "h",
              marker = list(color = "#dc3545"),
              text = ~paste0(region, ": ", round(security_index, 1)),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = "Security Risk Index"),
          yaxis = list(title = ""),
          margin = list(l = 150),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Cost analysis
    output$cost_analysis <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.SECU.ZS" %in% names(d)) return(NULL)

      d <- d |>
        arrange(desc(IC.FRM.SECU.ZS)) |>
        head(15)

      d$country <- factor(d$country, levels = rev(d$country))

      plot_ly(d, y = ~country, x = ~IC.FRM.SECU.ZS, type = "bar",
              orientation = "h",
              marker = list(color = "#F49B7A"),
              text = ~paste0(country, ": ", round(IC.FRM.SECU.ZS, 2), "% of sales"),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = "Security Costs (% of Sales)"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Crime vs performance
    output$crime_performance <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d) || !"IC.FRM.CAPU.ZS" %in% names(d)) return(NULL)

      plot_ly(d, x = ~IC.FRM.CRIM.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~paste0(country, "<br>Crime: ", round(IC.FRM.CRIM.ZS, 1),
                           "%<br>Capacity: ", round(IC.FRM.CAPU.ZS, 1), "%"),
              hoverinfo = "text",
              marker = list(size = 10,
                           color = ~IC.FRM.CRIM.ZS,
                           colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")),
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Crime as Obstacle (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Income security
    output$income_security <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d) || !"IC.FRM.SECU.ZS" %in% names(d)) return(NULL)

      plot_ly(d) |>
        add_trace(y = ~IC.FRM.CRIM.ZS, x = ~income_group, type = "box",
                 name = "Crime Obstacle",
                 marker = list(color = "#dc3545")) |>
        add_trace(y = ~IC.FRM.SECU.ZS * 10, x = ~income_group, type = "box",
                 name = "Security Costs (x10)",
                 marker = list(color = "#F4A460")) |>
        layout(
          boxmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Percentage (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Impact matrix
    output$impact_matrix <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d) || !"IC.FRM.SECU.ZS" %in% names(d)) return(NULL)

      plot_ly(d, x = ~IC.FRM.CRIM.ZS, y = ~IC.FRM.SECU.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 12,
                           color = ~region,
                           opacity = 0.7,
                           line = list(color = "white", width = 1))) |>
        layout(
          xaxis = list(title = "Crime as Obstacle (%)"),
          yaxis = list(title = "Security Costs (% of Sales)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Crime vs corruption
    output$crime_corruption <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d) || !"IC.FRM.CORR.ZS" %in% names(d)) return(NULL)

      plot_ly(d, x = ~IC.FRM.CORR.ZS, y = ~IC.FRM.CRIM.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 10,
                           color = ~income_group,
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Corruption Obstacle (%)"),
          yaxis = list(title = "Crime Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Risk distribution
    output$risk_distribution <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d)) return(NULL)

      d <- d |>
        mutate(
          risk_level = case_when(
            IC.FRM.CRIM.ZS > 30 ~ "High Risk",
            IC.FRM.CRIM.ZS > 15 ~ "Medium Risk",
            TRUE ~ "Low Risk"
          )
        ) |>
        group_by(risk_level) |>
        summarise(count = n())

      plot_ly(d, labels = ~risk_level, values = ~count,
              type = "pie",
              marker = list(colors = c("#2E7D32", "#F4A460", "#dc3545"))) |>
        layout(
          showlegend = TRUE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Insights
    output$insights <- renderUI({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d) || !"IC.FRM.SECU.ZS" %in% names(d)) return(NULL)

      avg_crime <- round(mean(d$IC.FRM.CRIM.ZS, na.rm = TRUE), 1)
      avg_security_cost <- round(mean(d$IC.FRM.SECU.ZS, na.rm = TRUE), 2)

      worst_crime <- d |> arrange(desc(IC.FRM.CRIM.ZS)) |> head(1)
      highest_cost <- d |> arrange(desc(IC.FRM.SECU.ZS)) |> head(1)
      safest <- d |> arrange(IC.FRM.CRIM.ZS) |> head(1)

      div(
        tags$ul(
          tags$li(tags$strong("Average Crime Concern: "),
                 paste0(avg_crime, "% of firms report crime as a major obstacle")),
          tags$li(tags$strong("Security Spending: "),
                 paste0(avg_security_cost, "% of annual sales spent on security")),
          tags$li(tags$strong("Highest Crime Concern: "),
                 paste0(worst_crime$country, " (", round(worst_crime$IC.FRM.CRIM.ZS, 1), "%)")),
          tags$li(tags$strong("Highest Security Costs: "),
                 paste0(highest_cost$country, " (", round(highest_cost$IC.FRM.SECU.ZS, 2), "% of sales)")),
          tags$li(tags$strong("Safest Environment: "),
                 paste0(safest$country, " (", round(safest$IC.FRM.CRIM.ZS, 1), "% crime concern)")),
          tags$li(tags$strong("Key Finding: "),
                 "Crime and corruption tend to be correlated, suggesting broader governance challenges. High crime environments significantly impact business capacity and productivity.")
        )
      )
    })

  })
}
