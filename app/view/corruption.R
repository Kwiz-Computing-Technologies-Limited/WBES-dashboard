# app/view/corruption.R
# Corruption & Governance Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent,
        renderText, textOutput],
  bslib[card, card_header, card_body, value_box],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select, pull],
  stats[setNames, reorder, lm, predict],
  utils[head]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(column(12, h2(icon("balance-scale"), " Corruption & Governance", class = "text-primary mb-4"))),

    # KPIs
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_corruption"))),
      column(3, uiOutput(ns("kpi_bribery"))),
      column(3, uiOutput(ns("kpi_affected_firms"))),
      column(3, uiOutput(ns("kpi_severity")))
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
                choices = c("Corruption as Obstacle" = "IC.FRM.CORR.ZS",
                           "Bribery Incidence" = "IC.FRM.BRIB.ZS"))),
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
          card_header(icon("chart-bar"), " Corruption by Country"),
          card_body(
            plotlyOutput(ns("bar_chart"), height = "450px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-danger",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "This horizontal bar chart ranks countries by the percentage of firms reporting corruption as a major obstacle. Darker red colors indicate higher corruption perception. Countries at the top face more severe governance challenges that impede business operations."
              )
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("globe-africa"), " Regional Comparison"),
          card_body(
            plotlyOutput(ns("regional_chart"), height = "450px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-warning",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "The Governance Severity Index combines corruption perception and bribery incidence to provide a composite measure. Higher scores indicate regions where governance challenges are most acute."
              )
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
          card_header(icon("project-diagram"), " Corruption vs. Business Growth"),
          card_body(
            plotlyOutput(ns("scatter_growth"), height = "350px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-info",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "This scatter plot reveals the negative relationship between corruption and capacity utilization. Countries with higher corruption (right side) tend to have lower capacity utilization, indicating that governance issues directly impact firm productivity."
              )
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-line"), " Corruption vs. Investment"),
          card_body(
            plotlyOutput(ns("scatter_investment"), height = "350px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-success",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "This chart shows how corruption and finance access obstacles tend to co-occur. Countries with high corruption often also face challenges in accessing finance, suggesting broader institutional weaknesses. Each point represents a country, colored by region."
              )
            )
          )
        )
      )
    ),

    # Income Group Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("layer-group"), " Corruption by Income Group"),
          card_body(
            plotlyOutput(ns("income_box"), height = "350px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-primary",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "Box plots show the distribution of corruption perception across income groups. The median line, box (25th-75th percentile), and whiskers reveal that lower-income countries tend to report higher and more variable corruption levels."
              )
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-area"), " Bribery Depth vs. Breadth"),
          card_body(
            plotlyOutput(ns("bribery_scatter"), height = "350px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-secondary",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "This scatter plot compares bribery incidence (x-axis) with corruption perception (y-axis). Points in the upper-right quadrant face both high bribery frequency and high corruption perception, indicating systemic governance challenges."
              )
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
          card_body(
            uiOutput(ns("insights"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {

    # Update filters when data changes
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
    output$kpi_corruption <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.CORR.ZS, na.rm = TRUE), 1)
      div(class = "card bg-danger text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Corruption as Major Obstacle")))
    })

    output$kpi_bribery <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.BRIB.ZS, na.rm = TRUE), 1)
      div(class = "card bg-warning text-dark h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Bribery Incidence Rate")))
    })

    output$kpi_affected_firms <- renderUI({
      req(filtered())
      d <- filtered()
      affected <- sum(d$IC.FRM.CORR.ZS > 20, na.rm = TRUE)
      total <- sum(!is.na(d$IC.FRM.CORR.ZS))
      pct <- round(affected / total * 100, 1)
      div(class = "card bg-info text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(pct, "%")),
          p("Countries with High Corruption")))
    })

    output$kpi_severity <- renderUI({
      req(filtered())
      d <- filtered()
      # Calculate composite severity score
      severity <- round(mean((d$IC.FRM.CORR.ZS + d$IC.FRM.BRIB.ZS) / 2, na.rm = TRUE), 1)
      div(class = "card bg-secondary text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(severity, "%")),
          p("Governance Severity Index")))
    })

    # Bar chart
    output$bar_chart <- renderPlotly({
      req(filtered())
      d <- filtered()
      indicator <- input$indicator

      # Sort data
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
              text = ~paste0(country, ": ", round(get(indicator), 1), "%"),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = "% of Firms Reporting", titlefont = list(size = 12)),
          yaxis = list(title = "", titlefont = list(size = 10)),
          margin = list(l = 120, r = 20, t = 20, b = 40),
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
          corruption_index = (IC.FRM.CORR.ZS + IC.FRM.BRIB.ZS) / 2
        ) |>
        arrange(desc(corruption_index))

      plot_ly(regional, x = ~corruption_index, y = ~reorder(region, corruption_index),
              type = "bar", orientation = "h",
              marker = list(color = "#dc3545"),
              text = ~paste0(region, ": ", round(corruption_index, 1), "%"),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = "Governance Severity Index"),
          yaxis = list(title = ""),
          margin = list(l = 150),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Scatter - Corruption vs Growth
    output$scatter_growth <- renderPlotly({
      req(filtered())
      d <- filtered() |> filter(!is.na(IC.FRM.CORR.ZS) & !is.na(IC.FRM.CAPU.ZS))

      # Fit linear model for trend line
      if (nrow(d) > 2) {
        fit <- lm(IC.FRM.CAPU.ZS ~ IC.FRM.CORR.ZS, data = d)
        d$predicted <- predict(fit, newdata = d)
      }

      p <- plot_ly(d, x = ~IC.FRM.CORR.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers", name = "Countries",
              text = ~paste0(country, "<br>Corruption: ", round(IC.FRM.CORR.ZS, 1),
                           "%<br>Capacity: ", round(IC.FRM.CAPU.ZS, 1), "%"),
              hoverinfo = "text",
              marker = list(size = 10,
                           color = ~IC.FRM.CORR.ZS,
                           colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")),
                           opacity = 0.7,
                           line = list(color = "white", width = 1)))

      # Add trend line if we have enough data
      if (nrow(d) > 2 && exists("fit")) {
        p <- p |> add_trace(x = ~IC.FRM.CORR.ZS, y = ~predicted,
                           type = "scatter", mode = "lines",
                           name = "Trend Line",
                           line = list(color = "#1B6B5F", width = 2, dash = "dash"),
                           hoverinfo = "skip", showlegend = TRUE)
      }

      p |>
        layout(
          xaxis = list(title = "Corruption Obstacle (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Scatter - Corruption vs Investment
    output$scatter_investment <- renderPlotly({
      req(filtered())
      d <- filtered() |> filter(!is.na(IC.FRM.CORR.ZS) & !is.na(IC.FRM.FINA.ZS))

      # Fit linear model for trend line
      if (nrow(d) > 2) {
        fit <- lm(IC.FRM.FINA.ZS ~ IC.FRM.CORR.ZS, data = d)
        d$predicted <- predict(fit, newdata = d)
      }

      p <- plot_ly(d, x = ~IC.FRM.CORR.ZS, y = ~IC.FRM.FINA.ZS,
              type = "scatter", mode = "markers", name = "Countries",
              text = ~paste0(country, "<br>Corruption: ", round(IC.FRM.CORR.ZS, 1),
                           "%<br>Finance Access: ", round(IC.FRM.FINA.ZS, 1), "%"),
              hoverinfo = "text",
              marker = list(size = 10,
                           color = ~region,
                           opacity = 0.7,
                           line = list(color = "white", width = 1)))

      # Add trend line if we have enough data
      if (nrow(d) > 2 && exists("fit")) {
        p <- p |> add_trace(x = ~IC.FRM.CORR.ZS, y = ~predicted,
                           type = "scatter", mode = "lines",
                           name = "Trend Line",
                           line = list(color = "#1B6B5F", width = 2, dash = "dash"),
                           hoverinfo = "skip", showlegend = TRUE)
      }

      p |>
        layout(
          xaxis = list(title = "Corruption Obstacle (%)"),
          yaxis = list(title = "Finance as Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          showlegend = TRUE
        ) |>
        config(displayModeBar = FALSE)
    })

    # Box plot by income
    output$income_box <- renderPlotly({
      req(filtered())
      d <- filtered()

      plot_ly(d, y = ~IC.FRM.CORR.ZS, x = ~income_group, type = "box",
              marker = list(color = "#1B6B5F"),
              line = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Corruption Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Bribery scatter
    output$bribery_scatter <- renderPlotly({
      req(filtered())
      d <- filtered() |> filter(!is.na(IC.FRM.BRIB.ZS) & !is.na(IC.FRM.CORR.ZS))

      # Fit linear model for trend line
      if (nrow(d) > 2) {
        fit <- lm(IC.FRM.CORR.ZS ~ IC.FRM.BRIB.ZS, data = d)
        d$predicted <- predict(fit, newdata = d)
      }

      p <- plot_ly(d, x = ~IC.FRM.BRIB.ZS, y = ~IC.FRM.CORR.ZS,
              type = "scatter", mode = "markers", name = "Countries",
              text = ~country,
              marker = list(size = 12,
                           color = ~income_group,
                           opacity = 0.7,
                           line = list(color = "white", width = 1)))

      # Add trend line if we have enough data
      if (nrow(d) > 2 && exists("fit")) {
        p <- p |> add_trace(x = ~IC.FRM.BRIB.ZS, y = ~predicted,
                           type = "scatter", mode = "lines",
                           name = "Trend Line",
                           line = list(color = "#1B6B5F", width = 2, dash = "dash"),
                           hoverinfo = "skip", showlegend = TRUE)
      }

      p |>
        layout(
          xaxis = list(title = "Bribery Incidence (%)"),
          yaxis = list(title = "Corruption as Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          showlegend = TRUE
        ) |>
        config(displayModeBar = FALSE)
    })

    # Insights
    output$insights <- renderUI({
      req(filtered())
      d <- filtered()

      avg_corruption <- round(mean(d$IC.FRM.CORR.ZS, na.rm = TRUE), 1)
      avg_bribery <- round(mean(d$IC.FRM.BRIB.ZS, na.rm = TRUE), 1)
      worst_country <- d |> arrange(desc(IC.FRM.CORR.ZS)) |> head(1)
      best_country <- d |> arrange(IC.FRM.CORR.ZS) |> head(1)

      div(
        tags$ul(
          tags$li(tags$strong("Average Corruption Perception: "),
                 paste0(avg_corruption, "% of firms report corruption as a major obstacle")),
          tags$li(tags$strong("Bribery Prevalence: "),
                 paste0(avg_bribery, "% of firms report bribery incidence")),
          tags$li(tags$strong("Highest Concern: "),
                 paste0(worst_country$country, " (", round(worst_country$IC.FRM.CORR.ZS, 1), "%)")),
          tags$li(tags$strong("Best Performance: "),
                 paste0(best_country$country, " (", round(best_country$IC.FRM.CORR.ZS, 1), "%)")),
          tags$li(tags$strong("Key Finding: "),
                 "Countries with higher corruption tend to have lower capacity utilization and higher financing obstacles.")
        )
      )
    })

  })
}
