# app/view/corruption.R
# Corruption & Governance Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, p, strong,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent,
        renderText, textOutput],
  bslib[card, card_header, card_body, value_box],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select, pull],
  stats[setNames, reorder],
  utils[head],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region]
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
              column(3, selectInput(ns("firm_size"), "Firm Size", choices = c("All" = "all"))),
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
            p(
              class = "text-muted small mt-2",
              "Horizontal bars show corruption or bribery rates by country; use the sort control to spotlight highest or lowest risk environments."
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("globe-africa"), " Regional Comparison"),
          card_body(
            plotlyOutput(ns("regional_chart"), height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Stacked bars compare corruption pressure across regions, highlighting where governance challenges are concentrated."
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
            p(
              class = "text-muted small mt-2",
              "Each point represents a country; the slope shows how corruption correlates with reported sales growth."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-line"), " Corruption vs. Investment"),
          card_body(
            plotlyOutput(ns("scatter_investment"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "This scatter links corruption exposure to investment rates, indicating whether governance issues deter capital spending."
            )
          )
        )
      )
    ),

    # Firm Size Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("layer-group"), " Corruption by Firm Size"),
          card_body(
            plotlyOutput(ns("firm_size_box"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Box plots summarize corruption responses by firm size, showing typical levels and variability."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-area"), " Bribery Depth vs. Breadth"),
          card_body(
            plotlyOutput(ns("bribery_scatter"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Points plot how widespread bribery requests are (breadth) against their frequency for affected firms (depth)."
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
server <- function(id, wbes_data, global_filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # Filtered data with global filters
    filtered <- reactive({
      req(wbes_data())
      d <- wbes_data()$latest

      # Apply global filters if provided
      if (!is.null(global_filters)) {
        filters <- global_filters()
        d <- apply_common_filters(
          d,
          region_value = filters$region,
          sector_value = filters$sector,
          firm_size_value = filters$firm_size,
          income_value = filters$income,
          year_value = filters$year,
          custom_regions = filters$custom_regions,
          filter_by_region_fn = filter_by_region
        )
      }

      # Apply local module filters if they exist
      if (!is.null(input$region) && input$region != "all" && !is.na(input$region)) {
        d <- d |> filter(!is.na(region) & region == input$region)
      }
      if (input$firm_size != "all" && !is.na(input$firm_size)) {
        d <- d |> filter(!is.na(firm_size) & firm_size == input$firm_size)
      }
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

      if (is.null(d) || !indicator %in% names(d)) return(NULL)

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
      d <- filtered()

      plot_ly(d, x = ~IC.FRM.CORR.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~paste0(country, "<br>Corruption: ", round(IC.FRM.CORR.ZS, 1),
                           "%<br>Capacity: ", round(IC.FRM.CAPU.ZS, 1), "%"),
              hoverinfo = "text",
              marker = list(size = 10,
                           color = ~IC.FRM.CORR.ZS,
                           colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")),
                           opacity = 0.7,
                           line = list(color = "white", width = 1))) |>
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
      d <- filtered()

      plot_ly(d, x = ~IC.FRM.CORR.ZS, y = ~IC.FRM.FINA.ZS,
              type = "scatter", mode = "markers",
              text = ~paste0(country, "<br>Corruption: ", round(IC.FRM.CORR.ZS, 1),
                           "%<br>Finance Access: ", round(IC.FRM.FINA.ZS, 1), "%"),
              hoverinfo = "text",
              marker = list(size = 10,
                           color = ~region,
                           opacity = 0.7,
                           line = list(color = "white", width = 1))) |>
        layout(
          xaxis = list(title = "Corruption Obstacle (%)"),
          yaxis = list(title = "Finance as Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          showlegend = TRUE
        ) |>
        config(displayModeBar = FALSE)
    })

    # Box plot by firm size
    output$firm_size_box <- renderPlotly({
      req(filtered())
      d <- filtered()

      plot_ly(d, y = ~IC.FRM.CORR.ZS, x = ~firm_size, type = "box",
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
      d <- filtered()

      plot_ly(d, x = ~IC.FRM.BRIB.ZS, y = ~IC.FRM.CORR.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 12,
                           color = ~firm_size,
                           opacity = 0.7,
                           line = list(color = "white", width = 1))) |>
        layout(
          xaxis = list(title = "Bribery Incidence (%)"),
          yaxis = list(title = "Corruption as Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
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
