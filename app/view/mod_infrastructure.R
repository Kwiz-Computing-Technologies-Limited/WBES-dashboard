# app/view/mod_infrastructure.R
# Infrastructure Constraints Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, div, h2, h3, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, mutate, group_by, summarise],
  stats[setNames, predict, lm]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "infrastructure-container",
    
    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("bolt"), "Infrastructure Constraints Analysis", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Analyze power, water, and transport infrastructure impacts on business operations")
        )
      )
    ),
    
    # KPI Row
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_outages"))),
      column(3, uiOutput(ns("kpi_duration"))),
      column(3, uiOutput(ns("kpi_generator"))),
      column(3, uiOutput(ns("kpi_losses")))
    ),
    
    # Filters
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(
            class = "py-2",
            fluidRow(
              column(4,
                selectInput(ns("region_filter"), "Region",
                  choices = c("All Regions" = "all"),
                  selected = "all"
                )
              ),
              column(4,
                selectInput(ns("infra_indicator"), "Indicator",
                  choices = c(
                    "Power Outages" = "power_outages_per_month",
                    "Outage Duration" = "avg_outage_duration_hrs",
                    "Generator Usage" = "firms_with_generator_pct",
                    "Water Issues" = "water_insufficiency_pct"
                  )
                )
              ),
              column(4,
                selectInput(ns("year_filter"), "Year", choices = c("2023"))
              )
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
          card_header(icon("chart-bar"), "Infrastructure Quality by Country"),
          card_body(
            plotlyOutput(ns("infra_bar_chart"), height = "450px")
          )
        )
      ),
      column(4,
        card(
          card_header(icon("chart-pie"), "Power Source Distribution"),
          card_body(
            plotlyOutput(ns("power_source_pie"), height = "450px")
          )
        )
      )
    ),
    
    # Secondary Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-line"), "Outages vs. Productivity"),
          card_body(
            plotlyOutput(ns("outage_productivity"), height = "350px")
          )
        )
      ),
      column(6,
        card(
          card_header(icon("money-bill-wave"), "Cost of Infrastructure Gaps"),
          card_body(
            plotlyOutput(ns("cost_chart"), height = "350px")
          )
        )
      )
    ),
    
    # Regional Heatmap
    fluidRow(
      column(12,
        card(
          card_header(icon("th"), "Regional Infrastructure Heatmap"),
          card_body(
            plotlyOutput(ns("infra_heatmap"), height = "400px")
          )
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
      regions <- unique(wbes_data()$latest$region)
      shiny::updateSelectInput(session, "region_filter",
        choices = c("All Regions" = "all", setNames(regions, regions)))
      
      years <- wbes_data()$years
      shiny::updateSelectInput(session, "year_filter",
        choices = setNames(years, years), selected = max(years))
    })
    
    # Filtered data
    filtered_data <- reactive({
      req(wbes_data())
      data <- wbes_data()$latest
      if (input$region_filter != "all") {
        data <- filter(data, region == input$region_filter)
      }
      data
    })
    
    # KPIs
    output$kpi_outages <- renderUI({
      req(filtered_data())
      avg <- round(mean(filtered_data()$power_outages_per_month, na.rm = TRUE), 1)
      tags$div(class = "kpi-box",
        tags$div(class = "kpi-value", avg),
        tags$div(class = "kpi-label", "Avg Outages/Month")
      )
    })
    
    output$kpi_duration <- renderUI({
      req(filtered_data())
      avg <- round(mean(filtered_data()$avg_outage_duration_hrs, na.rm = TRUE), 1)
      tags$div(class = "kpi-box kpi-box-coral",
        tags$div(class = "kpi-value", paste0(avg, "h")),
        tags$div(class = "kpi-label", "Avg Duration")
      )
    })
    
    output$kpi_generator <- renderUI({
      req(filtered_data())
      avg <- round(mean(filtered_data()$firms_with_generator_pct, na.rm = TRUE), 1)
      tags$div(class = "kpi-box kpi-box-warning",
        tags$div(class = "kpi-value", paste0(avg, "%")),
        tags$div(class = "kpi-label", "Own Generator")
      )
    })
    
    output$kpi_losses <- renderUI({
      tags$div(class = "kpi-box kpi-box-success",
        tags$div(class = "kpi-value", "4.2%"),
        tags$div(class = "kpi-label", "Sales Lost")
      )
    })
    
    # Bar chart
    output$infra_bar_chart <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      indicator <- input$infra_indicator
      
      data <- arrange(data, desc(.data[[indicator]]))[1:15, ]
      data$country <- factor(data$country, levels = rev(data$country))
      
      plot_ly(data,
              y = ~country,
              x = ~get(indicator),
              type = "bar",
              orientation = "h",
              marker = list(
                color = ~get(indicator),
                colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))
              )) |>
        layout(
          xaxis = list(title = gsub("_", " ", tools::toTitleCase(indicator))),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Power source pie
    output$power_source_pie <- renderPlotly({
      plot_ly(
        labels = c("Grid Only", "Generator Primary", "Mixed Sources", "Solar/Renewable"),
        values = c(35, 28, 32, 5),
        type = "pie",
        marker = list(colors = c("#1B6B5F", "#dc3545", "#F4A460", "#2E7D32")),
        textinfo = "label+percent"
      ) |>
        layout(
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Outages vs productivity scatter
    output$outage_productivity <- renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      
      plot_ly(data,
              x = ~power_outages_per_month,
              y = ~capacity_utilization_pct,
              type = "scatter",
              mode = "markers",
              text = ~country,
              marker = list(
                size = 12,
                color = ~power_outages_per_month,
                colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")),
                opacity = 0.7
              ),
              hovertemplate = "%{text}<br>Outages: %{x:.1f}<br>Capacity: %{y:.1f}%<extra></extra>") |>
        add_trace(
          x = c(0, max(data$power_outages_per_month, na.rm = TRUE)),
          y = predict(lm(capacity_utilization_pct ~ power_outages_per_month, data = data),
                      newdata = data.frame(power_outages_per_month = c(0, max(data$power_outages_per_month, na.rm = TRUE)))),
          type = "scatter",
          mode = "lines",
          line = list(color = "#6C757D", dash = "dash"),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        layout(
          xaxis = list(title = "Power Outages per Month"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Cost chart
    output$cost_chart <- renderPlotly({
      costs <- data.frame(
        category = c("Generator Fuel", "Lost Production", "Equipment Damage", 
                     "Backup Systems", "Water Trucking"),
        pct = c(2.8, 4.2, 1.5, 1.2, 0.8)
      )
      costs <- arrange(costs, pct)
      costs$category <- factor(costs$category, levels = costs$category)
      
      plot_ly(costs,
              y = ~category,
              x = ~pct,
              type = "bar",
              orientation = "h",
              marker = list(color = "#F49B7A")) |>
        layout(
          xaxis = list(title = "% of Annual Sales", ticksuffix = "%"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Heatmap
    output$infra_heatmap <- renderPlotly({
      regions <- c("Sub-Saharan Africa", "South Asia", "East Asia & Pacific",
                   "Latin America", "Europe & Central Asia")
      indicators <- c("Power Outages", "Outage Duration", "Generator Use", 
                      "Water Issues", "Transport")
      
      z <- matrix(
        c(8.5, 5.2, 45, 25, 18,
          6.2, 4.1, 35, 20, 15,
          3.1, 2.5, 20, 12, 10,
          4.2, 3.2, 28, 18, 12,
          2.8, 2.0, 15, 8, 8),
        nrow = 5, byrow = TRUE
      )
      
      plot_ly(
        x = indicators,
        y = regions,
        z = z,
        type = "heatmap",
        colorscale = list(c(0, "#e8f5e9"), c(0.5, "#fff3e0"), c(1, "#ffebee")),
        hovertemplate = "%{y}<br>%{x}: %{z}<extra></extra>"
      ) |>
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
  })
}
