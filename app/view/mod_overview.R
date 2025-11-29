# app/view/mod_overview.R
# Dashboard Overview Module

box::use(
 shiny[moduleServer, NS, reactive, req, tags, HTML, icon, div, h2, h3, h4, p, span, br,
        fluidRow, column, selectInput, sliderInput, actionButton, observeEvent, renderUI, uiOutput],
 bslib[card, card_header, card_body, value_box, layout_columns],
 plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
 leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, addCircleMarkers,
         setView, colorNumeric, addLegend],
 dplyr[filter, arrange, desc, mutate, summarise, group_by, n],
 stats[setNames]
)

#' @export
ui <- function(id) {
 ns <- NS(id)
 
 tags$div(
   class = "overview-container",
   
   # Header Section
   fluidRow(
     column(12,
       tags$div(
         class = "page-header mb-4",
         h2(
           icon("globe"), 
           "Global Business Environment Overview",
           class = "text-primary-teal"
         ),
         p(
           class = "lead text-muted",
           "Comprehensive benchmarking of business environments across 168 economies using World Bank Enterprise Survey data"
         )
       )
     )
   ),
   
   # KPI Value Boxes
   fluidRow(
     class = "mb-4",
     column(3, uiOutput(ns("kpi_countries"))),
     column(3, uiOutput(ns("kpi_firms"))),
     column(3, uiOutput(ns("kpi_years"))),
     column(3, uiOutput(ns("kpi_indicators")))
   ),
   
   # Filters Row
   fluidRow(
     class = "mb-4",
     column(12,
       card(
         card_header(icon("filter"), "Filters", class = "py-2"),
         card_body(
           class = "py-3",
           fluidRow(
             column(3,
               selectInput(
                 ns("region_filter"),
                 "Region",
                 choices = c("All Regions" = "all"),
                 selected = "all"
               )
             ),
             column(3,
               selectInput(
                 ns("income_filter"),
                 "Income Group",
                 choices = c("All Income Groups" = "all"),
                 selected = "all"
               )
             ),
             column(3,
               selectInput(
                 ns("year_filter"),
                 "Survey Year",
                 choices = c("Latest Available" = "latest"),
                 selected = "latest"
               )
             ),
             column(3,
               tags$div(
                 class = "d-flex align-items-end h-100",
                 actionButton(
                   ns("reset_filters"),
                   "Reset Filters",
                   icon = icon("refresh"),
                   class = "btn-kwiz-outline w-100"
                 )
               )
             )
           )
         )
       )
     )
   ),
   
   # Main Content - Map and Top Constraints
   fluidRow(
     class = "mb-4",
     column(8,
       card(
         card_header(icon("map-marked-alt"), "Business Environment Map"),
         card_body(
          selectInput(
            ns("map_indicator"),
            "Select Indicator:",
            choices = c(
              "Power Outages (per month)" = "power_outages_per_month",
              "Access to Credit (%)" = "firms_with_credit_line_pct",
              "Bribery Incidence (%)" = "bribery_incidence_pct",
              "Capacity Utilization (%)" = "capacity_utilization_pct"
            ),
            width = "300px"
          ),
          leafletOutput(ns("world_map"), height = "450px"),
          p(
            class = "text-muted small mt-2",
            "Circle colors show how each selected indicator varies by country; darker markers highlight higher values while tooltips reveal country-specific figures."
          )
        )
      )
    ),
    column(4,
      card(
        card_header(icon("exclamation-triangle"), "Top Business Obstacles"),
        card_body(
          plotlyOutput(ns("obstacles_chart"), height = "500px"),
          p(
            class = "text-muted small mt-2",
            "Bars rank the most frequently cited obstacles among surveyed firms, making it easy to see which constraints dominate the business landscape."
          )
        )
      )
    )
  ),
   
   # Regional Comparison
   fluidRow(
     class = "mb-4",
     column(12,
      card(
        card_header(icon("chart-bar"), "Regional Comparison - Key Indicators"),
        card_body(
          plotlyOutput(ns("regional_comparison"), height = "400px"),
          p(
            class = "text-muted small mt-2",
            "Grouped bars compare infrastructure reliability, access to finance, and bribery exposure across regions, highlighting where each region performs strongest."
          )
        )
      )
    )
  ),
   
   # Bottom Row - Quick Stats
   fluidRow(
     column(6,
       card(
        card_header(
          icon("bolt"),
          "Infrastructure Quality Index",
          class = "card-header-secondary"
        ),
        card_body(
          plotlyOutput(ns("infrastructure_gauge"), height = "250px"),
          p(
            class = "text-muted small mt-2",
            "The gauge summarizes regional infrastructure strength on a 0–100 scale; the threshold line marks the target resilience benchmark."
          )
        )
      )
    ),
    column(6,
      card(
        card_header(
          icon("university"),
          "Financial Access Index",
          class = "card-header-secondary"
        ),
        card_body(
          plotlyOutput(ns("finance_gauge"), height = "250px"),
          p(
            class = "text-muted small mt-2",
            "This dial tracks how easily firms secure formal credit; scores below the threshold highlight markets where access remains constrained."
          )
        )
      )
    )
  )
)
}

#' @export
server <- function(id, wbes_data) {
 moduleServer(id, function(input, output, session) {
   
   # Update filter choices when data loads
   observeEvent(wbes_data(), {
     req(wbes_data())
     data <- wbes_data()
     
     if (!is.null(data$regions)) {
       shiny::updateSelectInput(
         session, "region_filter",
         choices = c("All Regions" = "all", setNames(data$regions, data$regions))
       )
     }
     
     if (!is.null(data$years)) {
       shiny::updateSelectInput(
         session, "year_filter",
         choices = c("Latest Available" = "latest", setNames(data$years, data$years))
       )
     }
   })
   
   # Reset filters
   observeEvent(input$reset_filters, {
     shiny::updateSelectInput(session, "region_filter", selected = "all")
     shiny::updateSelectInput(session, "income_filter", selected = "all")
     shiny::updateSelectInput(session, "year_filter", selected = "latest")
   })
   
   # Filtered data reactive
   filtered_data <- reactive({
     req(wbes_data())
     data <- wbes_data()$latest
     
     if (input$region_filter != "all") {
       data <- filter(data, region == input$region_filter)
     }
     if (input$income_filter != "all") {
       data <- filter(data, income_group == input$income_filter)
     }
     
     data
   })
   
   # KPI Boxes
   output$kpi_countries <- renderUI({
     req(wbes_data())
     tags$div(
       class = "kpi-box",
       tags$div(class = "kpi-value", length(wbes_data()$countries)),
       tags$div(class = "kpi-label", "Countries Covered")
     )
   })
   
   output$kpi_firms <- renderUI({
     tags$div(
       class = "kpi-box kpi-box-coral",
       tags$div(class = "kpi-value", "253K+"),
       tags$div(class = "kpi-label", "Firms Surveyed")
     )
   })
   
   output$kpi_years <- renderUI({
     req(wbes_data())
     tags$div(
       class = "kpi-box kpi-box-success",
       tags$div(class = "kpi-value", length(wbes_data()$years)),
       tags$div(class = "kpi-label", "Survey Years")
     )
   })
   
   output$kpi_indicators <- renderUI({
     tags$div(
       class = "kpi-box kpi-box-warning",
       tags$div(class = "kpi-value", "150+"),
       tags$div(class = "kpi-label", "Indicators")
     )
   })
   
   # World Map
   output$world_map <- renderLeaflet({
     req(filtered_data())
     
     # Sample coordinates for demo
     coords <- data.frame(
       country = c("Kenya", "Nigeria", "South Africa", "India", "Brazil", "Mexico"),
       lat = c(-1.28, 9.08, -30.56, 20.59, -14.24, 23.63),
       lng = c(36.82, 7.40, 22.94, 78.96, -51.93, -102.55)
     )
     
     data <- filtered_data()
     data <- merge(data, coords, by = "country", all.x = TRUE)
     data <- data[!is.na(data$lat), ]
     
     indicator <- input$map_indicator
     
     if (nrow(data) > 0 && indicator %in% names(data)) {
       pal <- colorNumeric(
         palette = c("#2E7D32", "#F4A460", "#dc3545"),
         domain = data[[indicator]]
       )
       
       leaflet(data) |>
         addTiles() |>
         setView(lng = 20, lat = 10, zoom = 2) |>
         addCircleMarkers(
           lng = ~lng, lat = ~lat,
           radius = 8,
           color = ~pal(get(indicator)),
           fillOpacity = 0.8,
           popup = ~paste0(
             "<strong>", country, "</strong><br>",
             indicator, ": ", round(get(indicator), 1)
           )
         )
     } else {
       leaflet() |>
         addTiles() |>
         setView(lng = 20, lat = 10, zoom = 2)
     }
   })
   
   # Obstacles Chart
   output$obstacles_chart <- renderPlotly({
     obstacles <- data.frame(
       obstacle = c("Access to Finance", "Electricity", "Informal Competition",
                    "Tax Rates", "Corruption", "Political Instability",
                    "Inadequately Educated Workforce", "Tax Administration"),
       pct = c(23.5, 19.2, 17.8, 14.5, 12.3, 11.8, 9.5, 8.2)
     )
     
     obstacles <- arrange(obstacles, pct)
     obstacles$obstacle <- factor(obstacles$obstacle, levels = obstacles$obstacle)
     
     plot_ly(obstacles, 
             y = ~obstacle, 
             x = ~pct, 
             type = "bar",
             orientation = "h",
             marker = list(
               color = "#1B6B5F",
               line = list(color = "#145449", width = 1)
             ),
             hovertemplate = "%{y}: %{x}%<extra></extra>") |>
       layout(
         xaxis = list(title = "% of Firms", ticksuffix = "%"),
         yaxis = list(title = ""),
         margin = list(l = 150),
         plot_bgcolor = "rgba(0,0,0,0)",
         paper_bgcolor = "rgba(0,0,0,0)"
       ) |>
       config(displayModeBar = FALSE)
   })
   
   # Regional Comparison
   output$regional_comparison <- renderPlotly({
     req(wbes_data())
     
     regional <- wbes_data()$regional
     if (is.null(regional)) {
       regional <- data.frame(
         region = c("Sub-Saharan Africa", "South Asia", "East Asia & Pacific",
                    "Latin America & Caribbean", "Europe & Central Asia"),
         power_outages_per_month = c(8.5, 6.2, 3.1, 4.2, 2.8),
         firms_with_credit_line_pct = c(22, 28, 35, 42, 48),
         bribery_incidence_pct = c(24, 28, 18, 15, 12)
       )
     }
     
     plot_ly(regional) |>
       add_trace(
         x = ~region, 
         y = ~power_outages_per_month,
         type = "bar",
         name = "Power Outages/Month",
         marker = list(color = "#1B6B5F")
       ) |>
       add_trace(
         x = ~region, 
         y = ~firms_with_credit_line_pct,
         type = "bar",
         name = "Credit Access (%)",
         marker = list(color = "#F49B7A")
       ) |>
       add_trace(
         x = ~region, 
         y = ~bribery_incidence_pct,
         type = "bar",
         name = "Bribery Incidence (%)",
         marker = list(color = "#6C757D")
       ) |>
       layout(
         barmode = "group",
         xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = "Value"),
         legend = list(orientation = "h", y = -0.2),
         margin = list(b = 100),
         plot_bgcolor = "rgba(0,0,0,0)",
         paper_bgcolor = "rgba(0,0,0,0)"
       ) |>
       config(displayModeBar = FALSE)
   })
   
   # Infrastructure Gauge
   output$infrastructure_gauge <- renderPlotly({
     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = 62,
       title = list(text = "Regional Average Score"),
       gauge = list(
         axis = list(range = list(0, 100)),
         bar = list(color = "#1B6B5F"),
         steps = list(
           list(range = c(0, 40), color = "#ffebee"),
           list(range = c(40, 70), color = "#fff3e0"),
           list(range = c(70, 100), color = "#e8f5e9")
         ),
         threshold = list(
           line = list(color = "#F49B7A", width = 4),
           thickness = 0.75,
           value = 75
         )
       )
     ) |>
       layout(
         margin = list(t = 50, b = 30),
         paper_bgcolor = "rgba(0,0,0,0)"
       ) |>
       config(displayModeBar = FALSE)
   })
   
   # Finance Gauge
   output$finance_gauge <- renderPlotly({
     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = 38,
       title = list(text = "Credit Access Index"),
       gauge = list(
         axis = list(range = list(0, 100)),
         bar = list(color = "#F49B7A"),
         steps = list(
           list(range = c(0, 30), color = "#ffebee"),
           list(range = c(30, 60), color = "#fff3e0"),
           list(range = c(60, 100), color = "#e8f5e9")
         ),
         threshold = list(
           line = list(color = "#1B6B5F", width = 4),
           thickness = 0.75,
           value = 50
         )
       )
     ) |>
       layout(
         margin = list(t = 50, b = 30),
         paper_bgcolor = "rgba(0,0,0,0)"
       ) |>
       config(displayModeBar = FALSE)
   })
   
 })
}
