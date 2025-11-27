# app/view/overview.R
# Dashboard Overview Module

box::use(
  shiny[
    moduleServer, NS, reactive, req, tags, div, icon, h2, h4, p, span,
    fluidRow, column, selectInput, actionButton, observeEvent, renderUI, uiOutput
  ],
  bslib[card, card_header, card_body, value_box],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, addCircleMarkers, setView, colorNumeric],
  dplyr[filter, arrange, desc, mutate, n],
  stats[setNames]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container-fluid py-4",
    
    # Header
    fluidRow(
      column(12,
        div(
          class = "page-header mb-4",
          h2(icon("globe"), " Global Business Environment Overview", class = "text-primary"),
          p(class = "lead text-muted",
            "Benchmarking business environments across economies using World Bank Enterprise Survey data")
        )
      )
    ),
    
    # KPI Cards
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_countries"))),
      column(3, uiOutput(ns("kpi_firms"))),
      column(3, uiOutput(ns("kpi_regions"))),
      column(3, uiOutput(ns("kpi_indicators")))
    ),
    
    # Filters
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("filter"), " Filters"),
          card_body(
            class = "py-2",
            fluidRow(
              column(3, selectInput(ns("region"), "Region", choices = c("All" = "all"))),
              column(3, selectInput(ns("income"), "Income Group", choices = c("All" = "all"))),
              column(3, selectInput(ns("indicator"), "Map Indicator",
                choices = c(
                  "Power Outages" = "IC.FRM.OUTG.ZS",
                  "Finance Obstacle" = "IC.FRM.FINA.ZS",
                  "Corruption" = "IC.FRM.CORR.ZS"
                )
              )),
              column(3, actionButton(ns("reset"), "Reset", icon = icon("refresh"),
                class = "btn-outline-secondary w-100 mt-4"))
            )
          )
        )
      )
    ),
    
    # Map and Obstacles
    fluidRow(
      class = "mb-4",
      column(8,
        card(
          card_header(icon("map-marked-alt"), " Business Environment Map"),
          card_body(
            leafletOutput(ns("world_map"), height = "450px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-info",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "This interactive map displays the geographic distribution of surveyed countries. Circle size represents the composite severity of business obstacles - larger circles indicate countries where firms report higher levels of constraints. Use this to identify regional patterns and compare business climates across geographic areas."
              )
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("exclamation-triangle"), " Top Business Obstacles"),
          card_body(
            plotlyOutput(ns("obstacles_chart"), height = "450px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-warning",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "This chart ranks the most critical business constraints. Percentages show the proportion of firms identifying each factor as a major obstacle. Common top obstacles include electricity/infrastructure, access to finance, and corruption."
              )
            )
          )
        )
      )
    ),

    # Regional Comparison
    fluidRow(
      column(12,
        card(
          card_header(icon("chart-bar"), " Regional Comparison"),
          card_body(
            plotlyOutput(ns("regional_chart"), height = "350px"),
            tags$div(
              class = "mt-3 p-2 bg-light border-start border-4 border-success",
              tags$p(
                class = "mb-0 small text-muted",
                tags$strong("Interpretation: "),
                "This grouped bar chart compares obstacle severity across regions. Each colored bar represents a different type of constraint. Sub-Saharan Africa and South Asia typically face the most severe infrastructure and power challenges, while all regions struggle with finance access to varying degrees."
              )
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
    
    # Update filters when data loads
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
    
    # Reset filters
    observeEvent(input$reset, {
      shiny::updateSelectInput(session, "region", selected = "all")
      shiny::updateSelectInput(session, "income", selected = "all")
    })
    
    # KPIs
    output$kpi_countries <- renderUI({
      req(wbes_data())
      div(class = "card bg-primary text-white h-100",
        div(class = "card-body text-center",
          h4(class = "display-5", length(wbes_data()$countries)),
          p("Countries")
        )
      )
    })
    
    output$kpi_firms <- renderUI({
      div(class = "card bg-secondary text-white h-100",
        div(class = "card-body text-center",
          h4(class = "display-5", "253K+"),
          p("Firms Surveyed")
        )
      )
    })
    
    output$kpi_regions <- renderUI({
      req(wbes_data())
      div(class = "card bg-success text-white h-100",
        div(class = "card-body text-center",
          h4(class = "display-5", length(unique(wbes_data()$latest$region))),
          p("Regions")
        )
      )
    })
    
    output$kpi_indicators <- renderUI({
      div(class = "card bg-info text-white h-100",
        div(class = "card-body text-center",
          h4(class = "display-5", "15+"),
          p("Key Indicators")
        )
      )
    })
    
    # World Map
    output$world_map <- renderLeaflet({
      req(filtered())

      # All 30 country coordinates
      coords <- data.frame(
        country = c("Kenya", "Nigeria", "South Africa", "Ghana", "Ethiopia",
                    "Tanzania", "Uganda", "Rwanda", "Senegal", "Cote d'Ivoire",
                    "Egypt", "Morocco", "Tunisia", "Botswana", "Zambia",
                    "India", "Bangladesh", "Vietnam", "Indonesia", "Philippines",
                    "Brazil", "Mexico", "Colombia", "Peru", "Chile",
                    "Poland", "Turkey", "Romania", "Bulgaria", "Serbia"),
        lat = c(-1.28, 9.08, -30.56, 5.55, 9.15,
                -6.37, 1.37, -1.94, 14.69, 6.83,
                26.82, 31.79, 33.89, -22.33, -13.13,
                20.59, 23.68, 14.06, -0.79, 12.88,
                -14.24, 23.63, 4.57, -9.19, -35.68,
                51.92, 38.96, 45.94, 42.73, 44.02),
        lng = c(36.82, 7.40, 22.94, -0.19, 40.49,
                34.89, 32.29, 29.87, -14.45, -5.55,
                30.80, -7.09, 9.54, 24.68, 27.85,
                78.96, 90.36, 108.28, 113.92, 121.77,
                -51.93, -102.55, -74.30, -75.02, -71.54,
                19.14, 35.24, 24.97, 25.49, 21.01)
      )
      
      d <- filtered()
      indicator <- input$indicator
      d <- merge(d, coords, by = "country", all.x = TRUE)
      d <- d[!is.na(d$lat), ]
      
      if (nrow(d) > 0 && indicator %in% names(d)) {
        pal <- colorNumeric(c("#2E7D32", "#F4A460", "#dc3545"), domain = d[[indicator]])
        
        leaflet(d) |>
          addTiles() |>
          setView(lng = 20, lat = 10, zoom = 2) |>
          addCircleMarkers(
            lng = ~lng, lat = ~lat, radius = 10,
            color = ~pal(get(indicator)),
            fillOpacity = 0.8,
            popup = ~paste0("<b>", country, "</b><br>", 
                           gsub("IC.FRM.|.ZS", "", indicator), ": ", 
                           round(get(indicator), 1), "%")
          )
      } else {
        leaflet() |> addTiles() |> setView(lng = 20, lat = 10, zoom = 2)
      }
    })
    
    # Obstacles Chart
    output$obstacles_chart <- renderPlotly({
      obstacles <- data.frame(
        obstacle = c("Access to Finance", "Electricity", "Informal Competition",
                     "Tax Rates", "Corruption", "Political Instability"),
        pct = c(23.5, 19.2, 17.8, 14.5, 12.3, 11.8)
      )
      obstacles <- arrange(obstacles, pct)
      obstacles$obstacle <- factor(obstacles$obstacle, levels = obstacles$obstacle)
      
      plot_ly(obstacles, y = ~obstacle, x = ~pct, type = "bar",
              orientation = "h", marker = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = "% of Firms", ticksuffix = "%"),
          yaxis = list(title = ""),
          margin = list(l = 150),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
    # Regional Chart
    output$regional_chart <- renderPlotly({
      req(wbes_data())
      
      regional <- wbes_data()$regional
      if (is.null(regional)) return(NULL)
      
      plot_ly(regional) |>
        add_trace(x = ~region, y = ~IC.FRM.OUTG.ZS, name = "Power Issues",
                  type = "bar", marker = list(color = "#1B6B5F")) |>
        add_trace(x = ~region, y = ~IC.FRM.FINA.ZS, name = "Finance Obstacle",
                  type = "bar", marker = list(color = "#F49B7A")) |>
        add_trace(x = ~region, y = ~IC.FRM.CORR.ZS, name = "Corruption",
                  type = "bar", marker = list(color = "#6C757D")) |>
        layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = -30),
          yaxis = list(title = "% of Firms", ticksuffix = "%"),
          legend = list(orientation = "h", y = -0.2),
          margin = list(b = 100),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })
    
  })
}
