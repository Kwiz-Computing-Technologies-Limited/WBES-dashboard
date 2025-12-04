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
  dplyr[filter, arrange, desc, mutate, n]
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
              column(3, selectInput(ns("firm_size"), "Firm Size", choices = c("All" = "all"))),
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
          card_body(leafletOutput(ns("world_map"), height = "450px"))
        )
      ),
      column(4,
        card(
          card_header(icon("exclamation-triangle"), " Top Business Obstacles"),
          card_body(plotlyOutput(ns("obstacles_chart"), height = "450px"))
        )
      )
    ),

    # Regional Comparison
    fluidRow(
      column(12,
        card(
          card_header(icon("chart-bar"), " Regional Comparison"),
          card_body(plotlyOutput(ns("regional_chart"), height = "350px"))
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
      # Filter out NA values from region and firm_size before creating dropdown choices
      regions_vec <- unique(d$region) |> stats::na.omit() |> as.character() |> sort()
      firm_sizes_vec <- unique(d$firm_size) |> stats::na.omit() |> as.character() |> sort()
      regions <- c("All" = "all", setNames(regions_vec, regions_vec))
      firm_sizes <- c("All" = "all", setNames(firm_sizes_vec, firm_sizes_vec))

      shiny::updateSelectInput(session, "region", choices = regions)
      shiny::updateSelectInput(session, "firm_size", choices = firm_sizes)
    })

    # Filtered data
    filtered <- reactive({
      req(wbes_data())
      d <- wbes_data()$latest
      if (input$region != "all" && !is.na(input$region)) {
        d <- d |> filter(!is.na(region) & region == input$region)
      }
      if (input$firm_size != "all" && !is.na(input$firm_size)) {
        d <- d |> filter(!is.na(firm_size) & firm_size == input$firm_size)
      }
      d
    })

    # Reset filters
    observeEvent(input$reset, {
      shiny::updateSelectInput(session, "region", selected = "all")
      shiny::updateSelectInput(session, "firm_size", selected = "all")
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

      # Country coordinates (sample)
      coords <- data.frame(
        country = c("Kenya", "Nigeria", "South Africa", "Ghana", "Ethiopia",
                    "India", "Bangladesh", "Brazil", "Mexico", "Poland"),
        lat = c(-1.28, 9.08, -30.56, 5.55, 9.15,
                20.59, 23.68, -14.24, 23.63, 51.92),
        lng = c(36.82, 7.40, 22.94, -0.19, 40.49,
                78.96, 90.36, -51.93, -102.55, 19.14)
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
