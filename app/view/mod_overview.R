# app/view/mod_overview.R
# Dashboard Overview Module

box::use(
 shiny[moduleServer, NS, reactive, req, tags, HTML, icon, div, h2, h3, h4, p, span, br,
        fluidRow, column, selectInput, sliderInput, actionButton, observeEvent, renderUI, uiOutput],
 bslib[card, card_header, card_body, value_box, layout_columns],
 plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
 leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, addCircleMarkers,
         setView, colorNumeric, addLegend],
 dplyr[filter, arrange, desc, mutate, summarise, group_by, n, select, across, left_join, distinct, coalesce],
 tidyr[pivot_longer],
 countrycode[countrycode, codelist],
 stats[setNames, na.omit]
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
              "Power Outages Obstacle (%)" = "IC.FRM.OUTG.ZS",
              "Access to Finance Obstacle (%)" = "IC.FRM.FINA.ZS",
              "Bribery Incidence (%)" = "IC.FRM.BRIB.ZS",
              "Capacity Utilization (%)" = "IC.FRM.CAPU.ZS"
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
   
   safe_max_year <- function(x) {
     if (all(is.na(x))) {
       return(NA_integer_)
     }
     suppressWarnings(max(x, na.rm = TRUE))
   }

   indicator_cols <- reactive({
     req(wbes_data())
     cols <- wbes_data()$indicator_columns
     if (is.null(cols)) {
       cols <- names(wbes_data()$latest)[vapply(wbes_data()$latest, is.numeric, logical(1))]
     }
     cols
   })

   country_panel_filtered <- reactive({
     req(wbes_data())
     panel <- wbes_data()$country_panel
     if (is.null(panel)) {
       panel <- wbes_data()$latest
     }

     if (!is.null(panel$year)) {
       if (input$year_filter == "latest") {
         panel <- panel |>
           group_by(country) |>
           filter(year == safe_max_year(year) | all(is.na(year))) |>
           ungroup()
       } else {
         panel <- filter(panel, year == as.integer(input$year_filter))
       }
     }

     if (input$region_filter != "all") {
       panel <- filter(panel, region == input$region_filter)
     }
     if (input$income_filter != "all") {
       panel <- filter(panel, income_group == input$income_filter)
     }

     panel
   })

   filtered_data <- reactive({
     data <- country_panel_filtered()
     cols <- intersect(indicator_cols(), names(data))

     if (!"year" %in% names(data)) {
       data$year <- NA_integer_
     }

     data |>
       group_by(country, country_code, region, income_group) |>
       summarise(
         across(all_of(cols), ~mean(.x, na.rm = TRUE)),
         year = safe_max_year(year),
         .groups = "drop"
       )
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

     data <- filtered_data()
     indicator <- input$map_indicator

     coords <- codelist |>
       transmute(
         iso3c,
         lat = as.numeric(capitalLatitude),
         lng = as.numeric(capitalLongitude)
       ) |>
       distinct()

     data <- data |>
       mutate(
         iso3c = dplyr::coalesce(
           countrycode(country, origin = "country.name", destination = "iso3c", warn = FALSE),
           countrycode(country_code, origin = "iso3c", destination = "iso3c", warn = FALSE)
         )
       ) |>
       left_join(coords, by = "iso3c") |>
       filter(!is.na(lat) & !is.na(lng))

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
     panel <- country_panel_filtered()
     obstacles <- c("IC.FRM.FINA.ZS", "IC.FRM.ELEC.ZS", "IC.FRM.CORR.ZS",
                    "IC.FRM.WKFC.ZS", "IC.FRM.CRIM.ZS")
     available <- intersect(obstacles, names(panel))

     if (length(available) == 0) {
       return(NULL)
     }

     obstacle_summary <- panel |>
       summarise(across(all_of(available), ~mean(as.numeric(.x), na.rm = TRUE))) |>
       pivot_longer(everything(), names_to = "obstacle", values_to = "pct") |>
       mutate(obstacle = gsub("IC.FRM.|.ZS", "", obstacle)) |>
       arrange(pct)

     obstacle_summary$obstacle <- factor(obstacle_summary$obstacle, levels = obstacle_summary$obstacle)

     plot_ly(obstacle_summary,
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

     panel <- country_panel_filtered()
     key_indicators <- c("IC.FRM.OUTG.ZS", "IC.FRM.FINA.ZS", "IC.FRM.CORR.ZS")
     available <- intersect(key_indicators, names(panel))

     if (length(available) == 0) {
       return(NULL)
     }

     regional <- panel |>
       filter(!is.na(region)) |>
       group_by(region) |>
       summarise(across(all_of(available), ~mean(as.numeric(.x), na.rm = TRUE)), .groups = "drop")

     missing_cols <- setdiff(key_indicators, names(regional))
     if (length(missing_cols) > 0) {
       for (col in missing_cols) {
         regional[[col]] <- NA_real_
       }
     }

     regional <- select(regional, region, all_of(key_indicators))

     plot_ly(regional) |>
       add_trace(
         x = ~region,
         y = ~`IC.FRM.OUTG.ZS`,
         type = "bar",
         name = "Power Outages/Month",
         marker = list(color = "#1B6B5F")
       ) |>
       add_trace(
         x = ~region,
         y = ~`IC.FRM.FINA.ZS`,
         type = "bar",
         name = "Credit Access (%)",
         marker = list(color = "#F49B7A")
       ) |>
       add_trace(
         x = ~region,
         y = ~`IC.FRM.CORR.ZS`,
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
     panel <- country_panel_filtered()
     value <- if ("IC.FRM.INFRA.ZS" %in% names(panel)) {
       round(mean(panel$IC.FRM.INFRA.ZS, na.rm = TRUE), 1)
     } else {
       NA_real_
     }

     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = value,
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
     panel <- country_panel_filtered()
     value <- if ("IC.FRM.FINA.ZS" %in% names(panel)) {
       round(mean(panel$IC.FRM.FINA.ZS, na.rm = TRUE), 1)
     } else {
       NA_real_
     }

     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = value,
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
