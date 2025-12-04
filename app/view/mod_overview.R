# app/view/mod_overview.R
# Dashboard Overview Module

box::use(
 shiny[moduleServer, NS, reactive, req, tags, HTML, icon, div, h2, h3, h4, p, span, br,
        fluidRow, column, selectInput, sliderInput, actionButton, observeEvent, renderUI, uiOutput,
        showModal, removeModal, textInput, selectizeInput, modalDialog, modalButton, updateSelectInput],
 bslib[card, card_header, card_body, value_box, layout_columns],
 plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
 leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, addCircleMarkers,
         setView, colorNumeric, addLegend],
 dplyr[filter, arrange, desc, mutate, summarise, group_by, n],
 stats[setNames, na.omit],
 app/logic/custom_regions[get_region_choices, filter_by_region, custom_region_modal_ui,
                           manage_regions_modal_ui, custom_regions_storage]
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

   # Filters Row (Sticky)
   fluidRow(
     class = "sticky-filters",
     column(12,
       card(
         class = "filter-card",
         card_header(icon("filter"), "Filters", class = "py-2"),
         card_body(
           class = "py-3",
           fluidRow(
             column(4,
               tags$div(
                 class = "d-flex gap-2",
                 tags$div(
                   class = "flex-grow-1",
                   selectInput(
                     ns("region_filter"),
                     "Region",
                     choices = c("All Regions" = "all"),
                     selected = "all"
                   )
                 ),
                 tags$div(
                   class = "d-flex align-items-end pb-3 gap-1",
                   actionButton(
                     ns("create_custom_region"),
                     NULL,
                     icon = icon("plus-circle"),
                     class = "btn-sm btn-outline-primary",
                     title = "Create Custom Region",
                     style = "height: 38px; margin-bottom: 0;"
                   ),
                   actionButton(
                     ns("manage_custom_regions"),
                     NULL,
                     icon = icon("cog"),
                     class = "btn-sm btn-outline-secondary",
                     title = "Manage Custom Regions",
                     style = "height: 38px; margin-bottom: 0;"
                   )
                 )
               )
             ),
             column(4,
               selectInput(
                 ns("firm_size_filter"),
                 "Firm Size",
                 choices = c("All Sizes" = "all"),
                 selected = "all"
               )
             ),
             column(4,
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
   ns <- session$ns

   # Custom regions storage
   custom_regions <- shiny::reactiveVal(list())

   # Update filter choices when data loads or custom regions change
   observeEvent(list(wbes_data(), custom_regions()), {
     req(wbes_data())
     data <- wbes_data()

     # Update region filter with custom regions
     region_choices <- get_region_choices(wbes_data, custom_regions())
     updateSelectInput(session, "region_filter", choices = region_choices)

     # Update firm size filter
     if (!is.null(data$latest) && "firm_size" %in% names(data$latest)) {
       firm_sizes <- data$latest$firm_size |>
         unique() |>
         na.omit() |>
         as.character() |>
         sort()
       if (length(firm_sizes) > 0) {
         updateSelectInput(
           session, "firm_size_filter",
           choices = c("All Sizes" = "all", setNames(firm_sizes, firm_sizes))
         )
       }
     }
   }, ignoreNULL = FALSE)

   # Show modal to create custom region
   observeEvent(input$create_custom_region, {
     req(wbes_data())
     countries <- sort(wbes_data()$countries)
     showModal(custom_region_modal_ui(ns, countries))
   })

   # Save custom region
   observeEvent(input$save_custom_region, {
     req(input$custom_region_name, input$custom_region_countries)

     region_name <- trimws(input$custom_region_name)
     if (region_name == "" || length(input$custom_region_countries) == 0) {
       return(NULL)
     }

     new_region <- list(
       name = region_name,
       countries = input$custom_region_countries,
       created = Sys.time()
     )

     current_regions <- custom_regions()
     current_regions[[region_name]] <- new_region
     custom_regions(current_regions)
     custom_regions_storage(current_regions)

     removeModal()
   })

   # Show modal to manage custom regions
   observeEvent(input$manage_custom_regions, {
     showModal(manage_regions_modal_ui(ns, custom_regions()))
   })

   # Delete custom region
   observeEvent(input$delete_region_name, {
     req(input$delete_region_name)
     region_to_delete <- input$delete_region_name

     current_regions <- custom_regions()
     current_regions[[region_to_delete]] <- NULL
     custom_regions(current_regions)
     custom_regions_storage(current_regions)

     # Refresh the modal
     showModal(manage_regions_modal_ui(ns, custom_regions()))
   })

   # Reset filters
   observeEvent(input$reset_filters, {
     updateSelectInput(session, "region_filter", selected = "all")
     updateSelectInput(session, "firm_size_filter", selected = "all")
   })

   # Filtered data reactive
   filtered_data <- reactive({
     req(wbes_data())
     data <- wbes_data()$latest

     # Apply region filter (supports custom regions)
     data <- filter_by_region(data, input$region_filter, custom_regions())

     if (input$firm_size_filter != "all") {
       data <- filter(data, !is.na(firm_size) & firm_size == input$firm_size_filter)
     }

     data
   })

   # KPI Boxes
   output$kpi_countries <- renderUI({
     req(filtered_data())
     # Count distinct countries from filtered data (respects region/income filters)
     n_countries <- length(unique(filtered_data()$country[!is.na(filtered_data()$country)]))
     tags$div(
       class = "kpi-box",
       tags$div(class = "kpi-value", n_countries),
       tags$div(class = "kpi-label", "Countries Covered")
     )
   })

   output$kpi_firms <- renderUI({
     req(filtered_data())
     # Sum actual sample_size to get total firms surveyed
     n_firms <- sum(filtered_data()$sample_size, na.rm = TRUE)
     # Format with K or M suffix
     firms_label <- if (n_firms >= 1000000) {
       paste0(round(n_firms / 1000000, 1), "M")
     } else if (n_firms >= 1000) {
       paste0(round(n_firms / 1000, 1), "K")
     } else {
       format(n_firms, big.mark = ",")
     }
     tags$div(
       class = "kpi-box kpi-box-info",
       tags$div(class = "kpi-value", firms_label),
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

     # Get data with coordinates (already merged in wbes_data)
     data <- filtered_data()

     # Check if lat/lng columns exist
     has_coords <- "lat" %in% names(data) && "lng" %in% names(data)

     if (!has_coords) {
       # No coordinates available - show empty map
       return(
         leaflet() |>
           addTiles() |>
           setView(lng = 20, lat = 10, zoom = 2)
       )
     }

     # Filter to only countries with valid coordinates and indicator data
     indicator <- input$map_indicator

     data <- data[!is.na(data$lat) & !is.na(data$lng), ]

     # Filter to countries with non-NA indicator values
     if (indicator %in% names(data)) {
       data <- data[!is.na(data[[indicator]]), ]
     }

     if (nrow(data) > 0 && indicator %in% names(data)) {
       # Create color palette
       pal <- colorNumeric(
         palette = c("#2E7D32", "#F4A460", "#dc3545"),
         domain = data[[indicator]],
         na.color = "#808080"
       )

       leaflet(data) |>
         addTiles() |>
         setView(lng = 20, lat = 10, zoom = 2) |>
         addCircleMarkers(
           lng = ~lng, lat = ~lat,
           radius = 8,
           color = ~pal(get(indicator)),
           fillOpacity = 0.8,
           stroke = TRUE,
           weight = 1,
           opacity = 0.9,
           popup = ~paste0(
             "<strong>", country, "</strong><br>",
             indicator, ": ", round(get(indicator), 1)
           )
         ) |>
         addLegend(
           "bottomright",
           pal = pal,
           values = ~get(indicator),
           title = "Value",
           opacity = 0.8
         )
     } else {
       leaflet() |>
         addTiles() |>
         setView(lng = 20, lat = 10, zoom = 2)
     }
   })

   # Obstacles Chart
   output$obstacles_chart <- renderPlotly({
     req(filtered_data())
     data <- filtered_data()

     # Calculate average values for major obstacles from actual data
     obstacles <- data.frame(
       obstacle = character(),
       pct = numeric(),
       stringsAsFactors = FALSE
     )

     # Add obstacles if columns exist in data
     if ("IC.FRM.FINA.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Access to Finance",
         pct = mean(data$IC.FRM.FINA.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.ELEC.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Electricity",
         pct = mean(data$IC.FRM.ELEC.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.CORR.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Corruption",
         pct = mean(data$IC.FRM.CORR.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.INFRA.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Infrastructure",
         pct = mean(data$IC.FRM.INFRA.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.CRIM.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Crime",
         pct = mean(data$IC.FRM.CRIM.ZS, na.rm = TRUE)
       ))
     }
     if ("IC.FRM.WKFC.ZS" %in% names(data)) {
       obstacles <- rbind(obstacles, data.frame(
         obstacle = "Workforce Quality",
         pct = mean(data$IC.FRM.WKFC.ZS, na.rm = TRUE)
       ))
     }

     # Remove rows with NA values
     obstacles <- obstacles[!is.na(obstacles$pct), ]

     if (nrow(obstacles) > 0) {
       # Sort and prepare for plotting
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
               hovertemplate = "%{y}: %{x:.1f}%<extra></extra>") |>
         layout(
           xaxis = list(title = "% of Firms", ticksuffix = "%"),
           yaxis = list(title = ""),
           margin = list(l = 150),
           plot_bgcolor = "rgba(0,0,0,0)",
           paper_bgcolor = "rgba(0,0,0,0)"
         ) |>
         config(displayModeBar = FALSE)
     } else {
       # Return empty plot if no data
       plot_ly() |>
         layout(
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           annotations = list(
             text = "No obstacle data available",
             xref = "paper",
             yref = "paper",
             x = 0.5,
             y = 0.5,
             showarrow = FALSE
           )
         )
     }
   })

   # Regional Comparison
   output$regional_comparison <- renderPlotly({
     req(filtered_data())

     # Use filtered data (respects region and firm_size filters)
     data <- filtered_data()

     # Calculate regional aggregates from filtered data
     if (!is.null(data) && "region" %in% names(data)) {
       regional <- data |>
         filter(!is.na(region)) |>
         group_by(region) |>
         summarise(
           power_outages_per_month = mean(power_outages_per_month, na.rm = TRUE),
           firms_with_credit_line_pct = mean(firms_with_credit_line_pct, na.rm = TRUE),
           bribery_incidence_pct = mean(bribery_incidence_pct, na.rm = TRUE),
           .groups = "drop"
         )

       if (nrow(regional) > 0) {
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
       } else {
         # Empty plot if no regional data
         plot_ly() |>
           layout(
             annotations = list(
               text = "No regional data available",
               xref = "paper",
               yref = "paper",
               x = 0.5,
               y = 0.5,
               showarrow = FALSE
             )
           )
       }
     } else {
       # Empty plot if no data
       plot_ly() |>
         layout(
           annotations = list(
             text = "No regional data available",
             xref = "paper",
             yref = "paper",
             x = 0.5,
             y = 0.5,
             showarrow = FALSE
           )
         )
     }
   })

   # Infrastructure Gauge
   output$infrastructure_gauge <- renderPlotly({
     req(filtered_data())
     data <- filtered_data()

     # Calculate infrastructure quality index from actual data
     # Higher power outages = worse infrastructure, so invert the scale
     # Scale: 100 - (average power outages * 10) or use capacity utilization as proxy
     infra_score <- 50  # default

     if ("power_outages_per_month" %in% names(data)) {
       avg_outages <- mean(data$power_outages_per_month, na.rm = TRUE)
       # Convert outages to a 0-100 score (fewer outages = better score)
       # Assume 0 outages = 100, 10+ outages = 0
       infra_score <- max(0, min(100, 100 - (avg_outages * 10)))
     }

     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = round(infra_score, 1),
       title = list(text = "Infrastructure Quality Index"),
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
     req(filtered_data())
     data <- filtered_data()

     # Calculate financial access index from actual data
     finance_score <- 50  # default

     if ("firms_with_credit_line_pct" %in% names(data)) {
       finance_score <- mean(data$firms_with_credit_line_pct, na.rm = TRUE)
     }

     plot_ly(
       type = "indicator",
       mode = "gauge+number",
       value = round(finance_score, 1),
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
