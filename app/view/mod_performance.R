# app/view/performance.R
# Business Performance & Trade Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, h4, p, strong,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent,
        downloadButton, downloadHandler],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  leaflet[leafletOutput, renderLeaflet],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select, case_when, n],
  stats[setNames, reorder],
  utils[head],
  htmlwidgets[saveWidget],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates]
)

# Helper function to create chart container with download button
chart_with_download <- function(ns, output_id, height = "400px", title = NULL) {
  div(
    class = "position-relative",
    if (!is.null(title)) h4(title, class = "text-primary-teal mb-2"),
    div(
      class = "position-absolute",
      style = "top: 5px; right: 10px; z-index: 100;",
      downloadButton(
        ns(paste0("dl_", output_id)),
        label = "",
        icon = icon("download"),
        class = "btn-sm btn-outline-secondary",
        title = "Download chart"
      )
    ),
    plotlyOutput(ns(output_id), height = height)
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(column(12, h2(icon("chart-line"), " Business Performance & Trade", class = "text-primary mb-4"))),

    # KPIs
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_capacity"))),
      column(3, uiOutput(ns("kpi_exports"))),
      column(3, uiOutput(ns("kpi_performance"))),
      column(3, uiOutput(ns("kpi_competitiveness")))
    ),

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Distribution of Business Performance"),
          card_body(
            leafletOutput(ns("performance_map"), height = "400px"),
            p(
              class = "text-muted small mt-2",
              "Interactive map showing capacity utilization by country. Darker colors indicate higher utilization."
            )
          )
        )
      )
    ),

    # Tab-specific filters (Region and Firm Size are in sidebar)
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(class = "py-2",
            fluidRow(
              column(6, selectInput(ns("indicator"), "Indicator",
                choices = c("Capacity Utilization" = "IC.FRM.CAPU.ZS",
                           "Export Participation" = "IC.FRM.EXPRT.ZS"))),
              column(6, selectInput(ns("sort"), "Sort By",
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
          card_header(icon("chart-bar"), " Performance Indicators by Country"),
          card_body(
            chart_with_download(ns, "bar_chart", height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Bars compare capacity utilization or export participation across countries to spotlight standout performers."
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("globe-africa"), " Regional Performance"),
          card_body(
            chart_with_download(ns, "regional_chart", height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Regional averages reveal how performance differs across geographies, contextualizing country scores."
            )
          )
        )
      )
    ),

    # Performance Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("shipping-fast"), " Export Intensity Matrix"),
          card_body(
            chart_with_download(ns, "export_matrix", height = "350px"),
            p(
              class = "text-muted small mt-2",
              "The matrix contrasts export participation with firm characteristics, helping identify segments driving trade."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("industry"), " Capacity vs. Obstacles"),
          card_body(
            chart_with_download(ns, "capacity_obstacles", height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Scatter points show how operational capacity aligns with reported obstacles, highlighting binding constraints."
            )
          )
        )
      )
    ),

    # Competitiveness Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("trophy"), " Competitiveness Index"),
          card_body(
            chart_with_download(ns, "competitiveness_chart", height = "350px"),
            p(
              class = "text-muted small mt-2",
              "This index blends performance indicators to benchmark overall competitiveness across markets."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-area"), " Performance by Firm Size"),
          card_body(
            chart_with_download(ns, "firm_size_performance", height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Box plots summarize performance dispersion by firm size to reveal variability."
            )
          )
        )
      )
    ),

    # Trade Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("balance-scale"), " Export vs. Infrastructure"),
          card_body(
            chart_with_download(ns, "export_infra", height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Scatter compares export participation with infrastructure reliability to show how logistics affect trade."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-pie"), " Performance Segmentation"),
          card_body(
            chart_with_download(ns, "performance_segments", height = "350px"),
            p(
              class = "text-muted small mt-2",
              "The pie groups firms into performance tiers, clarifying how many operate at high, medium, or low capacity."
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
server <- function(id, wbes_data, global_filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # Update filters
    observeEvent(wbes_data(), {
      req(wbes_data())
      d <- wbes_data()$latest
      # Filter out NA values from region and firm_size
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
      if (!is.null(input$region) && input$region != "all" && !is.na(input$region)) {
        d <- d |> filter(!is.na(region) & region == input$region)
      }
      if (!is.null(input$firm_size) && input$firm_size != "all" && !is.na(input$firm_size)) {
        d <- d |> filter(!is.na(firm_size) & firm_size == input$firm_size)
      }
      d
    })

    # Interactive Map
    output$performance_map <- renderLeaflet({
      req(filtered(), wbes_data())
      d <- filtered()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$indicator)) input$indicator else "IC.FRM.CAPU.ZS"

      create_wbes_map(
        data = d,
        coordinates = coords,
        indicator_col = indicator,
        indicator_label = switch(indicator,
          "IC.FRM.CAPU.ZS" = "Capacity Utilization (%)",
          "IC.FRM.EXPRT.ZS" = "Export Participation (%)",
          indicator
        ),
        color_palette = "Greens",
        reverse_colors = FALSE  # Higher is better
      )
    })

    # KPIs
    output$kpi_capacity <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.CAPU.ZS, na.rm = TRUE), 1)
      div(class = "card bg-success text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Avg Capacity Utilization")))
    })

    output$kpi_exports <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.EXPRT.ZS, na.rm = TRUE), 1)
      div(class = "card bg-info text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Firms Exporting")))
    })

    output$kpi_performance <- renderUI({
      req(filtered())
      d <- filtered()
      # Performance index: combination of capacity and exports
      perf_index <- round(mean((d$IC.FRM.CAPU.ZS + d$IC.FRM.EXPRT.ZS) / 2, na.rm = TRUE), 1)
      div(class = "card bg-primary text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(perf_index, "%")),
          p("Performance Index")))
    })

    output$kpi_competitiveness <- renderUI({
      req(filtered())
      d <- filtered()
      # Simple competitiveness score
      high_performers <- sum(d$IC.FRM.CAPU.ZS > 70 & d$IC.FRM.EXPRT.ZS > 20, na.rm = TRUE)
      total <- nrow(d)
      pct <- round(high_performers / total * 100, 1)
      div(class = "card bg-warning text-dark h-100",
        div(class = "card-body text-center",
          h2(paste0(pct, "%")),
          p("Competitive Countries")))
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
                           colorscale = list(c(0, "#dc3545"), c(0.5, "#F4A460"), c(1, "#2E7D32"))),
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

    # Regional chart
    output$regional_chart <- renderPlotly({
      req(wbes_data())
      regional <- wbes_data()$regional

      # Check if data exists
      if (is.null(regional) || nrow(regional) == 0) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = "No regional data available",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      # Check if required columns exist
      required_cols <- c("IC.FRM.CAPU.ZS", "IC.FRM.EXPRT.ZS")
      missing_cols <- required_cols[!required_cols %in% names(regional)]

      if (length(missing_cols) > 0) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = paste0("Missing data: ", paste(missing_cols, collapse = ", ")),
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      regional <- regional |>
        mutate(
          perf_index = (IC.FRM.CAPU.ZS + IC.FRM.EXPRT.ZS) / 2
        ) |>
        arrange(desc(perf_index))

      plot_ly(regional, x = ~perf_index, y = ~reorder(region, perf_index),
              type = "bar", orientation = "h",
              marker = list(color = "#2E7D32"),
              text = ~paste0(region, ": ", round(perf_index, 1), "%"),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = "Performance Index"),
          yaxis = list(title = ""),
          margin = list(l = 150),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Export matrix
    output$export_matrix <- renderPlotly({
      req(filtered())
      d <- filtered()

      d <- d |>
        mutate(
          export_category = case_when(
            IC.FRM.EXPRT.ZS < 10 ~ "Low",
            IC.FRM.EXPRT.ZS < 25 ~ "Medium",
            TRUE ~ "High"
          )
        )

      plot_ly(d, x = ~IC.FRM.CAPU.ZS, y = ~IC.FRM.EXPRT.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 12,
                           color = ~firm_size,
                           opacity = 0.7,
                           line = list(color = "white", width = 1))) |>
        layout(
          xaxis = list(title = "Capacity Utilization (%)"),
          yaxis = list(title = "Export Participation (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Capacity vs obstacles
    output$capacity_obstacles <- renderPlotly({
      req(filtered())
      d <- filtered()

      d <- d |>
        mutate(
          total_obstacles = (IC.FRM.INFRA.ZS + IC.FRM.FINA.ZS + IC.FRM.CORR.ZS) / 3
        )

      plot_ly(d, x = ~total_obstacles, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~paste0(country, "<br>Capacity: ", round(IC.FRM.CAPU.ZS, 1),
                           "%<br>Obstacles: ", round(total_obstacles, 1), "%"),
              hoverinfo = "text",
              marker = list(size = 10,
                           color = ~IC.FRM.CAPU.ZS,
                           colorscale = list(c(0, "#dc3545"), c(0.5, "#F4A460"), c(1, "#2E7D32")),
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Business Obstacles Index (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Competitiveness chart
    output$competitiveness_chart <- renderPlotly({
      req(filtered())
      d <- filtered()

      d <- d |>
        mutate(
          competitiveness = (IC.FRM.CAPU.ZS * 0.6 + IC.FRM.EXPRT.ZS * 0.4)
        ) |>
        arrange(desc(competitiveness)) |>
        head(15)

      d$country <- factor(d$country, levels = rev(d$country))

      plot_ly(d, y = ~country, x = ~competitiveness, type = "bar",
              orientation = "h",
              marker = list(color = "#1B6B5F"),
              text = ~paste0(country, ": ", round(competitiveness, 1)),
              hoverinfo = "text") |>
        layout(
          xaxis = list(title = "Competitiveness Score"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Firm size performance
    output$firm_size_performance <- renderPlotly({
      req(filtered())
      d <- filtered()

      plot_ly(d) |>
        add_trace(y = ~IC.FRM.CAPU.ZS, x = ~firm_size, type = "box",
                 name = "Capacity",
                 marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~IC.FRM.EXPRT.ZS, x = ~firm_size, type = "box",
                 name = "Exports",
                 marker = list(color = "#F49B7A")) |>
        layout(
          boxmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Percentage (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Export vs infrastructure
    output$export_infra <- renderPlotly({
      req(filtered())
      d <- filtered()

      # Check if data exists
      if (is.null(d) || nrow(d) == 0) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = "No data available",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      # Check if required columns exist
      required_cols <- c("IC.FRM.INFRA.ZS", "IC.FRM.EXPRT.ZS")
      missing_cols <- required_cols[!required_cols %in% names(d)]

      if (length(missing_cols) > 0) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = paste0("Missing data: ", paste(missing_cols, collapse = ", ")),
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      plot_ly(d, x = ~IC.FRM.INFRA.ZS, y = ~IC.FRM.EXPRT.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 10,
                           color = ~region,
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Infrastructure Obstacle (%)"),
          yaxis = list(title = "Export Participation (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Performance segments
    output$performance_segments <- renderPlotly({
      req(filtered())
      d <- filtered()

      d <- d |>
        mutate(
          segment = case_when(
            IC.FRM.CAPU.ZS > 70 & IC.FRM.EXPRT.ZS > 20 ~ "High Performers",
            IC.FRM.CAPU.ZS > 70 ~ "Domestic Focus",
            IC.FRM.EXPRT.ZS > 20 ~ "Export Oriented",
            TRUE ~ "Developing"
          )
        ) |>
        group_by(segment) |>
        summarise(count = n())

      plot_ly(d, labels = ~segment, values = ~count,
              type = "pie",
              marker = list(colors = c("#2E7D32", "#1B6B5F", "#F49B7A", "#F4A460"))) |>
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

      avg_capacity <- round(mean(d$IC.FRM.CAPU.ZS, na.rm = TRUE), 1)
      avg_exports <- round(mean(d$IC.FRM.EXPRT.ZS, na.rm = TRUE), 1)

      best_performer <- d |>
        mutate(perf = (IC.FRM.CAPU.ZS * 0.6 + IC.FRM.EXPRT.ZS * 0.4)) |>
        arrange(desc(perf)) |>
        head(1)

      top_exporter <- d |> arrange(desc(IC.FRM.EXPRT.ZS)) |> head(1)

      div(
        tags$ul(
          tags$li(tags$strong("Average Capacity Utilization: "),
                 paste0(avg_capacity, "% - firms operating at ", avg_capacity, "% of their capacity")),
          tags$li(tags$strong("Export Participation: "),
                 paste0(avg_exports, "% of firms engage in international trade")),
          tags$li(tags$strong("Top Performer: "),
                 paste0(best_performer$country, " with strong capacity utilization and export activity")),
          tags$li(tags$strong("Export Leader: "),
                 paste0(top_exporter$country, " (", round(top_exporter$IC.FRM.EXPRT.ZS, 1), "% firms exporting)")),
          tags$li(tags$strong("Key Finding: "),
                 "Countries with better infrastructure and lower business obstacles show higher capacity utilization and export participation")
        )
      )
    })

    # Download handlers
    output$dl_bar_chart <- downloadHandler(
      filename = function() paste0("performance_by_country_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$bar_chart(), file, selfcontained = TRUE) }
    )
    output$dl_regional_chart <- downloadHandler(
      filename = function() paste0("performance_regional_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$regional_chart(), file, selfcontained = TRUE) }
    )
    output$dl_export_matrix <- downloadHandler(
      filename = function() paste0("export_matrix_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$export_matrix(), file, selfcontained = TRUE) }
    )
    output$dl_capacity_obstacles <- downloadHandler(
      filename = function() paste0("capacity_vs_obstacles_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$capacity_obstacles(), file, selfcontained = TRUE) }
    )
    output$dl_competitiveness_chart <- downloadHandler(
      filename = function() paste0("competitiveness_index_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$competitiveness_chart(), file, selfcontained = TRUE) }
    )
    output$dl_firm_size_performance <- downloadHandler(
      filename = function() paste0("performance_by_firm_size_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$firm_size_performance(), file, selfcontained = TRUE) }
    )
    output$dl_export_infra <- downloadHandler(
      filename = function() paste0("export_vs_infrastructure_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$export_infra(), file, selfcontained = TRUE) }
    )
    output$dl_performance_segments <- downloadHandler(
      filename = function() paste0("performance_segments_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$performance_segments(), file, selfcontained = TRUE) }
    )

  })
}
