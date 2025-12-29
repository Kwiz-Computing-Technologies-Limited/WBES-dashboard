# app/view/crime.R
# Crime & Security Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, h4, p, strong,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent,
        downloadButton, downloadHandler],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  leaflet[leafletOutput, renderLeaflet],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select, case_when, n],
  stats[setNames, reorder, lm, predict, coef],
  utils[head],
  htmlwidgets[saveWidget],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates],
  app/logic/chart_utils[create_chart_caption, map_with_caption]
)

# Helper function to create chart container with download button and caption
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
    plotlyOutput(ns(output_id), height = height),
    create_chart_caption(output_id)
  )
}

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

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Distribution of Crime & Security"),
          card_body(
            fluidRow(
              column(4,
                selectInput(
                  ns("map_indicator"),
                  "Map Indicator",
                  choices = c(
                    "Crime as Obstacle (%)" = "crime_obstacle_pct",
                    "Security Costs (% Sales)" = "security_costs_pct"
                  )
                )
              )
            ),
            map_with_caption(ns, "crime_map", height = "400px", title = "Crime & Security Indicators by Country")
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
                choices = c("Crime as Obstacle" = "IC.FRM.CRIM.ZS",
                           "Security Costs" = "IC.FRM.SECU.ZS"))),
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
          card_header(icon("chart-bar"), " Security Indicators by Country"),
          card_body(
            chart_with_download(ns, "bar_chart", height = "450px", title = "Security Indicators by Country"),
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
            chart_with_download(ns, "regional_chart", height = "450px", title = "Regional Security Overview"),
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
      column(12,
        card(
          card_header(icon("project-diagram"), " Crime vs. Business Performance"),
          card_body(
            chart_with_download(ns, "crime_performance", height = "350px", title = "Crime vs. Business Performance"),
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
          card_header(icon("layer-group"), " Security by Firm Size"),
          card_body(
            chart_with_download(ns, "firm_size_security", height = "350px", title = "Security by Firm Size"),
            p(
              class = "text-muted small mt-2",
              "Box plots summarize crime and security costs across firm sizes, highlighting risk dispersion."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-area"), " Crime Impact Matrix"),
          card_body(
            chart_with_download(ns, "impact_matrix", height = "350px", title = "Crime Impact Matrix"),
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
            chart_with_download(ns, "crime_corruption", height = "350px", title = "Crime vs. Corruption"),
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
            chart_with_download(ns, "risk_distribution", height = "350px", title = "Risk Distribution"),
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
server <- function(id, wbes_data, global_filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # Filtered data with global filters
    filtered <- reactive({
      req(wbes_data())

      # Use country_panel (has year) if year filter is active, otherwise use latest
      filters <- if (!is.null(global_filters)) global_filters() else NULL
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      d <- if (use_panel) wbes_data()$country_panel else wbes_data()$latest

      # Apply global filters if provided
      if (!is.null(filters)) {
        d <- apply_common_filters(
          d,
          region_value = filters$region,
          sector_value = filters$sector,
          firm_size_value = filters$firm_size,
          income_value = filters$income,
          year_value = filters$year,
          custom_regions = filters$custom_regions,
          custom_sectors = filters$custom_sectors,
          filter_by_region_fn = filter_by_region
        )
      }

      # Add coordinates if using panel data (for maps)
      if (use_panel && !is.null(wbes_data()$country_coordinates)) {
        coords <- wbes_data()$country_coordinates
        if ("lat" %in% names(coords) && "lng" %in% names(coords)) {
          d <- merge(d, coords, by = "country", all.x = TRUE)
        }
      }

      # Apply local module filters if they exist
      if (!is.null(input$region) && input$region != "all" && !is.na(input$region)) {
        d <- d |> filter(!is.na(region) & region == input$region)
      }
      if (!is.null(input$firm_size) && input$firm_size != "all" && !is.na(input$firm_size)) {
        d <- d |> filter(!is.na(firm_size) & firm_size == input$firm_size)
      }
      d
    })

    # Interactive Map
    output$crime_map <- renderLeaflet({
      req(filtered(), wbes_data())
      d <- filtered()
      coords <- get_country_coordinates(wbes_data())

      # Get selected map indicator (use dedicated map_indicator input)
      indicator <- if (!is.null(input$map_indicator)) input$map_indicator else "crime_obstacle_pct"

      # Determine color palette based on indicator
      palette_info <- switch(indicator,
        "crime_obstacle_pct" = list(palette = "Reds", reverse = TRUE, label = "Crime as Obstacle (%)"),
        "security_costs_pct" = list(palette = "Oranges", reverse = TRUE, label = "Security Costs (% Sales)"),
        list(palette = "Reds", reverse = TRUE, label = indicator)
      )

      create_wbes_map(
        data = d,
        coordinates = coords,
        indicator_col = indicator,
        indicator_label = palette_info$label,
        color_palette = palette_info$palette,
        reverse_colors = palette_info$reverse
      )
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
      # Re-arrange in opposite order for correct horizontal bar display
      if (input$sort == "desc") {
        d <- arrange(d, .data[[indicator]])
      } else {
        d <- arrange(d, desc(.data[[indicator]]))
      }
      d$country <- factor(d$country, levels = unique(d$country))

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

      # Check if required columns exist - use friendly names first, then IC.FRM.* as fallback
      crime_col <- if ("crime_obstacle_pct" %in% names(regional)) "crime_obstacle_pct" else "IC.FRM.CRIM.ZS"
      security_col <- if ("security_costs_pct" %in% names(regional)) "security_costs_pct" else "IC.FRM.SECU.ZS"

      if (!crime_col %in% names(regional) || !security_col %in% names(regional)) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = "Missing crime or security data columns",
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
          security_index = (.data[[crime_col]] * 0.7 + .data[[security_col]] * 10 * 0.3)
        ) |>
        filter(!is.na(security_index)) |>
        arrange(desc(security_index))

      if (nrow(regional) == 0) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = "No valid regional data",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

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

    # Crime vs performance
    output$crime_performance <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d) || !"IC.FRM.CAPU.ZS" %in% names(d)) return(NULL)

      valid_data <- d[!is.na(d$IC.FRM.CRIM.ZS) & !is.na(d$IC.FRM.CAPU.ZS), ]

      p <- plot_ly(d, x = ~IC.FRM.CRIM.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              name = "Countries",
              text = ~paste0(country, "<br>Crime: ", round(IC.FRM.CRIM.ZS, 1),
                           "%<br>Capacity: ", round(IC.FRM.CAPU.ZS, 1), "%"),
              hoverinfo = "text",
              marker = list(size = 10,
                           color = ~IC.FRM.CRIM.ZS,
                           colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")),
                           opacity = 0.7))

      # Add trend line
      if (nrow(valid_data) >= 2) {
        tryCatch({
          model <- lm(IC.FRM.CAPU.ZS ~ IC.FRM.CRIM.ZS, data = valid_data)
          x_range <- range(valid_data$IC.FRM.CRIM.ZS, na.rm = TRUE)
          trend_x <- seq(x_range[1], x_range[2], length.out = 50)
          trend_y <- predict(model, newdata = data.frame(IC.FRM.CRIM.ZS = trend_x))
          r_sq <- round(summary(model)$r.squared, 3)

          p <- p |> add_trace(
            x = trend_x, y = trend_y,
            type = "scatter", mode = "lines",
            name = paste0("Trend (R²=", r_sq, ")"),
            line = list(color = "#6C757D", dash = "dash", width = 2),
            inherit = FALSE
          )
        }, error = function(e) {})
      }

      p |> layout(
          xaxis = list(title = "Crime as Obstacle (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
          margin = list(b = 70),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Firm size security
    output$firm_size_security <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.CRIM.ZS" %in% names(d) || !"IC.FRM.SECU.ZS" %in% names(d)) return(NULL)

      plot_ly(d) |>
        add_trace(y = ~IC.FRM.CRIM.ZS, x = ~firm_size, type = "box",
                 name = "Crime Obstacle",
                 marker = list(color = "#dc3545")) |>
        add_trace(y = ~IC.FRM.SECU.ZS * 10, x = ~firm_size, type = "box",
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

      valid_data <- d[!is.na(d$IC.FRM.CRIM.ZS) & !is.na(d$IC.FRM.SECU.ZS), ]
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      p <- plot_ly(d, x = ~IC.FRM.CRIM.ZS, y = ~IC.FRM.SECU.ZS,
              type = "scatter", mode = "markers",
              color = ~region,
              colors = colors,
              text = ~country,
              marker = list(size = 12, opacity = 0.7,
                           line = list(color = "white", width = 1)))

      # Add trend line
      if (nrow(valid_data) >= 2) {
        tryCatch({
          model <- lm(IC.FRM.SECU.ZS ~ IC.FRM.CRIM.ZS, data = valid_data)
          x_range <- range(valid_data$IC.FRM.CRIM.ZS, na.rm = TRUE)
          trend_x <- seq(x_range[1], x_range[2], length.out = 50)
          trend_y <- predict(model, newdata = data.frame(IC.FRM.CRIM.ZS = trend_x))
          r_sq <- round(summary(model)$r.squared, 3)

          p <- p |> add_trace(
            x = trend_x, y = trend_y,
            type = "scatter", mode = "lines",
            name = paste0("Trend (R²=", r_sq, ")"),
            line = list(color = "#6C757D", dash = "dash", width = 2),
            inherit = FALSE
          )
        }, error = function(e) {})
      }

      p |> layout(
          xaxis = list(title = "Crime as Obstacle (%)"),
          yaxis = list(title = "Security Costs (% of Sales)"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
          margin = list(b = 70),
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

      valid_data <- d[!is.na(d$IC.FRM.CRIM.ZS) & !is.na(d$IC.FRM.CORR.ZS), ]
      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      p <- plot_ly(d, x = ~IC.FRM.CORR.ZS, y = ~IC.FRM.CRIM.ZS,
              type = "scatter", mode = "markers",
              color = ~region,
              colors = colors,
              text = ~country,
              name = "Countries",
              marker = list(size = 10, opacity = 0.7))

      # Add trend line
      if (nrow(valid_data) >= 2) {
        tryCatch({
          model <- lm(IC.FRM.CRIM.ZS ~ IC.FRM.CORR.ZS, data = valid_data)
          x_range <- range(valid_data$IC.FRM.CORR.ZS, na.rm = TRUE)
          trend_x <- seq(x_range[1], x_range[2], length.out = 50)
          trend_y <- predict(model, newdata = data.frame(IC.FRM.CORR.ZS = trend_x))
          r_sq <- round(summary(model)$r.squared, 3)

          p <- p |> add_trace(
            x = trend_x, y = trend_y,
            type = "scatter", mode = "lines",
            name = paste0("Trend (R²=", r_sq, ")"),
            line = list(color = "#6C757D", dash = "dash", width = 2),
            inherit = FALSE
          )
        }, error = function(e) {})
      }

      p |> layout(
          xaxis = list(title = "Corruption Obstacle (%)"),
          yaxis = list(title = "Crime Obstacle (%)"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
          margin = list(b = 70),
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

    # Download handlers
    output$dl_bar_chart <- downloadHandler(
      filename = function() paste0("crime_by_country_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$bar_chart(), file, selfcontained = TRUE) }
    )
    output$dl_regional_chart <- downloadHandler(
      filename = function() paste0("crime_regional_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$regional_chart(), file, selfcontained = TRUE) }
    )
    output$dl_crime_performance <- downloadHandler(
      filename = function() paste0("crime_vs_performance_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$crime_performance(), file, selfcontained = TRUE) }
    )
    output$dl_firm_size_security <- downloadHandler(
      filename = function() paste0("security_by_firm_size_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$firm_size_security(), file, selfcontained = TRUE) }
    )
    output$dl_impact_matrix <- downloadHandler(
      filename = function() paste0("crime_impact_matrix_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$impact_matrix(), file, selfcontained = TRUE) }
    )
    output$dl_crime_corruption <- downloadHandler(
      filename = function() paste0("crime_vs_corruption_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$crime_corruption(), file, selfcontained = TRUE) }
    )
    output$dl_risk_distribution <- downloadHandler(
      filename = function() paste0("risk_distribution_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$risk_distribution(), file, selfcontained = TRUE) }
    )

  })
}
