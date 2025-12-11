# app/view/workforce.R
# Workforce & Gender Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, p, strong,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  leaflet[leafletOutput, renderLeaflet],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select],
  stats[setNames],
  utils[head],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(column(12, h2(icon("users"), " Workforce & Gender Inclusion", class = "text-primary mb-4"))),

    # KPIs
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_workforce"))),
      column(3, uiOutput(ns("kpi_female_workers"))),
      column(3, uiOutput(ns("kpi_female_ownership"))),
      column(3, uiOutput(ns("kpi_gender_gap")))
    ),

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Distribution of Female Ownership"),
          card_body(
            leafletOutput(ns("workforce_map"), height = "400px"),
            p(
              class = "text-muted small mt-2",
              "Interactive map showing female ownership rates by country. Darker colors indicate higher female ownership."
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
              column(4, selectInput(ns("indicator"), "Indicator",
                choices = c("Workforce Obstacle" = "IC.FRM.WKFC.ZS",
                           "Female Workers" = "IC.FRM.FEMW.ZS",
                           "Female Ownership" = "IC.FRM.FEMO.ZS"))),
              column(4, selectInput(ns("sort"), "Sort By",
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
          card_header(icon("chart-bar"), " Workforce Indicators by Country"),
          card_body(
            plotlyOutput(ns("bar_chart"), height = "450px"),
            p(
              class = "text-muted small mt-2",
              "Bars compare workforce-related indicators across countries, surfacing where labor challenges or gender gaps are most severe."
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("venus-mars"), " Gender Balance Overview"),
          card_body(
            plotlyOutput(ns("gender_overview"), height = "450px"),
            p(
              class = "text-muted small mt-2",
              "The composition chart shows the share of female workers and leaders, highlighting representation gaps."
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
          card_header(icon("chart-line"), " Female Participation Trends"),
          card_body(
            plotlyOutput(ns("participation_chart"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Lines trace how female employment indicators evolve across income groups, flagging progress or setbacks."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("project-diagram"), " Workforce vs. Productivity"),
          card_body(
            plotlyOutput(ns("scatter_productivity"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Each point connects workforce constraints with capacity utilization, highlighting whether labor shortages coincide with lower productivity."
            )
          )
        )
      )
    ),

    # Gender Analysis
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("layer-group"), " Female Ownership by Region"),
          card_body(
            plotlyOutput(ns("regional_gender"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Bars summarize female ownership prevalence by region, revealing where women-led firms are most common."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("chart-area"), " Gender Gap Analysis"),
          card_body(
            plotlyOutput(ns("gender_gap_chart"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "This chart contrasts employment and ownership gaps, showing where representation diverges the most."
            )
          )
        )
      )
    ),

    # Firm Size Comparison
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("coins"), " Workforce Challenge by Firm Size"),
          card_body(
            plotlyOutput(ns("firm_size_comparison"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Firm size box plots show how workforce obstacles differ by company size."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("graduation-cap"), " Skills & Gender Correlation"),
          card_body(
            plotlyOutput(ns("skills_correlation"), height = "350px"),
            p(
              class = "text-muted small mt-2",
              "Scatter points compare skilled labor availability with female employment shares to reveal inclusion linkages."
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
    output$workforce_map <- renderLeaflet({
      req(filtered(), wbes_data())
      d <- filtered()
      coords <- get_country_coordinates(wbes_data())

      indicator <- if (!is.null(input$indicator)) input$indicator else "IC.FRM.FEMO.ZS"

      create_wbes_map(
        data = d,
        coordinates = coords,
        indicator_col = indicator,
        indicator_label = switch(indicator,
          "IC.FRM.WKFC.ZS" = "Workforce Obstacle (%)",
          "IC.FRM.FEMW.ZS" = "Female Workers (%)",
          "IC.FRM.FEMO.ZS" = "Female Ownership (%)",
          indicator
        ),
        color_palette = if (indicator == "IC.FRM.WKFC.ZS") "YlOrRd" else "Purples",
        reverse_colors = indicator == "IC.FRM.WKFC.ZS"
      )
    })

    # KPIs
    output$kpi_workforce <- renderUI({
      req(filtered())
      d <- filtered()
      if (is.null(d) || !"IC.FRM.WKFC.ZS" %in% names(d)) return(NULL)
      val <- round(mean(d$IC.FRM.WKFC.ZS, na.rm = TRUE), 1)
      div(class = "card bg-primary text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Workforce as Obstacle")))
    })

    output$kpi_female_workers <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.FEMW.ZS, na.rm = TRUE), 1)
      div(class = "card bg-info text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Female Workers (Avg)")))
    })

    output$kpi_female_ownership <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)
      div(class = "card bg-secondary text-white h-100",
        div(class = "card-body text-center",
          h2(paste0(val, "%")),
          p("Female Ownership")))
    })

    output$kpi_gender_gap <- renderUI({
      req(filtered())
      d <- filtered()
      # Gender gap: difference between worker participation and ownership
      gap <- round(mean(d$IC.FRM.FEMW.ZS - d$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)
      color <- if(gap > 15) "warning" else "success"
      div(class = paste0("card bg-", color, " text-white h-100"),
        div(class = "card-body text-center",
          h2(paste0(gap, "%")),
          p("Gender Leadership Gap")))
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
                           colorscale = list(c(0, "#1B6B5F"), c(0.5, "#17a2b8"), c(1, "#F49B7A"))),
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

    # Gender overview
    output$gender_overview <- renderPlotly({
      req(filtered())
      d <- filtered()

      avg_workers <- round(mean(d$IC.FRM.FEMW.ZS, na.rm = TRUE), 1)
      avg_ownership <- round(mean(d$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)

      data <- data.frame(
        category = c("Female Workers", "Female Ownership"),
        value = c(avg_workers, avg_ownership),
        color = c("#1B6B5F", "#F49B7A")
      )

      plot_ly(data, x = ~category, y = ~value, type = "bar",
              marker = list(color = ~color),
              text = ~paste0(value, "%"),
              textposition = "outside") |>
        layout(
          yaxis = list(title = "Percentage (%)"),
          xaxis = list(title = ""),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Participation chart
    output$participation_chart <- renderPlotly({
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
      required_cols <- c("IC.FRM.FEMW.ZS", "IC.FRM.FEMO.ZS")
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

      plot_ly(regional, x = ~IC.FRM.FEMW.ZS, y = ~IC.FRM.FEMO.ZS,
              type = "scatter", mode = "markers+text",
              text = ~region,
              textposition = "top center",
              marker = list(size = 15, color = "#1B6B5F", opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Female Workers (%)"),
          yaxis = list(title = "Female Ownership (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Scatter - Workforce vs Productivity
    output$scatter_productivity <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.WKFC.ZS" %in% names(d) || !"IC.FRM.CAPU.ZS" %in% names(d)) return(NULL)

      plot_ly(d, x = ~IC.FRM.WKFC.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 10,
                           color = ~IC.FRM.FEMW.ZS,
                           colorscale = list(c(0, "#dc3545"), c(0.5, "#F4A460"), c(1, "#2E7D32")),
                           colorbar = list(title = "Female<br>Workers (%)"),
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Workforce as Obstacle (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Regional gender
    output$regional_gender <- renderPlotly({
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
      required_cols <- c("IC.FRM.FEMW.ZS", "IC.FRM.FEMO.ZS")
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

      plot_ly(regional) |>
        add_trace(y = ~region, x = ~IC.FRM.FEMW.ZS, type = "bar",
                 orientation = "h", name = "Female Workers",
                 marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~region, x = ~IC.FRM.FEMO.ZS, type = "bar",
                 orientation = "h", name = "Female Ownership",
                 marker = list(color = "#F49B7A")) |>
        layout(
          barmode = "group",
          xaxis = list(title = "Percentage (%)"),
          yaxis = list(title = ""),
          legend = list(x = 0.7, y = 0.1),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Gender gap chart
    output$gender_gap_chart <- renderPlotly({
      req(filtered())
      d <- filtered() |>
        mutate(gender_gap = IC.FRM.FEMW.ZS - IC.FRM.FEMO.ZS) |>
        arrange(desc(gender_gap)) |>
        head(15)

      d$country <- factor(d$country, levels = rev(d$country))

      plot_ly(d, y = ~country, x = ~gender_gap, type = "bar",
              orientation = "h",
              marker = list(color = ~gender_gap,
                           colorscale = list(c(0, "#2E7D32"), c(1, "#dc3545")))) |>
        layout(
          xaxis = list(title = "Gap: Workers - Ownership (%)"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Firm size comparison
    output$firm_size_comparison <- renderPlotly({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.WKFC.ZS" %in% names(d)) return(NULL)

      plot_ly(d, y = ~IC.FRM.WKFC.ZS, x = ~firm_size, type = "box",
              marker = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Workforce Obstacle (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Skills correlation
    output$skills_correlation <- renderPlotly({
      req(filtered())
      d <- filtered()

      plot_ly(d, x = ~IC.FRM.FEMO.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 10,
                           color = ~region,
                           opacity = 0.7)) |>
        layout(
          xaxis = list(title = "Female Ownership (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Insights
    output$insights <- renderUI({
      req(filtered())
      d <- filtered()

      if (is.null(d) || !"IC.FRM.WKFC.ZS" %in% names(d) || !"IC.FRM.FEMW.ZS" %in% names(d) || !"IC.FRM.FEMO.ZS" %in% names(d)) return(NULL)

      avg_workforce <- round(mean(d$IC.FRM.WKFC.ZS, na.rm = TRUE), 1)
      avg_female_workers <- round(mean(d$IC.FRM.FEMW.ZS, na.rm = TRUE), 1)
      avg_female_ownership <- round(mean(d$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)
      gender_gap <- round(avg_female_workers - avg_female_ownership, 1)

      best_gender <- d |> arrange(desc(IC.FRM.FEMO.ZS)) |> head(1)
      worst_workforce <- d |> arrange(desc(IC.FRM.WKFC.ZS)) |> head(1)

      div(
        tags$ul(
          tags$li(tags$strong("Workforce Challenge: "),
                 paste0(avg_workforce, "% of firms report workforce as a major obstacle")),
          tags$li(tags$strong("Female Participation: "),
                 paste0(avg_female_workers, "% female workers on average")),
          tags$li(tags$strong("Female Leadership: "),
                 paste0(avg_female_ownership, "% of firms have female ownership")),
          tags$li(tags$strong("Gender Leadership Gap: "),
                 paste0(gender_gap, "% gap between workforce and ownership participation")),
          tags$li(tags$strong("Best Gender Inclusion: "),
                 paste0(best_gender$country, " (", round(best_gender$IC.FRM.FEMO.ZS, 1), "% female ownership)")),
          tags$li(tags$strong("Highest Workforce Challenge: "),
                 paste0(worst_workforce$country, " (", round(worst_workforce$IC.FRM.WKFC.ZS, 1), "%)"))
        )
      )
    })

  })
}
