# app/view/workforce.R
# Workforce & Gender Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, h4, p, strong,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent,
        downloadButton, downloadHandler],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  leaflet[leafletOutput, renderLeaflet],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select],
  stats[setNames, lm, predict, coef],
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
          card_header(icon("map-marked-alt"), "Geographic Distribution of Workforce & Gender"),
          card_body(
            fluidRow(
              column(4,
                selectInput(
                  ns("map_indicator"),
                  "Map Indicator",
                  choices = c(
                    "Female Ownership (%)" = "female_ownership_pct",
                    "Female Workers (%)" = "female_workers_pct",
                    "Workforce Obstacle (%)" = "workforce_obstacle_pct"
                  )
                )
              )
            ),
            map_with_caption(ns, "workforce_map", height = "400px", title = "Workforce & Gender Indicators by Country")
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
            chart_with_download(ns, "bar_chart", height = "450px", title = "Workforce Indicators by Country"),
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
            chart_with_download(ns, "gender_overview", height = "450px", title = "Gender Balance Overview"),
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
            chart_with_download(ns, "participation_chart", height = "350px", title = "Female Participation Trends"),
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
            chart_with_download(ns, "scatter_productivity", height = "350px", title = "Workforce vs. Productivity"),
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
            chart_with_download(ns, "regional_gender", height = "350px", title = "Female Ownership by Region"),
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
            chart_with_download(ns, "gender_gap_chart", height = "350px", title = "Gender Gap Analysis"),
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
            chart_with_download(ns, "firm_size_comparison", height = "350px", title = "Workforce Challenge by Firm Size"),
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
            chart_with_download(ns, "skills_correlation", height = "350px", title = "Skills & Gender Correlation"),
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

    # Filtered data with global filters
    filtered <- reactive({
      req(wbes_data())

      # Get filters - always access global_filters() to establish reactive dependency
      filters <- if (!is.null(global_filters)) global_filters() else NULL

      # Check if year filter is active
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      # Check if sector filter is active
      sector_filter_active <- !is.null(filters$sector) &&
                              filters$sector != "all" &&
                              filters$sector != ""

      # Choose appropriate data source based on active filters
      d <- if (sector_filter_active && !is.null(wbes_data()$country_sector)) {
        wbes_data()$country_sector  # Country-sector combinations
      } else if (use_panel) {
        wbes_data()$country_panel  # Has year dimension
      } else {
        wbes_data()$latest  # Global country aggregates
      }

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

      # Add coordinates for maps
      if (!is.null(wbes_data()$country_coordinates)) {
        coords <- wbes_data()$country_coordinates
        if ("lat" %in% names(coords) && "lng" %in% names(coords) && !"lat" %in% names(d)) {
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
    output$workforce_map <- renderLeaflet({
      req(filtered(), wbes_data())
      d <- filtered()
      coords <- get_country_coordinates(wbes_data())

      # Get selected map indicator (use dedicated map_indicator input)
      indicator <- if (!is.null(input$map_indicator)) input$map_indicator else "female_ownership_pct"

      # Determine color palette based on indicator
      palette_info <- switch(indicator,
        "female_ownership_pct" = list(palette = "Purples", reverse = FALSE, label = "Female Ownership (%)"),
        "female_workers_pct" = list(palette = "Purples", reverse = FALSE, label = "Female Workers (%)"),
        "workforce_obstacle_pct" = list(palette = "YlOrRd", reverse = TRUE, label = "Workforce Obstacle (%)"),
        list(palette = "Purples", reverse = FALSE, label = indicator)
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

      # Get indicator label for chart title
      ind_label <- switch(indicator,
        "IC.FRM.WKFC.ZS" = "Workforce as Obstacle (%)",
        "IC.FRM.FEMW.ZS" = "Female Workers (%)",
        "IC.FRM.FEMO.ZS" = "Female Ownership (%)",
        indicator
      )

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
                           colorscale = list(c(0, "#1B6B5F"), c(0.5, "#17a2b8"), c(1, "#F49B7A"))),
              text = ~paste0(country, ": ", round(get(indicator), 1), "%"),
              hoverinfo = "text") |>
        layout(
          title = list(text = ind_label, font = list(size = 14), x = 0.5, y = 0.98),
          xaxis = list(title = ind_label, titlefont = list(size = 12)),
          yaxis = list(title = ""),
          margin = list(l = 120, r = 20, t = 40, b = 40),
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

    # Regional data reactive that respects global filters
    regional_filtered <- reactive({
      req(wbes_data())

      # Check if year filter is active
      filters <- if (!is.null(global_filters)) global_filters() else NULL
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      if (use_panel) {
        # Aggregate from filtered country_panel data by region
        data <- filtered()
        if (is.null(data) || nrow(data) == 0 || !"region" %in% names(data)) {
          return(NULL)
        }

        # Aggregate by region
        regional_agg <- data |>
          filter(!is.na(region)) |>
          group_by(region) |>
          summarise(
            IC.FRM.FEMW.ZS = mean(IC.FRM.FEMW.ZS, na.rm = TRUE),
            IC.FRM.FEMO.ZS = mean(IC.FRM.FEMO.ZS, na.rm = TRUE),
            female_workers_pct = mean(IC.FRM.FEMW.ZS, na.rm = TRUE),
            female_ownership_pct = mean(IC.FRM.FEMO.ZS, na.rm = TRUE),
            .groups = "drop"
          )
        return(regional_agg)
      } else {
        # Use pre-computed regional data
        return(wbes_data()$regional)
      }
    })

    # Participation chart
    output$participation_chart <- renderPlotly({
      regional <- regional_filtered()

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
      female_workers_col <- if ("female_workers_pct" %in% names(regional)) "female_workers_pct" else "IC.FRM.FEMW.ZS"
      female_ownership_col <- if ("female_ownership_pct" %in% names(regional)) "female_ownership_pct" else "IC.FRM.FEMO.ZS"

      if (!female_workers_col %in% names(regional) || !female_ownership_col %in% names(regional)) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = "Missing female workers or ownership data columns",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      # Filter valid data
      regional <- regional |> filter(!is.na(.data[[female_workers_col]]) & !is.na(.data[[female_ownership_col]]))

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

      plot_ly(regional, x = ~get(female_workers_col), y = ~get(female_ownership_col),
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

      # Filter valid data for trend line
      valid_data <- d[!is.na(d$IC.FRM.WKFC.ZS) & !is.na(d$IC.FRM.CAPU.ZS), ]

      p <- plot_ly(d, x = ~IC.FRM.WKFC.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              text = ~country,
              name = "Countries",
              marker = list(size = 10,
                           color = ~IC.FRM.FEMW.ZS,
                           colorscale = list(c(0, "#dc3545"), c(0.5, "#F4A460"), c(1, "#2E7D32")),
                           colorbar = list(title = "Female<br>Workers (%)"),
                           opacity = 0.7))

      # Add trend line if enough data
      if (nrow(valid_data) >= 2) {
        tryCatch({
          model <- lm(IC.FRM.CAPU.ZS ~ IC.FRM.WKFC.ZS, data = valid_data)
          x_range <- range(valid_data$IC.FRM.WKFC.ZS, na.rm = TRUE)
          trend_x <- seq(x_range[1], x_range[2], length.out = 50)
          trend_y <- predict(model, newdata = data.frame(IC.FRM.WKFC.ZS = trend_x))
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
          xaxis = list(title = "Workforce as Obstacle (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
          margin = list(b = 70),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Regional gender
    output$regional_gender <- renderPlotly({
      regional <- regional_filtered()

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

      # Use friendly column names with fallbacks to IC.FRM.* codes
      female_workers_col <- if ("female_workers_pct" %in% names(regional)) "female_workers_pct" else "IC.FRM.FEMW.ZS"
      female_ownership_col <- if ("female_ownership_pct" %in% names(regional)) "female_ownership_pct" else "IC.FRM.FEMO.ZS"

      # Check if at least one column exists
      if (!female_workers_col %in% names(regional) && !female_ownership_col %in% names(regional)) {
        return(
          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = "Missing female workers and ownership data columns",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666666")
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        )
      }

      # Filter valid data
      regional <- regional |> filter(!is.na(region))

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

      # Extract values for plotting
      workers_vals <- if (female_workers_col %in% names(regional)) regional[[female_workers_col]] else rep(NA, nrow(regional))
      ownership_vals <- if (female_ownership_col %in% names(regional)) regional[[female_ownership_col]] else rep(NA, nrow(regional))

      plot_ly(regional) |>
        add_trace(y = ~region, x = workers_vals, type = "bar",
                 orientation = "h", name = "Female Workers",
                 marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~region, x = ownership_vals, type = "bar",
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
        head(15) |>
        arrange(gender_gap)  # Re-arrange ascending for horizontal bar display

      d$country <- factor(d$country, levels = unique(d$country))

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

      # Filter valid data for trend line
      valid_data <- d[!is.na(d$IC.FRM.FEMO.ZS) & !is.na(d$IC.FRM.CAPU.ZS), ]

      colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D", "#F4A460")

      p <- plot_ly(d, x = ~IC.FRM.FEMO.ZS, y = ~IC.FRM.CAPU.ZS,
              type = "scatter", mode = "markers",
              color = ~region,
              colors = colors,
              text = ~country,
              marker = list(size = 10, opacity = 0.7))

      # Add trend line if enough data
      if (nrow(valid_data) >= 2) {
        tryCatch({
          model <- lm(IC.FRM.CAPU.ZS ~ IC.FRM.FEMO.ZS, data = valid_data)
          x_range <- range(valid_data$IC.FRM.FEMO.ZS, na.rm = TRUE)
          trend_x <- seq(x_range[1], x_range[2], length.out = 50)
          trend_y <- predict(model, newdata = data.frame(IC.FRM.FEMO.ZS = trend_x))
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
          xaxis = list(title = "Female Ownership (%)"),
          yaxis = list(title = "Capacity Utilization (%)"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
          margin = list(b = 70),
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

    # Download handlers
    output$dl_bar_chart <- downloadHandler(
      filename = function() paste0("workforce_by_country_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$bar_chart(), file, selfcontained = TRUE) }
    )
    output$dl_gender_overview <- downloadHandler(
      filename = function() paste0("gender_overview_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$gender_overview(), file, selfcontained = TRUE) }
    )
    output$dl_participation_chart <- downloadHandler(
      filename = function() paste0("female_participation_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$participation_chart(), file, selfcontained = TRUE) }
    )
    output$dl_scatter_productivity <- downloadHandler(
      filename = function() paste0("workforce_vs_productivity_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$scatter_productivity(), file, selfcontained = TRUE) }
    )
    output$dl_regional_gender <- downloadHandler(
      filename = function() paste0("regional_gender_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$regional_gender(), file, selfcontained = TRUE) }
    )
    output$dl_gender_gap_chart <- downloadHandler(
      filename = function() paste0("gender_gap_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$gender_gap_chart(), file, selfcontained = TRUE) }
    )
    output$dl_firm_size_comparison <- downloadHandler(
      filename = function() paste0("workforce_by_firm_size_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$firm_size_comparison(), file, selfcontained = TRUE) }
    )
    output$dl_skills_correlation <- downloadHandler(
      filename = function() paste0("skills_gender_correlation_", format(Sys.Date(), "%Y%m%d"), ".html"),
      content = function(file) { saveWidget(output$skills_correlation(), file, selfcontained = TRUE) }
    )

  })
}
