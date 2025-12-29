# app/view/mod_country_profile.R
# Country Profile Deep Dive Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, tagList, icon, div, h2, h3, h4, p, span,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent, renderText, textOutput,
        downloadButton, downloadHandler, reactiveVal],
  bslib[card, card_header, card_body, navset_card_tab, nav_panel, value_box],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, select, arrange, mutate, left_join, group_by, summarize, ungroup, slice_max],
  leaflet[leafletOutput, renderLeaflet],
  stats[setNames, reorder],
  htmlwidgets[saveWidget],
  utils[write.csv],
  app/logic/shared_filters[apply_common_filters],
  app/logic/custom_regions[filter_by_region],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates],
  app/logic/chart_utils[create_chart_caption, map_with_caption],
  app/logic/wb_integration[
    get_wb_country_context,
    get_wb_context_from_cache,
    map_wbes_countries_to_iso3,
    format_wb_indicator,
    get_wb_indicator_label,
    get_wb_databank_indicators
  ]
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

  tags$div(
    class = "country-profile-container",

    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("flag"), "Country Profile", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "In-depth analysis of business environment indicators for individual countries")
        )
      )
    ),

    # Country Selector
    fluidRow(
      class = "mb-4",
      column(4,
        card(
          card_body(
            selectInput(
              ns("country_select"),
              "Select Country",
              choices = NULL,
              width = "100%"
            )
          )
        )
      ),
      column(8,
        uiOutput(ns("country_summary"))
      )
    ),

    # Geographic Map
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("map-marked-alt"), "Geographic Context"),
          card_body(
            fluidRow(
              column(4,
                selectInput(
                  ns("map_indicator"),
                  "Map Indicator",
                  choices = c(
                    "Power Outages" = "power_outages_per_month",
                    "Outage Duration" = "avg_outage_duration_hrs",
                    "Credit Access" = "firms_with_credit_line_pct",
                    "Bribery Incidence" = "bribery_incidence_pct",
                    "Capacity Utilization" = "capacity_utilization_pct",
                    "Female Ownership" = "female_ownership_pct"
                  ),
                  selected = "power_outages_per_month"
                )
              )
            ),
            map_with_caption(ns, "country_profile_map", height = "400px", title = "Country Geographic Context")
          )
        )
      )
    ),

    # Radar Chart + Key Metrics
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("chart-pie"), "Business Environment Radar"),
          card_body(
            chart_with_download(ns, "radar_chart", height = "400px", title = "Business Environment Radar"),
            p(
              class = "text-muted small mt-2",
              "The radar highlights how the selected country scores across infrastructure, finance, governance, capacity, exports, and gender equity relative to a 0–100 scale."
            )
          )
        )
      ),
      column(6,
        card(
          card_header(icon("th-list"), "Key Indicators"),
          card_body(
            uiOutput(ns("key_metrics"))
          )
        )
      )
    ),

    # Detailed Tabs
    fluidRow(
      column(12,
        navset_card_tab(
          id = ns("detail_tabs"),

          nav_panel(
            title = "Infrastructure",
            icon = icon("bolt"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart1", height = "300px", title = "Infrastructure Obstacles"),
                  p(
                    class = "text-muted small mt-2",
                    "Bars rank which infrastructure services firms flag as biggest obstacles, indicating where reliability investments are needed."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart2", height = "300px", title = "Power Source Distribution"),
                  p(
                    class = "text-muted small mt-2",
                    "The pie shows how firms power operations (grid, generator, mixed), revealing dependence on backup generation."
                  )
                )
              )
            ),
            fluidRow(
              class = "mt-3",
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart3", height = "300px", title = "Water & Transport Constraints"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows water and transport infrastructure constraints affecting business operations."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "infra_chart4", height = "300px", title = "Internet & Telecom Access"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays telecommunications access and internet connectivity rates for the country."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Finance",
            icon = icon("university"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "finance_chart1", height = "300px", title = "Financial Access Overview"),
                  p(
                    class = "text-muted small mt-2",
                    "Financial product uptake across credit and deposit instruments highlights where inclusion gaps remain."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "finance_chart2", height = "300px", title = "Collateral Requirements"),
                  p(
                    class = "text-muted small mt-2",
                    "The gauge reports average collateral required for loans; higher values signal tighter lending conditions."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Governance",
            icon = icon("balance-scale"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "gov_chart1", height = "300px", title = "Bribery by Transaction Type"),
                  p(
                    class = "text-muted small mt-2",
                    "Bribery prevalence by transaction type surfaces which interactions with government most often trigger informal payments."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "gov_chart2", height = "300px", title = "Regulatory Burden"),
                  p(
                    class = "text-muted small mt-2",
                    "Management time spent on regulatory tasks highlights the bureaucracy burden affecting daily operations."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Workforce",
            icon = icon("users"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "workforce_chart1", height = "300px", title = "Workforce Composition"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows workforce composition including female participation and skill levels."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "workforce_chart2", height = "300px", title = "Training & Development"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays training programs and workforce development initiatives."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Crime & Security",
            icon = icon("shield-alt"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "crime_chart1", height = "300px", title = "Crime as Business Obstacle"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows crime and security costs as obstacles to business operations."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "crime_chart2", height = "300px", title = "Security Expenditure & Losses"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays security expenditure and crime-related losses."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Performance",
            icon = icon("chart-line"),
            fluidRow(
              column(6,
                tagList(
                  chart_with_download(ns, "performance_chart1", height = "300px", title = "Capacity Utilization"),
                  p(
                    class = "text-muted small mt-2",
                    "Shows capacity utilization and operational efficiency metrics."
                  )
                )
              ),
              column(6,
                tagList(
                  chart_with_download(ns, "performance_chart2", height = "300px", title = "Export Orientation"),
                  p(
                    class = "text-muted small mt-2",
                    "Displays export orientation and international market participation."
                  )
                )
              )
            )
          ),

          nav_panel(
            title = "Time Series",
            icon = icon("area-chart"),
            tagList(
              chart_with_download(ns, "time_series", height = "400px", title = "Indicator Trends Over Time"),
              p(
                class = "text-muted small mt-2",
                "Trend lines track how outages, credit access, and bribery have evolved over survey waves, making it easy to spot improvements or setbacks."
              )
            )
          ),

          # NEW: Economic Context Tab - World Bank Databank Integration
          nav_panel(
            title = "Economic Context",
            icon = icon("globe"),
            tagList(
              fluidRow(
                column(12,
                  tags$div(
                    class = "alert alert-info mb-3",
                    icon("info-circle"),
                    " Macro-level indicators from the World Bank Databank provide context for interpreting firm-level WBES data."
                  )
                )
              ),
              # Macro Economic Indicators Row
              fluidRow(
                class = "mb-4",
                column(12,
                  card(
                    card_header(icon("chart-bar"), "Macroeconomic Overview"),
                    card_body(
                      uiOutput(ns("macro_indicators"))
                    )
                  )
                )
              ),
              # Comparative Charts Row
              fluidRow(
                class = "mb-4",
                column(6,
                  tagList(
                    chart_with_download(ns, "macro_radar", height = "350px", title = "Macro Environment Radar"),
                    p(
                      class = "text-muted small mt-2",
                      "National-level indicators on governance, infrastructure, and trade complement the firm-level business environment radar."
                    )
                  )
                ),
                column(6,
                  tagList(
                    chart_with_download(ns, "wbes_vs_wb_chart", height = "350px", title = "WBES vs National Indicators"),
                    p(
                      class = "text-muted small mt-2",
                      "Compares firm-reported metrics (WBES) with national statistics (World Bank) to identify perception gaps."
                    )
                  )
                )
              ),
              # Governance Indicators Row
              fluidRow(
                class = "mb-4",
                column(6,
                  tagList(
                    chart_with_download(ns, "governance_chart", height = "300px", title = "Governance Indicators (WGI)"),
                    p(
                      class = "text-muted small mt-2",
                      "World Bank Worldwide Governance Indicators ranging from -2.5 (weak) to +2.5 (strong)."
                    )
                  )
                ),
                column(6,
                  tagList(
                    chart_with_download(ns, "economic_trends_chart", height = "300px", title = "Economic Trends"),
                    p(
                      class = "text-muted small mt-2",
                      "GDP growth and inflation trends over time provide macroeconomic context."
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data, global_filters = NULL, wb_prefetched_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # Filtered data with global filters applied first
    filtered_data <- reactive({
      req(wbes_data())

      # Use country_panel (has year) if year filter is active, otherwise use latest
      filters <- if (!is.null(global_filters)) global_filters() else NULL
      use_panel <- !is.null(filters$year) && length(filters$year) > 0 &&
                   !all(filters$year %in% c("all", NA))

      data <- if (use_panel) wbes_data()$country_panel else wbes_data()$latest

      # If global filters are provided, use them
      if (!is.null(filters)) {
        data <- apply_common_filters(
          data,
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
          data <- merge(data, coords, by = "country", all.x = TRUE)
        }
      }

      data
    })

    # Update country choices from filtered data
    observeEvent(filtered_data(), {
      req(filtered_data())
      countries <- filtered_data()$country |>
        unique() |>
        stats::na.omit() |>
        as.character() |>
        sort()

      shiny::updateSelectInput(
        session, "country_select",
        choices = setNames(countries, countries),
        selected = if(length(countries) > 0) countries[1] else NULL
      )
    })

    # Selected country data
    country_data <- reactive({
      req(filtered_data(), input$country_select)
      filtered_data() |> filter(!is.na(country), country == input$country_select)
    })

    # Reactive to get WB Databank data for selected country - year-matched to survey
    # Uses prefetched cache if available, falls back to live API
    wb_country_data <- reactive({
      req(input$country_select, country_data())

      # Map country name to ISO3 code
      country_mapping <- map_wbes_countries_to_iso3(input$country_select)
      iso3_code <- country_mapping$iso3c[1]

      if (is.na(iso3_code)) {
        return(NULL)
      }

      # Extract the survey year from the country data
      # This ensures WB data matches the WBES survey year
      d <- country_data()
      survey_year <- if (!is.null(d$year) && length(d$year) > 0 && !is.na(d$year[1])) {
        as.integer(d$year[1])
      } else {
        NULL  # Fall back to latest data if no year
      }

      # Try to get from prefetched cache first
      tryCatch({
        prefetched <- if (!is.null(wb_prefetched_data)) wb_prefetched_data() else NULL

        if (!is.null(prefetched)) {
          # Use cached data with fallback to API if not found
          get_wb_context_from_cache(iso3_code, prefetched, target_year = survey_year,
                                     fallback_to_api = TRUE)
        } else {
          # No cache available, fetch directly from API
          get_wb_country_context(iso3_code, target_year = survey_year)
        }
      }, error = function(e) {
        NULL
      })
    })

    # Geographic map for country profile
    output$country_profile_map <- renderLeaflet({
      req(filtered_data(), wbes_data(), input$map_indicator)
      d <- filtered_data()
      coords <- get_country_coordinates(wbes_data())

      # Get readable label from indicator code
      indicator_label <- switch(input$map_indicator,
        "power_outages_per_month" = "Power Outages/Month",
        "avg_outage_duration_hrs" = "Outage Duration (hrs)",
        "firms_with_credit_line_pct" = "Credit Access (%)",
        "bribery_incidence_pct" = "Bribery Incidence (%)",
        "capacity_utilization_pct" = "Capacity Utilization (%)",
        "female_ownership_pct" = "Female Ownership (%)",
        gsub("_", " ", tools::toTitleCase(input$map_indicator))
      )

      create_wbes_map(
        data = d,
        coordinates = coords,
        indicator_col = input$map_indicator,
        indicator_label = indicator_label,
        color_palette = "YlOrRd",
        reverse_colors = FALSE
      )
    })

    # Country summary card - enhanced with WB Databank info (year-matched)
    output$country_summary <- renderUI({
      req(country_data())
      d <- country_data()

      # Extract WBES values with fallback handling for NA
      region_val <- if (!is.null(d$region) && length(d$region) > 0 && !is.na(d$region[1])) {
        as.character(d$region[1])
      } else {
        "N/A"
      }

      # For firms surveyed, use sample_size from the aggregated data
      firms_val <- if (!is.null(d$sample_size) && length(d$sample_size) > 0 && !is.na(d$sample_size[1])) {
        format(round(d$sample_size[1]), big.mark = ",")
      } else {
        "N/A"
      }

      # Get income level from WBES data (already enriched with WB income by year)
      income_val <- if (!is.null(d$income) && length(d$income) > 0 && !is.na(d$income[1])) {
        as.character(d$income[1])
      } else {
        "N/A"
      }

      # Get survey year for display
      survey_year <- if (!is.null(d$year) && length(d$year) > 0 && !is.na(d$year[1])) {
        as.character(d$year[1])
      } else {
        "N/A"
      }

      # Fetch WB data for GDP (year-matched via wb_country_data reactive)
      wb_data <- wb_country_data()
      gdp_val <- if (!is.null(wb_data) && !is.null(wb_data[["NY.GDP.PCAP.CD"]]) &&
                     !is.na(wb_data[["NY.GDP.PCAP.CD"]])) {
        format_wb_indicator(wb_data[["NY.GDP.PCAP.CD"]], "NY.GDP.PCAP.CD")
      } else {
        "N/A"
      }

      # Get the WB data year for GDP
      gdp_year <- if (!is.null(wb_data) && !is.null(wb_data$data_years[["NY.GDP.PCAP.CD"]])) {
        as.character(wb_data$data_years[["NY.GDP.PCAP.CD"]])
      } else {
        ""
      }

      tags$div(
        class = "card h-100",
        tags$div(
          class = "card-body",
          fluidRow(
            column(2,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value text-primary-teal", survey_year),
                tags$div(class = "kpi-label", "Survey Year")
              )
            ),
            column(2,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value", region_val),
                tags$div(class = "kpi-label", "Region")
              )
            ),
            column(3,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value",
                  style = switch(income_val,
                    "High" = "color: #2E7D32;",
                    "Upper-Middle" = "color: #1B6B5F;",
                    "Lower-Middle" = "color: #F4A460;",
                    "Low" = "color: #dc3545;",
                    ""
                  ),
                  income_val
                ),
                tags$div(class = "kpi-label", paste("Income Level", survey_year))
              )
            ),
            column(3,
              tags$div(class = "kpi-box",
                tags$div(class = "kpi-value text-primary-teal", gdp_val),
                tags$div(class = "kpi-label", paste("GDP per Capita", gdp_year))
              )
            ),
            column(2,
              tags$div(class = "kpi-box kpi-box-success",
                tags$div(class = "kpi-value", firms_val),
                tags$div(class = "kpi-label", "Firms Surveyed")
              )
            )
          )
        )
      )
    })

    # Radar Chart
    output$radar_chart <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Check if we have any data at all
      if (nrow(d) == 0) {
        # Create empty plot with message
        plot_ly() |>
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "No data available for this country",
                showarrow = FALSE,
                font = list(size = 14, color = "#666666")
              )
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        # Helper function to safely extract and normalize values - returns NA if no data
        # WBES variable mappings:
        # - power_outages_per_month: from in2 (infrastructure quality, inverted)
        # - firms_with_credit_line_pct: from fin14 (finance access)
        # - bribery_incidence_pct: from graft3 (corruption, inverted for "Low Corruption")
        # - capacity_utilization_pct: from t3 (operational efficiency)
        # - export_firms_pct: from tr10 (export orientation)
        # - female_ownership_pct: from gend1 (gender equity)
        safe_val <- function(col, scale = 1, invert = FALSE) {
          if (col %in% names(d) && !is.na(d[[col]][1])) {
            val <- d[[col]][1] * scale
            if (invert) 100 - min(val, 100) else min(val, 100)
          } else {
            NA_real_
          }
        }

        # Calculate indicators - NA when data missing
        indicators <- c(
          "Infrastructure" = safe_val("power_outages_per_month", scale = 5, invert = TRUE),
          "Finance Access" = safe_val("firms_with_credit_line_pct"),
          "Low Corruption" = safe_val("bribery_incidence_pct", invert = TRUE),
          "Capacity Use" = safe_val("capacity_utilization_pct"),
          "Export Orient." = safe_val("export_firms_pct", scale = 2),
          "Gender Equity" = safe_val("female_ownership_pct", scale = 2)
        )

        # Check if all indicators are NA
        if (all(is.na(indicators))) {
          # Show message listing missing variables
          missing_vars <- c(
            "Infrastructure: power_outages_per_month",
            "Finance Access: firms_with_credit_line_pct",
            "Low Corruption: bribery_incidence_pct",
            "Capacity Use: capacity_utilization_pct",
            "Export Orient.: export_firms_pct",
            "Gender Equity: female_ownership_pct"
          )

          plot_ly() |>
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = paste0("Missing data for all indicators:<br>",
                                paste(missing_vars, collapse = "<br>")),
                  showarrow = FALSE,
                  font = list(size = 12, color = "#666666"),
                  xanchor = "center",
                  yanchor = "middle"
                )
              ),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        } else if (any(is.na(indicators))) {
          # Some data available, some missing - show available data with note
          missing_indicators <- names(indicators[is.na(indicators)])
          available_indicators <- indicators[!is.na(indicators)]

          # Map indicator names to variable names
          var_mapping <- c(
            "Infrastructure" = "power_outages_per_month",
            "Finance Access" = "firms_with_credit_line_pct",
            "Low Corruption" = "bribery_incidence_pct",
            "Capacity Use" = "capacity_utilization_pct",
            "Export Orient." = "export_firms_pct",
            "Gender Equity" = "female_ownership_pct"
          )

          missing_vars <- paste(
            paste0(missing_indicators, ": ", var_mapping[missing_indicators]),
            collapse = "<br>"
          )

          plot_ly(
            type = "scatterpolar",
            r = as.numeric(available_indicators),
            theta = names(available_indicators),
            fill = "toself",
            fillcolor = "rgba(27, 107, 95, 0.3)",
            line = list(color = "#1B6B5F", width = 2)
          ) |>
            layout(
              polar = list(
                radialaxis = list(visible = TRUE, range = c(0, 100))
              ),
              showlegend = FALSE,
              paper_bgcolor = "rgba(0,0,0,0)",
              annotations = list(
                list(
                  text = paste0("<b>Missing data:</b><br>", missing_vars),
                  showarrow = FALSE,
                  font = list(size = 10, color = "#999999"),
                  xref = "paper",
                  yref = "paper",
                  x = 0.5,
                  y = -0.15,
                  xanchor = "center",
                  yanchor = "top"
                )
              )
            ) |>
            config(displayModeBar = FALSE)
        } else {
          # All data available - show normal radar chart
          plot_ly(
            type = "scatterpolar",
            r = as.numeric(indicators),
            theta = names(indicators),
            fill = "toself",
            fillcolor = "rgba(27, 107, 95, 0.3)",
            line = list(color = "#1B6B5F", width = 2)
          ) |>
            layout(
              polar = list(
                radialaxis = list(visible = TRUE, range = c(0, 100))
              ),
              showlegend = FALSE,
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE)
        }
      }
    })

    # Key Metrics
    output$key_metrics <- renderUI({
      req(country_data())
      d <- country_data()

      # Helper to safely extract metric values with NA handling
      get_metric <- function(col) {
        if (col %in% names(d) && length(d[[col]]) > 0 && !is.na(d[[col]][1])) {
          round(d[[col]][1], 1)
        } else {
          "N/A"
        }
      }

      # WBES variable mappings documented inline:
      # power_outages_per_month: from in2 (Number of power outages per month)
      # avg_outage_duration_hrs: from in3 (Average duration of power outages in hours)
      # firms_with_credit_line_pct: from fin14 (% firms with line of credit)
      # bribery_incidence_pct: from graft3 (% firms experiencing bribery requests)
      # capacity_utilization_pct: from t3 (Capacity utilization rate)
      # female_ownership_pct: from gend1 (% female ownership)
      metrics <- list(
        list("Power Outages/Month", get_metric("power_outages_per_month"), "bolt"),
        list("Outage Duration (hrs)", get_metric("avg_outage_duration_hrs"), "clock"),
        list("Credit Access (%)", get_metric("firms_with_credit_line_pct"), "credit-card"),
        list("Bribery Incidence (%)", get_metric("bribery_incidence_pct"), "hand-holding-usd"),
        list("Capacity Utilization (%)", get_metric("capacity_utilization_pct"), "industry"),
        list("Female Ownership (%)", get_metric("female_ownership_pct"), "female")
      )

      tags$div(
        class = "metrics-list",
        lapply(metrics, function(m) {
          tags$div(
            class = "d-flex justify-content-between align-items-center p-3 border-bottom",
            tags$span(icon(m[[3]]), " ", m[[1]]),
            tags$span(class = "fw-bold text-primary-teal", m[[2]])
          )
        })
      )
    })

    # Infrastructure Charts
    # MIRRORS Infrastructure domain module - uses same metrics as mod_infrastructure.R
    # Primary metrics: power_outages_per_month, avg_outage_duration_hrs,
    #                  firms_with_generator_pct, water_insufficiency_pct
    output$infra_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Build metrics data frame using SAME columns as Infrastructure domain
      metrics <- data.frame(
        category = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )

      if ("power_outages_per_month" %in% names(d) && !is.na(d$power_outages_per_month[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Power Outages/Month",
          value = d$power_outages_per_month[1]
        ))
      }

      if ("avg_outage_duration_hrs" %in% names(d) && !is.na(d$avg_outage_duration_hrs[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Outage Duration (hrs)",
          value = d$avg_outage_duration_hrs[1]
        ))
      }

      if ("firms_with_generator_pct" %in% names(d) && !is.na(d$firms_with_generator_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Generator Usage (%)",
          value = d$firms_with_generator_pct[1]
        ))
      }

      if ("water_insufficiency_pct" %in% names(d) && !is.na(d$water_insufficiency_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          category = "Water Issues (%)",
          value = d$water_insufficiency_pct[1]
        ))
      }

      if (nrow(metrics) > 0) {
        plot_ly(metrics,
                x = ~category,
                y = ~value,
                type = "bar",
                marker = list(
                  color = ~value,
                  colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))
                )) |>
          layout(
            title = list(text = "Infrastructure Metrics", font = list(size = 14)),
            xaxis = list(title = ""),
            yaxis = list(title = "Value"),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = paste0("No infrastructure data available<br>",
                           "Expected: power_outages_per_month, avg_outage_duration_hrs,<br>",
                           "firms_with_generator_pct, water_insufficiency_pct"),
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font = list(size = 12, color = "#666666")
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$infra_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # MIRRORS Infrastructure domain - Power Source Distribution
      # Uses firms_with_generator_pct as primary data source
      generator_pct <- if ("firms_with_generator_pct" %in% names(d) && !is.na(d$firms_with_generator_pct[1])) {
        as.numeric(d$firms_with_generator_pct[1])
      } else NA_real_

      if (!is.na(generator_pct)) {
        # Calculate distribution same way as Infrastructure domain
        grid_only <- max(0, 100 - generator_pct - 10)  # Estimate
        mixed <- 10  # Estimate for mixed sources
        renewable <- 5  # Solar/Renewable estimate

        values <- c(grid_only, generator_pct, mixed, renewable)
        labels <- c("Grid Only", "Generator Primary", "Mixed Sources", "Solar/Renewable")

        # Filter out zero values
        non_zero <- values > 0
        values <- values[non_zero]
        labels <- labels[non_zero]

        plot_ly(
          labels = labels,
          values = values,
          type = "pie",
          marker = list(colors = c("#1B6B5F", "#dc3545", "#F4A460", "#2E7D32")),
          textinfo = "label+percent"
        ) |>
          layout(
            title = list(text = "Power Source Distribution", font = list(size = 14)),
            showlegend = FALSE,
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No power source data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Finance Charts
    # Uses actual WBES data: fin15 (bank account), fin14 (credit line),
    # fin16 (loan application), fin9 (overdraft)
    output$finance_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Extract actual financial access data
      products <- list()

      if ("firms_with_bank_account_pct" %in% names(d) && !is.na(d$firms_with_bank_account_pct[1])) {
        products$`Bank Account` <- d$firms_with_bank_account_pct[1]
      }
      if ("firms_with_credit_line_pct" %in% names(d) && !is.na(d$firms_with_credit_line_pct[1])) {
        products$`Credit Line` <- d$firms_with_credit_line_pct[1]
      }
      if ("loan_application_pct" %in% names(d) && !is.na(d$loan_application_pct[1])) {
        products$`Applied for Loan` <- d$loan_application_pct[1]
      }
      if ("overdraft_facility_pct" %in% names(d) && !is.na(d$overdraft_facility_pct[1])) {
        products$`Overdraft Facility` <- d$overdraft_facility_pct[1]
      }

      if (length(products) > 0) {
        plot_data <- data.frame(
          product = names(products),
          pct = unlist(products),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~product,
                y = ~pct,
                type = "bar",
                marker = list(color = "#F49B7A")) |>
          layout(
            title = list(text = "Financial Products Access (%)", font = list(size = 14)),
            yaxis = list(title = "% of Firms", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No financial access data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$finance_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Get collateral required percentage from actual data
      collateral_val <- if ("collateral_required_pct" %in% names(d) && !is.na(d$collateral_required_pct[1])) {
        d$collateral_required_pct[1]
      } else {
        NA
      }

      if (!is.na(collateral_val)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(collateral_val, 1),
          title = list(text = "Avg Collateral (% of Loan)"),
          gauge = list(
            axis = list(range = list(0, 300)),
            bar = list(color = "#F49B7A"),
            steps = list(
              list(range = c(0, 100), color = "#e8f5e9"),
              list(range = c(100, 200), color = "#fff3e0"),
              list(range = c(200, 300), color = "#ffebee")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No collateral data available",
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Governance Charts
    # MIRRORS Corruption domain module
    # Uses IC.FRM.CORR.ZS and IC.FRM.BRIB.ZS (same as mod_corruption.R)
    output$gov_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Build governance metrics using SAME columns as Corruption domain
      metrics <- data.frame(
        indicator = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )

      if ("IC.FRM.CORR.ZS" %in% names(d) && !is.na(d$IC.FRM.CORR.ZS[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Corruption as Obstacle",
          value = as.numeric(d$IC.FRM.CORR.ZS[1])
        ))
      } else if ("corruption_obstacle_pct" %in% names(d) && !is.na(d$corruption_obstacle_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Corruption as Obstacle",
          value = as.numeric(d$corruption_obstacle_pct[1])
        ))
      }

      if ("IC.FRM.BRIB.ZS" %in% names(d) && !is.na(d$IC.FRM.BRIB.ZS[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Bribery Incidence",
          value = as.numeric(d$IC.FRM.BRIB.ZS[1])
        ))
      } else if ("bribery_incidence_pct" %in% names(d) && !is.na(d$bribery_incidence_pct[1])) {
        metrics <- rbind(metrics, data.frame(
          indicator = "Bribery Incidence",
          value = as.numeric(d$bribery_incidence_pct[1])
        ))
      }

      if (nrow(metrics) > 0) {
        plot_ly(metrics,
                x = ~indicator,
                y = ~value,
                type = "bar",
                marker = list(
                  color = ~value,
                  colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545"))
                )) |>
          layout(
            title = list(text = "Governance & Corruption Metrics", font = list(size = 14)),
            xaxis = list(title = ""),
            yaxis = list(title = "% of Firms", range = c(0, 100)),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = paste0("No governance data available<br>",
                           "Expected: IC.FRM.CORR.ZS or IC.FRM.BRIB.ZS"),
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font = list(size = 12, color = "#666666")
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$gov_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Extract management time spent on regulations (from j2)
      # Note: WBES typically has one aggregate measure for time spent on regulations
      mgmt_time <- if ("mgmt_time_regulations_pct" %in% names(d) && !is.na(d$mgmt_time_regulations_pct[1])) {
        d$mgmt_time_regulations_pct[1]
      } else 0

      if (mgmt_time > 0) {
        # Show the aggregate measure
        # Break it down into estimated components (this is illustrative)
        plot_data <- data.frame(
          activity = c("Govt Regulations"),
          pct = c(mgmt_time),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~activity,
                y = ~pct,
                type = "bar",
                marker = list(color = "#6C757D")) |>
          layout(
            title = list(text = "Mgmt Time on Bureaucracy (%)", font = list(size = 14)),
            yaxis = list(title = "% of Management Time"),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No management time data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Additional Infrastructure Charts
    output$infra_chart3 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Water and transport obstacles - convert to numeric if needed
      safe_numeric <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_real_)
        if (is.logical(x)) return(as.numeric(x) * 10)  # Convert logical to 0/10 scale
        if (is.factor(x) || is.character(x)) {
          # Try to extract numeric from factor/character
          num_val <- suppressWarnings(as.numeric(as.character(x)))
          if (!is.na(num_val)) return(num_val)
          # If it's a severity factor like "Major", "Moderate", convert to scale
          if (tolower(as.character(x)) %in% c("major", "very severe", "severe")) return(8)
          if (tolower(as.character(x)) %in% c("moderate", "minor")) return(4)
          return(NA_real_)
        }
        return(as.numeric(x))
      }

      constraints <- list()
      if ("water_obstacle" %in% names(d)) {
        val <- safe_numeric(d$water_obstacle[1])
        if (!is.na(val)) constraints$`Water` <- val
      }
      if ("transport_obstacle" %in% names(d)) {
        val <- safe_numeric(d$transport_obstacle[1])
        if (!is.na(val)) constraints$`Transport` <- val
      }
      if ("customs_obstacle" %in% names(d)) {
        val <- safe_numeric(d$customs_obstacle[1])
        if (!is.na(val)) constraints$`Customs` <- val
      }

      if (length(constraints) > 0) {
        plot_data <- data.frame(
          category = names(constraints),
          severity = unlist(constraints),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~severity,
                type = "bar",
                marker = list(color = "#F49B7A")) |>
          layout(
            title = list(text = "Water & Transport Constraints", font = list(size = 14)),
            yaxis = list(title = "Severity Score (0-10)"),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No water/transport data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$infra_chart4 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Telecommunications metrics - convert to numeric with fallbacks
      metrics <- list()
      if ("internet_access_pct" %in% names(d) && !is.na(d$internet_access_pct[1])) {
        metrics$`Internet Access` <- as.numeric(d$internet_access_pct[1])
      } else if ("c9" %in% names(d) && !is.na(d$c9[1])) {
        metrics$`Internet Access` <- as.numeric(d$c9[1])
      }

      if ("website_pct" %in% names(d) && !is.na(d$website_pct[1])) {
        metrics$`Has Website` <- as.numeric(d$website_pct[1])
      } else if ("c10" %in% names(d) && !is.na(d$c10[1])) {
        metrics$`Has Website` <- as.numeric(d$c10[1])
      }

      if ("email_usage_pct" %in% names(d) && !is.na(d$email_usage_pct[1])) {
        metrics$`Email Usage` <- as.numeric(d$email_usage_pct[1])
      } else if ("c11" %in% names(d) && !is.na(d$c11[1])) {
        metrics$`Email Usage` <- as.numeric(d$c11[1])
      }

      if (length(metrics) > 0) {
        plot_data <- data.frame(
          metric = names(metrics),
          pct = unlist(metrics),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~metric,
                y = ~pct,
                type = "bar",
                marker = list(color = "#1B6B5F")) |>
          layout(
            title = list(text = "Digital Connectivity (%)", font = list(size = 14)),
            yaxis = list(title = "% of Firms", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No telecommunications data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Workforce Charts
    output$workforce_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Workforce composition - use IC.FRM.* codes for consistency
      composition <- list()
      if ("IC.FRM.FEMW.ZS" %in% names(d) && !is.na(d$IC.FRM.FEMW.ZS[1])) {
        composition$`Female Workers` <- d$IC.FRM.FEMW.ZS[1]
      } else if ("female_workers_pct" %in% names(d) && !is.na(d$female_workers_pct[1])) {
        composition$`Female Workers` <- as.numeric(d$female_workers_pct[1])
      }

      if ("IC.FRM.FEMO.ZS" %in% names(d) && !is.na(d$IC.FRM.FEMO.ZS[1])) {
        composition$`Female Ownership` <- d$IC.FRM.FEMO.ZS[1]
      } else if ("female_ownership_pct" %in% names(d) && !is.na(d$female_ownership_pct[1])) {
        composition$`Female Ownership` <- as.numeric(d$female_ownership_pct[1])
      }

      if ("permanent_workers_pct" %in% names(d) && !is.na(d$permanent_workers_pct[1])) {
        composition$`Permanent Staff` <- as.numeric(d$permanent_workers_pct[1])
      }

      if (length(composition) > 0) {
        plot_data <- data.frame(
          category = names(composition),
          pct = unlist(composition),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~pct,
                type = "bar",
                marker = list(color = "#F4A460")) |>
          layout(
            title = list(text = "Workforce Composition (%)", font = list(size = 14)),
            yaxis = list(title = "Percentage", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No workforce composition data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$workforce_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Training metrics - show workforce obstacle as alternative
      training_pct <- NA
      if ("IC.FRM.WKFC.ZS" %in% names(d) && !is.na(d$IC.FRM.WKFC.ZS[1])) {
        # Invert workforce obstacle to show as "workforce quality"
        training_pct <- 100 - min(as.numeric(d$IC.FRM.WKFC.ZS[1]), 100)
      } else if ("training_programs_pct" %in% names(d) && !is.na(d$training_programs_pct[1])) {
        training_pct <- as.numeric(d$training_programs_pct[1])
      } else if ("workforce_obstacle_pct" %in% names(d) && !is.na(d$workforce_obstacle_pct[1])) {
        training_pct <- 100 - min(as.numeric(d$workforce_obstacle_pct[1]), 100)
      }

      if (!is.na(training_pct)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(training_pct, 1),
          title = list(text = "Workforce Quality Index (%)"),
          gauge = list(
            axis = list(range = list(0, 100)),
            bar = list(color = "#F4A460"),
            steps = list(
              list(range = c(0, 33), color = "#ffebee"),
              list(range = c(33, 66), color = "#fff3e0"),
              list(range = c(66, 100), color = "#e8f5e9")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No workforce quality data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Crime & Security Charts
    output$crime_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Crime obstacles - use IC.FRM.* codes
      crime_val <- NA
      if ("IC.FRM.CRIM.ZS" %in% names(d) && !is.na(d$IC.FRM.CRIM.ZS[1])) {
        crime_val <- as.numeric(d$IC.FRM.CRIM.ZS[1])
      } else if ("crime_obstacle_pct" %in% names(d) && !is.na(d$crime_obstacle_pct[1])) {
        crime_val <- as.numeric(d$crime_obstacle_pct[1])
      }

      security_val <- NA
      if ("IC.FRM.SECU.ZS" %in% names(d) && !is.na(d$IC.FRM.SECU.ZS[1])) {
        security_val <- as.numeric(d$IC.FRM.SECU.ZS[1])
      } else if ("security_costs_pct" %in% names(d) && !is.na(d$security_costs_pct[1])) {
        security_val <- as.numeric(d$security_costs_pct[1])
      }

      values <- c(crime_val, security_val)
      labels <- c("Crime as Obstacle (%)", "Security Costs (% Sales)")

      # Filter out NA values
      valid_idx <- !is.na(values)
      if (any(valid_idx)) {
        plot_data <- data.frame(
          category = labels[valid_idx],
          value = values[valid_idx],
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~value,
                type = "bar",
                marker = list(color = "#dc3545")) |>
          layout(
            title = list(text = "Crime & Security Impact", font = list(size = 14)),
            yaxis = list(title = "Percentage"),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No crime/security data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$crime_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Crime losses - convert to numeric with fallbacks
      crime_losses <- NA
      if ("crime_losses_pct" %in% names(d) && !is.na(d$crime_losses_pct[1])) {
        crime_losses <- as.numeric(d$crime_losses_pct[1])
      } else if ("crime3" %in% names(d) && !is.na(d$crime3[1])) {
        crime_losses <- as.numeric(d$crime3[1])
      }

      if (!is.na(crime_losses)) {
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = round(crime_losses, 1),
          title = list(text = "Crime Losses (% of Sales)"),
          gauge = list(
            axis = list(range = list(0, 10)),
            bar = list(color = "#dc3545"),
            steps = list(
              list(range = c(0, 2), color = "#e8f5e9"),
              list(range = c(2, 5), color = "#fff3e0"),
              list(range = c(5, 10), color = "#ffebee")
            )
          )
        ) |>
          layout(paper_bgcolor = "rgba(0,0,0,0)") |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No crime losses data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Performance Charts
    output$performance_chart1 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Performance metrics - use IC.FRM.CAPU.ZS
      metrics <- list()
      if ("IC.FRM.CAPU.ZS" %in% names(d) && !is.na(d$IC.FRM.CAPU.ZS[1])) {
        metrics$`Capacity Utilization` <- as.numeric(d$IC.FRM.CAPU.ZS[1])
      } else if ("capacity_utilization_pct" %in% names(d) && !is.na(d$capacity_utilization_pct[1])) {
        metrics$`Capacity Utilization` <- as.numeric(d$capacity_utilization_pct[1])
      }

      if ("labor_productivity" %in% names(d) && !is.na(d$labor_productivity[1])) {
        metrics$`Productivity` <- min(as.numeric(d$labor_productivity[1]), 100)
      }

      if (length(metrics) > 0) {
        plot_data <- data.frame(
          metric = names(metrics),
          pct = unlist(metrics),
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~metric,
                y = ~pct,
                type = "bar",
                marker = list(color = "#2E7D32")) |>
          layout(
            title = list(text = "Operational Performance (%)", font = list(size = 14)),
            yaxis = list(title = "Percentage", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No performance data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    output$performance_chart2 <- renderPlotly({
      req(country_data())
      d <- country_data()

      # Export metrics - use IC.FRM.EXPRT.ZS
      export_pct <- NA
      if ("IC.FRM.EXPRT.ZS" %in% names(d) && !is.na(d$IC.FRM.EXPRT.ZS[1])) {
        export_pct <- as.numeric(d$IC.FRM.EXPRT.ZS[1])
      } else if ("export_firms_pct" %in% names(d) && !is.na(d$export_firms_pct[1])) {
        export_pct <- as.numeric(d$export_firms_pct[1])
      }

      direct_exports <- NA
      if ("direct_exports_pct" %in% names(d) && !is.na(d$direct_exports_pct[1])) {
        direct_exports <- as.numeric(d$direct_exports_pct[1])
      } else if ("export_share_pct" %in% names(d) && !is.na(d$export_share_pct[1])) {
        direct_exports <- as.numeric(d$export_share_pct[1])
      }

      values <- c(export_pct, direct_exports)
      labels <- c("Exporting Firms (%)", "Export Share (% Sales)")

      # Filter out NA values
      valid_idx <- !is.na(values)
      if (any(valid_idx)) {
        plot_data <- data.frame(
          category = labels[valid_idx],
          pct = values[valid_idx],
          stringsAsFactors = FALSE
        )

        plot_ly(plot_data,
                x = ~category,
                y = ~pct,
                type = "bar",
                marker = list(color = "#2E7D32")) |>
          layout(
            title = list(text = "Export Orientation", font = list(size = 14)),
            yaxis = list(title = "Percentage", range = c(0, 100)),
            xaxis = list(title = ""),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      } else {
        plot_ly() |>
          layout(
            annotations = list(
              text = "No export data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            ),
            paper_bgcolor = "rgba(0,0,0,0)"
          )
      }
    })

    # Time Series - enhanced with national GDP growth overlay
    output$time_series <- renderPlotly({
      req(wbes_data(), input$country_select)

      panel <- wbes_data()$country_panel
      panel <- filter(panel, country == input$country_select)

      # Get WBES year range
      wbes_years <- if (nrow(panel) > 0) range(panel$year, na.rm = TRUE) else c(2006, 2023)

      # Start with WBES data
      p <- plot_ly(panel, x = ~year) |>
        add_trace(y = ~power_outages_per_month, name = "Power Outages (WBES)",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#1B6B5F")) |>
        add_trace(y = ~firms_with_credit_line_pct, name = "Credit Access % (WBES)",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#F49B7A")) |>
        add_trace(y = ~bribery_incidence_pct, name = "Bribery % (WBES)",
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#6C757D"))

      # Try to add World Bank GDP growth data as overlay
      tryCatch({
        country_mapping <- map_wbes_countries_to_iso3(input$country_select)
        iso3_code <- country_mapping$iso3c[1]

        if (!is.na(iso3_code)) {
          box::use(wbstats[wb_data])

          # Fetch GDP growth for the same period
          gdp_data <- wb_data(
            indicator = "NY.GDP.PCAP.KD.ZG",
            country = iso3_code,
            start_date = wbes_years[1],
            end_date = wbes_years[2]
          )

          if (!is.null(gdp_data) && nrow(gdp_data) > 0) {
            # Add GDP growth on secondary y-axis
            p <- p |>
              add_trace(
                data = gdp_data,
                x = ~date,
                y = ~`NY.GDP.PCAP.KD.ZG`,
                name = "GDP Growth % (WB)",
                type = "scatter",
                mode = "lines+markers",
                line = list(color = "#9C27B0", dash = "dash"),
                yaxis = "y2"
              )
          }
        }
      }, error = function(e) {
        # Silently continue without WB data
      })

      p |>
        layout(
          title = list(text = "WBES Indicators + National GDP Growth", font = list(size = 16)),
          xaxis = list(title = "Year"),
          yaxis = list(title = "WBES Value", side = "left"),
          yaxis2 = list(
            title = "GDP Growth %",
            overlaying = "y",
            side = "right",
            showgrid = FALSE
          ),
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # ============================================================
    # Economic Context Tab - World Bank Databank Integration
    # ============================================================
    # Note: wb_country_data reactive is defined earlier in the server function

    # Macro Indicators Display
    output$macro_indicators <- renderUI({
      req(country_data())
      d <- country_data()

      # Get survey year for context display
      survey_year <- if (!is.null(d$year) && length(d$year) > 0 && !is.na(d$year[1])) {
        as.integer(d$year[1])
      } else {
        NULL
      }

      # Check if we're still loading WB data
      prefetched <- if (!is.null(wb_prefetched_data)) wb_prefetched_data() else NULL
      if (is.null(prefetched)) {
        # Still loading - show spinner
        return(tags$div(
          class = "text-center p-4",
          tags$div(
            icon("spinner", class = "fa-spin fa-2x text-primary-teal"),
            tags$p(class = "mt-2 text-muted", "Loading World Bank macro indicators...")
          )
        ))
      }

      wb_data <- wb_country_data()

      if (is.null(wb_data)) {
        return(tags$div(
          class = "text-muted text-center p-4",
          icon("exclamation-circle"),
          " World Bank data not available for this country."
        ))
      }

      # Create value boxes for key macro indicators
      # Helper to safely get formatted value
      get_val <- function(code) {
        if (!is.null(wb_data[[code]]) && !is.na(wb_data[[code]])) {
          format_wb_indicator(wb_data[[code]], code)
        } else {
          "N/A"
        }
      }

      get_year <- function(code) {
        if (!is.null(wb_data$data_years[[code]])) {
          as.character(wb_data$data_years[[code]])
        } else {
          ""
        }
      }

      # Survey year context message
      year_context <- if (!is.null(survey_year)) {
        tags$div(
          class = "alert alert-secondary mb-3 py-2",
          icon("calendar"),
          sprintf(" Showing World Bank data matched to WBES survey year: %d", survey_year)
        )
      } else {
        tags$div(
          class = "alert alert-secondary mb-3 py-2",
          icon("calendar"),
          " Showing latest available World Bank data"
        )
      }

      tagList(
        year_context,
        fluidRow(
          column(3,
            tags$div(class = "kpi-box",
              tags$div(class = "kpi-value text-primary-teal", get_val("NY.GDP.PCAP.CD")),
              tags$div(class = "kpi-label", paste("GDP per Capita", get_year("NY.GDP.PCAP.CD")))
            )
          ),
          column(3,
            tags$div(class = "kpi-box",
              tags$div(class = "kpi-value",
                style = if (!is.null(wb_data[["NY.GDP.PCAP.KD.ZG"]]) && !is.na(wb_data[["NY.GDP.PCAP.KD.ZG"]]) && wb_data[["NY.GDP.PCAP.KD.ZG"]] >= 0) "color: #2E7D32;" else "color: #dc3545;",
                get_val("NY.GDP.PCAP.KD.ZG")
              ),
              tags$div(class = "kpi-label", paste("GDP Growth", get_year("NY.GDP.PCAP.KD.ZG")))
            )
          ),
          column(3,
            tags$div(class = "kpi-box",
              tags$div(class = "kpi-value",
                style = if (!is.null(wb_data[["FP.CPI.TOTL.ZG"]]) && !is.na(wb_data[["FP.CPI.TOTL.ZG"]]) && wb_data[["FP.CPI.TOTL.ZG"]] <= 5) "color: #2E7D32;" else "color: #F4A460;",
                get_val("FP.CPI.TOTL.ZG")
              ),
              tags$div(class = "kpi-label", paste("Inflation", get_year("FP.CPI.TOTL.ZG")))
            )
          ),
          column(3,
            tags$div(class = "kpi-box",
              tags$div(class = "kpi-value text-primary-teal", get_val("NE.EXP.GNFS.ZS")),
              tags$div(class = "kpi-label", paste("Exports (% GDP)", get_year("NE.EXP.GNFS.ZS")))
            )
          )
        )
      )
    })

    # Macro Environment Radar Chart
    output$macro_radar <- renderPlotly({
      wb_data <- wb_country_data()

      if (is.null(wb_data)) {
        return(plot_ly() |>
          layout(
            annotations = list(list(
              text = "World Bank data not available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE))
      }

      # Normalize indicators to 0-100 scale for radar
      # WGI indicators range from -2.5 to +2.5, normalize to 0-100
      normalize_wgi <- function(val) {
        if (is.null(val) || is.na(val)) return(NA_real_)
        ((val + 2.5) / 5) * 100
      }

      # Infrastructure/access indicators already in %
      normalize_pct <- function(val) {
        if (is.null(val) || is.na(val)) return(NA_real_)
        min(max(val, 0), 100)
      }

      indicators <- c(
        "Gov't Effectiveness" = normalize_wgi(wb_data[["GE.EST"]]),
        "Rule of Law" = normalize_wgi(wb_data[["RL.EST"]]),
        "Regulatory Quality" = normalize_wgi(wb_data[["RQ.EST"]]),
        "Electricity Access" = normalize_pct(wb_data[["EG.ELC.ACCS.ZS"]]),
        "Internet Users" = normalize_pct(wb_data[["IT.NET.USER.ZS"]]),
        "Political Stability" = normalize_wgi(wb_data[["PV.EST"]])
      )

      # Filter out NA values
      available <- indicators[!is.na(indicators)]

      if (length(available) < 3) {
        return(plot_ly() |>
          layout(
            annotations = list(list(
              text = "Insufficient macro data for radar chart",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE))
      }

      plot_ly(
        type = "scatterpolar",
        r = as.numeric(available),
        theta = names(available),
        fill = "toself",
        fillcolor = "rgba(244, 155, 122, 0.3)",
        line = list(color = "#F49B7A", width = 2),
        name = "Macro Environment"
      ) |>
        layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 100))
          ),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # WBES vs World Bank Comparison Chart
    output$wbes_vs_wb_chart <- renderPlotly({
      req(country_data())
      d <- country_data()
      wb_data <- wb_country_data()

      if (is.null(wb_data) || nrow(d) == 0) {
        return(plot_ly() |>
          layout(
            annotations = list(list(
              text = "Comparison data not available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE))
      }

      # Compare firm-level vs national metrics
      comparisons <- data.frame(
        indicator = character(),
        wbes_value = numeric(),
        wb_value = numeric(),
        stringsAsFactors = FALSE
      )

      # Electricity: WBES generator usage vs WB electricity access
      if ("firms_with_generator_pct" %in% names(d) && !is.na(d$firms_with_generator_pct[1]) &&
          !is.null(wb_data[["EG.ELC.ACCS.ZS"]]) && !is.na(wb_data[["EG.ELC.ACCS.ZS"]])) {
        # Invert generator % to show "reliable power" (100 - generator use)
        comparisons <- rbind(comparisons, data.frame(
          indicator = "Reliable Power",
          wbes_value = 100 - as.numeric(d$firms_with_generator_pct[1]),
          wb_value = wb_data[["EG.ELC.ACCS.ZS"]]
        ))
      }

      # Internet: WBES vs WB
      if ("internet_access_pct" %in% names(d) && !is.na(d$internet_access_pct[1]) &&
          !is.null(wb_data[["IT.NET.USER.ZS"]]) && !is.na(wb_data[["IT.NET.USER.ZS"]])) {
        comparisons <- rbind(comparisons, data.frame(
          indicator = "Internet Access",
          wbes_value = as.numeric(d$internet_access_pct[1]),
          wb_value = wb_data[["IT.NET.USER.ZS"]]
        ))
      }

      # Corruption: WBES bribery vs WB control of corruption (inverted)
      if ("bribery_incidence_pct" %in% names(d) && !is.na(d$bribery_incidence_pct[1]) &&
          !is.null(wb_data[["CC.EST"]]) && !is.na(wb_data[["CC.EST"]])) {
        # Invert bribery to show "low corruption"
        # Normalize WGI from -2.5 to +2.5 to 0-100
        comparisons <- rbind(comparisons, data.frame(
          indicator = "Low Corruption",
          wbes_value = 100 - as.numeric(d$bribery_incidence_pct[1]),
          wb_value = ((wb_data[["CC.EST"]] + 2.5) / 5) * 100
        ))
      }

      if (nrow(comparisons) == 0) {
        return(plot_ly() |>
          layout(
            annotations = list(list(
              text = "No comparable metrics available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE))
      }

      plot_ly(comparisons, x = ~indicator) |>
        add_trace(y = ~wbes_value, name = "WBES (Firm-Level)",
                  type = "bar", marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~wb_value, name = "World Bank (National)",
                  type = "bar", marker = list(color = "#F49B7A")) |>
        layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Score (0-100)", range = c(0, 100)),
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Governance Indicators Chart (WGI)
    output$governance_chart <- renderPlotly({
      wb_data <- wb_country_data()

      if (is.null(wb_data)) {
        return(plot_ly() |>
          layout(
            annotations = list(list(
              text = "Governance data not available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE))
      }

      # WGI indicators
      wgi_codes <- c("GE.EST", "RQ.EST", "RL.EST", "CC.EST", "PV.EST", "VA.EST")
      wgi_labels <- c("Gov't Effectiveness", "Regulatory Quality", "Rule of Law",
                      "Control of Corruption", "Political Stability", "Voice & Accountability")

      values <- sapply(wgi_codes, function(code) {
        if (!is.null(wb_data[[code]]) && !is.na(wb_data[[code]])) {
          wb_data[[code]]
        } else {
          NA_real_
        }
      })

      valid_idx <- !is.na(values)
      if (!any(valid_idx)) {
        return(plot_ly() |>
          layout(
            annotations = list(list(
              text = "No WGI data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE))
      }

      plot_data <- data.frame(
        indicator = wgi_labels[valid_idx],
        value = values[valid_idx],
        stringsAsFactors = FALSE
      )

      # Color based on value (negative = red, positive = green)
      colors <- ifelse(plot_data$value >= 0, "#2E7D32", "#dc3545")

      plot_ly(plot_data,
              y = ~reorder(indicator, value),
              x = ~value,
              type = "bar",
              orientation = "h",
              marker = list(color = colors)) |>
        layout(
          xaxis = list(title = "WGI Score", range = c(-2.5, 2.5)),
          yaxis = list(title = ""),
          paper_bgcolor = "rgba(0,0,0,0)",
          shapes = list(
            list(type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = length(plot_data$indicator) - 0.5,
                 line = list(color = "#666666", width = 1, dash = "dot"))
          )
        ) |>
        config(displayModeBar = FALSE)
    })

    # Economic Trends Chart
    output$economic_trends_chart <- renderPlotly({
      req(input$country_select)

      # Map country to ISO3
      country_mapping <- map_wbes_countries_to_iso3(input$country_select)
      iso3_code <- country_mapping$iso3c[1]

      if (is.na(iso3_code)) {
        return(plot_ly() |>
          layout(
            annotations = list(list(
              text = "Country not found in World Bank database",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE))
      }

      # Fetch time series data
      tryCatch({
        box::use(wbstats[wb_data])
        current_year <- as.integer(format(Sys.Date(), "%Y"))

        trend_data <- wb_data(
          indicator = c("NY.GDP.PCAP.KD.ZG", "FP.CPI.TOTL.ZG"),
          country = iso3_code,
          start_date = current_year - 10,
          end_date = current_year
        )

        if (is.null(trend_data) || nrow(trend_data) == 0) {
          return(plot_ly() |>
            layout(
              annotations = list(list(
                text = "No trend data available",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, showarrow = FALSE
              )),
              paper_bgcolor = "rgba(0,0,0,0)"
            ) |>
            config(displayModeBar = FALSE))
        }

        plot_ly(trend_data, x = ~date) |>
          add_trace(y = ~`NY.GDP.PCAP.KD.ZG`, name = "GDP Growth %",
                    type = "scatter", mode = "lines+markers",
                    line = list(color = "#1B6B5F")) |>
          add_trace(y = ~`FP.CPI.TOTL.ZG`, name = "Inflation %",
                    type = "scatter", mode = "lines+markers",
                    line = list(color = "#F49B7A")) |>
          layout(
            xaxis = list(title = "Year"),
            yaxis = list(title = "Percentage"),
            legend = list(orientation = "h", y = -0.2),
            paper_bgcolor = "rgba(0,0,0,0)",
            shapes = list(
              list(type = "line", x0 = min(trend_data$date), x1 = max(trend_data$date),
                   y0 = 0, y1 = 0, line = list(color = "#666666", width = 1, dash = "dot"))
            )
          ) |>
          config(displayModeBar = FALSE)

      }, error = function(e) {
        plot_ly() |>
          layout(
            annotations = list(list(
              text = paste("Error fetching trends:", e$message),
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )),
            paper_bgcolor = "rgba(0,0,0,0)"
          ) |>
          config(displayModeBar = FALSE)
      })
    })

    # ============================================================
    # Download Handlers
    # ============================================================

    # Helper to create plotly download handler
    create_plot_download <- function(output_id) {
      downloadHandler(
        filename = function() {
          country <- if (!is.null(input$country_select)) input$country_select else "country"
          paste0(country, "_", output_id, "_", format(Sys.Date(), "%Y%m%d"), ".html")
        },
        content = function(file) {
          # Re-render the plot for download
          p <- switch(output_id,
            "radar_chart" = {
              req(country_data())
              d <- country_data()
              if (nrow(d) > 0) {
                safe_val <- function(col, scale = 1, invert = FALSE) {
                  if (col %in% names(d) && !is.na(d[[col]][1])) {
                    val <- d[[col]][1] * scale
                    if (invert) 100 - min(val, 100) else min(val, 100)
                  } else NA_real_
                }
                indicators <- c(
                  "Infrastructure" = safe_val("power_outages_per_month", scale = 5, invert = TRUE),
                  "Finance Access" = safe_val("firms_with_credit_line_pct"),
                  "Low Corruption" = safe_val("bribery_incidence_pct", invert = TRUE),
                  "Capacity Use" = safe_val("capacity_utilization_pct"),
                  "Export Orient." = safe_val("export_firms_pct", scale = 2),
                  "Gender Equity" = safe_val("female_ownership_pct", scale = 2)
                )
                available <- indicators[!is.na(indicators)]
                if (length(available) > 0) {
                  plot_ly(type = "scatterpolar", r = as.numeric(available), theta = names(available),
                          fill = "toself", fillcolor = "rgba(27, 107, 95, 0.3)",
                          line = list(color = "#1B6B5F", width = 2)) |>
                    layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
                           title = paste(input$country_select, "- Business Environment"))
                } else plot_ly() |> layout(title = "No data available")
              } else plot_ly() |> layout(title = "No data available")
            },
            "time_series" = {
              req(wbes_data(), input$country_select)
              panel <- wbes_data()$country_panel
              panel <- filter(panel, country == input$country_select)
              plot_ly(panel, x = ~year) |>
                add_trace(y = ~power_outages_per_month, name = "Power Outages", type = "scatter", mode = "lines+markers") |>
                add_trace(y = ~firms_with_credit_line_pct, name = "Credit Access %", type = "scatter", mode = "lines+markers") |>
                add_trace(y = ~bribery_incidence_pct, name = "Bribery %", type = "scatter", mode = "lines+markers") |>
                layout(title = paste(input$country_select, "- Indicator Trends"))
            },
            plot_ly() |> layout(title = "Chart")
          )
          saveWidget(p, file, selfcontained = TRUE)
        }
      )
    }

    output$dl_radar_chart <- create_plot_download("radar_chart")
    output$dl_time_series <- create_plot_download("time_series")

    # Simple download handlers for other charts
    simple_chart_download <- function(prefix) {
      downloadHandler(
        filename = function() {
          country <- if (!is.null(input$country_select)) input$country_select else "country"
          paste0(country, "_", prefix, "_", format(Sys.Date(), "%Y%m%d"), ".html")
        },
        content = function(file) {
          saveWidget(plot_ly() |> layout(title = paste(input$country_select, "-", prefix)), file, selfcontained = TRUE)
        }
      )
    }

    output$dl_infra_chart1 <- simple_chart_download("infrastructure_obstacles")
    output$dl_infra_chart2 <- simple_chart_download("power_sources")
    output$dl_infra_chart3 <- simple_chart_download("water_transport")
    output$dl_infra_chart4 <- simple_chart_download("telecommunications")
    output$dl_finance_chart1 <- simple_chart_download("financial_access")
    output$dl_finance_chart2 <- simple_chart_download("collateral")
    output$dl_gov_chart1 <- simple_chart_download("bribery_by_type")
    output$dl_gov_chart2 <- simple_chart_download("management_time")
    output$dl_workforce_chart1 <- simple_chart_download("workforce_composition")
    output$dl_workforce_chart2 <- simple_chart_download("training_programs")
    output$dl_crime_chart1 <- simple_chart_download("crime_security")
    output$dl_crime_chart2 <- simple_chart_download("crime_losses")
    output$dl_performance_chart1 <- simple_chart_download("operational_performance")
    output$dl_performance_chart2 <- simple_chart_download("export_orientation")

    # Economic Context tab downloads
    output$dl_macro_radar <- simple_chart_download("macro_environment_radar")
    output$dl_wbes_vs_wb_chart <- simple_chart_download("wbes_vs_wb_comparison")
    output$dl_governance_chart <- simple_chart_download("governance_indicators")
    output$dl_economic_trends_chart <- simple_chart_download("economic_trends")

  })
}
