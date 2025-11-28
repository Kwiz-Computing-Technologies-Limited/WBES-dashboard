# app/view/custom_analysis.R
# Custom Analysis Configuration & Report Generator Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h3, h4, h5, p, hr,
        fluidRow, column, selectInput, checkboxGroupInput, sliderInput,
        actionButton, downloadButton, renderUI, uiOutput, observeEvent,
        renderPlot, plotOutput, textInput, dateInput, radioButtons, downloadHandler],
  bslib[card, card_header, card_body, accordion, accordion_panel],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, desc, mutate, group_by, summarise, across, select, pull],
  stats[setNames],
  ggplot2[ggplot, aes, geom_bar, geom_point, geom_line, geom_boxplot,
          coord_flip, theme_minimal, theme, labs, element_text, ggtitle,
          facet_wrap, scale_fill_manual, scale_color_manual],
  tidyr[pivot_longer],
  DT[dataTableOutput, renderDataTable, datatable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(column(12, h2(icon("cogs"), " Custom Analysis & Report Builder", class = "text-primary mb-4"))),

    # Configuration Panel
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("sliders-h"), " Analysis Configuration"),
          card_body(
            accordion(
              id = ns("config_accordion"),

              # Data Selection
              accordion_panel(
                title = "1. Data Selection",
                icon = icon("database"),
                fluidRow(
                  column(6,
                    selectInput(ns("countries"), "Select Countries",
                      choices = c("All" = "all"),
                      multiple = TRUE,
                      selected = "all")
                  ),
                  column(6,
                    selectInput(ns("regions"), "Select Regions",
                      choices = c("All" = "all"),
                      multiple = TRUE,
                      selected = "all")
                  )
                ),
                fluidRow(
                  column(6,
                    selectInput(ns("income_groups"), "Income Groups",
                      choices = c("All" = "all"),
                      multiple = TRUE,
                      selected = "all")
                  ),
                  column(6,
                    sliderInput(ns("year_range"), "Year Range",
                      min = 2015, max = 2024, value = c(2019, 2024), step = 1, sep = "")
                  )
                )
              ),

              # Indicator Selection
              accordion_panel(
                title = "2. Indicator Selection",
                icon = icon("check-square"),
                h4("Select indicators for your analysis:"),
                fluidRow(
                  column(3,
                    h5(icon("bolt"), " Infrastructure"),
                    checkboxGroupInput(ns("infra_indicators"), NULL,
                      choices = c(
                        "Power Outages" = "IC.FRM.OUTG.ZS",
                        "Electricity Obstacle" = "IC.FRM.ELEC.ZS",
                        "Infrastructure Obstacle" = "IC.FRM.INFRA.ZS"
                      ))
                  ),
                  column(3,
                    h5(icon("university"), " Finance"),
                    checkboxGroupInput(ns("finance_indicators"), NULL,
                      choices = c(
                        "Finance Obstacle" = "IC.FRM.FINA.ZS",
                        "Bank Account" = "IC.FRM.BANK.ZS",
                        "Credit Constraint" = "IC.FRM.CRED.ZS"
                      ))
                  ),
                  column(3,
                    h5(icon("balance-scale"), " Governance"),
                    checkboxGroupInput(ns("governance_indicators"), NULL,
                      choices = c(
                        "Corruption Obstacle" = "IC.FRM.CORR.ZS",
                        "Bribery Incidence" = "IC.FRM.BRIB.ZS",
                        "Crime Obstacle" = "IC.FRM.CRIM.ZS",
                        "Security Costs" = "IC.FRM.SECU.ZS"
                      ))
                  ),
                  column(3,
                    h5(icon("users"), " Workforce & Performance"),
                    checkboxGroupInput(ns("workforce_indicators"), NULL,
                      choices = c(
                        "Workforce Obstacle" = "IC.FRM.WKFC.ZS",
                        "Female Workers" = "IC.FRM.FEMW.ZS",
                        "Female Ownership" = "IC.FRM.FEMO.ZS",
                        "Capacity Utilization" = "IC.FRM.CAPU.ZS",
                        "Export Participation" = "IC.FRM.EXPRT.ZS"
                      ))
                  )
                )
              ),

              # Analysis Type
              accordion_panel(
                title = "3. Analysis Type",
                icon = icon("chart-bar"),
                radioButtons(ns("analysis_type"), "Select Analysis Type:",
                  choices = c(
                    "Country Comparison" = "country_comparison",
                    "Regional Analysis" = "regional",
                    "Income Group Analysis" = "income",
                    "Time Trend Analysis" = "trend",
                    "Correlation Analysis" = "correlation",
                    "Baseline Profile" = "baseline"
                  )),
                checkboxGroupInput(ns("viz_types"), "Visualizations to Include:",
                  choices = c(
                    "Bar Charts" = "bar",
                    "Line Charts" = "line",
                    "Box Plots" = "box",
                    "Scatter Plots" = "scatter",
                    "Heatmaps" = "heatmap",
                    "Summary Table" = "table"
                  ),
                  selected = c("bar", "table"))
              ),

              # Report Settings
              accordion_panel(
                title = "4. Report Settings",
                icon = icon("file-pdf"),
                fluidRow(
                  column(6,
                    textInput(ns("report_title"), "Report Title",
                      value = "WBES Business Environment Analysis",
                      placeholder = "Enter report title")
                  ),
                  column(6,
                    textInput(ns("report_author"), "Author/Organization",
                      value = "Kwiz Research",
                      placeholder = "Enter author name")
                  )
                ),
                fluidRow(
                  column(12,
                    tags$textarea(
                      id = ns("report_notes"),
                      class = "form-control",
                      rows = 3,
                      placeholder = "Add any notes or context for this analysis..."
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),

    # Action Buttons
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(class = "py-2 text-center",
            actionButton(ns("generate_analysis"), "Generate Analysis",
              icon = icon("play"), class = "btn-primary btn-lg me-2"),
            downloadButton(ns("download_report"), "Download PDF Report",
              class = "btn-success btn-lg me-2"),
            downloadButton(ns("download_data"), "Download Filtered Data (CSV)",
              class = "btn-info btn-lg"),
            tags$div(
              class = "mt-2",
              uiOutput(ns("status_message"))
            )
          )
        )
      )
    ),

    # Results Panel
    fluidRow(
      column(12,
        card(
          card_header(icon("chart-area"), " Analysis Results"),
          card_body(
            uiOutput(ns("analysis_results"))
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
      d <- wbes_data()$latest

      countries <- c("All" = "all", setNames(unique(d$country), unique(d$country)))
      regions <- c("All" = "all", setNames(unique(d$region), unique(d$region)))
      incomes <- c("All" = "all", setNames(unique(d$income_group), unique(d$income_group)))

      shiny::updateSelectInput(session, "countries", choices = countries)
      shiny::updateSelectInput(session, "regions", choices = regions)
      shiny::updateSelectInput(session, "income_groups", choices = incomes)
    })

    # Filtered data based on user selection
    filtered_data <- reactive({
      req(wbes_data())
      d <- wbes_data()$latest

      # Apply filters
      if (!("all" %in% input$countries) && length(input$countries) > 0) {
        d <- filter(d, country %in% input$countries)
      }
      if (!("all" %in% input$regions) && length(input$regions) > 0) {
        d <- filter(d, region %in% input$regions)
      }
      if (!("all" %in% input$income_groups) && length(input$income_groups) > 0) {
        d <- filter(d, income_group %in% input$income_groups)
      }

      d
    })

    # Selected indicators
    selected_indicators <- reactive({
      c(
        input$infra_indicators,
        input$finance_indicators,
        input$governance_indicators,
        input$workforce_indicators
      )
    })

    # Status message
    output$status_message <- renderUI({
      n_countries <- nrow(filtered_data())
      n_indicators <- length(selected_indicators())

      if (n_indicators == 0) {
        return(tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"), " Please select at least one indicator"
        ))
      }

      tags$div(
        class = "text-muted",
        paste0("Ready to analyze ", n_indicators, " indicators across ", n_countries, " countries")
      )
    })

    # Generate analysis
    observeEvent(input$generate_analysis, {
      req(filtered_data(), selected_indicators())

      if (length(selected_indicators()) == 0) {
        output$analysis_results <- renderUI({
          tags$div(
            class = "alert alert-warning",
            icon("exclamation-triangle"), " Please select at least one indicator to analyze"
          )
        })
        return(NULL)
      }

      output$analysis_results <- renderUI({
        generate_analysis_output(
          filtered_data(),
          selected_indicators(),
          input$analysis_type,
          input$viz_types
        )
      })
    })

    # Helper function to generate analysis output
    generate_analysis_output <- function(data, indicators, analysis_type, viz_types) {

      result_panels <- list()

      # Summary Statistics
      result_panels[[length(result_panels) + 1]] <- div(
        class = "mb-4",
        h3(icon("info-circle"), " Summary Statistics"),
        uiOutput(session$ns("summary_stats"))
      )

      # Generate visualizations based on selected types
      if ("bar" %in% viz_types) {
        result_panels[[length(result_panels) + 1]] <- div(
          class = "mb-4",
          h3(icon("chart-bar"), " Country Comparison"),
          plotlyOutput(session$ns("bar_chart_output"), height = "500px")
        )
      }

      if ("box" %in% viz_types) {
        result_panels[[length(result_panels) + 1]] <- div(
          class = "mb-4",
          h3(icon("chart-line"), " Distribution Analysis"),
          plotlyOutput(session$ns("box_plot_output"), height = "400px")
        )
      }

      if ("scatter" %in% viz_types && length(indicators) >= 2) {
        result_panels[[length(result_panels) + 1]] <- div(
          class = "mb-4",
          h3(icon("project-diagram"), " Correlation Analysis"),
          plotlyOutput(session$ns("scatter_output"), height = "400px")
        )
      }

      if ("heatmap" %in% viz_types) {
        result_panels[[length(result_panels) + 1]] <- div(
          class = "mb-4",
          h3(icon("th"), " Indicator Heatmap"),
          plotlyOutput(session$ns("heatmap_output"), height = "500px")
        )
      }

      if ("table" %in% viz_types) {
        result_panels[[length(result_panels) + 1]] <- div(
          class = "mb-4",
          h3(icon("table"), " Detailed Data Table"),
          dataTableOutput(session$ns("data_table_output"))
        )
      }

      # Render outputs
      output$summary_stats <- renderUI({
        generate_summary_stats(data, indicators)
      })

      output$bar_chart_output <- renderPlotly({
        generate_bar_chart(data, indicators[1])
      })

      output$box_plot_output <- renderPlotly({
        generate_box_plot(data, indicators)
      })

      if (length(indicators) >= 2) {
        output$scatter_output <- renderPlotly({
          generate_scatter_plot(data, indicators[1], indicators[2])
        })
      }

      output$heatmap_output <- renderPlotly({
        generate_heatmap(data, indicators)
      })

      output$data_table_output <- renderDataTable({
        cols <- c("country", "region", "income_group", indicators)
        cols <- cols[cols %in% names(data)]
        datatable(
          data[, cols],
          options = list(pageLength = 20, scrollX = TRUE),
          rownames = FALSE
        )
      })

      do.call(div, result_panels)
    }

    # Helper functions for visualizations
    generate_summary_stats <- function(data, indicators) {
      stats <- lapply(indicators, function(ind) {
        if (ind %in% names(data)) {
          val <- data[[ind]]
          data.frame(
            Indicator = ind,
            Mean = round(mean(val, na.rm = TRUE), 2),
            Median = round(median(val, na.rm = TRUE), 2),
            SD = round(sd(val, na.rm = TRUE), 2),
            Min = round(min(val, na.rm = TRUE), 2),
            Max = round(max(val, na.rm = TRUE), 2)
          )
        }
      })
      stats_df <- do.call(rbind, stats)

      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-striped table-hover",
          tags$thead(
            tags$tr(
              lapply(names(stats_df), function(col) tags$th(col))
            )
          ),
          tags$tbody(
            lapply(1:nrow(stats_df), function(i) {
              tags$tr(
                lapply(stats_df[i, ], function(val) tags$td(val))
              )
            })
          )
        )
      )
    }

    generate_bar_chart <- function(data, indicator) {
      if (!indicator %in% names(data)) return(NULL)

      d <- data |>
        arrange(desc(.data[[indicator]])) |>
        head(20)

      d$country <- factor(d$country, levels = rev(d$country))

      plot_ly(d, y = ~country, x = ~get(indicator), type = "bar",
              orientation = "h",
              marker = list(color = ~get(indicator),
                           colorscale = list(c(0, "#2E7D32"), c(0.5, "#F4A460"), c(1, "#dc3545")))) |>
        layout(
          xaxis = list(title = indicator),
          yaxis = list(title = ""),
          margin = list(l = 150),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    }

    generate_box_plot <- function(data, indicators) {
      if (length(indicators) == 0) return(NULL)

      # Use first indicator for box plot by region
      ind <- indicators[1]
      if (!ind %in% names(data)) return(NULL)

      plot_ly(data, y = ~get(ind), x = ~region, type = "box",
              marker = list(color = "#1B6B5F")) |>
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ind),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    }

    generate_scatter_plot <- function(data, ind1, ind2) {
      if (!ind1 %in% names(data) || !ind2 %in% names(data)) return(NULL)

      plot_ly(data, x = ~get(ind1), y = ~get(ind2),
              type = "scatter", mode = "markers",
              text = ~country,
              marker = list(size = 10, color = ~region, opacity = 0.7)) |>
        layout(
          xaxis = list(title = ind1),
          yaxis = list(title = ind2),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    }

    generate_heatmap <- function(data, indicators) {
      if (length(indicators) == 0) return(NULL)

      # Filter to indicators that exist
      indicators <- indicators[indicators %in% names(data)]
      if (length(indicators) == 0) return(NULL)

      # Select top 15 countries and indicators
      d <- data[1:min(15, nrow(data)), ]
      mat <- as.matrix(d[, indicators, drop = FALSE])

      plot_ly(x = indicators, y = d$country, z = mat, type = "heatmap",
              colorscale = list(c(0, "#e8f5e9"), c(0.5, "#fff3e0"), c(1, "#ffebee"))) |>
        layout(
          margin = list(l = 150),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    }

    # Download handlers
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("wbes_custom_analysis_", Sys.Date(), ".csv")
      },
      content = function(file) {
        d <- filtered_data()
        indicators <- selected_indicators()
        cols <- c("country", "region", "income_group", indicators)
        cols <- cols[cols %in% names(d)]
        write.csv(d[, cols], file, row.names = FALSE)
      }
    )

    output$download_report <- downloadHandler(
      filename = function() {
        paste0("wbes_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        # Generate HTML report (PDF requires additional setup)
        html_content <- generate_html_report(
          filtered_data(),
          selected_indicators(),
          input$report_title,
          input$report_author
        )
        writeLines(html_content, file)
      }
    )

    # HTML report generator
    generate_html_report <- function(data, indicators, title, author) {
      paste0(
        "<!DOCTYPE html><html><head>",
        "<title>", title, "</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 40px; }",
        "h1 { color: #1B6B5F; }",
        "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
        "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
        "th { background-color: #1B6B5F; color: white; }",
        "</style></head><body>",
        "<h1>", title, "</h1>",
        "<p><strong>Author:</strong> ", author, "</p>",
        "<p><strong>Date:</strong> ", Sys.Date(), "</p>",
        "<p><strong>Countries Analyzed:</strong> ", nrow(data), "</p>",
        "<p><strong>Indicators:</strong> ", length(indicators), "</p>",
        "<hr>",
        "<h2>Summary Statistics</h2>",
        "<p>This report provides a comprehensive analysis of business environment indicators.</p>",
        "</body></html>"
      )
    }

  })
}
