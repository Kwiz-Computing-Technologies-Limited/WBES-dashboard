# app/view/finance_access.R
# Access to Finance Analysis Module

box::use(
  shiny[moduleServer, NS, reactive, req, tags, div, icon, h2, h4, p,
        fluidRow, column, selectInput, renderUI, uiOutput, observeEvent],
  bslib[card, card_header, card_body],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, config],
  dplyr[filter, arrange, desc, mutate, group_by, summarise]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    fluidRow(column(12, h2(icon("university"), " Access to Finance", class = "text-primary mb-4"))),

    # KPIs
    fluidRow(
      class = "mb-4",
      column(3, uiOutput(ns("kpi_finance"))),
      column(3, uiOutput(ns("kpi_bank"))),
      column(3, uiOutput(ns("kpi_credit"))),
      column(3, uiOutput(ns("kpi_female")))
    ),

    # Filters
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_body(class = "py-2",
            fluidRow(
              column(4, selectInput(ns("region"), "Region", choices = c("All" = "all"))),
              column(4, selectInput(ns("firm_size"), "Firm Size", choices = c("All" = "all"))),
              column(4, selectInput(ns("indicator"), "Indicator",
                choices = c("Finance Obstacle" = "IC.FRM.FINA.ZS",
                           "Bank Account" = "IC.FRM.BANK.ZS",
                           "Credit Constraint" = "IC.FRM.CRED.ZS")))
            )
          )
        )
      )
    ),

    # Charts
    fluidRow(
      class = "mb-4",
      column(6,
        card(
          card_header(icon("credit-card"), " Finance Access by Region"),
          card_body(plotlyOutput(ns("regional_bar"), height = "400px"))
        )
      ),
      column(6,
        card(
          card_header(icon("chart-pie"), " Reasons for Not Applying"),
          card_body(plotlyOutput(ns("reasons_pie"), height = "400px"))
        )
      )
    ),

    # SME Gap and Gender
    fluidRow(
      class = "mb-4",
      column(8,
        card(
          card_header(icon("chart-bar"), " Finance Gap by Country"),
          card_body(plotlyOutput(ns("gap_chart"), height = "350px"))
        )
      ),
      column(4,
        card(
          card_header(icon("venus"), " Gender Gap"),
          card_body(plotlyOutput(ns("gender_gap"), height = "350px"))
        )
      )
    )
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {

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

    # KPIs
    output$kpi_finance <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.FINA.ZS, na.rm = TRUE), 1)
      div(class = "card bg-primary text-white h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Finance Obstacle")))
    })

    output$kpi_bank <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.BANK.ZS, na.rm = TRUE), 1)
      div(class = "card bg-success text-white h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Bank Account")))
    })

    output$kpi_credit <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.CRED.ZS, na.rm = TRUE), 1)
      div(class = "card bg-secondary text-white h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Credit Constraint")))
    })

    output$kpi_female <- renderUI({
      req(filtered())
      val <- round(mean(filtered()$IC.FRM.FEMO.ZS, na.rm = TRUE), 1)
      div(class = "card bg-info text-white h-100",
        div(class = "card-body text-center", h2(paste0(val, "%")), p("Female Ownership")))
    })

    # Regional bar chart
    output$regional_bar <- renderPlotly({
      req(wbes_data())
      regional <- wbes_data()$regional
      if (is.null(regional)) return(NULL)

      plot_ly(regional) |>
        add_trace(x = ~region, y = ~IC.FRM.BANK.ZS, name = "Bank Account",
                  type = "bar", marker = list(color = "#1B6B5F")) |>
        add_trace(x = ~region, y = ~IC.FRM.FINA.ZS, name = "Finance Obstacle",
                  type = "bar", marker = list(color = "#F49B7A")) |>
        add_trace(x = ~region, y = ~IC.FRM.CRED.ZS, name = "Credit Constraint",
                  type = "bar", marker = list(color = "#6C757D")) |>
        layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = -30),
          yaxis = list(title = "% of Firms"),
          legend = list(orientation = "h", y = -0.2),
          margin = list(b = 100),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Reasons pie
    output$reasons_pie <- renderPlotly({
      reasons <- data.frame(
        reason = c("No Need", "High Interest", "Complex Procedures",
                   "Collateral Issues", "Informal Alt.", "Other"),
        pct = c(42, 18, 15, 12, 8, 5)
      )

      plot_ly(reasons, labels = ~reason, values = ~pct, type = "pie", hole = 0.4,
              marker = list(colors = c("#1B6B5F", "#F49B7A", "#2E7D32",
                                       "#17a2b8", "#6C757D", "#ffc107"))) |>
        layout(showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    # Gap chart
    output$gap_chart <- renderPlotly({
      req(filtered())
      d <- arrange(filtered(), desc(IC.FRM.FINA.ZS))[1:12, ]
      d$country <- factor(d$country, levels = rev(d$country))

      # Simulated gap
      d$gap <- runif(nrow(d), 15, 35)

      plot_ly(d) |>
        add_trace(y = ~country, x = ~IC.FRM.FINA.ZS, name = "Current Obstacle",
                  type = "bar", orientation = "h", marker = list(color = "#1B6B5F")) |>
        add_trace(y = ~country, x = ~gap, name = "Unmet Need",
                  type = "bar", orientation = "h", marker = list(color = "#F49B7A")) |>
        layout(
          barmode = "stack",
          xaxis = list(title = "% of Firms"),
          yaxis = list(title = ""),
          legend = list(orientation = "h", y = -0.15),
          margin = list(l = 100),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

    # Gender gap
    output$gender_gap <- renderPlotly({
      gender <- data.frame(
        category = c("Credit Access", "Bank Account", "Formal Training"),
        female = c(28, 85, 35),
        male = c(35, 90, 42)
      )

      plot_ly(gender) |>
        add_trace(x = ~category, y = ~female, name = "Female-Owned",
                  type = "bar", marker = list(color = "#F49B7A")) |>
        add_trace(x = ~category, y = ~male, name = "Male-Owned",
                  type = "bar", marker = list(color = "#1B6B5F")) |>
        layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "% of Firms"),
          legend = list(orientation = "h", y = -0.15),
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    })

  })
}
