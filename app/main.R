# app/main.R
# WBES Business Environment Benchmarking Dashboard
# Main Application Entry Point

box::use(
  shiny[
    shinyApp, moduleServer, NS, reactive, reactiveVal, req, observe, observeEvent,
    tags, div, icon, HTML, fluidPage, navbarPage, tabPanel, fluidRow, column,
    selectInput, actionButton, renderUI, uiOutput
  ],
  bslib[
    bs_theme, page_navbar, nav_panel, nav_spacer, nav_item,
    card, card_header, card_body
  ],
  waiter[useWaiter, waiter_show, waiter_hide, spin_fading_circles]
)

box::use(
  app/logic/wbes_data[load_wbes_data, load_sample_data],
  app/view/overview,
  app/view/country_profile,
  app/view/benchmark,
  app/view/infrastructure,
  app/view/finance_access,
  app/view/data_quality,
  app/view/about
)

# Kwiz Research Theme
kwiz_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1B6B5F",
  secondary = "#F49B7A",
  success = "#2E7D32",
  warning = "#F4A460",
  danger = "#dc3545",
  info = "#17a2b8",
  base_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif",
  heading_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif"
)

#' @export
ui <- page_navbar(
  id = "main_nav",
  title = tags$span(
    tags$img(src = "static/favicon.ico", height = "28px", class = "me-2"),
    "WBES Dashboard"
  ),
  theme = kwiz_theme,
  fillable = TRUE,
  
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles/main.css"),
    tags$script(src = "js/index.js"),
    useWaiter()
  ),
  
  nav_panel(
    title = "Overview",
    value = "overview",
    icon = icon("globe"),
    overview$ui("overview")
  ),
  
  nav_panel(
    title = "Country Profile",
    value = "country",
    icon = icon("flag"),
    country_profile$ui("country_profile")
  ),
  
  nav_panel(
    title = "Benchmark",
    value = "benchmark",
    icon = icon("chart-bar"),
    benchmark$ui("benchmark")
  ),
  
  nav_panel(
    title = "Infrastructure",
    value = "infrastructure",
    icon = icon("bolt"),
    infrastructure$ui("infrastructure")
  ),
  
  nav_panel(
    title = "Finance",
    value = "finance",
    icon = icon("university"),
    finance_access$ui("finance_access")
  ),
  
  nav_panel(
    title = "Data Quality",
    value = "quality",
    icon = icon("clipboard-check"),
    data_quality$ui("data_quality")
  ),
  
  nav_spacer(),
  
  nav_panel(
    title = "About",
    value = "about",
    icon = icon("info-circle"),
    about$ui("about")
  ),
  
  nav_item(
    tags$a(
      href = "https://kwizresearch.com",
      target = "_blank",
      class = "nav-link",
      tags$span(class = "text-secondary-coral", "Kwiz Research")
    )
  ),
  
  footer = tags$footer(
    class = "bg-primary-teal text-white py-3 mt-4",
    fluidRow(
      column(6,
        tags$p(
          class = "mb-0",
          icon("database"), " Data: ",
          tags$a(
            href = "https://www.enterprisesurveys.org",
            target = "_blank",
            class = "text-white",
            "World Bank Enterprise Surveys"
          )
        )
      ),
      column(6,
        tags$p(
          class = "mb-0 text-end",
          "Developed by ",
          tags$a(
            href = "https://kwizresearch.com",
            target = "_blank",
            class = "text-secondary-coral",
            "Kwiz Computing Technologies"
          )
        )
      )
    )
  )
)

#' @export
server <- function(input, output, session) {
  
  # Show loading spinner
  waiter_show(
    html = tags$div(
      spin_fading_circles(color = "#1B6B5F"),
      tags$h4("Loading WBES Data...", class = "mt-3 text-primary-teal")
    ),
    color = "#ffffff"
  )
  
  # Load data reactively
  wbes_data <- reactiveVal(NULL)
  
  observe({
    data <- tryCatch({
      load_wbes_data()
    }, error = function(e) {
      message("Error loading data: ", e$message)
      load_sample_data()
    })
    
    wbes_data(data)
    waiter_hide()
  })
  
  # Initialize modules
  overview$server("overview", wbes_data)
  country_profile$server("country_profile", wbes_data)
  benchmark$server("benchmark", wbes_data)
  infrastructure$server("infrastructure", wbes_data)
  finance_access$server("finance_access", wbes_data)
  data_quality$server("data_quality", wbes_data)
  about$server("about")
}
