# app/main.R
# World Bank Enterprise Surveys - Business Environment Benchmarking Dashboard
# Main Application Entry Point

box::use(
  shiny[bootstrapPage, moduleServer, NS, tags, icon, HTML],
  bslib[
    bs_theme, bs_add_rules, nav_panel, nav_spacer, nav_menu,
    nav_item, page_navbar, card, card_header, card_body
  ],
  waiter[useWaiter, waiterPreloader, spin_fading_circles, waiter_show, waiter_hide],
  here[here]
)

box::use(
  app/view/mod_overview,
  app/view/mod_country_profile,
  app/view/mod_sector_profile,
  app/view/mod_regional_profile,
  app/view/mod_size_profile,
  app/view/mod_benchmark,
  app/view/mod_benchmark_sector,
  app/view/mod_benchmark_regional,
  app/view/mod_benchmark_size,
  app/view/mod_infrastructure,
  app/view/mod_finance_access,
  app/view/mod_corruption,
  app/view/mod_workforce,
  app/view/mod_performance,
  app/view/mod_crime,
  app/view/mod_custom_analysis,
  app/view/mod_data_quality,
  app/view/mod_about,
  app/logic/wbes_data[load_wbes_data]
)

#' @export
ui <- function(request) {

  # Kwiz Research Theme
  kwiz_theme <- bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B6B5F",
    secondary = "#F49B7A",
    success = "#2E7D32",
    info = "#17a2b8",
    warning = "#F4A460",
    danger = "#dc3545",
    bg = "#FFFFFF",
    fg = "#333333",
    base_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif",
    heading_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif"
  ) |>
    bs_add_rules(sass::sass_file("app/styles/main.scss"))

  page_navbar(
    id = "main_navbar",
    title = tags$span(
      tags$img(
        src = "static/images/logo.svg",
        height = "35px",
        style = "margin-right: 10px; vertical-align: middle;"
      ),
      "Business Environment Benchmarking"
    ),
    theme = kwiz_theme,
    fillable = TRUE,
    bg = "#1B6B5F",
    inverse = TRUE,

    # Header with branding
    header = tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "static/images/favicon.svg"),
      tags$meta(name = "description", content = "World Bank Enterprise Surveys Dashboard"),
      tags$meta(name = "author", content = "Kwiz Computing Technologies"),
      useWaiter()
    ),

    # Navigation Panels
    nav_panel(
      title = "Overview",
      value = "overview",
      icon = icon("globe"),
      mod_overview$ui("overview")
    ),

    # Profiles Menu
    nav_menu(
      title = "Profiles",
      icon = icon("id-card"),
      nav_panel(
        title = "Country Profile",
        value = "country_profile",
        icon = icon("flag"),
        mod_country_profile$ui("country_profile")
      ),
      nav_panel(
        title = "Sector Profile",
        value = "sector_profile",
        icon = icon("industry"),
        mod_sector_profile$ui("sector_profile")
      ),
      nav_panel(
        title = "Regional Profile",
        value = "regional_profile",
        icon = icon("globe-africa"),
        mod_regional_profile$ui("regional_profile")
      ),
      nav_panel(
        title = "Size Profile",
        value = "size_profile",
        icon = icon("building"),
        mod_size_profile$ui("size_profile")
      )
    ),

    # Benchmarks Menu
    nav_menu(
      title = "Benchmarks",
      icon = icon("chart-bar"),
      nav_panel(
        title = "Cross-Country",
        value = "benchmark",
        icon = icon("flag"),
        mod_benchmark$ui("benchmark")
      ),
      nav_panel(
        title = "Cross-Sector",
        value = "benchmark_sector",
        icon = icon("industry"),
        mod_benchmark_sector$ui("benchmark_sector")
      ),
      nav_panel(
        title = "Cross-Regional",
        value = "benchmark_regional",
        icon = icon("globe-africa"),
        mod_benchmark_regional$ui("benchmark_regional")
      ),
      nav_panel(
        title = "Cross-Size",
        value = "benchmark_size",
        icon = icon("building"),
        mod_benchmark_size$ui("benchmark_size")
      )
    ),

    # Domains Menu
    nav_menu(
      title = "Domains",
      icon = icon("layer-group"),
      nav_panel(
        title = "Infrastructure",
        value = "infrastructure",
        icon = icon("bolt"),
        mod_infrastructure$ui("infrastructure")
      ),
      nav_panel(
        title = "Access to Finance",
        value = "finance",
        icon = icon("university"),
        mod_finance_access$ui("finance")
      ),
      nav_panel(
        title = "Corruption",
        value = "corruption",
        icon = icon("balance-scale"),
        mod_corruption$ui("corruption")
      ),
      nav_panel(
        title = "Workforce",
        value = "workforce",
        icon = icon("users"),
        mod_workforce$ui("workforce")
      ),
      nav_panel(
        title = "Performance",
        value = "performance",
        icon = icon("chart-line"),
        mod_performance$ui("performance")
      ),
      nav_panel(
        title = "Crime & Security",
        value = "crime",
        icon = icon("shield-alt"),
        mod_crime$ui("crime")
      )
    ),

    nav_panel(
      title = "Custom Analysis",
      value = "custom_analysis",
      icon = icon("cogs"),
      mod_custom_analysis$ui("custom_analysis")
    ),

    nav_spacer(),

    nav_panel(
      title = "Data Quality",
      value = "data_quality",
      icon = icon("clipboard-check"),
      mod_data_quality$ui("data_quality")
    ),

    nav_panel(
      title = "About",
      value = "about",
      icon = icon("info-circle"),
      mod_about$ui("about")
    ),

    nav_item(
      tags$a(
        href = "https://kwizresearch.com",
        target = "_blank",
        icon("external-link-alt"),
        " Kwiz Research",
        class = "nav-link",
        style = "color: #F49B7A !important;"
      )
    ),

    # Footer
    footer = tags$footer(
      class = "dashboard-footer",
      tags$div(
        class = "container-fluid",
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            tags$span(
              icon("copyright"),
              " 2025 ",
              tags$a(
                href = "https://kwizresearch.com",
                target = "_blank",
                "Kwiz Computing Technologies"
              )
            )
          ),
          tags$div(
            class = "col-md-6 text-end",
            tags$span(
              "Data: ",
              tags$a(
                href = "https://www.enterprisesurveys.org",
                target = "_blank",
                "World Bank Enterprise Surveys"
              )
            )
          )
        )
      )
    )
  )
}

  #' @export
  server <- function(input, output, session) {

  # Show loading screen
  waiter_show(
    html = tags$div(
      spin_fading_circles(),
      tags$h4("Loading WBES Data...", class = "mt-3", style = "color: #1B6B5F;")
    ),
    color = "#FFFFFF"
  )

    # Load data reactively (shared across modules)
    wbes_data <- shiny::reactiveVal(NULL)

  # Initialize data loading with improved error handling
  shiny::observe({
    tryCatch({
      # Load data from assets.zip or .dta files (real data required)
      data <- load_wbes_data(
        data_path = here("data"),
        use_cache = TRUE,
        cache_hours = 24
      )
      wbes_data(data)

    }, error = function(e) {
      message("Error loading WBES data: ", e$message)
      stop("Failed to load WBES data. Please ensure data/assets.zip is present.")
    })

    # Hide waiter after data loads
    waiter_hide()
  })

  # Module servers
  mod_overview$server("overview", wbes_data)

  # Profile modules
  mod_country_profile$server("country_profile", wbes_data)
  mod_sector_profile$server("sector_profile", wbes_data)
  mod_regional_profile$server("regional_profile", wbes_data)
  mod_size_profile$server("size_profile", wbes_data)

  # Benchmark modules
  mod_benchmark$server("benchmark", wbes_data)
  mod_benchmark_sector$server("benchmark_sector", wbes_data)
  mod_benchmark_regional$server("benchmark_regional", wbes_data)
  mod_benchmark_size$server("benchmark_size", wbes_data)

  # Domain modules
  mod_infrastructure$server("infrastructure", wbes_data)
  mod_finance_access$server("finance", wbes_data)
  mod_corruption$server("corruption", wbes_data)
  mod_workforce$server("workforce", wbes_data)
  mod_performance$server("performance", wbes_data)
  mod_crime$server("crime", wbes_data)

  # Other modules
  mod_custom_analysis$server("custom_analysis", wbes_data)
  mod_data_quality$server("data_quality", wbes_data)
  mod_about$server("about")
}
