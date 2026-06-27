# app/main.R
# World Bank Enterprise Surveys - Business Environment Benchmarking Dashboard
# Main Application Entry Point

box::use(
  shiny[bootstrapPage, moduleServer, NS, tags, icon, HTML, selectInput, tagList,
        updateSelectInput, updateSelectizeInput, observeEvent, reactive, req, div, fluidRow, column,
        actionButton, selectizeInput, sliderInput, uiOutput, renderUI, reactiveVal,
        getQueryString, parseQueryString, httpResponse, conditionalPanel],
  shinyjs[useShinyjs, show, hide, hidden, runjs],
  rlang[`%||%`],
  bslib[
    bs_theme, bs_add_rules, nav_panel, nav_spacer, nav_menu,
    nav_item, page_navbar, card, card_header, card_body, sidebar, layout_sidebar
  ],
  waiter[useWaiter]
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
  app/view/mod_mobile_ui,
  app/logic/data_artifacts[load_app_data],
  app/logic/shared_filters[get_filter_choices],
  app/logic/custom_regions[get_region_choices, filter_by_region, custom_region_modal_ui,
                           manage_regions_modal_ui, edit_region_modal_ui, custom_regions_storage],
  app/logic/custom_sectors[get_sector_choices, filter_by_sector, custom_sector_modal_ui,
                           manage_sectors_modal_ui, edit_sector_modal_ui, custom_sectors_storage]
)

# ---- App-scope data load: ONCE per R process, shared across all sessions ----
# The heavy ETL (read .dta, World Bank enrichment, aggregation, macro prefetch)
# runs OFFLINE in scripts/build_data.R. Here we only read the small precomputed
# artifacts from data/processed/. No .dta read, no World Bank API on this path.
# Falls back to a one-time ETL if artifacts are missing (see load_app_data()).
APP_DATA <- load_app_data()

# Shared, bounded LRU cache backing bindCache() across ALL sessions. Cached
# values (e.g. World Bank macro lookups, heatmap matrices) are read-only, so a
# cross-session cache is safe and lets common queries reuse work between users.
shiny::shinyOptions(cache = cachem::cache_mem(max_size = 250 * 1024^2, evict = "lru"))

# Compile the Bootstrap/Sass theme ONCE at app scope. SCSS compilation is ~1-2s;
# doing it inside ui() recompiled it on EVERY page load, which dominated request
# latency. The theme is request-independent, so build it once and share it.
KWIZ_THEME <- bs_theme(
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

# Helper function to detect mobile from User-Agent
detect_mobile_from_ua <- function(request) {
  if (is.null(request) || is.null(request$HTTP_USER_AGENT)) {
    return(FALSE)
  }
  ua <- tolower(request$HTTP_USER_AGENT)
  mobile_patterns <- c("android", "webos", "iphone", "ipad", "ipod",
                       "blackberry", "iemobile", "opera mini", "mobile")
  any(sapply(mobile_patterns, function(p) grepl(p, ua)))
}

# Desktop UI function (original page_navbar UI)
desktop_ui <- function(kwiz_theme) {
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
      tags$meta(name = "description", content = "World Bank Enterprise Surveys Dashboard"),
      tags$meta(name = "author", content = "Kwiz Computing Technologies"),
      useWaiter(),
      tags$style(HTML("
        .bslib-sidebar-layout { --bslib-sidebar-width: 280px; }
        .sidebar { background-color: #f8f9fa; border-right: 1px solid #dee2e6; overflow-y: auto; }
        .sidebar .card { margin-bottom: 1rem; background-color: white; }
        .sidebar h5 { color: #1B6B5F; font-size: 0.9rem; font-weight: 600; margin-bottom: 1rem; }
        .sidebar .form-group { margin-bottom: 0.75rem; position: relative; z-index: auto; }
        .sidebar .form-label { font-size: 0.85rem; font-weight: 500; margin-bottom: 0.25rem; }
        .sidebar .form-select { font-size: 0.85rem; padding: 0.375rem 0.75rem; }

        /* Ensure dropdowns don't conflict with each other */
        .sidebar .selectize-dropdown {
          z-index: 1050 !important;
          max-height: 200px;
        }
        .sidebar .selectize-input {
          position: relative;
          z-index: 1;
        }
        .sidebar .selectize-control.single .selectize-input:after {
          right: 15px;
        }

        /* Stagger z-index for multiple selects to prevent overlap */
        #common_filters > div:nth-child(1) { z-index: 15; }
        #common_filters > div:nth-child(2) { z-index: 14; }
        #common_filters > div:nth-child(3) { z-index: 13; }
        #common_filters > div:nth-child(4) { z-index: 12; }
        #common_filters > div:nth-child(5) { z-index: 11; }

        /* Ensure proper spacing and containment */
        .sidebar .mb-3 {
          position: relative;
          clear: both;
        }
      "))
    ),
    sidebar = sidebar(
      id = "sidebar_filters",
      bg = "#f8f9fa",
      width = 280,
      tags$div(
        style = "padding: 0.5rem;",
        tags$h5(icon("filter"), " Filters", style = "margin-bottom: 1rem; color: #1B6B5F;"),

        # Common Filters (always visible) - with better spacing
        tags$div(
          id = "common_filters",
          style = "margin-bottom: 1rem;",

          # Region filter with custom region buttons below
          tags$div(
            class = "mb-3",
            selectInput(
              "global_region_filter",
              "Region",
              choices = c("All Regions" = "all"),
              selected = "all",
              width = "100%"
            ),
            tags$div(
              class = "d-flex gap-2 mt-2",
              actionButton(
                "create_custom_region",
                "Create Region",
                icon = icon("plus-circle"),
                class = "btn-sm btn-outline-primary flex-grow-1",
                style = "font-size: 0.75rem;"
              ),
              actionButton(
                "manage_custom_regions",
                "Manage",
                icon = icon("cog"),
                class = "btn-sm btn-outline-secondary flex-grow-1",
                style = "font-size: 0.75rem;"
              )
            )
          ),

          # Sector filter with custom sector buttons
          tags$div(
            class = "mb-3",
            selectInput(
              "global_sector_filter",
              "Sector",
              choices = c("All Sectors" = "all"),
              selected = "all",
              width = "100%"
            ),
            tags$div(
              class = "d-flex gap-2 mt-2",
              actionButton(
                "create_custom_sector",
                "Create Group",
                icon = icon("plus-circle"),
                class = "btn-sm btn-outline-primary flex-grow-1",
                style = "font-size: 0.75rem;"
              ),
              actionButton(
                "manage_custom_sectors",
                "Manage",
                icon = icon("cog"),
                class = "btn-sm btn-outline-secondary flex-grow-1",
                style = "font-size: 0.75rem;"
              )
            )
          ),

          tags$div(
            class = "mb-3",
            selectInput(
              "global_firm_size_filter",
              "Firm Size",
              choices = c("All Sizes" = "all"),
              selected = "all",
              width = "100%"
            )
          ),

          tags$div(
            class = "mb-3",
            selectInput(
              "global_income_filter",
              "Income Group",
              choices = c("All Income Levels" = "all"),
              selected = "all",
              width = "100%"
            )
          ),

          tags$div(
            class = "mb-3",
            selectizeInput(
              "global_year_filter",
              "Survey Year",
              choices = c("Latest Year (per country)" = "latest", "All Years" = "all"),
              selected = "latest",
              multiple = TRUE,
              options = list(plugins = list('remove_button')),
              width = "100%"
            )
          )
        ),

        # Tab-specific filters placeholder
        tags$div(
          id = "tab_specific_filters",
          style = "margin-top: 1.5rem; padding-top: 1rem; border-top: 1px solid #dee2e6;"
        ),

        # Reset button
        actionButton(
          "reset_all_filters",
          "Reset All Filters",
          icon = icon("refresh"),
          class = "btn-outline-secondary w-100",
          style = "margin-top: 1rem;"
        )
      )
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
    footer = tags$div(
      # Switch to mobile button (shown on desktop) - uses JS for no-reload switching
      tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-secondary ui-switch-btn d-none d-md-block",
        onclick = "switchUIMode('mobile'); return false;",
        icon("mobile-alt"),
        " Mobile View"
      ),
      tags$footer(
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
  )
}

# Mobile UI wrapper with switch button (now uses action button instead of link for no-reload switch)
mobile_ui_wrapper <- function(ns = function(x) x) {
  # shinyMobile's f7Page must be the root - use tagList instead of div wrapper
  tagList(
    # Add viewport meta and styles in head
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"),
      tags$link(rel = "icon", type = "image/svg+xml", href = "static/images/favicon.svg"),
      # Mark body with UI mode for JS detection
      tags$script(HTML("document.body.setAttribute('data-ui-mode', 'mobile');")),
      tags$style(HTML("
        /* Mobile-specific styles */
        .desktop-switch-fab {
          position: fixed;
          bottom: 80px;
          right: 16px;
          z-index: 9999;
          width: 44px;
          height: 44px;
          border-radius: 50%;
          background: #1B6B5F;
          color: white;
          border: none;
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
          display: flex;
          align-items: center;
          justify-content: center;
          text-decoration: none;
          cursor: pointer;
        }
        .desktop-switch-fab:hover {
          background: #145449;
        }
        .desktop-switch-fab:active {
          transform: scale(0.95);
        }
      "))
    ),
    # f7Page must be the root element for shinyMobile to work
    mod_mobile_ui$ui("mobile_ui"),
    # Switch to desktop button (FAB style) - now an action button for no-reload switching
    actionButton(
      ns("switch_to_desktop"),
      label = NULL,
      icon = icon("desktop"),
      class = "desktop-switch-fab",
      title = "Switch to Desktop View"
    )
  )
}

# Pre-render the heavy desktop UI ONCE to an HTML blob, preserving its <head>
# content and JS/CSS dependencies. Serializing the ~768 KB / ~25-module tag tree
# dominated per-request TTFB (~2 s locally, ~5 s on Cloud Run). Emitting the
# cached string instead is effectively free; Shiny still injects the preserved
# dependencies and head into the document, so widgets and styling are intact.
.desktop_rt <- htmltools::renderTags(desktop_ui(KWIZ_THEME))
DESKTOP_UI <- htmltools::attachDependencies(
  tagList(
    if (length(.desktop_rt$head)) tags$head(HTML(as.character(.desktop_rt$head))),
    HTML(.desktop_rt$html)
  ),
  .desktop_rt$dependencies
)
# Mobile UI is small (~25 KB); build it once too.
MOBILE_UI <- mod_mobile_ui$ui("mobile_ui")

#' @export
ui <- function(request) {
  # Check for explicit UI mode in query string
  query <- parseQueryString(request$QUERY_STRING %||% "")
  ui_mode <- query$ui

  # Determine initial UI to show
  if (!is.null(ui_mode)) {
    # Explicit mode requested via query parameter
    initial_mobile <- (ui_mode == "mobile")
  } else {
    # Auto-detect from User-Agent
    initial_mobile <- detect_mobile_from_ua(request)
  }

  # Desktop UI uses the app-scope KWIZ_THEME (compiled once, see top of module).
  # Build unified UI with both views - switching happens client-side without reload
  tagList(
    useShinyjs(),
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "icon", type = "image/svg+xml", href = "static/images/favicon.svg"),
      tags$style(HTML("
        /* Hide containers based on view mode */
        .desktop-view-container { display: block; }
        .mobile-view-container { display: none; }
        body[data-ui-mode='mobile'] .desktop-view-container { display: none; }
        body[data-ui-mode='mobile'] .mobile-view-container { display: block; }

        /* Mobile FAB button for switching to desktop */
        .desktop-switch-fab {
          position: fixed;
          bottom: 80px;
          right: 16px;
          z-index: 9999;
          width: 44px;
          height: 44px;
          border-radius: 50%;
          background: #1B6B5F;
          color: white;
          border: none;
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
          display: flex;
          align-items: center;
          justify-content: center;
          cursor: pointer;
        }
        .desktop-switch-fab:hover { background: #145449; }

        /* Desktop button for switching to mobile */
        .ui-switch-btn {
          position: fixed;
          bottom: 20px;
          right: 20px;
          z-index: 9999;
          opacity: 0.7;
          transition: opacity 0.3s;
        }
        .ui-switch-btn:hover { opacity: 1; }
      ")),
      # JavaScript for view switching without page reload
      tags$script(HTML(sprintf("
        // Set initial UI mode
        document.body.setAttribute('data-ui-mode', '%s');

        // Function to switch views
        window.switchUIMode = function(mode) {
          document.body.setAttribute('data-ui-mode', mode);
          // Update URL without reload (for bookmarking)
          var url = new URL(window.location);
          url.searchParams.set('ui', mode);
          window.history.replaceState({}, '', url);
          // Trigger Shiny input update
          Shiny.setInputValue('current_ui_mode', mode, {priority: 'event'});
        };
      ", if (initial_mobile) "mobile" else "desktop")))
    ),

    # Desktop UI container
    tags$div(
      id = "desktop_container",
      class = "desktop-view-container",
      DESKTOP_UI
    ),

    # Mobile UI container
    tags$div(
      id = "mobile_container",
      class = "mobile-view-container",
      MOBILE_UI,
      # Switch to desktop button (FAB style)
      actionButton(
        "switch_to_desktop",
        label = NULL,
        icon = icon("desktop"),
        class = "desktop-switch-fab",
        title = "Switch to Desktop View",
        onclick = "switchUIMode('desktop'); return false;"
      )
    )
  )
}

  #' @export
  server <- function(input, output, session) {

  # Data is already loaded ONCE at app scope (APP_DATA) and shared across all
  # sessions. Expose it through reactives so existing module wiring is unchanged.
  # No per-session ETL, no .dta read, no World Bank API on the startup path.
  wbes_data <- shiny::reactiveVal(APP_DATA)
  wb_prefetched_data <- shiny::reactiveVal(APP_DATA$wb_macro)

  # Custom regions and sectors storage
  custom_regions <- shiny::reactiveVal(list())
  custom_sectors <- shiny::reactiveVal(list())

  # Update filter choices when data loads or custom regions/sectors change
  observeEvent(list(wbes_data(), custom_regions(), custom_sectors()), {
    req(wbes_data())
    data <- wbes_data()

    # Update region filter with custom regions
    region_choices <- get_region_choices(wbes_data, custom_regions())
    current_selection <- input$global_region_filter
    updateSelectInput(
      session,
      "global_region_filter",
      choices = region_choices,
      selected = if (!is.null(current_selection)) current_selection else "all"
    )

    # Update sector filter with custom sectors (exclude NA values)
    if (!is.null(data$latest)) {
      sector_choices <- get_sector_choices(wbes_data, custom_sectors())
      current_sector_selection <- input$global_sector_filter
      updateSelectInput(
        session,
        "global_sector_filter",
        choices = sector_choices,
        selected = if (!is.null(current_sector_selection)) current_sector_selection else "all"
      )

      # Update firm size filter (exclude NA values)
      size_choices <- get_filter_choices(data$latest, "firm_size", add_all = TRUE, all_label = "All Sizes")
      updateSelectInput(session, "global_firm_size_filter", choices = size_choices)

      # Update income filter (exclude NA values)
      income_choices <- get_filter_choices(data$latest, "income", add_all = TRUE, all_label = "All Income Levels")
      updateSelectInput(session, "global_income_filter", choices = income_choices)
    }

    # Update year filter (exclude NA values)
    if (!is.null(data$years) && length(data$years) > 0) {
      year_choices <- c("Latest Year (per country)" = "latest", "All Years" = "all", stats::setNames(as.character(data$years), as.character(data$years)))
      updateSelectizeInput(session, "global_year_filter", choices = year_choices, selected = "latest")
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Custom region modal handlers
  observeEvent(input$create_custom_region, {
    req(wbes_data())
    countries <- sort(wbes_data()$countries)
    shiny::showModal(custom_region_modal_ui(session$ns, countries))
  })

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

    shiny::removeModal()
  })

  observeEvent(input$manage_custom_regions, {
    shiny::showModal(manage_regions_modal_ui(session$ns, custom_regions()))
  })

  observeEvent(input$delete_region_name, {
    req(input$delete_region_name)
    region_to_delete <- input$delete_region_name

    current_regions <- custom_regions()
    current_regions[[region_to_delete]] <- NULL
    custom_regions(current_regions)
    custom_regions_storage(current_regions)

    shiny::showModal(manage_regions_modal_ui(session$ns, custom_regions()))
  })

  # Edit custom region - show edit modal
  observeEvent(input$edit_region_name, {
    req(input$edit_region_name, wbes_data())
    region_to_edit <- input$edit_region_name
    current_regions <- custom_regions()

    if (!is.null(current_regions[[region_to_edit]])) {
      countries <- sort(wbes_data()$countries)
      shiny::showModal(edit_region_modal_ui(
        session$ns,
        countries,
        region_to_edit,
        current_regions[[region_to_edit]]
      ))
    }
  })

  # Update custom region - save changes
  observeEvent(input$update_custom_region, {
    req(input$edit_region_new_name, input$edit_region_countries)

    new_name <- trimws(input$edit_region_new_name)
    if (new_name == "" || length(input$edit_region_countries) == 0) {
      return(NULL)
    }

    # Get the original name from the hidden input
    original_name <- input$edit_region_original_name

    current_regions <- custom_regions()

    # If name changed, remove old entry
    if (!is.null(original_name) && original_name != new_name && !is.null(current_regions[[original_name]])) {
      current_regions[[original_name]] <- NULL
    }

    # Update/create region with new data
    current_regions[[new_name]] <- list(
      name = new_name,
      countries = input$edit_region_countries,
      created = if (!is.null(original_name) && !is.null(custom_regions()[[original_name]]$created)) {
        custom_regions()[[original_name]]$created
      } else {
        Sys.time()
      },
      modified = Sys.time()
    )

    custom_regions(current_regions)
    custom_regions_storage(current_regions)

    shiny::removeModal()
  })

  # Custom sector modal handlers
  observeEvent(input$create_custom_sector, {
    req(wbes_data())
    sectors <- sort(wbes_data()$sectors)
    shiny::showModal(custom_sector_modal_ui(session$ns, sectors))
  })

  observeEvent(input$save_custom_sector, {
    req(input$custom_sector_name, input$custom_sector_sectors)

    sector_name <- trimws(input$custom_sector_name)
    if (sector_name == "" || length(input$custom_sector_sectors) == 0) {
      return(NULL)
    }

    new_sector <- list(
      name = sector_name,
      sectors = input$custom_sector_sectors,
      created = Sys.time()
    )

    current_sectors <- custom_sectors()
    current_sectors[[sector_name]] <- new_sector
    custom_sectors(current_sectors)
    custom_sectors_storage(current_sectors)

    shiny::removeModal()
  })

  observeEvent(input$manage_custom_sectors, {
    shiny::showModal(manage_sectors_modal_ui(session$ns, custom_sectors()))
  })

  observeEvent(input$delete_sector_name, {
    req(input$delete_sector_name)
    sector_to_delete <- input$delete_sector_name

    current_sectors <- custom_sectors()
    current_sectors[[sector_to_delete]] <- NULL
    custom_sectors(current_sectors)
    custom_sectors_storage(current_sectors)

    shiny::showModal(manage_sectors_modal_ui(session$ns, custom_sectors()))
  })

  # Edit custom sector - show edit modal
  observeEvent(input$edit_sector_name, {
    req(input$edit_sector_name, wbes_data())
    sector_to_edit <- input$edit_sector_name
    current_sectors <- custom_sectors()

    if (!is.null(current_sectors[[sector_to_edit]])) {
      sectors <- sort(wbes_data()$sectors)
      shiny::showModal(edit_sector_modal_ui(
        session$ns,
        sectors,
        sector_to_edit,
        current_sectors[[sector_to_edit]]
      ))
    }
  })

  # Update custom sector - save changes
  observeEvent(input$update_custom_sector, {
    req(input$edit_sector_new_name, input$edit_sector_sectors)

    new_name <- trimws(input$edit_sector_new_name)
    if (new_name == "" || length(input$edit_sector_sectors) == 0) {
      return(NULL)
    }

    # Get the original name from the hidden input
    original_name <- input$edit_sector_original_name

    current_sectors <- custom_sectors()

    # If name changed, remove old entry
    if (!is.null(original_name) && original_name != new_name && !is.null(current_sectors[[original_name]])) {
      current_sectors[[original_name]] <- NULL
    }

    # Update/create sector with new data
    current_sectors[[new_name]] <- list(
      name = new_name,
      sectors = input$edit_sector_sectors,
      created = if (!is.null(original_name) && !is.null(custom_sectors()[[original_name]]$created)) {
        custom_sectors()[[original_name]]$created
      } else {
        Sys.time()
      },
      modified = Sys.time()
    )

    custom_sectors(current_sectors)
    custom_sectors_storage(current_sectors)

    shiny::removeModal()
  })

  # Reset all filters
  observeEvent(input$reset_all_filters, {
    updateSelectInput(session, "global_region_filter", selected = "all")
    updateSelectInput(session, "global_sector_filter", selected = "all")
    updateSelectInput(session, "global_firm_size_filter", selected = "all")
    updateSelectInput(session, "global_income_filter", selected = "all")
    updateSelectizeInput(session, "global_year_filter", selected = "latest")
  })

  # Create reactive for global filter state
  global_filters <- reactive({
    list(
      region = input$global_region_filter,
      sector = input$global_sector_filter,
      firm_size = input$global_firm_size_filter,
      income = input$global_income_filter,
      year = input$global_year_filter,
      custom_regions = custom_regions(),
      custom_sectors = custom_sectors()
    )
  })

  # Module servers - pass both raw data and filter state
  # Pass root session for dual-rendering to mobile UI
  mod_overview$server("overview", wbes_data, global_filters, root_session = session)

  # Profile modules - pass wb_prefetched_data for cached WB access
  mod_country_profile$server("country_profile", wbes_data, global_filters, wb_prefetched_data)
  mod_sector_profile$server("sector_profile", wbes_data, global_filters)
  mod_regional_profile$server("regional_profile", wbes_data, global_filters)
  mod_size_profile$server("size_profile", wbes_data, global_filters)

  # Benchmark modules - pass wb_prefetched_data for cached WB access
  mod_benchmark$server("benchmark", wbes_data, global_filters, wb_prefetched_data)
  mod_benchmark_sector$server("benchmark_sector", wbes_data, global_filters)
  mod_benchmark_regional$server("benchmark_regional", wbes_data, global_filters)
  mod_benchmark_size$server("benchmark_size", wbes_data, global_filters)

  # Domain modules
  mod_infrastructure$server("infrastructure", wbes_data, global_filters)
  mod_finance_access$server("finance", wbes_data, global_filters)
  mod_corruption$server("corruption", wbes_data, global_filters)
  mod_workforce$server("workforce", wbes_data, global_filters)
  mod_performance$server("performance", wbes_data, global_filters)
  mod_crime$server("crime", wbes_data, global_filters)

  # Other modules
  mod_custom_analysis$server("custom_analysis", wbes_data, global_filters)
  mod_data_quality$server("data_quality", wbes_data, global_filters)
  mod_about$server("about")

  # Mobile UI module - shares data and filters with desktop
  mod_mobile_ui$server("mobile_ui", wbes_data, global_filters, wb_prefetched_data)
}
