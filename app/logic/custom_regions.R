# app/logic/custom_regions.R
# Custom Region Builder Logic

box::use(
  shiny[reactiveVal, observeEvent, req, showModal, modalDialog, textInput,
        selectizeInput, actionButton, modalButton, removeModal, icon],
  dplyr[filter],
  stats[setNames]
)

#' @export
custom_regions_storage <- reactiveVal(list())

#' UI for custom region builder modal
#' @export
custom_region_modal_ui <- function(ns, countries) {
  modalDialog(
    title = tags$div(
      icon("plus-circle"),
      " Create Custom Region"
    ),
    size = "l",

    textInput(
      ns("custom_region_name"),
      "Region Name",
      placeholder = "e.g., East African Community"
    ),

    selectizeInput(
      ns("custom_region_countries"),
      "Select Countries",
      choices = countries,
      multiple = TRUE,
      options = list(
        placeholder = "Choose countries to include in this region...",
        plugins = list('remove_button')
      )
    ),

    tags$div(
      class = "alert alert-info mt-3",
      icon("info-circle"),
      " Custom regions allow you to group countries for analysis. The region will be available in all filter dropdowns."
    ),

    footer = tags$div(
      modalButton("Cancel"),
      actionButton(
        ns("save_custom_region"),
        "Save Region",
        icon = icon("save"),
        class = "btn-primary"
      )
    )
  )
}

#' Server logic for managing custom regions
#' @export
manage_custom_regions <- function(id, wbes_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize custom regions from browser storage if available
    custom_regions <- reactiveVal(list())

    # Show modal to create custom region
    observeEvent(input$create_custom_region, {
      req(wbes_data())
      countries <- sort(wbes_data()$countries)
      showModal(custom_region_modal_ui(ns, countries))
    })

    # Save custom region
    observeEvent(input$save_custom_region, {
      req(input$custom_region_name, input$custom_region_countries)

      # Validate region name
      region_name <- trimws(input$custom_region_name)
      if (region_name == "" || length(input$custom_region_countries) == 0) {
        return(NULL)
      }

      # Create new custom region
      new_region <- list(
        name = region_name,
        countries = input$custom_region_countries,
        created = Sys.time()
      )

      # Add to custom regions list
      current_regions <- custom_regions()
      current_regions[[region_name]] <- new_region
      custom_regions(current_regions)

      # Store globally for access by other modules
      custom_regions_storage(current_regions)

      removeModal()
    })

    # Delete custom region
    observeEvent(input$delete_custom_region, {
      req(input$region_to_delete)
      current_regions <- custom_regions()
      current_regions[[input$region_to_delete]] <- NULL
      custom_regions(current_regions)
      custom_regions_storage(current_regions)
    })

    return(custom_regions)
  })
}

#' Get combined region choices (standard + custom)
#' @export
get_region_choices <- function(wbes_data, custom_regions = list()) {
  standard_regions <- if (!is.null(wbes_data) && !is.null(wbes_data()$regions)) {
    wbes_data()$regions
  } else {
    character(0)
  }

  custom_region_names <- names(custom_regions)

  if (length(custom_region_names) > 0) {
    # Create named vector with separator
    all_regions <- c(
      "All Regions" = "all",
      "--- Standard Regions ---" = "",
      setNames(standard_regions, paste0("   ", standard_regions)),
      "--- Custom Regions ---" = "",
      setNames(
        paste0("custom:", custom_region_names),
        paste0("   [Custom] ", custom_region_names)
      )
    )
  } else {
    all_regions <- c(
      "All Regions" = "all",
      setNames(standard_regions, standard_regions)
    )
  }

  return(all_regions)
}

#' Filter data by region (supports custom regions)
#' @export
filter_by_region <- function(data, region_value, custom_regions = list()) {
  if (is.null(region_value) || region_value == "all" || region_value == "") {
    return(data)
  }

  # Check if it's a custom region
  if (grepl("^custom:", region_value)) {
    region_name <- sub("^custom:", "", region_value)
    custom_region <- custom_regions[[region_name]]

    if (!is.null(custom_region)) {
      # Filter by countries in custom region
      data <- data |> filter(country %in% custom_region$countries)
    }
  } else {
    # Standard region filter
    data <- data |> filter(region == region_value)
  }

  return(data)
}
