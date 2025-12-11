# app/logic/custom_sectors.R
# Custom Sector Builder Logic
# Mirrors custom regions functionality for sectors

box::use(
  shiny[reactiveVal, observeEvent, req, showModal, modalDialog, textInput,
        selectizeInput, actionButton, modalButton, removeModal, icon, tags],
  dplyr[filter],
  stats[setNames],
  utils[head]
)

#' @export
custom_sectors_storage <- reactiveVal(list())

#' UI for custom sector builder modal
#' @export
custom_sector_modal_ui <- function(ns, sectors) {
  modalDialog(
    title = tags$div(
      icon("plus-circle"),
      " Create Custom Sector Group"
    ),
    size = "l",

    textInput(
      ns("custom_sector_name"),
      "Sector Group Name",
      placeholder = "e.g., Technology & Services"
    ),

    selectizeInput(
      ns("custom_sector_sectors"),
      "Select Sectors",
      choices = sectors,
      multiple = TRUE,
      options = list(
        placeholder = "Choose sectors to include in this group...",
        plugins = list('remove_button')
      )
    ),

    tags$div(
      class = "alert alert-info mt-3",
      icon("info-circle"),
      " Custom sector groups allow you to combine sectors for targeted analysis. The group will be available in all filter dropdowns."
    ),

    footer = tags$div(
      modalButton("Cancel"),
      actionButton(
        ns("save_custom_sector"),
        "Save Sector Group",
        icon = icon("save"),
        class = "btn-primary"
      )
    )
  )
}

#' UI for managing existing custom sectors
#' @export
manage_sectors_modal_ui <- function(ns, custom_sectors) {
  modalDialog(
    title = tags$div(
      icon("cog"),
      " Manage Custom Sector Groups"
    ),
    size = "l",

    if (length(custom_sectors) == 0) {
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " No custom sector groups created yet. Click the '+' button to create one."
      )
    } else {
      tags$div(
        tags$h5("Your Custom Sector Groups:"),
        tags$div(
          class = "list-group mt-3",
          lapply(names(custom_sectors), function(sector_name) {
            sector <- custom_sectors[[sector_name]]
            tags$div(
              class = "list-group-item d-flex justify-content-between align-items-start",
              tags$div(
                tags$h6(class = "mb-1", icon("industry"), " ", sector_name),
                tags$small(
                  class = "text-muted",
                  paste(length(sector$sectors), "sectors:",
                        paste(head(sector$sectors, 3), collapse = ", "),
                        if(length(sector$sectors) > 3) "..." else "")
                )
              ),
              tags$div(
                class = "btn-group",
                actionButton(
                  ns(paste0("edit_sector_", sector_name)),
                  NULL,
                  icon = icon("edit"),
                  class = "btn-sm btn-outline-primary me-1",
                  title = "Edit this sector group",
                  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                  ns("edit_sector_name"), sector_name)
                ),
                actionButton(
                  ns(paste0("delete_sector_", sector_name)),
                  NULL,
                  icon = icon("trash"),
                  class = "btn-sm btn-outline-danger",
                  title = "Delete this sector group",
                  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                  ns("delete_sector_name"), sector_name)
                )
              )
            )
          })
        )
      )
    },

    footer = tags$div(
      modalButton("Close")
    )
  )
}

#' UI for editing an existing custom sector group
#' @export
edit_sector_modal_ui <- function(ns, sectors, sector_name, sector_data) {
  modalDialog(
    title = tags$div(
      icon("edit"),
      " Edit Custom Sector Group: ", sector_name
    ),
    size = "l",

    textInput(
      ns("edit_sector_new_name"),
      "Sector Group Name",
      value = sector_name
    ),

    selectizeInput(
      ns("edit_sector_sectors"),
      "Select Sectors",
      choices = sectors,
      selected = sector_data$sectors,
      multiple = TRUE,
      options = list(
        placeholder = "Choose sectors to include in this group...",
        plugins = list('remove_button')
      )
    ),

    # Store original name for reference using a hidden input
    tags$input(
      type = "hidden",
      id = ns("edit_sector_original_name"),
      value = sector_name
    ),

    tags$div(
      class = "alert alert-info mt-3",
      icon("info-circle"),
      " Modify the sector group name or update the sectors included in this custom group."
    ),

    footer = tags$div(
      modalButton("Cancel"),
      actionButton(
        ns("update_custom_sector"),
        "Update Sector Group",
        icon = icon("save"),
        class = "btn-primary"
      )
    )
  )
}

#' Server logic for managing custom sectors
#' @export
manage_custom_sectors <- function(id, wbes_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize custom sectors from browser storage if available
    custom_sectors <- reactiveVal(list())

    # Show modal to create custom sector
    observeEvent(input$create_custom_sector, {
      req(wbes_data())
      sectors <- sort(wbes_data()$sectors)
      showModal(custom_sector_modal_ui(ns, sectors))
    })

    # Save custom sector
    observeEvent(input$save_custom_sector, {
      req(input$custom_sector_name, input$custom_sector_sectors)

      # Validate sector name
      sector_name <- trimws(input$custom_sector_name)
      if (sector_name == "" || length(input$custom_sector_sectors) == 0) {
        return(NULL)
      }

      # Create new custom sector
      new_sector <- list(
        name = sector_name,
        sectors = input$custom_sector_sectors,
        created = Sys.time()
      )

      # Add to custom sectors list
      current_sectors <- custom_sectors()
      current_sectors[[sector_name]] <- new_sector
      custom_sectors(current_sectors)

      # Store globally for access by other modules
      custom_sectors_storage(current_sectors)

      removeModal()
    })

    # Show modal to manage custom sectors
    observeEvent(input$manage_custom_sectors, {
      showModal(manage_sectors_modal_ui(ns, custom_sectors()))
    })

    # Delete custom sector
    observeEvent(input$delete_sector_name, {
      req(input$delete_sector_name)
      sector_to_delete <- input$delete_sector_name

      current_sectors <- custom_sectors()
      current_sectors[[sector_to_delete]] <- NULL
      custom_sectors(current_sectors)
      custom_sectors_storage(current_sectors)

      # Refresh the modal
      showModal(manage_sectors_modal_ui(ns, custom_sectors()))
    })

    return(custom_sectors)
  })
}

#' Get combined sector choices (standard + custom)
#' @export
get_sector_choices <- function(wbes_data, custom_sectors = list()) {
  standard_sectors <- if (!is.null(wbes_data) && !is.null(wbes_data()$sectors)) {
    wbes_data()$sectors
  } else {
    character(0)
  }

  custom_sector_names <- names(custom_sectors)

  if (length(custom_sector_names) > 0) {
    # Create named vector with separator
    all_sectors <- c(
      "All Sectors" = "all",
      "--- Standard Sectors ---" = "",
      setNames(standard_sectors, paste0("   ", standard_sectors)),
      "--- Custom Sector Groups ---" = "",
      setNames(
        paste0("custom:", custom_sector_names),
        paste0("   [Custom] ", custom_sector_names)
      )
    )
  } else {
    all_sectors <- c(
      "All Sectors" = "all",
      setNames(standard_sectors, standard_sectors)
    )
  }

  return(all_sectors)
}

#' Filter data by sector (supports custom sectors)
#' @export
filter_by_sector <- function(data, sector_value, custom_sectors = list()) {
  if (is.null(sector_value) || sector_value == "all" || sector_value == "") {
    return(data)
  }

  # Check if it's a custom sector
  if (grepl("^custom:", sector_value)) {
    sector_name <- sub("^custom:", "", sector_value)
    custom_sector <- custom_sectors[[sector_name]]

    if (!is.null(custom_sector)) {
      # Filter by sectors in custom sector group
      data <- data |> filter(sector %in% custom_sector$sectors)
    }
  } else {
    # Standard sector filter
    data <- data |> filter(sector == sector_value)
  }

  return(data)
}
