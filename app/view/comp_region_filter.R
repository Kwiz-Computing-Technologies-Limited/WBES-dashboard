# app/view/comp_region_filter.R
# Reusable Region Filter Component with Custom Region Support

box::use(
  shiny[moduleServer, NS, reactive, req, tags, icon, selectInput,
        actionButton, observeEvent, reactiveVal, updateSelectInput],
  bslib[tooltip],
  app/logic/custom_regions[get_region_choices, filter_by_region,
                            manage_custom_regions, custom_regions_storage]
)

#' @export
ui <- function(id, label = "Region") {
  ns <- NS(id)

  tags$div(
    class = "region-filter-wrapper",
    tags$div(
      class = "d-flex gap-2",
      tags$div(
        class = "flex-grow-1",
        selectInput(
          ns("region_select"),
          label,
          choices = c("All Regions" = "all"),
          selected = "all",
          width = "100%"
        )
      ),
      tags$div(
        class = "d-flex align-items-end pb-3",
        actionButton(
          ns("create_custom_region"),
          NULL,
          icon = icon("plus-circle"),
          class = "btn-sm btn-outline-primary",
          title = "Create Custom Region",
          style = "height: 38px; margin-bottom: 0;"
        ) |>
          tooltip("Create a custom region by selecting specific countries")
      )
    )
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Manage custom regions
    custom_regions <- manage_custom_regions(id, wbes_data)

    # Update region choices when data loads or custom regions change
    observeEvent(list(wbes_data(), custom_regions()), {
      req(wbes_data())

      region_choices <- get_region_choices(wbes_data, custom_regions())

      updateSelectInput(
        session,
        "region_select",
        choices = region_choices
      )
    }, ignoreNULL = FALSE)

    # Return selected region value
    return(reactive({
      input$region_select
    }))
  })
}

#' Helper function to filter data using the region filter
#' @export
apply_region_filter <- function(data, region_value, custom_regions = list()) {
  filter_by_region(data, region_value, custom_regions)
}
