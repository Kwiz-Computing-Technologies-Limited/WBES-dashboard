# Sidebar Layout Implementation Guide

## Overview

This guide documents the approach for implementing a global sidebar with filters for the WBES Dashboard. The sidebar layout was requested to provide:
1. Global filters in a left-hand sidebar
2. Independent scrolling for sidebar and main content
3. Consistent filter state across all app tabs

## Current Architecture

The app currently uses `bslib::page_navbar()` with tab-based navigation:
- Each module has its own filters embedded in the page content
- Filters are not shared across tabs
- No physical sidebar structure

## Implementation Approach

### Option 1: Global Sidebar with Shared State (Recommended for Future)

**Pros:**
- True global filtering across all tabs
- Single source of truth for filter state
- Better UX consistency

**Cons:**
- Requires major refactoring of main.R
- May break existing module structure
- Higher risk of bugs

**Implementation Steps:**

1. **Create a Global Filter Module** (`app/logic/global_filters.R`):

```r
box::use(
  shiny[moduleServer, NS, reactive, reactiveValues, observe, updateSelectInput],
  bslib[...],
  dplyr[...],
)

#' @export
ui <- function(id, wbes_data) {
  ns <- NS(id)

  tags$div(
    class = "global-filters-sidebar",
    style = "position: fixed; left: 0; top: 60px; width: 250px;
             height: calc(100vh - 60px); overflow-y: auto;
             padding: 15px; border-right: 1px solid #dee2e6;",

    h4("Global Filters", class = "mb-3"),

    selectInput(
      ns("country"),
      "Country",
      choices = NULL,
      multiple = TRUE
    ),

    selectInput(
      ns("year"),
      "Survey Year",
      choices = NULL,
      multiple = TRUE
    ),

    selectInput(
      ns("region"),
      "Region",
      choices = NULL,
      multiple = TRUE
    ),

    selectInput(
      ns("income_group"),
      "Income Group",
      choices = NULL,
      multiple = TRUE
    ),

    selectInput(
      ns("sector"),
      "Sector",
      choices = NULL,
      multiple = TRUE
    ),

    selectInput(
      ns("firm_size"),
      "Firm Size",
      choices = c("All" = "all", "Small (5-19)" = "small",
                  "Medium (20-99)" = "medium", "Large (100+)" = "large")
    ),

    hr(),

    actionButton(ns("reset"), "Reset All Filters",
                 class = "btn-outline-primary w-100")
  )
}

#' @export
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {

    # Initialize filter choices when data loads
    observe({
      req(wbes_data())
      data <- wbes_data()

      updateSelectInput(session, "country",
                       choices = sort(data$countries))
      updateSelectInput(session, "year",
                       choices = sort(data$years))
      updateSelectInput(session, "region",
                       choices = sort(data$regions))
      # ... etc
    })

    # Return reactive filter state
    reactive({
      list(
        country = input$country,
        year = input$year,
        region = input$region,
        income_group = input$income_group,
        sector = input$sector,
        firm_size = input$firm_size
      )
    })
  })
}
```

2. **Modify main.R to use sidebar layout**:

```r
ui <- function(request) {
  kwiz_theme <- bs_theme(...)  # existing theme

  page_fillable(
    theme = kwiz_theme,

    # Top navbar
    tags$nav(
      class = "navbar navbar-expand-lg navbar-dark bg-primary",
      style = "position: fixed; top: 0; left: 0; right: 0; z-index: 1000;",
      tags$div(
        class = "container-fluid",
        tags$span(
          class = "navbar-brand",
          tags$img(src = "static/images/logo.svg", height = "35px"),
          "Business Environment Benchmarking"
        ),
        # Tab navigation buttons here
      )
    ),

    # Main layout with sidebar
    tags$div(
      style = "margin-top: 60px; display: flex;",

      # Sidebar
      tags$div(
        id = "sidebar",
        style = "position: fixed; left: 0; width: 250px; height: calc(100vh - 60px);",
        global_filters$ui("global_filters", wbes_data)
      ),

      # Main content area
      tags$div(
        id = "main-content",
        style = "margin-left: 250px; width: calc(100% - 250px);
                 height: calc(100vh - 60px); overflow-y: auto; padding: 20px;",
        uiOutput("tab_content")
      )
    )
  )
}

server <- function(input, output, session) {
  wbes_data <- reactiveVal(NULL)

  # Load data
  observe({
    data <- load_wbes_data(...)
    wbes_data(data)
  })

  # Get global filters
  global_filters_state <- global_filters$server("global_filters", wbes_data)

  # Pass filters to all modules
  mod_overview$server("overview", wbes_data, global_filters_state)
  mod_country_profile$server("country_profile", wbes_data, global_filters_state)
  # ... etc
}
```

3. **Update each module to accept and use global filters**:

```r
#' @export
server <- function(id, wbes_data, global_filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # Apply global filters to data
    filtered_data <- reactive({
      req(wbes_data())
      data <- wbes_data()$latest

      if (!is.null(global_filters)) {
        filters <- global_filters()

        if (!is.null(filters$region) && length(filters$region) > 0) {
          data <- data[data$region %in% filters$region, ]
        }
        # ... apply other filters
      }

      data
    })

    # Rest of module logic uses filtered_data()
  })
}
```

### Option 2: CSS-Based Sidebar on Each Page (Quick Win)

**Pros:**
- Less risky, no major refactoring
- Can be implemented incrementally per module
- Maintains existing module structure

**Cons:**
- Filters not truly global across tabs
- Some code duplication

**Implementation:**

Add to `app/styles/main.scss`:

```scss
// Sidebar layout for individual pages
.page-with-sidebar {
  display: flex;
  height: calc(100vh - 120px);

  .filter-sidebar {
    position: sticky;
    top: 0;
    width: 250px;
    min-width: 250px;
    height: calc(100vh - 120px);
    overflow-y: auto;
    padding: 20px;
    border-right: 1px solid #dee2e6;
    background-color: #f8f9fa;
  }

  .main-content {
    flex: 1;
    overflow-y: auto;
    padding: 20px;
  }
}
```

Then update module UIs:

```r
ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "page-with-sidebar",

    # Sidebar
    tags$div(
      class = "filter-sidebar",
      h4("Filters"),
      selectInput(ns("region_filter"), "Region", ...),
      selectInput(ns("year_filter"), "Year", ...),
      # ... more filters
      actionButton(ns("reset"), "Reset", class = "btn-outline-primary w-100")
    ),

    # Main content
    tags$div(
      class = "main-content",
      # All the visualizations
    )
  )
}
```

### Option 3: Hybrid Approach (Practical Recommendation)

1. Keep existing tab-based navigation
2. Add shared filter state via reactive values
3. Use CSS for independent scrolling
4. Create filter component that can be reused

**Benefits:**
- Balances ease of implementation with functionality
- Incremental adoption possible
- Lower risk

## Recommended Next Steps

1. **Phase 1: CSS Improvements**
   - Add independent scrolling CSS (Option 2)
   - Test with one module (e.g., Overview)
   - Verify scrolling behavior

2. **Phase 2: Shared Filter State**
   - Create `global_filters` module (Option 1, step 1)
   - Add to main.R without changing layout
   - Update one module to use shared state

3. **Phase 3: Full Sidebar Layout**
   - Refactor main.R to sidebar layout (Option 1, step 2)
   - Update all modules to use shared filters
   - Comprehensive testing

## Code Examples

### Independent Scrolling CSS

Add to `app/styles/main.scss`:

```scss
// Enable independent scrolling
html, body {
  height: 100%;
  overflow: hidden;
}

.dashboard-container {
  display: flex;
  height: 100vh;
}

.dashboard-sidebar {
  width: 280px;
  height: 100%;
  overflow-y: auto;
  overflow-x: hidden;
  border-right: 1px solid var(--bs-border-color);
  background-color: var(--bs-light);
}

.dashboard-content {
  flex: 1;
  height: 100%;
  overflow-y: auto;
  overflow-x: hidden;
  padding: 2rem;
}
```

### Shared Reactive Filter State

```r
# In main.R server
global_filter_state <- reactiveValues(
  country = NULL,
  year = NULL,
  region = NULL,
  income_group = NULL,
  sector = NULL,
  firm_size = "all"
)

# Update filter state from any module
observeEvent(input$overview_region_filter, {
  global_filter_state$region <- input$overview_region_filter
})

# Pass to all modules
mod_overview$server("overview", wbes_data, global_filter_state)
```

## Testing Checklist

- [ ] Sidebar scrolls independently from main content
- [ ] Main content scrolls independently from sidebar
- [ ] Filter changes update all relevant visualizations
- [ ] Filter state persists when switching tabs (if global)
- [ ] Reset button clears all filters
- [ ] Mobile responsiveness (sidebar collapses on small screens)
- [ ] No layout breaks with long filter lists
- [ ] Performance acceptable with many filters applied

## Performance Considerations

1. **Reactive Efficiency**: Use `reactive()` not `observe()` for filter state
2. **Debouncing**: Consider debouncing filter inputs if many filters
3. **Data Caching**: Cache filtered results when possible
4. **Lazy Loading**: Load module data only when tab is active

## Browser Compatibility

- CSS `position: sticky` requires:
  - Chrome 56+
  - Firefox 59+
  - Safari 13+
  - Edge 16+

## References

- [bslib layout functions](https://rstudio.github.io/bslib/reference/index.html#layouts)
- [Shiny modules](https://shiny.rstudio.com/articles/modules.html)
- [CSS Flexbox](https://css-tricks.com/snippets/css/a-guide-to-flexbox/)
- [Rhino framework](https://appsilon.github.io/rhino/)

## Notes

- Current implementation preserves existing functionality
- Sidebar layout is a UX enhancement, not a core requirement
- Can be implemented incrementally without breaking changes
- Consider user testing before full rollout
