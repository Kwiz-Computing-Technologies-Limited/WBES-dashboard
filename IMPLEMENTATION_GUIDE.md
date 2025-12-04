# WBES Dashboard - Feature Implementation Guide

## Overview
This guide explains how to use the new features added to the WBES Dashboard:
1. **Nested Tab Navigation** - Organized tabs into logical groups
2. **Custom Regions** - Create and filter by custom country groupings
3. **Sticky Filters** - Keep filter panels visible while scrolling
4. **Styled KPI Cards** - Consistent colored backgrounds for all KPI cards
5. **Fixed Dropdown Z-Index** - Dropdowns now appear above other elements

---

## 1. Nested Tab Navigation

The dashboard now organizes tabs into three main groups:

### Structure:
- **Profiles** - Country, Sector, Regional, and Size profiles
- **Benchmarks** - Cross-Country, Cross-Sector, Cross-Regional, and Cross-Size comparisons
- **Domains** - Infrastructure, Finance, Corruption, Workforce, Performance, and Crime & Security

### Files Modified:
- `app/main.R` - Updated navigation structure using `nav_menu()`

### Usage:
Users can now navigate through dropdown menus in the main navigation bar to access related tabs.

---

## 2. Custom Regions Feature

### What It Does:
Allows users to create and manage custom regional groupings by selecting specific countries. Custom regions:
- Appear in all region filter dropdowns
- Can be deleted via the "Manage Custom Regions" button (gear icon)
- Persist across the session
- Work seamlessly with standard regions

### Files Created:
1. **`app/logic/custom_regions.R`** - Core logic for custom region management
   - `custom_region_modal_ui()` - Modal for creating regions
   - `manage_regions_modal_ui()` - Modal for viewing/deleting regions
   - `get_region_choices()` - Combines standard and custom regions
   - `filter_by_region()` - Filters data by region (supports custom)
2. **`app/view/comp_region_filter.R`** - Reusable region filter component (optional)

### How to Implement in Your Module:

#### Step 1: Add Required Imports
```r
box::use(
  shiny[showModal, removeModal, textInput, selectizeInput, modalDialog,
        modalButton, updateSelectInput],
  app/logic/custom_regions[get_region_choices, filter_by_region,
                            custom_region_modal_ui, manage_regions_modal_ui,
                            custom_regions_storage]
)
```

#### Step 2: Add UI Components
In your module's UI function, add both create and manage buttons:

```r
column(4,
  tags$div(
    class = "d-flex gap-2",
    tags$div(
      class = "flex-grow-1",
      selectInput(
        ns("region_filter"),
        "Region",
        choices = c("All Regions" = "all"),
        selected = "all"
      )
    ),
    tags$div(
      class = "d-flex align-items-end pb-3 gap-1",
      actionButton(
        ns("create_custom_region"),
        NULL,
        icon = icon("plus-circle"),
        class = "btn-sm btn-outline-primary",
        title = "Create Custom Region",
        style = "height: 38px; margin-bottom: 0;"
      ),
      actionButton(
        ns("manage_custom_regions"),
        NULL,
        icon = icon("cog"),
        class = "btn-sm btn-outline-secondary",
        title = "Manage Custom Regions",
        style = "height: 38px; margin-bottom: 0;"
      )
    )
  )
)
```

#### Step 3: Add Server Logic
In your module's server function:

```r
server <- function(id, wbes_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize custom regions storage
    custom_regions <- shiny::reactiveVal(list())

    # Update region choices when data or custom regions change
    observeEvent(list(wbes_data(), custom_regions()), {
      req(wbes_data())
      region_choices <- get_region_choices(wbes_data, custom_regions())
      updateSelectInput(session, "region_filter", choices = region_choices)
    }, ignoreNULL = FALSE)

    # Show modal to create custom region
    observeEvent(input$create_custom_region, {
      req(wbes_data())
      countries <- sort(wbes_data()$countries)
      showModal(custom_region_modal_ui(ns, countries))
    })

    # Save custom region
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

      removeModal()
    })

    # Show modal to manage custom regions
    observeEvent(input$manage_custom_regions, {
      showModal(manage_regions_modal_ui(ns, custom_regions()))
    })

    # Delete custom region
    observeEvent(input$delete_region_name, {
      req(input$delete_region_name)
      region_to_delete <- input$delete_region_name

      current_regions <- custom_regions()
      current_regions[[region_to_delete]] <- NULL
      custom_regions(current_regions)
      custom_regions_storage(current_regions)

      # Refresh the modal
      showModal(manage_regions_modal_ui(ns, custom_regions()))
    })

    # Use custom region filter in your data filtering
    filtered_data <- reactive({
      req(wbes_data())
      data <- wbes_data()$latest

      # This function handles both standard and custom regions
      data <- filter_by_region(data, input$region_filter, custom_regions())

      # ... other filters ...

      data
    })
  })
}
```

### Example Implementation:
See `app/view/mod_overview.R` for a complete working example.

---

## 3. Sticky Filter Panels

### What It Does:
Keeps filter controls visible at the top of the page while users scroll through charts and data, making it easy to adjust filters without scrolling back to the top.

### How to Implement:

Simply add the `sticky-filters` class to your filter row:

```r
# Before:
fluidRow(
  class = "mb-4",
  column(12, card(...))
)

# After:
fluidRow(
  class = "sticky-filters",  # <- Add this class
  column(12,
    card(
      class = "filter-card",  # <- Add this class to the card
      ...
    )
  )
)
```

### CSS Classes:
- `.sticky-filters` - Makes the row sticky at the top (positioned 60px below top to account for navbar)
- `.filter-card` - Ensures proper z-index for the filter card

### Important Notes:
- The sticky position is set to `top: 60px` to account for the navbar height
- Filters will stay visible as you scroll down the page
- The gradient effect at the top provides a smooth visual transition

### Files Modified:
- `app/styles/main.scss` - Added sticky filter styles with proper positioning

---

## 4. Styled KPI Cards

### What It Does:
Provides consistent, colored backgrounds for KPI/metric cards across all modules, matching the style used in Crime, Workforce, and Performance tabs.

### Available Card Styles:
- **Default (Primary)** - Teal gradient background
- `.kpi-box-coral` - Coral/orange gradient
- `.kpi-box-success` - Green gradient
- `.kpi-box-warning` - Yellow/amber gradient
- `.kpi-box-info` - Blue gradient
- `.kpi-box-danger` - Red gradient

### How to Use:

```r
# Example KPI card with colored background
tags$div(class = "kpi-box kpi-box-success",  # <- Add modifier class
  tags$div(class = "kpi-value", "150"),
  tags$div(class = "kpi-label", "Countries Surveyed")
)

# Example card structure
tags$div(class = "card h-100",
  tags$div(class = "card-body",
    fluidRow(
      column(6,
        tags$div(class = "kpi-box",  # Default teal
          tags$div(class = "kpi-value", "168"),
          tags$div(class = "kpi-label", "Countries")
        )
      ),
      column(6,
        tags$div(class = "kpi-box kpi-box-success",  # Green
          tags$div(class = "kpi-value", "150,000"),
          tags$div(class = "kpi-label", "Firms Surveyed")
        )
      )
    )
  )
)
```

### Color Scheme Guidelines:
- **Default/Primary (Teal)** - General metrics, counts
- **Success (Green)** - Positive metrics, growth, capacity utilization
- **Coral/Secondary** - Featured metrics, highlights
- **Info (Blue)** - Informational metrics, percentages
- **Warning (Yellow)** - Caution metrics, moderate concerns
- **Danger (Red)** - Problem metrics, obstacles, constraints

### Files Modified:
- `app/styles/main.scss` - Updated `.kpi-box` styles with gradients

---

## 5. Fixed Dropdown Z-Index

### What It Does:
Ensures that dropdown menus from filters appear ABOVE cards and other content, instead of behind them.

### How It Works:
The fix is automatically applied via CSS. No code changes needed in your modules.

### Files Modified:
- `app/styles/main.scss` - Added z-index rules for:
  - `.selectize-dropdown`
  - `.selectize-input`
  - `.bootstrap-select .dropdown-menu`
  - `.shiny-input-container`

### CSS Added:
```scss
.selectize-dropdown {
  z-index: 1031 !important;
  box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15) !important;
  border: 1px solid $gray-300 !important;
}

.selectize-input,
.bootstrap-select .dropdown-menu,
.shiny-input-container {
  z-index: 1030 !important;
}
```

---

## Quick Reference: Updating Existing Modules

### Checklist for Adding All Features:

1. **Add sticky filters:**
   - [ ] Add `class = "sticky-filters"` to filter row
   - [ ] Add `class = "filter-card"` to filter card

2. **Add custom regions:**
   - [ ] Import custom region functions in `box::use()`
   - [ ] Add "Create Custom Region" button next to region filter
   - [ ] Add `custom_regions` reactiveVal
   - [ ] Update region filter choices with `get_region_choices()`
   - [ ] Add modal and save logic
   - [ ] Use `filter_by_region()` in data filtering

3. **Style KPI cards:**
   - [ ] Ensure cards use `.kpi-box` class
   - [ ] Add appropriate color modifier classes
   - [ ] Use consistent structure (`.kpi-value` and `.kpi-label`)

4. **Dropdown z-index:**
   - [ ] No action needed - automatic via CSS

---

## Testing Checklist

After implementing these features:

- [ ] Filter panel stays visible when scrolling down
- [ ] Clicking "Create Custom Region" button opens modal
- [ ] Can create custom region with multiple countries
- [ ] Custom region appears in region filter dropdown
- [ ] Filtering by custom region works correctly
- [ ] Dropdown menus appear above cards (not behind)
- [ ] KPI cards have colored backgrounds
- [ ] All KPI cards have consistent styling
- [ ] Nested navigation menus work correctly
- [ ] All profile tabs accessible via "Profiles" menu
- [ ] All benchmark tabs accessible via "Benchmarks" menu
- [ ] All domain tabs accessible via "Domains" menu

---

## Example Modules

For complete working examples, see:
- **Custom Regions + Sticky Filters:** `app/view/mod_overview.R`
- **Styled KPI Cards:** `app/view/mod_crime.R`, `app/view/mod_workforce.R`, `app/view/mod_performance.R`
- **Nested Tabs:** `app/main.R`

---

## Troubleshooting

### Custom Regions Not Showing
- Ensure `custom_regions_storage` is imported
- Check that `observeEvent` for custom_regions is set with `ignoreNULL = FALSE`
- Verify `get_region_choices()` is called when updating filter

### Filters Not Sticky
- Confirm `sticky-filters` class is on the `fluidRow`, not the `card`
- Check that there's no conflicting CSS

### Dropdowns Still Behind Cards
- Clear browser cache
- Verify SCSS was recompiled
- Check browser console for CSS errors

### KPI Cards No Background
- Ensure using `.kpi-box` class (not `.value-box`)
- Check SCSS compilation
- Verify gradient colors in `main.scss`

---

## File Reference

### New Files:
```
app/logic/custom_regions.R
app/view/comp_region_filter.R
app/view/mod_sector_profile.R
app/view/mod_regional_profile.R
app/view/mod_size_profile.R
app/view/mod_benchmark_sector.R
app/view/mod_benchmark_regional.R
app/view/mod_benchmark_size.R
IMPLEMENTATION_GUIDE.md
```

### Modified Files:
```
app/main.R
app/styles/main.scss
app/view/mod_overview.R
```

---

## Support

For questions or issues, please refer to the example implementations or create an issue in the repository.
