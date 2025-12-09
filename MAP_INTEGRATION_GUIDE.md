# Map Integration Guide

## How to Add Interactive Maps to Any Module

This guide shows the exact steps to add interactive maps to any WBES dashboard module.

---

## Step 1: Add Imports to Module

In the `box::use()` section at the top of the module file, add:

```r
box::use(
  # ... existing imports ...
  leaflet[leafletOutput, renderLeaflet],
  app/logic/wbes_map[create_wbes_map, get_country_coordinates]
)
```

---

## Step 2: Add Map Card to UI

Add this fluidRow to the UI function, typically right after KPIs and before main charts:

```r
# Geographic Distribution Map
fluidRow(
  class = "mb-4",
  column(12,
    card(
      card_header(icon("map-marked-alt"), " Geographic Distribution"),
      card_body(
        leafletOutput(ns("indicator_map"), height = "400px"),
        p(
          class = "text-muted small mt-2",
          "Interactive map showing geographic distribution. Click markers for details."
        )
      )
    )
  )
),
```

**Replace**:
- `"indicator_map"` → Use a descriptive ID like `"corruption_map"`, `"infrastructure_map"`, etc.
- `" Geographic Distribution"` → Customize title for the module

---

## Step 3: Add Map Rendering to Server

In the server function, add the renderLeaflet after KPI outputs:

```r
# Interactive Map
output$indicator_map <- renderLeaflet({
  req(filtered(), wbes_data())
  d <- filtered()
  coords <- get_country_coordinates(wbes_data())

  # Choose which indicator to visualize
  indicator_col <- "IC.FRM.INFRA.ZS"  # Example: infrastructure indicator

  create_wbes_map(
    data = d,
    coordinates = coords,
    indicator_col = indicator_col,
    indicator_label = "Infrastructure as Obstacle",
    color_palette = "YlOrRd",  # "Reds", "Blues", "Greens", "YlOrRd", etc.
    reverse_colors = TRUE      # TRUE if higher values are worse
  )
})
```

**Customize**:
- `output$indicator_map` → Match the ID from Step 2
- `indicator_col` → The column name from WBES data to visualize
- `indicator_label` → Human-readable label for the legend
- `color_palette` → Choose appropriate color scheme
- `reverse_colors` → TRUE for "higher is worse" (obstacles), FALSE for "higher is better" (performance)

---

## Color Palette Guide

**For Obstacles/Problems** (higher = worse):
- `color_palette = "Reds"` or `"YlOrRd"`
- `reverse_colors = TRUE`

**For Performance/Capacity** (higher = better):
- `color_palette = "Greens"` or `"YlGn"`
- `reverse_colors = FALSE`

**Neutral/Mixed**:
- `color_palette = "Blues"` or `"Purples"`

---

## Dynamic Indicator Selection

If your module has an indicator selector (like Corruption has), make the map reactive:

```r
output$indicator_map <- renderLeaflet({
  req(filtered(), wbes_data())
  d <- filtered()
  coords <- get_country_coordinates(wbes_data())

  # Use the selected indicator from input
  indicator <- input$indicator  # Assumes selectInput with id="indicator"

  # Conditional label based on selection
  label <- if (indicator == "IC.FRM.CORR.ZS") {
    "Corruption as Obstacle"
  } else {
    "Bribery Incidence"
  }

  create_wbes_map(
    data = d,
    coordinates = coords,
    indicator_col = indicator,
    indicator_label = label,
    color_palette = "Reds",
    reverse_colors = TRUE
  )
})
```

---

## Complete Example: Corruption Module

### UI Addition:
```r
# Geographic Distribution Map
fluidRow(
  class = "mb-4",
  column(12,
    card(
      card_header(icon("map-marked-alt"), " Geographic Distribution of Corruption"),
      card_body(
        leafletOutput(ns("corruption_map"), height = "400px"),
        p(
          class = "text-muted small mt-2",
          "Interactive map showing corruption levels by country. Larger, redder circles indicate higher corruption. Click markers for details."
        )
      )
    )
  )
),
```

### Server Addition:
```r
# Interactive Map
output$corruption_map <- renderLeaflet({
  req(filtered(), wbes_data())
  d <- filtered()
  coords <- get_country_coordinates(wbes_data())

  indicator <- input$indicator

  create_wbes_map(
    data = d,
    coordinates = coords,
    indicator_col = indicator,
    indicator_label = if (indicator == "IC.FRM.CORR.ZS") "Corruption as Obstacle" else "Bribery Incidence",
    color_palette = "Reds",
    reverse_colors = TRUE
  )
})
```

---

## Module-Specific Indicator Codes

### Domain Modules:
- **Infrastructure**: `IC.FRM.INFRA.ZS` - Infrastructure as Obstacle
- **Finance**: `IC.FRM.FINA.ZS` - Finance Access as Obstacle
- **Corruption**: `IC.FRM.CORR.ZS` - Corruption as Obstacle, `IC.FRM.BRIB.ZS` - Bribery Incidence
- **Workforce**: `IC.FRM.LBRC.ZS` - Labor as Obstacle
- **Performance**: `IC.FRM.CAPU.ZS` - Capacity Utilization, `IC.FRM.EXPRT.ZS` - Export Participation
- **Crime**: `IC.FRM.CRIM.ZS` - Crime as Obstacle

### Profile/Benchmark Modules:
Use composite indices or key indicators relevant to the profile:
- Business Environment Score (average of multiple indicators)
- Primary constraint indicator

---

## Troubleshooting

### Map doesn't appear:
- Check that `leaflet` and map component imports are present
- Verify `filtered()` data contains the indicator column
- Check browser console for JavaScript errors

### Coordinates missing:
- Ensure `wbes_data()$country_coordinates` exists
- Verify country names match between data and coordinates

### Colors not showing correctly:
- Check `reverse_colors` setting
- Try different color palettes
- Verify indicator values are numeric

---

## Status: Modules with Maps

- ✅ **Corruption** - Complete with dynamic indicator
- ⏳ **Infrastructure** - Pending
- ⏳ **Finance** - Pending
- ⏳ **Workforce** - Pending
- ⏳ **Performance** - Pending
- ⏳ **Crime** - Pending
- ⏳ **Country Profile** - Pending
- ⏳ **Regional Profile** - Pending
- ⏳ **Sector Profile** - Pending
- ⏳ **Size Profile** - Pending
- ⏳ **Country Benchmark** - Pending
- ⏳ **Regional Benchmark** - Pending
- ⏳ **Sector Benchmark** - Pending
- ⏳ **Size Benchmark** - Pending
- ⏳ **Overview** - Pending
- ⏳ **Custom Analysis** - Pending
- ⏳ **Data Quality** - Pending

---

**Last Updated**: 2025-12-09
