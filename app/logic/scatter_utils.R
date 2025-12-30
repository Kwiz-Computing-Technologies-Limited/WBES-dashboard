# app/logic/scatter_utils.R
# Reusable scatter plot utilities with trend lines and shape encoding

box::use(
  plotly[plot_ly, layout, add_trace, config],
  stats[lm, predict, coef, na.omit]
)

# Shape mapping for categorical variables
SHAPE_MAP <- c(
  "circle", "square", "diamond", "cross", "x",
  "triangle-up", "triangle-down", "pentagon", "hexagon", "star"
)

#' Create scatter plot with trend line and optional shape encoding
#'
#' @param data Data frame with x and y columns
#' @param x_col Column name for x-axis
#' @param y_col Column name for y-axis
#' @param color_col Column name for color grouping (e.g., "region")
#' @param shape_col Column name for shape encoding (3rd dimension, optional)
#' @param title Plot title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param show_trend_line Whether to show trend line (default TRUE)
#' @param marker_size Size of markers (default 12)
#' @param marker_opacity Opacity of markers (default 0.7)
#' @return Plotly scatter plot object
#' @export
create_scatter_with_trend <- function(
  data,
  x_col,
  y_col,
  color_col = NULL,
  shape_col = NULL,
  title = "",
  x_label = x_col,
  y_label = y_col,
  show_trend_line = TRUE,
  marker_size = 12,
  marker_opacity = 0.7
) {
  # Validate inputs

if (is.null(data) || nrow(data) == 0) {
    return(
      plot_ly() |>
        layout(
          annotations = list(list(
            text = "No data available",
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5
          ))
        )
    )
  }

  if (!x_col %in% names(data) || !y_col %in% names(data)) {
    return(
      plot_ly() |>
        layout(
          annotations = list(list(
            text = paste("Missing column:", x_col, "or", y_col),
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5
          ))
        )
    )
  }

  # Filter valid rows
  valid_data <- data[!is.na(data[[x_col]]) & !is.na(data[[y_col]]), ]
  if (nrow(valid_data) == 0) {
    return(
      plot_ly() |>
        layout(
          annotations = list(list(
            text = "No valid data points",
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5
          ))
        )
    )
  }

  # Color palette
  colors <- c("#1B6B5F", "#F49B7A", "#2E7D32", "#17a2b8", "#6C757D",
              "#F4A460", "#dc3545", "#9c27b0", "#3f51b5", "#009688")

  # Determine if we have grouping
  has_color <- !is.null(color_col) && color_col %in% names(valid_data)
  has_shape <- !is.null(shape_col) && shape_col %in% names(valid_data) &&
               !all(is.na(valid_data[[shape_col]]))

  # Build the plot
  if (has_color && has_shape) {
    # Both color and shape grouping
    color_groups <- unique(valid_data[[color_col]][!is.na(valid_data[[color_col]])])
    shape_groups <- unique(valid_data[[shape_col]][!is.na(valid_data[[shape_col]])])

    p <- plot_ly()

    for (i in seq_along(color_groups)) {
      cg <- color_groups[i]
      for (j in seq_along(shape_groups)) {
        sg <- shape_groups[j]
        subset_data <- valid_data[valid_data[[color_col]] == cg & valid_data[[shape_col]] == sg, ]

        if (nrow(subset_data) > 0) {
          p <- p |> add_trace(
            data = subset_data,
            x = ~get(x_col),
            y = ~get(y_col),
            type = "scatter",
            mode = "markers",
            name = paste(cg, "-", sg),
            legendgroup = cg,
            text = if ("country" %in% names(subset_data)) ~country else ~get(color_col),
            hoverinfo = "text+x+y",
            marker = list(
              size = marker_size,
              opacity = marker_opacity,
              color = colors[((i - 1) %% length(colors)) + 1],
              symbol = SHAPE_MAP[((j - 1) %% length(SHAPE_MAP)) + 1]
            )
          )
        }
      }
    }
  } else if (has_shape) {
    # Only shape grouping (no color)
    shape_groups <- unique(valid_data[[shape_col]][!is.na(valid_data[[shape_col]])])

    p <- plot_ly()

    for (j in seq_along(shape_groups)) {
      sg <- shape_groups[j]
      subset_data <- valid_data[valid_data[[shape_col]] == sg, ]

      if (nrow(subset_data) > 0) {
        p <- p |> add_trace(
          data = subset_data,
          x = ~get(x_col),
          y = ~get(y_col),
          type = "scatter",
          mode = "markers",
          name = as.character(sg),
          text = if ("country" %in% names(subset_data)) ~country else as.character(sg),
          hoverinfo = "text+x+y",
          marker = list(
            size = marker_size,
            opacity = marker_opacity,
            color = colors[((j - 1) %% length(colors)) + 1],
            symbol = SHAPE_MAP[((j - 1) %% length(SHAPE_MAP)) + 1]
          )
        )
      }
    }
  } else if (has_color) {
    # Only color grouping (existing behavior + trend line)
    p <- plot_ly(
      valid_data,
      x = ~get(x_col),
      y = ~get(y_col),
      type = "scatter",
      mode = "markers",
      color = ~get(color_col),
      colors = colors,
      text = if ("country" %in% names(valid_data)) ~country else ~get(color_col),
      hoverinfo = "text+x+y",
      marker = list(size = marker_size, opacity = marker_opacity)
    )
  } else {
    # No grouping
    p <- plot_ly(
      valid_data,
      x = ~get(x_col),
      y = ~get(y_col),
      type = "scatter",
      mode = "markers",
      text = if ("country" %in% names(valid_data)) ~country else NULL,
      hoverinfo = "text+x+y",
      marker = list(size = marker_size, opacity = marker_opacity, color = colors[1])
    )
  }

  # Add trend line if requested and we have enough data points
  if (show_trend_line && nrow(valid_data) >= 2) {
    # Fit linear model
    fit_data <- valid_data[, c(x_col, y_col)]
    names(fit_data) <- c("x", "y")
    fit_data <- na.omit(fit_data)

    if (nrow(fit_data) >= 2) {
      tryCatch({
        model <- lm(y ~ x, data = fit_data)

        # Generate trend line points
        x_range <- range(fit_data$x, na.rm = TRUE)
        trend_x <- seq(x_range[1], x_range[2], length.out = 100)
        trend_y <- predict(model, newdata = data.frame(x = trend_x))

        # Calculate R-squared for hover info
        r_squared <- round(summary(model)$r.squared, 3)
        slope <- round(coef(model)[2], 3)

        p <- p |> add_trace(
          x = trend_x,
          y = trend_y,
          type = "scatter",
          mode = "lines",
          name = paste0("Trend (R\u00b2=", r_squared, ")"),
          line = list(color = "#6C757D", dash = "dash", width = 2),
          hovertemplate = paste0("Trend Line<br>Slope: ", slope, "<br>R\u00b2: ", r_squared, "<extra></extra>"),
          inherit = FALSE
        )
      }, error = function(e) {
        # Silently skip trend line if fitting fails
      })
    }
  }

  # Apply layout with legend positioned to avoid x-axis overlap
  p <- p |>
    layout(
      title = list(text = title, font = list(size = 14)),
      xaxis = list(title = x_label, titlefont = list(size = 11)),
      yaxis = list(title = y_label, titlefont = list(size = 11)),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        y = -0.2,
        x = 0.5,
        xanchor = "center",
        font = list(size = 10)
      ),
      margin = list(l = 60, r = 40, t = 40, b = 100),  # Add more bottom margin for legend
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = FALSE)

  return(p)
}
