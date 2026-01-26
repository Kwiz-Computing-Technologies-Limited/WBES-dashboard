# app/logic/stat_utils.R
# Statistical utility functions for correlation matrices and significance tests

box::use(
  stats[cor, cor.test, t.test, wilcox.test, aov, TukeyHSD, anova, pairwise.t.test,
        pairwise.wilcox.test, shapiro.test, bartlett.test, kruskal.test, p.adjust,
        complete.cases, var, median, residuals],
  tools[toTitleCase],
  dplyr[filter, select, mutate, summarise, group_by, n, across, where],
  tidyr[pivot_longer, drop_na],
  shiny[tags, HTML, icon, div, span, fluidRow, column, tagList]
)

#' Calculate correlation matrix with p-values
#' @param data Data frame with numeric columns
#' @param vars Character vector of variable names to include
#' @param method Correlation method: "pearson", "spearman", or "kendall"
#' @return List with correlation matrix and p-value matrix
#' @export
calculate_correlation_matrix <- function(data, vars, method = "pearson") {
  # Filter to only include specified variables that exist in data
  available_vars <- vars[vars %in% names(data)]

  if (length(available_vars) < 2) {
    return(NULL)
  }

  # Extract numeric data
  num_data <- data[, available_vars, drop = FALSE]
  num_data <- as.data.frame(lapply(num_data, as.numeric))

  # Remove rows with all NAs

  complete_rows <- rowSums(!is.na(num_data)) >= 2
  num_data <- num_data[complete_rows, , drop = FALSE]

  if (nrow(num_data) < 5) {
    return(NULL)
  }

  n_vars <- length(available_vars)
  cor_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  p_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  rownames(cor_matrix) <- colnames(cor_matrix) <- available_vars
  rownames(p_matrix) <- colnames(p_matrix) <- available_vars

  for (i in seq_len(n_vars)) {
    for (j in seq_len(n_vars)) {
      if (i == j) {
        cor_matrix[i, j] <- 1
        p_matrix[i, j] <- 0
      } else {
        x <- num_data[[available_vars[i]]]
        y <- num_data[[available_vars[j]]]

        # Get complete pairs
        complete_pairs <- complete.cases(x, y)
        if (sum(complete_pairs) >= 5) {
          tryCatch({
            test_result <- cor.test(x[complete_pairs], y[complete_pairs], method = method)
            cor_matrix[i, j] <- test_result$estimate
            p_matrix[i, j] <- test_result$p.value
          }, error = function(e) {
            cor_matrix[i, j] <- NA
            p_matrix[i, j] <- NA
          })
        }
      }
    }
  }

  list(
    correlation = cor_matrix,
    p_values = p_matrix,
    n_obs = nrow(num_data),
    method = method
  )
}

#' Format correlation matrix as HTML table
#' @param cor_result Result from calculate_correlation_matrix
#' @param var_labels Named vector mapping variable names to display labels
#' @param show_significance Whether to show significance stars
#' @return HTML string
#' @export
format_correlation_table <- function(cor_result, var_labels = NULL, show_significance = TRUE) {
  if (is.null(cor_result)) {
    return(tags$div(
      class = "text-muted text-center p-3",
      icon("exclamation-circle"),
      " Insufficient data for correlation analysis"
    ))
  }

  cor_mat <- cor_result$correlation
  p_mat <- cor_result$p_values
  vars <- rownames(cor_mat)
  n <- length(vars)

  # Create display labels
  if (is.null(var_labels)) {
    display_labels <- gsub("_", " ", toTitleCase(gsub("_pct$|_per_month$|_hrs$", "", vars)))
  } else {
    display_labels <- sapply(vars, function(v) {
      if (v %in% names(var_labels)) var_labels[[v]] else gsub("_", " ", toTitleCase(v))
    })
  }

  # Build HTML table
  header_cells <- paste0("<th style='padding: 4px; font-size: 11px; text-align: center; background: #f8f9fa; min-width: 60px;'>",
                         display_labels, "</th>", collapse = "")
  header_row <- paste0("<tr><th style='padding: 4px; background: #f8f9fa;'></th>", header_cells, "</tr>")

  body_rows <- sapply(seq_len(n), function(i) {
    cells <- sapply(seq_len(n), function(j) {
      r <- cor_mat[i, j]
      p <- p_mat[i, j]

      if (is.na(r)) {
        cell_content <- "-"
        bg_color <- "#ffffff"
      } else {
        # Color based on correlation strength
        if (i == j) {
          bg_color <- "#e9ecef"
          cell_content <- "1.00"
        } else {
          abs_r <- abs(r)
          if (abs_r >= 0.7) {
            bg_color <- if (r > 0) "#c8e6c9" else "#ffcdd2"
          } else if (abs_r >= 0.5) {
            bg_color <- if (r > 0) "#dcedc8" else "#ffe0b2"
          } else if (abs_r >= 0.3) {
            bg_color <- if (r > 0) "#f1f8e9" else "#fff3e0"
          } else {
            bg_color <- "#ffffff"
          }

          # Add significance stars
          stars <- ""
          if (show_significance && !is.na(p)) {
            if (p < 0.001) stars <- "***"
            else if (p < 0.01) stars <- "**"
            else if (p < 0.05) stars <- "*"
          }
          cell_content <- sprintf("%.2f%s", r, stars)
        }
      }

      sprintf("<td style='padding: 4px; text-align: center; font-size: 11px; background: %s;'>%s</td>",
              bg_color, cell_content)
    })

    paste0("<tr><th style='padding: 4px; font-size: 11px; text-align: right; background: #f8f9fa;'>",
           display_labels[i], "</th>", paste(cells, collapse = ""), "</tr>")
  })

  table_html <- paste0(
    "<div style='overflow-x: auto;'>",
    "<table class='table table-sm table-bordered' style='font-size: 11px; margin-bottom: 0;'>",
    "<thead>", header_row, "</thead>",
    "<tbody>", paste(body_rows, collapse = ""), "</tbody>",
    "</table>",
    "</div>",
    "<div class='small text-muted mt-1'>",
    sprintf("n = %d | Method: %s | ", cor_result$n_obs, cor_result$method),
    "Significance: * p < 0.05, ** p < 0.01, *** p < 0.001",
    "</div>"
  )

  HTML(table_html)
}

#' Perform paired comparison test (t-test or Wilcoxon) for two groups
#' @param group1_values Numeric vector for group 1
#' @param group2_values Numeric vector for group 2
#' @param group1_name Name of group 1
#' @param group2_name Name of group 2
#' @param paired Whether the test should be paired
#' @return List with test results
#' @export
paired_comparison_test <- function(group1_values, group2_values,
                                    group1_name = "Group 1", group2_name = "Group 2",
                                    paired = FALSE) {

  # Remove NAs
  g1 <- as.numeric(group1_values[!is.na(group1_values)])
  g2 <- as.numeric(group2_values[!is.na(group2_values)])

  if (length(g1) < 3 || length(g2) < 3) {
    return(list(
      test_type = "insufficient_data",
      p_value = NA,
      statistic = NA,
      message = "Insufficient data for statistical test"
    ))
  }

  # Check normality for each group
  normal_g1 <- tryCatch({
    if (length(g1) >= 3 && length(g1) <= 5000) {
      shapiro.test(g1)$p.value > 0.05
    } else {
      TRUE  # Assume normal for large samples
    }
  }, error = function(e) TRUE)

  normal_g2 <- tryCatch({
    if (length(g2) >= 3 && length(g2) <= 5000) {
      shapiro.test(g2)$p.value > 0.05
    } else {
      TRUE
    }
  }, error = function(e) TRUE)

  # Choose test based on normality
  if (normal_g1 && normal_g2) {
    test_result <- tryCatch({
      t.test(g1, g2, paired = paired)
    }, error = function(e) NULL)

    if (!is.null(test_result)) {
      return(list(
        test_type = if (paired) "Paired t-test" else "Welch's t-test",
        p_value = test_result$p.value,
        statistic = test_result$statistic,
        conf_int = test_result$conf.int,
        mean_diff = mean(g1) - mean(g2),
        effect_size = (mean(g1) - mean(g2)) / sqrt((var(g1) + var(g2)) / 2),  # Cohen's d
        message = sprintf("t = %.3f, df = %.1f, p = %.4f",
                          test_result$statistic, test_result$parameter, test_result$p.value)
      ))
    }
  }

  # Fall back to Wilcoxon test
  test_result <- tryCatch({
    wilcox.test(g1, g2, paired = paired)
  }, error = function(e) NULL)

  if (!is.null(test_result)) {
    return(list(
      test_type = if (paired) "Wilcoxon signed-rank test" else "Mann-Whitney U test",
      p_value = test_result$p.value,
      statistic = test_result$statistic,
      median_diff = median(g1) - median(g2),
      message = sprintf("W = %.1f, p = %.4f", test_result$statistic, test_result$p.value)
    ))
  }

  list(
    test_type = "test_failed",
    p_value = NA,
    statistic = NA,
    message = "Statistical test could not be performed"
  )
}

#' Perform ANOVA with Tukey HSD post-hoc test for multiple groups
#' @param data Data frame with values and groups
#' @param value_col Name of the column containing values
#' @param group_col Name of the column containing group labels
#' @return List with ANOVA results and Tukey HSD comparisons
#' @export
anova_with_tukey <- function(data, value_col, group_col) {
  # Prepare data
  df <- data.frame(
    value = as.numeric(data[[value_col]]),
    group = as.factor(data[[group_col]])
  )
  df <- df[complete.cases(df), ]

  # Check minimum requirements
  group_counts <- table(df$group)
  valid_groups <- names(group_counts)[group_counts >= 2]

  if (length(valid_groups) < 2) {
    return(list(
      test_type = "insufficient_groups",
      p_value = NA,
      message = "Need at least 2 groups with 2+ observations each"
    ))
  }

  df <- df[df$group %in% valid_groups, ]
  df$group <- droplevels(df$group)

  # Check homogeneity of variances
  homogeneous <- tryCatch({
    bartlett.test(value ~ group, data = df)$p.value > 0.05
  }, error = function(e) TRUE)

  # Check normality of residuals
  normal_residuals <- tryCatch({
    model <- aov(value ~ group, data = df)
    resids <- residuals(model)
    if (length(resids) >= 3 && length(resids) <= 5000) {
      shapiro.test(resids)$p.value > 0.05
    } else {
      TRUE
    }
  }, error = function(e) TRUE)

  # Perform appropriate test
  if (normal_residuals && homogeneous) {
    # Standard ANOVA + Tukey HSD
    tryCatch({
      model <- aov(value ~ group, data = df)
      anova_result <- anova(model)
      tukey_result <- TukeyHSD(model)

      # Extract Tukey results as data frame
      tukey_df <- as.data.frame(tukey_result$group)
      tukey_df$comparison <- rownames(tukey_df)
      tukey_df$significant <- tukey_df$`p adj` < 0.05

      return(list(
        test_type = "ANOVA + Tukey HSD",
        p_value = anova_result$`Pr(>F)`[1],
        f_statistic = anova_result$`F value`[1],
        df1 = anova_result$Df[1],
        df2 = anova_result$Df[2],
        tukey = tukey_df,
        n_groups = length(unique(df$group)),
        n_obs = nrow(df),
        message = sprintf("F(%d, %d) = %.2f, p = %.4f",
                          anova_result$Df[1], anova_result$Df[2],
                          anova_result$`F value`[1], anova_result$`Pr(>F)`[1])
      ))
    }, error = function(e) {
      list(test_type = "anova_failed", p_value = NA, message = e$message)
    })
  } else {
    # Kruskal-Wallis + pairwise Wilcoxon
    tryCatch({
      kw_result <- kruskal.test(value ~ group, data = df)

      # Pairwise comparisons with Holm correction
      pw_result <- pairwise.wilcox.test(df$value, df$group, p.adjust.method = "holm")

      # Convert p-value matrix to data frame
      pw_df <- as.data.frame(as.table(pw_result$p.value))
      names(pw_df) <- c("Group1", "Group2", "p_adj")
      pw_df <- pw_df[!is.na(pw_df$p_adj), ]
      pw_df$comparison <- paste(pw_df$Group1, "-", pw_df$Group2)
      pw_df$significant <- pw_df$p_adj < 0.05

      return(list(
        test_type = "Kruskal-Wallis + Pairwise Wilcoxon (Holm)",
        p_value = kw_result$p.value,
        statistic = kw_result$statistic,
        df = kw_result$parameter,
        pairwise = pw_df,
        n_groups = length(unique(df$group)),
        n_obs = nrow(df),
        message = sprintf("χ²(%d) = %.2f, p = %.4f",
                          kw_result$parameter, kw_result$statistic, kw_result$p.value)
      ))
    }, error = function(e) {
      list(test_type = "kruskal_failed", p_value = NA, message = e$message)
    })
  }
}

#' Format ANOVA/Tukey results as HTML
#' @param result Result from anova_with_tukey
#' @param title Optional title for the section
#' @return HTML tags
#' @export
format_anova_results <- function(result, title = "Statistical Test Results") {
  if (is.null(result) || result$test_type %in% c("insufficient_groups", "anova_failed", "kruskal_failed")) {
    return(tags$div(
      class = "alert alert-secondary py-2 mt-2",
      tags$small(
        icon("info-circle"),
        if (!is.null(result$message)) result$message else "Statistical test not available"
      )
    ))
  }

  # Determine significance
  sig_class <- if (!is.na(result$p_value) && result$p_value < 0.05) "alert-success" else "alert-secondary"
  sig_text <- if (!is.na(result$p_value) && result$p_value < 0.05) {
    "Significant differences found between groups"
  } else {
    "No significant differences between groups"
  }

  # Build pairwise comparisons table
  if (!is.null(result$tukey)) {
    pw_data <- result$tukey
    pw_rows <- apply(pw_data, 1, function(row) {
      sig_star <- if (as.numeric(row["p adj"]) < 0.001) "***"
                  else if (as.numeric(row["p adj"]) < 0.01) "**"
                  else if (as.numeric(row["p adj"]) < 0.05) "*"
                  else ""
      row_class <- if (as.numeric(row["p adj"]) < 0.05) "table-success" else ""
      sprintf("<tr class='%s'><td>%s</td><td>%.3f</td><td>%.3f</td><td>%.4f%s</td></tr>",
              row_class, row["comparison"], as.numeric(row["diff"]),
              as.numeric(row["upr"]) - as.numeric(row["lwr"]),
              as.numeric(row["p adj"]), sig_star)
    })

    pw_table <- paste0(
      "<table class='table table-sm table-bordered mt-2' style='font-size: 11px;'>",
      "<thead><tr><th>Comparison</th><th>Diff</th><th>CI Width</th><th>p (adj)</th></tr></thead>",
      "<tbody>", paste(pw_rows, collapse = ""), "</tbody></table>"
    )
  } else if (!is.null(result$pairwise)) {
    pw_data <- result$pairwise
    pw_rows <- apply(pw_data, 1, function(row) {
      p_val <- as.numeric(row["p_adj"])
      sig_star <- if (p_val < 0.001) "***"
                  else if (p_val < 0.01) "**"
                  else if (p_val < 0.05) "*"
                  else ""
      row_class <- if (p_val < 0.05) "table-success" else ""
      sprintf("<tr class='%s'><td>%s</td><td>%.4f%s</td></tr>",
              row_class, row["comparison"], p_val, sig_star)
    })

    pw_table <- paste0(
      "<table class='table table-sm table-bordered mt-2' style='font-size: 11px;'>",
      "<thead><tr><th>Comparison</th><th>p (adj)</th></tr></thead>",
      "<tbody>", paste(pw_rows, collapse = ""), "</tbody></table>"
    )
  } else {
    pw_table <- ""
  }

  tags$div(
    class = paste("alert py-2 mt-2", sig_class),
    tags$div(
      tags$strong(tags$small(icon("chart-bar"), " ", title)),
      tags$br(),
      tags$small(
        sprintf("Test: %s | %s", result$test_type, result$message),
        tags$br(),
        sig_text
      )
    ),
    if (nchar(pw_table) > 0) HTML(pw_table) else NULL,
    tags$div(
      class = "small text-muted",
      "Significance: * p < 0.05, ** p < 0.01, *** p < 0.001"
    )
  )
}

#' Format paired comparison results as HTML
#' @param result Result from paired_comparison_test
#' @param group1_name Name of first group
#' @param group2_name Name of second group
#' @return HTML tags
#' @export
format_paired_test_results <- function(result, group1_name = "WBES", group2_name = "National") {
  if (is.null(result) || result$test_type %in% c("insufficient_data", "test_failed")) {
    return(tags$div(
      class = "small text-muted mt-2",
      icon("info-circle"),
      if (!is.null(result$message)) result$message else "Statistical test not available"
    ))
  }

  sig_class <- if (!is.na(result$p_value) && result$p_value < 0.05) "text-success" else "text-muted"
  sig_symbol <- if (!is.na(result$p_value)) {
    if (result$p_value < 0.001) "***"
    else if (result$p_value < 0.01) "**"
    else if (result$p_value < 0.05) "*"
    else ""
  } else ""

  tags$div(
    class = paste("small mt-2", sig_class),
    tags$strong(result$test_type), ": ",
    result$message, sig_symbol,
    if (!is.na(result$p_value) && result$p_value < 0.05) {
      sprintf(" (Significant difference between %s and %s)", group1_name, group2_name)
    } else {
      sprintf(" (No significant difference)")
    }
  )
}
