# Helper Functions for Shiny App
# ============================================================================

library(ggplot2)
library(ggrepel)
library(scales)
library(dplyr)
library(forcats)

# Data Helper Functions ------------------------------------------------------

#' Get list of available models for selection
#'
#' @param eval_data Processed evaluation data
#' @return Tibble with model_display and model_join columns
get_available_models <- function(eval_data) {
  eval_data |>
    distinct(model_display, model_join) |>
    arrange(model_display)
}

#' Compute summary statistics for selected models
#'
#' @param eval_data Processed evaluation data
#' @param cost_data Cost data
#' @param selected_models Character vector of model_join IDs
#' @return Tibble with summary statistics per model
compute_summary_stats <- function(eval_data, cost_data, selected_models) {
  eval_data |>
    filter(model_join %in% selected_models) |>
    group_by(model_display, model_join) |>
    summarize(
      total_samples = n(),
      correct = sum(score == "Correct"),
      partially_correct = sum(score == "Partially Correct"),
      incorrect = sum(score == "Incorrect"),
      percent_correct = correct / total_samples,
      .groups = "drop"
    ) |>
    left_join(
      cost_data |> select(model_join, price, input, output),
      by = "model_join"
    ) |>
    arrange(desc(percent_correct))
}

# Plotting Functions ---------------------------------------------------------

#' Create performance bar chart
#'
#' @param eval_data Filtered evaluation data with model_display and score columns
#' @return ggplot object
plot_performance <- function(eval_data) {
  # Reorder models by correct count
  eval_data <- eval_data |>
    mutate(
      model_display = fct_reorder(
        model_display,
        score,
        .fun = \(x) sum(x == "Correct", na.rm = TRUE)
      )
    )

  eval_data |>
    ggplot(aes(y = model_display, fill = score)) +
    geom_bar(position = "fill") +
    scale_fill_manual(
      breaks = rev,
      values = c(
        "Correct" = "#6caea7",
        "Partially Correct" = "#f6e8c3",
        "Incorrect" = "#ef8a62"
      )
    ) +
    scale_x_continuous(labels = percent, expand = c(5e-3, 5e-3)) +
    labs(
      x = "Percent",
      y = NULL,
      fill = "Score"
    ) +
    theme_light() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(size = 14),
      title = element_text(size = 16),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
}

#' Create cost vs performance scatter plot
#'
#' @param summary_data Summary statistics with percent_correct and price columns
#' @return ggplot object
plot_cost_vs_performance <- function(summary_data) {
  # Convert factor to character for labeling
  plot_data <- summary_data |>
    mutate(model_display = as.character(model_display))

  # Calculate means for reference lines
  mean_correct <- mean(plot_data$percent_correct, na.rm = TRUE)
  mean_price <- mean(plot_data$price, na.rm = TRUE)

  ggplot(plot_data, aes(price, percent_correct)) +
    geom_point(size = 5, color = "#2c3e50") +
    geom_label_repel(
      aes(label = model_display),
      force = 3,
      max.overlaps = 20,
      size = 7,
      fill = "#f5f5f5",
      color = "#333333"
    ) +
    scale_x_continuous(labels = label_dollar()) +
    scale_y_continuous(
      labels = label_percent(),
      breaks = breaks_width(0.05)
    ) +
    labs(
      x = "Total Cost (USD)",
      y = "Percent Correct"
    ) +
    theme_light() +
    theme(
      plot.subtitle = element_text(face = "italic", size = 12),
      plot.margin = margin(10, 10, 20, 10),
      axis.title = element_text(size = 14),
      title = element_text(size = 16),
      axis.text = element_text(size = 12)
    )
}
