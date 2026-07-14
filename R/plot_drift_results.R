#' Plot drift-test detection results
#'
#' Builds a detection probability plot from the output of
#' `run_drift_range_analysis()` or `compile_drifts()`, including observed
#' detections, fitted probability curves, and annotated 50% range estimates.
#'
#' @param drift_results A list containing `compiled_data` and `predictions`, and
#'   optionally `summary`.
#' @param title Plot title.
#' @param subtitle Optional plot subtitle.
#' @param x_limits Numeric length-two vector giving x-axis limits in meters.
#' @param x_breaks Numeric vector of x-axis break positions in meters.
#' @return A `ggplot2` object showing detection probability versus distance.
#' @examples
#' \dontrun{
#' p <- plot_drift_results(drift_results, title = "Detection curve")
#' p
#' }
#' @export
plot_drift_results <- function(drift_results,
                               title = "Detection curve",
                               subtitle = NULL,
                               x_limits = c(0, 1200),
                               x_breaks = seq(0, 1200, 100)) {
  stopifnot(
    is.list(drift_results),
    all(c("compiled_data", "predictions") %in% names(drift_results))
  )

  plot_predictions <- drift_results$predictions %>%
    filter(distance >= 0)

  plot_detections <- drift_results$compiled_data %>%
    filter(distance >= 0)

  if (!("Full ID" %in% names(plot_predictions))) {
    plot_predictions <- plot_predictions %>%
      mutate(`Full ID` = "Curve 1")
  }

  if (!("Full ID" %in% names(plot_detections))) {
    plot_detections <- plot_detections %>%
      mutate(`Full ID` = "Curve 1")
  }

  if ("summary" %in% names(drift_results)) {
    plot_summary <- drift_results$summary
  } else {
    plot_summary <- plot_predictions %>%
      slice_min(abs(prob - 0.5), n = 1, with_ties = FALSE) %>%
      transmute(`Full ID`, range_50 = distance)
  }

  if (!("Full ID" %in% names(plot_summary))) {
    plot_summary <- plot_summary %>%
      mutate(`Full ID` = "Curve 1")
  }

  plot_summary <- plot_summary %>%
    arrange(`Full ID`, range_50) %>%
    mutate(
      label_y = seq(0.08, 0.08 + (0.06 * (n() - 1)), length.out = n()),
      range_label = paste0(round(range_50, 1), " m")
    )

  show_legend <- dplyr::n_distinct(plot_predictions$`Full ID`) > 1

  ggplot(plot_detections, aes(x = distance, y = success, color = `Full ID`)) +
    geom_jitter(
      height = 0.01,
      width = 0,
      alpha = 0.35,
      size = 2.6
    ) +
    geom_line(
      data = plot_predictions,
      aes(x = distance, y = prob, color = `Full ID`),
      linewidth = 1.2
    ) +
    geom_vline(
      data = plot_summary,
      aes(xintercept = range_50, color = `Full ID`),
      linetype = "dashed",
      alpha = 0.7,
      show.legend = FALSE
    ) +
    geom_text(
      data = plot_summary,
      aes(x = range_50, y = label_y, label = range_label), 
      color = 'black',
      angle = 90,
      vjust = -0.4,
      hjust = 0,
      size = 3,
      show.legend = FALSE
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey35") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Distance (m)",
      y = "Detection probability",
      color = "Full ID"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1)
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.major.y = element_line(color = "grey92"),
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    )
}
