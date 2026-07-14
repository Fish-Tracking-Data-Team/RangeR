#' Plot receiver deployment windows from diagnostic data
#'
#' Creates a faceted time-series plot of receiver diagnostics with annotated
#' deployment start and end times derived from `get_deployment_time()`. Each
#' facet shows a receiver serial number with vertical markers for deployment
#' bounds and a label describing deployment duration.
#'
#' @param diag_data Data frame containing at least `Serial`, `Time`, and the
#'   diagnostic columns required by `diagnostic`.
#' @param deployment_time_result A list containing an `operational_periods`
#'   element with `Serial`, `start`, `end`, and `duration_days` columns.
#' @param diagnostic Character scalar indicating which diagnostic to plot.
#'   Must be one of `"tilt"`, `"noise"`, or `"both"`. When `"both"` is
#'   selected, noise panels are fixed to `0-900` while tilt panels use their
#'   own y-axis ranges.
#' @param free_x Logical; if `TRUE`, allow each facet to use its own x-axis
#'   scale.
#' @param label_size Numeric text size scaling for the rotated deployment start
#'   and end labels.
#' @return A `ggplot2` object showing the selected diagnostic over time with
#'   deployment window annotations.
#' @examples
#' \dontrun{
#' p <- plot_deployment_windows(
#'   diag_data,
#'   deployment_time_result,
#'   diagnostic = "both"
#' )
#' p
#' }
#' @export
plot_deployment_windows <- function(diag_data,
                                    deployment_time_result,
                                    diagnostic = c("tilt", "noise", "both"),
                                    free_x = FALSE,
                                    label_size = 8) {
    diagnostic <- match.arg(diagnostic)

    if (!"operational_periods" %in% names(deployment_time_result)) {
        stop("`deployment_time_result` must contain an `operational_periods` element.")
    }

    diagnostic_columns <- c(
        tilt = "Tilt (deg)",
        noise = "Noise Mean (mV)"
    )

    selected_diagnostics <- switch(
        diagnostic,
        tilt = "tilt",
        noise = "noise",
        both = c("tilt", "noise")
    )

    required_columns <- unname(diagnostic_columns[selected_diagnostics])
    missing_columns <- setdiff(required_columns, names(diag_data))

    if (length(missing_columns) > 0) {
        stop(
            sprintf(
                "`diag_data` must contain %s when `diagnostic = \"%s\"`.",
                paste(sprintf("`%s`", missing_columns), collapse = " and "),
                diagnostic
            )
        )
    }

    plot_data <- diag_data %>%
        tidyr::pivot_longer(
            cols = dplyr::all_of(required_columns),
            names_to = ".diagnostic_column",
            values_to = ".plot_value_raw"
        ) %>%
        dplyr::mutate(
            .diagnostic = dplyr::recode(
                .diagnostic_column,
                "Tilt (deg)" = "tilt",
                "Noise Mean (mV)" = "noise"
            ),
            .diagnostic = factor(.diagnostic, levels = c("tilt", "noise")),
            .plot_value = as.numeric(.plot_value_raw)
        )

    diagnostic_strip_labels <- c(
        tilt = "Receiver Tilt (deg)",
        noise = "Noise Mean (mV)"
    )

    diagnostic_axis_labels <- c(
        tilt = "Receiver Tilt (deg)",
        noise = "Noise Mean (mV)",
        both = "Diagnostic Value"
    )

    plot_deployment_times <- deployment_time_result$operational_periods %>%
        dplyr::transmute(
            Serial,
            minTime = start,
            maxTime = end,
            duration_days
        )

    plot_labels <- plot_deployment_times %>%
        dplyr::mutate(
            label = paste0('Deployment Duration: ', round(duration_days, 1), ' days'),
            y = Inf
        )

    plot_vline_labels <- plot_deployment_times %>%
        tidyr::pivot_longer(
            cols = c(minTime, maxTime),
            names_to = "line_type",
            values_to = "Time"
        ) %>%
        dplyr::mutate(
            label = dplyr::if_else(
                line_type == "minTime",
                "Deployment Start",
                "Deployment End"
            ),
            y = Inf
        )

    noise_band_data <- NULL
    noise_scale_anchor_data <- NULL
    if ("noise" %in% selected_diagnostics) {
        noise_time_bounds <- plot_data %>%
            dplyr::filter(.diagnostic == "noise") %>%
            dplyr::group_by(Serial, .diagnostic) %>%
            dplyr::summarise(
                xmin = min(Time, na.rm = TRUE),
                xmax = max(Time, na.rm = TRUE),
                .groups = "drop"
            )

        noise_band_data <- noise_time_bounds %>%
            tidyr::crossing(
                tibble::tibble(
                    ymin = c(0, 300, 650),
                    ymax = c(300, 650, 900),
                    fill_band = factor(
                        c("good", "moderate", "high"),
                        levels = c("good", "moderate", "high")
                    )
                )
            )

        noise_scale_anchor_data <- noise_time_bounds %>%
            tidyr::crossing(.plot_value = c(0, 900)) %>%
            dplyr::rename(Time = xmin)
    }

    base_plot <- ggplot(plot_data)

    if (!is.null(noise_band_data)) {
        base_plot <- base_plot +
            geom_rect(
                data = noise_band_data,
                aes(
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax,
                    fill = fill_band
                ),
                inherit.aes = FALSE,
                alpha = 0.1
            )
    }

    if (!is.null(noise_scale_anchor_data)) {
        base_plot <- base_plot +
            geom_blank(
                data = noise_scale_anchor_data,
                aes(Time, .plot_value)
            )
    }

    facet_layer <- if (diagnostic == "both") {
        ggh4x::facet_grid2(
            rows = vars(Serial),
            cols = vars(.diagnostic),
            scales = if (free_x) "free" else "free_y",
            independent = "y",
            axes = "y",
            remove_labels = "x",
            labeller = labeller(.diagnostic = diagnostic_strip_labels)
        )
    } else {
        facet_grid(
            rows = vars(Serial),
            cols = vars(.diagnostic),
            scales = if (free_x) "free_x" else "fixed",
            labeller = labeller(.diagnostic = diagnostic_strip_labels)
        )
    }

    base_plot +
        geom_line(aes(Time, .plot_value, color = .diagnostic))+
        geom_vline(
            data = plot_deployment_times,
            aes(xintercept = minTime),
            color = 'green',
            linetype = 'dashed',
            size = 1.2
        )+
        geom_vline(
            data = plot_deployment_times,
            aes(xintercept = maxTime),
            color = 'red',
            linetype = 'dashed',
            size = 1.2
        )+
        geom_text(
            data = plot_labels,
            aes(x = minTime, y = y, label = label),
            inherit.aes = FALSE,
            hjust = 0,
            vjust = 1.2,
            size = 6
        )+
        geom_text(
            data = plot_vline_labels,
            aes(x = Time, y = y, label = label),
            inherit.aes = FALSE,
            angle = 90,
            hjust = 1,
            vjust = -0.4,
            size = label_size * 0.6
        )+
        scale_x_datetime(labels = scales::label_date_short())+
        scale_color_manual(
            values = c(tilt = 'blue', noise = 'orange'),
            labels = diagnostic_strip_labels,
            guide = 'none'
        )+
        scale_fill_manual(
            values = c(good = 'green', moderate = 'yellow', high = 'red'),
            guide = 'none'
        )+
        labs(
            x = 'Date',
            y = diagnostic_axis_labels[[diagnostic]],
            color = NULL,
            fill = NULL
        )+
        facet_layer+
        theme_minimal()
}
