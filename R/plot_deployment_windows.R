#' Plot receiver deployment windows from diagnostic tilt data
#'
#' Creates a faceted time-series plot of receiver tilt with annotated deployment
#' start and end times derived from `get_deployment_time()`. Each facet shows a
#' receiver serial number with vertical markers for deployment bounds and a
#' label describing deployment duration.
#'
#' @param diag_data Data frame containing at least `Serial`, `Time`, and
#'   `Tilt (deg)` columns.
#' @param deployment_time_result A list containing an `operational_periods`
#'   element with `Serial`, `start`, `end`, and `duration_days` columns.
#' @param free_x Logical; if `TRUE`, allow each facet to use its own x-axis
#'   scale.
#' @param label_size Numeric text size scaling for the rotated deployment start
#'   and end labels.
#' @return A `ggplot2` object showing tilt over time with deployment window
#'   annotations.
#' @examples
#' \dontrun{
#' p <- plot_deployment_windows(diag_data, deployment_time_result)
#' p
#' }
#' @export
plot_deployment_windows <- function(diag_data,
                                    deployment_time_result,
                                    free_x = FALSE,
                                    label_size = 8) {
    if (!"operational_periods" %in% names(deployment_time_result)) {
        stop("`deployment_time_result` must contain an `operational_periods` element.")
    }

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

    ggplot(diag_data)+
        geom_line(aes(Time, as.numeric(`Tilt (deg)`)))+
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
        labs(x = 'Date', y = 'Receiver Tilt (deg)')+
        facet_grid(rows = vars(Serial), scales = if (free_x) 'free_x' else 'fixed')+
        theme_minimal()
        
}
