#' Run drift-based range analysis for one or more transmitters
#'
#' Filters drift detections to a time window, compiles detection outcomes for
#' each requested transmitter, and combines model outputs into a single result.
#'
#' @param det Data frame containing detections and transmitter GPS positions.
#' @param start_time,end_time Start and end timestamps for the drift window.
#' @param devices List containing `VR100_serial` and `receiver_serial`.
#' @param transmitter_ids Character vector of transmitter `Full ID` values to
#'   analyze.
#' @param station_location Numeric vector, matrix row, or two-column coordinate
#'   object giving the station longitude and latitude used for distance
#'   calculations.
#' @param transmission_source Either `"VR100"` or `"GPS_tracklog"`.
#' @param synthetic_transmit_data Optional synthetic transmission table used
#'   when `transmission_source = "GPS_tracklog"`.
#' @param time_buffer Time tolerance in seconds used when matching transmitter
#'   pings to detections.
#' @param dist_threshold Maximum distance in meters retained for model fitting.
#' @param prediction_distance_max Optional maximum distance in meters used when
#'   generating prediction curves. Defaults to the maximum modeled distance for
#'   each transmitter.
#' @return A list containing combined `transmit_data`, `detection_data`,
#'   `compiled_data`, `predictions`, `summary`, and the analyzed
#'   `transmitter_ids`. Includes `transmission_source` in the result.
#' @examples
#' \dontrun{
#' res <- run_drift_range_analysis(
#'   det,
#'   start_time,
#'   end_time,
#'   devices,
#'   transmitter_ids,
#'   station_location,
#'   transmission_source = "VR100"
#' )
#' head(res$summary)
#' }
#' @export
run_drift_range_analysis <- function(det,
                                     start_time,
                                     end_time,
                                     devices,
                                     transmitter_ids,
                                     station_location,
                                     transmission_source = c("VR100", "GPS_tracklog"),
                                     synthetic_transmit_data = NULL,
                                     time_buffer = 5,
                                     dist_threshold = 1200,
                                     prediction_distance_max = NULL) {
    transmission_source <- match.arg(transmission_source)

    if (identical(transmission_source, "VR100") && !is.null(synthetic_transmit_data)) {
        stop(
            "`synthetic_transmit_data` must be NULL when transmission_source = 'VR100'.",
            call. = FALSE
        )
    }

    if (identical(transmission_source, "GPS_tracklog") &&
        (is.null(synthetic_transmit_data) || !nrow(synthetic_transmit_data))) {
        stop(
            "`synthetic_transmit_data` must be supplied and contain at least one row when transmission_source = 'GPS_tracklog'.",
            call. = FALSE
        )
    }

    transmitter_ids <- unique(transmitter_ids)

    results <- purrr::map(transmitter_ids, function(transmitter_id) {
        drift_data <- prepare_drift_data(
            det = det,
            start_time = start_time,
            end_time = end_time,
            devices = devices,
            transmitter_id = transmitter_id,
            station_location = station_location,
            transmission_source = transmission_source,
            synthetic_transmit_data = synthetic_transmit_data
        )

        drift_result <- compile_drifts(
            detection.data = drift_data$detection_data,
            transmit.data = drift_data$transmit_data,
            time_buffer = time_buffer,
            dist_threshold = dist_threshold,
            prediction_distance_max = prediction_distance_max
        )

        range_50 <- drift_result$predictions |>
            dplyr::slice_min(abs(prob - 0.5), n = 1, with_ties = FALSE) |>
            dplyr::pull(distance)

        list(
            transmitter_id = transmitter_id,
            transmit_data = drift_data$transmit_data,
            detection_data = drift_data$detection_data,
            compiled_data = drift_result$compiled_data |>
                dplyr::mutate(`Full ID` = transmitter_id),
            predictions = drift_result$predictions |>
                dplyr::mutate(`Full ID` = transmitter_id),
            summary = tibble::tibble(
                `Full ID` = transmitter_id,
                range_50 = range_50
            )
        )
    })

    list(
        transmission_source = transmission_source,
        transmitter_ids = transmitter_ids,
        transmit_data = dplyr::bind_rows(purrr::map(results, "transmit_data")),
        detection_data = dplyr::bind_rows(purrr::map(results, "detection_data")),
        compiled_data = dplyr::bind_rows(purrr::map(results, "compiled_data")),
        predictions = dplyr::bind_rows(purrr::map(results, "predictions")),
        summary = dplyr::bind_rows(purrr::map(results, "summary"))
    )
}
