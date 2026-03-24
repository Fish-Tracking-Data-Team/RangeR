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
#' @param time_buffer Time tolerance in seconds used when matching transmitter
#'   pings to detections.
#' @param dist_threshold Maximum distance in meters retained for model fitting.
#' @return A list containing combined `transmit_data`, `detection_data`,
#'   `compiled_data`, `predictions`, `summary`, and the analyzed
#'   `transmitter_ids`.
#' @examples
#' \dontrun{
#' res <- run_drift_range_analysis(
#'   det,
#'   start_time,
#'   end_time,
#'   devices,
#'   transmitter_ids,
#'   station_location
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
                                     time_buffer = 5,
                                     dist_threshold = 1200) {
    transmitter_ids <- unique(transmitter_ids)

    results <- purrr::map(transmitter_ids, function(transmitter_id) {
        drift_data <- prepare_drift_data(
            det,
            start_time,
            end_time,
            devices,
            transmitter_id,
            station_location
        )

        drift_result <- compile_drifts(
            drift_data$detection_data,
            drift_data$transmit_data,
            time_buffer = time_buffer,
            dist_threshold = dist_threshold
        )

        range_50 <- drift_result$predictions %>%
            slice_min(abs(prob - 0.5), n = 1, with_ties = FALSE) %>%
            pull(distance)

        list(
            transmitter_id = transmitter_id,
            transmit_data = drift_data$transmit_data,
            detection_data = drift_data$detection_data,
            compiled_data = drift_result$compiled_data %>%
                mutate(`Full ID` = transmitter_id),
            predictions = drift_result$predictions %>%
                mutate(`Full ID` = transmitter_id),
            summary = tibble::tibble(
                `Full ID` = transmitter_id,
                range_50 = range_50
            )
        )
    })

    list(
        transmitter_ids = transmitter_ids,
        transmit_data = bind_rows(purrr::map(results, "transmit_data")),
        detection_data = bind_rows(purrr::map(results, "detection_data")),
        compiled_data = bind_rows(purrr::map(results, "compiled_data")),
        predictions = bind_rows(purrr::map(results, "predictions")),
        summary = bind_rows(purrr::map(results, "summary"))
    )
}


