#' Compile drift detections and fit a detection model
#'
#' Joins transmitter pings to detections within a time buffer, filters by
#' distance, and fits a logistic model of detection probability versus distance.
#'
#' @param detection.data Data frame of detections with `Serial`, `Time`,
#'   `Full ID`, and `distance` columns.
#' @param transmit.data Data frame of transmitter pings with at least `Time`.
#' @param time_buffer Time tolerance in seconds used for the fuzzy time join.
#' @param dist_threshold Maximum distance (in meters) retained for modeling.
#' @param prediction_distance_max Optional maximum distance (in meters) used
#'   when generating the prediction curve. Defaults to the maximum modeled
#'   detection distance.
#' @return A list with:
#'   `compiled_data` containing detection outcomes and distances, and
#'   `predictions` containing model-predicted probabilities by distance.
#' @importFrom fuzzyjoin fuzzy_left_join
#' @examples
#' \dontrun{
#' res <- compile_drifts(detection.data, transmit.data, time_buffer = 5)
#' head(res$predictions)
#' }
#' @export
compile_drifts <- function(detection.data,
                           transmit.data,
                           time_buffer = 5,
                           dist_threshold = 1200,
                           prediction_distance_max = NULL) {
    serial_values <- unique(detection.data$Serial)

    results <- purrr::map(serial_values, function(serial_value) {
        df_subset <- detection.data |>
            dplyr::filter(Serial == serial_value)

        fuzzy_left_join(
            transmit.data,
            df_subset,
            by = c(Time = "Time"),
            match_fun = list(function(x, y) abs(difftime(x, y, units = "secs")) < time_buffer)
        ) |>
            dplyr::mutate(
                success = ifelse(!is.na(`Full ID.y`), 1, 0),
                Serial = serial_value
            ) |>
            dplyr::select(Time.x, Serial.x, Serial.y, Serial, `Full ID.y`, distance, success)
    })

    detection_table <- dplyr::bind_rows(results) |>
        dplyr::arrange(Serial, Time.x) |>
        dplyr::select(distance, success) |>
        dplyr::filter(distance <= dist_threshold)

    observed_max_distance <- max(detection_table$distance, na.rm = TRUE)

    if (!is.null(prediction_distance_max)) {
        if (!is.numeric(prediction_distance_max) ||
            length(prediction_distance_max) != 1 ||
            !is.finite(prediction_distance_max)) {
            stop(
                "`prediction_distance_max` must be a single finite numeric value.",
                call. = FALSE
            )
        }

        if (prediction_distance_max < observed_max_distance) {
            stop(
                "`prediction_distance_max` must be greater than or equal to the maximum modeled distance.",
                call. = FALSE
            )
        }
    }

    prediction_distance_max <- if (is.null(prediction_distance_max)) {
        observed_max_distance
    } else {
        prediction_distance_max
    }

    anchor <- data.frame(
        distance = rep(0, nrow(transmit.data)),
        success = rep(1, nrow(transmit.data))
    )

    compiled_data <- dplyr::bind_rows(detection_table, anchor)
    drift_glm <- stats::glm(success ~ distance, data = compiled_data, family = stats::binomial)
    predictions <- data.frame(
        distance = seq(0, prediction_distance_max, length.out = 500)
    )
    predictions$prob <- stats::predict(drift_glm, newdata = predictions, type = "response")

    list(
        compiled_data = compiled_data,
        predictions = predictions
    )
}
