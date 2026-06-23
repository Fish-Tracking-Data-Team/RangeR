#' Filter detections for a drift test window
#'
#' Subsets detections by time window, device serials, and transmitter ID, then
#' calculates transmitter-to-station distance for observed or synthetic
#' transmission records.
#'
#' @param det Data frame of detections with `Time`, `Serial`, `Full ID`,
#'   `Longitude`, and `Latitude` columns.
#' @param start_time,end_time Start and end timestamps used to filter detections.
#' @param devices List containing `VR100_serial` and `receiver_serial`.
#' @param transmitter_id Transmitter `Full ID` to retain.
#' @param station_location Numeric vector, matrix row, or two-column coordinate
#'   object giving the station longitude and latitude used for distance
#'   calculations.
#' @param transmission_source Either `"VR100"` or `"GPS_tracklog"`.
#' @param synthetic_transmit_data Optional synthetic transmission table used
#'   when `transmission_source = "GPS_tracklog"`.
#' @return A list with `transmit_data` and `detection_data`.
#' @examples
#' \dontrun{
#' drift_data <- prepare_drift_data(
#'   det,
#'   start_time,
#'   end_time,
#'   devices,
#'   transmitter_id,
#'   station_location,
#'   transmission_source = "VR100"
#' )
#' names(drift_data)
#' }
#' @export
prepare_drift_data <- function(det,
                               start_time,
                               end_time,
                               devices,
                               transmitter_id,
                               station_location,
                               transmission_source = c("VR100", "GPS_tracklog"),
                               synthetic_transmit_data = NULL) {
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

    detection_data <- det |>
        dplyr::filter(
            Time >= start_time,
            Time <= end_time,
            Serial == devices$receiver_serial,
            `Full ID` == transmitter_id
        )

    if (identical(transmission_source, "VR100")) {
        transmit_data <- det |>
            dplyr::filter(
                Time >= start_time,
                Time <= end_time,
                Serial == devices$VR100_serial,
                `Full ID` == transmitter_id
            )
    } else {
        transmit_data <- synthetic_transmit_data |>
            dplyr::filter(
                Time >= start_time,
                Time <= end_time,
                `Full ID` == transmitter_id
            ) |>
            dplyr::mutate(Serial = devices$VR100_serial %||% NA)
    }

    if (!nrow(transmit_data)) {
        stop(
            paste0(
                "No transmit rows found for transmitter_id = '",
                transmitter_id,
                "' using transmission_source = '",
                transmission_source,
                "'."
            ),
            call. = FALSE
        )
    }

    transmit_data <- transmit_data |>
        dplyr::mutate(
            distance = geosphere::distHaversine(
                matrix(c(as.numeric(Longitude), as.numeric(Latitude)), ncol = 2),
                station_location
            )
        )

    list(
        transmit_data = transmit_data,
        detection_data = detection_data
    )
}
