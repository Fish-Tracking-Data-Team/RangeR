#' Filter detections for a drift test window
#'
#' Subsets detections by time window, device serials, and transmitter ID, then
#' calculates transmitter-to-station distance for VR100 records.
#'
#' @param det Data frame of detections with `Time`, `Serial`, `Full ID`,
#'   `Longitude`, and `Latitude` columns.
#' @param start_time,end_time Start and end timestamps used to filter detections.
#' @param devices List containing `VR100_serial` and `receiver_serial`.
#' @param transmitter_id Transmitter `Full ID` to retain.
#' @param station_location Numeric vector, matrix row, or two-column coordinate
#'   object giving the station longitude and latitude used for distance
#'   calculations.
#' @return A list with `transmit_data` and `detection_data`.
#' @examples
#' \dontrun{
#' drift_data <- prepare_drift_data(
#'   det,
#'   start_time,
#'   end_time,
#'   devices,
#'   transmitter_id,
#'   station_location
#' )
#' names(drift_data)
#' }
#' @export
prepare_drift_data <- function(det,
                               start_time,
                               end_time,
                               devices,
                               transmitter_id,
                               station_location) {
    transmit_data <- det %>%
        filter(
            Time >= start_time,
            Time <= end_time,
            Serial == devices$VR100_serial,
            `Full ID` == transmitter_id
        ) %>%
        mutate(
            distance = distHaversine(
                matrix(c(as.numeric(Longitude), as.numeric(Latitude)), ncol = 2),
                station_location
            )
        )

    detection_data <- det %>%
        filter(
            Time >= start_time,
            Time <= end_time,
            Serial == devices$receiver_serial,
            `Full ID` == transmitter_id
        )

    list(
        transmit_data = transmit_data,
        detection_data = detection_data
    )
}
