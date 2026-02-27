#' Extract VR100 transmissions and compute distance to station
#'
#' Filters detections for a VR100 receiver and transmitter ID within a time
#' window, then computes distance to `stationLocation`.
#'
#' @param det Data frame of detections with `Time`, `Serial`, `Full ID`,
#'   `Longitude`, and `Latitude` columns.
#' @param startTime,endTime Start and end timestamps used to filter detections.
#' @param VR100 Serial number of the VR100 receiver.
#' @param transmitter_id Transmitter `Full ID` to retain.
#' @return A filtered data frame with a `distance` column (meters).
#' @importFrom geosphere distHaversine
#' @examples
#' \dontrun{
#' # stationLocation must be defined in the environment
#' transmit.data <- extract_VR100_transmission(det, startTime, endTime, VR100, transmitter_id)
#' }
#' @export
extract_VR100_transmission <- function(det, startTime, endTime, VR100, transmitter_id){

transmit.data <- det %>%
  filter(Time >= startTime & Time <= endTime) %>%
  filter(Serial == VR100 & `Full ID` == transmitter_id) %>%
  mutate(distance = distHaversine(matrix(c(as.numeric(Longitude), as.numeric(Latitude)), ncol = 2), stationLocation))

return(transmit.data)
}
