#' Filter detections for a drift test window
#'
#' Subsets detections by time window, receiver serial, and transmitter ID.
#'
#' @param det Data frame of detections with `Time`, `Serial`, and `Full ID`.
#' @param startTime,endTime Start and end timestamps used to filter detections.
#' @param receiver_serial Receiver serial to retain.
#' @param transmitter_id Transmitter `Full ID` to retain.
#' @return A filtered data frame of detections.
#' @examples
#' \dontrun{
#' det_sub <- drift_detections(det, startTime, endTime, receiver_serial, transmitter_id)
#' }
#' @export
drift_detections <- function(det, startTime, endTime, receiver_serial, transmitter_id){

  detection.data <- det %>%
    filter(Time >= startTime & Time <= endTime) %>%
    filter(Serial == receiver_serial & `Full ID` == transmitter_id)

  return(detection.data)
}
