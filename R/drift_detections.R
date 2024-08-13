drift_detections <- function(det, startTime, endTime, receiver_serial, transmitter_id){

  detection.data <- det %>%
    filter(Time >= startTime & Time <= endTime) %>%
    filter(Serial == receiver_serial & `Full ID` == transmitter_id)

  return(detection.data)
}
