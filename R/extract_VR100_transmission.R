extract_VR100_transmission <- function(det, startTime, endTime, VR100, transmitter_id){

transmit.data <- det %>%
  filter(Time >= startTime & Time <= endTime) %>%
  filter(Serial == VR100 & `Full ID` == transmitter_id) %>%
  mutate(distance = distHaversine(matrix(c(as.numeric(Longitude), as.numeric(Latitude)), ncol = 2), stationLocation))

return(transmit.data)
}
