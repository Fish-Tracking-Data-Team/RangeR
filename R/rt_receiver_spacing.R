
rt_receiver_spacing <- function(dist_set) {

  # Calculate the distance matrix
  dist_matrix <- distm(dist_set[, c("Long", "Lat")], fun = distHaversine)

  dist_set$distance <- dist_matrix[,1]
  return(dist_set)
}
