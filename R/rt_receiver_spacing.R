#' Compute distance to the first receiver in a set
#'
#' Calculates the Haversine distance from each receiver to the first row in
#' `dist_set` and returns the augmented data frame.
#'
#' @param dist_set Data frame with `Long` and `Lat` columns.
#' @return The input data frame with an added `distance` column (meters).
#' @importFrom geosphere distm distHaversine
#' @examples
#' \dontrun{
#' dist_set <- rt_receiver_spacing(dist_set)
#' }
#' @export
rt_receiver_spacing <- function(dist_set) {

  # Calculate the distance matrix
  dist_matrix <- distm(dist_set[, c("Long", "Lat")], fun = distHaversine)

  dist_set$distance <- dist_matrix[,1]
  return(dist_set)
}
