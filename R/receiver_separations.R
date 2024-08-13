
receiver_separations <- function(dist_set) {
   # Calculate the distance matrix
  dist_matrix <- distm(dist_set[, c("Long", "Lat")], fun = distHaversine)

  # Set dimension names with station names
  row_names <- dist_set$Station
  col_names <- dist_set$Station
  dimnames(dist_matrix) <- list(row_names, col_names)

  # Create a data frame from the distance matrix
  rx_combs <- expand.grid(receiver_id = row_names, neighbor_id = col_names)
  rx_combs$distance <- as.vector(dist_matrix)

  # Ensure character type for IDs (useful for filtering and joining later)
  rx_combs %>%
    dplyr::mutate(receiver_id = as.character(receiver_id),
                  neighbor_id = as.character(neighbor_id))
}


