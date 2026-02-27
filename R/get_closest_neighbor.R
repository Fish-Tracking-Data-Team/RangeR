#
# get_closest_neighbor <- function(current_receiver_id, distance_df) {
#   distance_df %>%
#     dplyr::filter(receiver_id == current_receiver_id, receiver_id != neighbor_id) %>%
#     dplyr::arrange(distance) %>%
#     dplyr::slice_head(n = 1) %>%
#     dplyr::select(receiver_id, neighbor_id, distance)
# }
#' Find closest receiver(s) from a separation table
#'
#' Returns the nearest neighboring receiver(s) for a given receiver ID.
#'
#' @param current_receiver_id Receiver ID to query.
#' @param distance_df Data frame with `receiver_id`, `neighbor_id`, and
#'   `distance` columns (for example from `receiver_separations()`).
#' @param num_neighbors Number of closest neighbors to return.
#' @return A data frame with `receiver_id`, `neighbor_id`, and `distance`.
#' @examples
#' \dontrun{
#' get_closest_neighbor("RX01", distance_df, num_neighbors = 2)
#' }
#' @export
get_closest_neighbor <- function(current_receiver_id, distance_df, num_neighbors = 1) {
  distance_df %>%
    filter(receiver_id == current_receiver_id, receiver_id != neighbor_id) %>%
    arrange(distance) %>%
    slice_head(n = num_neighbors) %>%
    select(receiver_id, neighbor_id, distance)
}
