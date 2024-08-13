#
# get_closest_neighbor <- function(current_receiver_id, distance_df) {
#   distance_df %>%
#     dplyr::filter(receiver_id == current_receiver_id, receiver_id != neighbor_id) %>%
#     dplyr::arrange(distance) %>%
#     dplyr::slice_head(n = 1) %>%
#     dplyr::select(receiver_id, neighbor_id, distance)
# }


get_closest_neighbor <- function(current_receiver_id, distance_df, num_neighbors = 1) {
  distance_df %>%
    filter(receiver_id == current_receiver_id, receiver_id != neighbor_id) %>%
    arrange(distance) %>%
    slice_head(n = num_neighbors) %>%
    select(receiver_id, neighbor_id, distance)
}
