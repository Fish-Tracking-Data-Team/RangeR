#' Compute receiver separation distances
#'
#' Computes Haversine distances between receiver locations.
#'
#' @param dist_set Data frame with `Long`, `Lat`, `Station`, and `Serial`
#'   columns.
#' @param reference_station Optional character vector of `Station` values to use
#'   as the reference locations. When supplied, distances are calculated from
#'   those stations to all stations in `dist_set`. When `NULL`, all pairwise
#'   station distances are returned.
#' @return A data frame with `Ref_Station`, `Station`, `Serial`, and `distance`
#'   in meters for the requested receiver pairs.
#' @importFrom geosphere distm distHaversine
#' @examples
#' \dontrun{
#' separations <- receiver_separations(dist_set)
#' separations_from_ref <- receiver_separations(dist_set, reference_station = c("ST01"))
#' }
#' @export
receiver_separations <- function(dist_set, reference_station = NULL) {
  required_cols <- c("Long", "Lat", "Station", "Serial")

  if (!all(required_cols %in% names(dist_set))) {
    stop(
      paste0(
        "`dist_set` must contain columns: ",
        paste(required_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (is.null(reference_station)) {
    reference_idx <- seq_len(nrow(dist_set))
  } else {
    if (!is.atomic(reference_station) || !length(reference_station)) {
      stop("`reference_station` must be a non-empty vector of Station values.", call. = FALSE)
    }

    missing_stations <- setdiff(reference_station, unique(dist_set$Station))
    if (length(missing_stations)) {
      stop(
        paste0(
          "The following `reference_station` values were not found in `dist_set$Station`: ",
          paste(missing_stations, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    reference_idx <- which(dist_set$Station %in% reference_station)
  }

  target_idx <- seq_len(nrow(dist_set))

  dist_matrix <- geosphere::distm(
    dist_set[reference_idx, c("Long", "Lat")],
    dist_set[target_idx, c("Long", "Lat")],
    fun = geosphere::distHaversine
  )

  row_names <- dist_set$Station[reference_idx]
  col_names <- dist_set$Station[target_idx]
  dimnames(dist_matrix) <- list(row_names, col_names)

  rx_combs <- expand.grid(
    Ref_Station = row_names,
    Station = col_names,
    stringsAsFactors = FALSE
  )
  rx_combs$Serial <- dist_set$Serial[match(rx_combs$Station, dist_set$Station)]
  rx_combs$distance <- as.vector(dist_matrix)

  rx_combs %>%
    dplyr::mutate(
      Ref_Station = as.character(Ref_Station),
      Station = as.character(Station),
      Serial = as.integer(Serial)
    )
}
