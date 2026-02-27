#' Standardize columns for distance calculations
#'
#' Renames the provided latitude, longitude, station, and serial columns to
#' `Lat`, `Long`, `Station`, and `Serial`.
#'
#' @param data Data frame containing receiver coordinates and identifiers.
#' @param lat_col,long_col Names of the latitude and longitude columns.
#' @param station_col,serial_col Names of the station and serial columns.
#' @return A data frame with `Long`, `Lat`, `Station`, and `Serial` columns.
#' @examples
#' \dontrun{
#' dist_set <- prepare_distance_df(data, "Latitude", "Longitude", "Station", "Serial")
#' }
#' @export
prepare_distance_df <- function(data, lat_col, long_col, station_col, serial_col) {
  data %>%
    mutate(
      Lat = !!sym(lat_col),
      Long = !!sym(long_col),
      Station = !!sym(station_col),
      Serial = !!sym(serial_col)
    ) %>%
    select(Long, Lat, Station, Serial)
}



