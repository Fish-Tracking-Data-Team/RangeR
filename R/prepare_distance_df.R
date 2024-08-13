
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




