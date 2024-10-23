convert_latlong_to_utm <- function(df, lat_var, long_var, current_epsg, new_epsg) {
  if (!(lat_var %in% names(df)) | !(long_var %in% names(df))) {
    stop("Latitude or Longitude column not found in the dataframe.")
  }

  sf_data <- st_as_sf(df, coords = c(long_var, lat_var), crs = current_epsg)
  sf_transformed <- st_transform(sf_data, crs = new_epsg)
  coords <- st_coordinates(sf_transformed)

  df$X <- coords[, 1]  # Easting
  df$Y <- coords[, 2]  # Northing

  return(df)
}
