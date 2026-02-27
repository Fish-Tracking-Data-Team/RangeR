#' Convert latitude/longitude columns to UTM coordinates
#'
#' Transforms a data frame with geodetic coordinates into a new coordinate
#' reference system and appends `X` (easting) and `Y` (northing) columns.
#'
#' @param df Data frame containing latitude and longitude columns.
#' @param lat_var,long_var Names of the latitude and longitude columns.
#' @param current_epsg EPSG code or CRS for the input coordinates.
#' @param new_epsg EPSG code or CRS for the output coordinates.
#' @return The input data frame with added `X` and `Y` columns.
#' @examples
#' \dontrun{
#' df_utm <- convert_latlong_to_utm(df, "Lat", "Long", 4326, 26919)
#' }
#' @export
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
