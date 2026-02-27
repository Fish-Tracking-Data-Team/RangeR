#' Read a GPX file into layer-wise `sf` objects
#'
#' Reads all layers in a GPX file using `sf::st_read()` and returns a named list.
#' Optionally extracts track point extensions (water temperature and depth).
#'
#' @param file_path Path to the GPX file.
#' @param extract_extension If `TRUE`, parses `TrackPointExtension` values in the
#'   `track_points` layer and appends `wtemp` and `depth` columns.
#' @return A named list of `sf` objects, one per GPX layer.
#' @examples
#' \dontrun{
#' gpx <- read_gpx("track.gpx", extract_extension = TRUE)
#' names(gpx)
#' }
#' @export
read_gpx <- function(file_path, extract_extension = FALSE) {
  extract_trackpoint_extension <- function(sf_object) {

    if (!"TrackPointExtension" %in% colnames(sf_object)) {
      warning("TrackPointExtension column not found in the sf object.")
      return(sf_object)
    }


    trackpoint_extension <- sf_object$TrackPointExtension

    # add variables in this list if required
    wtemp_values <- numeric(length(trackpoint_extension))
    depth_values <- numeric(length(trackpoint_extension))


    extract_number <- function(text) {
      as.numeric(gsub("[^0-9.]", "", text))
    }


    for (i in seq_along(trackpoint_extension)) {
      if (!is.na(trackpoint_extension[i])) {

        xml_fragments <- strsplit(trackpoint_extension[i], "><", fixed = TRUE)[[1]]

        # add additional variables here if required
        wtemp <- NA
        depth <- NA

        for (fragment in xml_fragments) {
          if (grepl("<wtemp", fragment)) {
            wtemp <- extract_number(fragment)
            #print(paste("Extracted wtemp:", wtemp))  # Debugging statement
          } else if (grepl("depth>", fragment)) {
            depth <- extract_number(fragment)
            #print(paste("Extracted depth:", depth))  # Debugging statement
          }
        }

        # Store the values in the vectors, add additional vectors if required
        wtemp_values[i] <- wtemp
        depth_values[i] <- depth
      }
    }

   sf_object <- sf_object %>%
      mutate(wtemp = wtemp_values, depth = depth_values)

    return(sf_object)
  }

  layers_info <- st_layers(file_path)
  layers <- layers_info$name

  gpx_list <- list()


  for (layer in layers) {
    gpx_list[[layer]] <- st_read(file_path, layer = layer)

    if (layer == "track_points" && extract_extension) {
      gpx_list[[layer]] <- extract_trackpoint_extension(gpx_list[[layer]])
    }
  }

  return(gpx_list)
}
