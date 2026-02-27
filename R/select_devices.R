#' Interactively select VR100 and receiver serials
#'
#' Prints unique device combinations and prompts for row selections.
#'
#' @param det Data frame with `Model` and `Serial` columns.
#' @return A list with `VR100_serial` and `receiver_serial`.
#' @examples
#' \dontrun{
#' ids <- select_devices(det)
#' }
#' @export
select_devices <- function(det) {
  # Create a data frame of unique 'Model' and 'Serial' combinations
  device_list <- det %>%
    dplyr::select(Model, Serial) %>%
    distinct()

  # Print the device list
  print(device_list)

  # Ask the user to select two devices
  selected_row_1 <- as.integer(readline(prompt = "Please enter the row number for the first device (VR100_serial) you would like to use: "))
  selected_row_2 <- as.integer(readline(prompt = "Please enter the row number for the second device (receiver_serial) you would like to use: "))

  # Check if the selected row numbers are valid
  if(selected_row_1 > 0 & selected_row_1 <= nrow(device_list) & selected_row_2 > 0 & selected_row_2 <= nrow(device_list)) {
    # Get the 'Serial' for the selected rows
    VR100_serial <- device_list$Serial[selected_row_1]
    receiver_serial <- device_list$Serial[selected_row_2]

    # Return a list with the selected 'Serial' values
    return(list(VR100_serial = VR100_serial, receiver_serial = receiver_serial))
  } else {
    stop("One or both of the selected row numbers are not valid.")
  }
}
