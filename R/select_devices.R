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
select_devices <- function(det){
    device_list <- det %>% 
    dplyr::select(Model, Serial) %>% 
    distinct()
    print(device_list)
    selected_row_1 <- as.integer(readline(prompt = "Please enter the row number for the first device (VR100_serial) you would like to use: "))
    selected_row_2 <- as.integer(readline(prompt = "Please enter the row number for the second device (receiver_serial) you would like to use: "))
    if (selected_row_1 > 0 & selected_row_1 <= nrow(device_list) &
        selected_row_2 > 0 & selected_row_2 <= nrow(device_list)) {
        VR100_serial <- device_list$Serial[selected_row_1]
        receiver_serial <- device_list$Serial[selected_row_2]
        return(list(VR100_serial = VR100_serial, receiver_serial = receiver_serial))
    }
    else {
        stop("One or both of the selected row numbers are not valid.")
    }
}
