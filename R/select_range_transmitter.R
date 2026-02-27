#' Interactively select a range-test transmitter
#'
#' Prints counts by `Full ID` and prompts for a selection.
#'
#' @param det Data frame with a `Full ID` column.
#' @return A length-one character vector containing the selected `Full ID`.
#' @examples
#' \dontrun{
#' rtt_transmitter <- select_range_transmitter(det)
#' }
#' @export
select_range_transmitter <- function(det) {
  # Group by 'Full ID' and count the instances
  full_id_counts <- det %>%
    group_by(`Full ID`) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  # Print the 'Full ID' counts
  print(full_id_counts)

  # Ask the user to select a row number
  selected_row <- as.integer(readline(prompt = "Please enter the row number for the 'Full ID' you would like to use: "))

  # Check if the selected row number is valid
  if(selected_row > 0 & selected_row <= nrow(full_id_counts)) {
    # Get the 'Full ID' for the selected row
    selected_id <- full_id_counts$`Full ID`[selected_row]

    # Extract the vector containing the 'Full ID'
    rtt_transmitter <- det %>%
      filter(`Full ID` == selected_id) %>%
      group_by(`Full ID`) %>%
      slice(1) %>%
      pull(`Full ID`)
    return(rtt_transmitter)
  } else {
    stop("The selected row number is not valid.")
  }
}
