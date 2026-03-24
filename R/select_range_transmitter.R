#' Interactively select a range-test transmitter
#'
#' Prints counts by `Full ID` and prompts for a selection.
#'
#' @param det Data frame with a `Full ID` column.
#' @return A character vector containing one or more selected `Full ID` values.
#' @examples
#' \dontrun{
#' rtt_transmitter <- select_range_transmitter(det)
#' }
#' @export
select_range_transmitter <- function(det) {
    full_id_counts <- det %>% 
    filter(Model == 'VR100') %>%
    count(`Full ID`, name = "count", sort = TRUE)

    print(full_id_counts)

    selected_rows <- readline(
        prompt = "Please enter one or more row numbers for the 'Full ID' values you would like to use, separated by commas: "
    ) %>%
        strsplit(",") %>%
        unlist() %>%
        trimws() %>%
        as.integer()

    if (length(selected_rows) == 0 || any(is.na(selected_rows))) {
        stop("Please provide one or more valid row numbers.")
    }

    if (any(selected_rows < 1 | selected_rows > nrow(full_id_counts))) {
        stop("One or more selected row numbers are not valid.")
    }

    full_id_counts$`Full ID`[selected_rows] %>%
        unique()
}
