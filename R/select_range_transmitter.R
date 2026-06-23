#' Interactively select one or more range-test transmitters
#'
#' Prints counts by `Full ID` and prompts for a selection.
#'
#' @param det Data frame with `Full ID` and `Model` columns.
#' @param transmission_source Either `"VR100"` or `"GPS_tracklog"`.
#' @param synthetic_transmit_data Optional synthetic transmission table used
#'   when `transmission_source = "GPS_tracklog"`.
#' @return A character vector containing one or more selected `Full ID` values.
#' @examples
#' \dontrun{
#' rtt_transmitter <- select_range_transmitter(det)
#' }
#' @export
select_range_transmitter <- function(det,
                                     transmission_source = c("VR100", "GPS_tracklog"),
                                     synthetic_transmit_data = NULL) {
    transmission_source <- match.arg(transmission_source)

    full_id_counts <- if (identical(transmission_source, "VR100")) {
        det |>
            dplyr::filter(Model == "VR100", !is.na(`Full ID`)) |>
            dplyr::count(`Full ID`, name = "count", sort = TRUE)
    } else if (!is.null(synthetic_transmit_data)) {
        synthetic_transmit_data |>
            dplyr::filter(!is.na(`Full ID`)) |>
            dplyr::count(`Full ID`, name = "count", sort = TRUE)
    } else {
        det |>
            dplyr::filter(!is.na(`Full ID`), Model != "VR100") |>
            dplyr::count(`Full ID`, name = "count", sort = TRUE)
    }

    .select_rows_from_console(
        choices = full_id_counts,
        value_col = "Full ID",
        prompt = paste0(
            "Please enter one or more row numbers for the transmitter 'Full ID' values to analyse",
            if (identical(transmission_source, "GPS_tracklog")) {
                " (based on receiver detections)"
            } else {
                ""
            },
            ", separated by commas: "
        ),
        allow_multiple = TRUE
    )
}
