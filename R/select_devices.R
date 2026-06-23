#' Interactively select receiver and VR100 device serials
#'
#' Prints available device serials and prompts for row selections.
#'
#' @param det Data frame with `Model` and `Serial` columns.
#' @param transmission_source Either `"VR100"` or `"GPS_tracklog"`.
#' @return A list with `receiver_serial` and `VR100_serial`.
#' @examples
#' \dontrun{
#' ids <- select_devices(det)
#' }
#' @export
select_devices <- function(det, transmission_source = c("VR100", "GPS_tracklog")) {
    transmission_source <- match.arg(transmission_source)

    receiver_choices <- det |>
        dplyr::filter(!is.na(Serial), Model != "VR100") |>
        dplyr::count(Serial, name = "count", sort = TRUE)

    if (!nrow(receiver_choices)) {
        receiver_choices <- det |>
            dplyr::filter(!is.na(Serial)) |>
            dplyr::count(Serial, name = "count", sort = TRUE)
    }

    receiver_serial <- .select_rows_from_console(
        choices = receiver_choices,
        value_col = "Serial",
        prompt = "Please enter the row number for the deployed receiver serial number: ",
        allow_multiple = FALSE
    )

    if (identical(transmission_source, "GPS_tracklog")) {
        message("GPS_tracklog transmission source selected: skipping VR100 serial selection.")

        return(list(
            receiver_serial = receiver_serial,
            VR100_serial = NA_character_
        ))
    }

    vr100_choices <- det |>
        dplyr::filter(Model == "VR100", !is.na(Serial)) |>
        dplyr::count(Serial, name = "count", sort = TRUE)

    if (!nrow(vr100_choices)) {
        stop(
            "No VR100 serial number was found in `det`. Use `transmission_source = 'GPS_tracklog'` for synthetic transmission workflows.",
            call. = FALSE
        )
    }

    vr100_serial <- .select_rows_from_console(
        choices = vr100_choices,
        value_col = "Serial",
        prompt = "Please enter the row number for the VR100 serial number used to log transmissions: ",
        allow_multiple = FALSE
    )

    list(
        receiver_serial = receiver_serial,
        VR100_serial = vr100_serial
    )
}
