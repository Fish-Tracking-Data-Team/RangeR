#' Build a synthetic transmission log from GPS tracklog and deployment metadata
#'
#' Reads a GPS tracklog, reads drift windows, annotates the tracklog against the
#' drift windows, and generates a synthetic transmission log in a VR100-like
#' tabular format.
#'
#' @param full_id Character vector of transmitter IDs to assign to output rows.
#' @param gps_tracklog_path Path to a GPX or CSV tracklog.
#' @param drift_windows_path Path to a drift-window CSV file.
#' @param delay_sec Transmission delay in seconds.
#' @param max_gap_sec Maximum allowed nearest-neighbour gap in seconds.
#' @param tracklog_offset_hours Numeric UTC offset in hours for the tracklog.
#' @param drift_offset_hours Numeric UTC offset in hours for the drift windows.
#' @param csv_dayfirst Logical; if `TRUE`, prefer day-month-year parsing for
#'   tracklog CSV files.
#' @param drift_dayfirst Logical; if `TRUE`, prefer day-month-year parsing for
#'   drift-window CSV files.
#' @param span_full Logical; if `TRUE`, generate transmissions across the full
#'   span from the first drift start to the last drift end.
#' @param snap_mode Either `"interp"` or `"nearest"`.
#' @param offset_sec Numeric offset in seconds between the start of each
#'   successive transmitter schedule. When `full_id` is supplied as a single
#'   numeric-like value, its schedule starts at (`full_id` - 1) * offset_sec.
#'   When multiple IDs are supplied, offsets are applied by position.
#' @return A tibble of synthetic transmission records with columns `Time`,
#'   `Latitude`, `Longitude`, `Full ID`, and `Model`.
#' @export
build_synthetic_transmission_log <- function(full_id,
                                             gps_tracklog_path,
                                             drift_windows_path,
                                             delay_sec,
                                             max_gap_sec,
                                             tracklog_offset_hours = 0,
                                             drift_offset_hours = 0,
                                             csv_dayfirst = TRUE,
                                             drift_dayfirst = TRUE,
                                             span_full = FALSE,
                                             snap_mode = c("interp", "nearest"),
                                             offset_sec = 0) {
    tracklog <- .read_gps_tracklog(
        path = gps_tracklog_path,
        tracklog_offset_hours = tracklog_offset_hours,
        csv_dayfirst = csv_dayfirst
    )

    drifts <- .read_drift_windows_csv(
        path = drift_windows_path,
        offset_hours = drift_offset_hours,
        dayfirst = drift_dayfirst
    )

    annotated <- .annotate_gps_tracklog(tracklog, drifts)

    snap_mode <- match.arg(snap_mode)
    full_id <- as.character(full_id)

    offset_index <- if (length(full_id) == 1L) {
        parsed_id <- suppressWarnings(as.integer(full_id))
        if (is.na(parsed_id) || parsed_id < 1L) 0L else parsed_id - 1L
    } else {
        seq_along(full_id) - 1L
    }

    purrr::map2_dfr(
        full_id,
        offset_index,
        function(this_full_id, offset_index) {
            .generate_synthetic_transmissions(
                full_id = this_full_id,
                delay_sec = delay_sec,
                max_gap_sec = max_gap_sec,
                wins = drifts,
                gpx = annotated,
                span_full = span_full,
                snap_mode = snap_mode,
                offset_sec = offset_index * offset_sec
            )
        }
    )
}



