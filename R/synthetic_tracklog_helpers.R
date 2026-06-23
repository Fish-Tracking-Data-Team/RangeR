.select_rows_from_console <- function(choices, value_col, prompt, allow_multiple = TRUE) {
    if (!nrow(choices)) {
        stop("No rows available to select from.", call. = FALSE)
    }

    print(choices)

    selected_rows <- readline(prompt = prompt) |>
        strsplit(",") |>
        unlist() |>
        trimws() |>
        as.integer()

    if (!allow_multiple && length(selected_rows) != 1) {
        stop("Please provide exactly one row number.", call. = FALSE)
    }

    if (!length(selected_rows) || any(is.na(selected_rows))) {
        stop("Please provide one or more valid row numbers.", call. = FALSE)
    }

    if (any(selected_rows < 1 | selected_rows > nrow(choices))) {
        stop("One or more selected row numbers are not valid.", call. = FALSE)
    }

    unique(choices[[value_col]][selected_rows])
}

.pick_first_existing_col <- function(data, candidates, label) {
    match <- intersect(candidates, names(data))[1]

    if (is.na(match)) {
        stop(
            sprintf(
                "Could not find a %s column. Checked: %s",
                label,
                paste(candidates, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    match
}

resolve_station_location <- function(stations,
                                      station_row = NULL,
                                      longitude_col = c("Longitude", "longitude", "Long", "long"),
                                      latitude_col = c("Latitude", "latitude", "Lat", "lat"),
                                      label_col = c("Station", "station", "Label", "label", "Serial", "serial"),
                                      prompt_user = TRUE) {
    if (!is.data.frame(stations) || !nrow(stations)) {
        stop("`stations` must be a data frame with at least one row.", call. = FALSE)
    }

    longitude_col <- .pick_first_existing_col(stations, longitude_col, "longitude")
    latitude_col <- .pick_first_existing_col(stations, latitude_col, "latitude")

    label_col_match <- intersect(label_col, names(stations))[1]

    station_preview <- stations

    if (!is.na(label_col_match)) {
        station_preview <- station_preview[, c(label_col_match, longitude_col, latitude_col), drop = FALSE]
    } else {
        station_preview <- station_preview[, c(longitude_col, latitude_col), drop = FALSE]
    }

    if (is.null(station_row)) {
        if (nrow(stations) == 1) {
            station_row <- 1L
        } else if (isTRUE(prompt_user) && interactive()) {
            print(station_preview)

            station_row <- as.integer(
                readline(
                    prompt = sprintf(
                        "Select the station row to use for the workflow [1-%s]: ",
                        nrow(stations)
                    )
                )
            )
        } else {
            stop(
                sprintf(
                    "`station_row` must be supplied when `stations` has multiple rows. Available rows: 1-%s.",
                    nrow(stations)
                ),
                call. = FALSE
            )
        }
    }

    if (length(station_row) != 1 || is.na(station_row) ||
        station_row < 1 || station_row > nrow(stations)) {
        stop(
            sprintf("`station_row` must be one integer between 1 and %s.", nrow(stations)),
            call. = FALSE
        )
    }

    station_row <- as.integer(station_row)

    longitude <- suppressWarnings(as.numeric(stations[[longitude_col]][station_row]))
    latitude <- suppressWarnings(as.numeric(stations[[latitude_col]][station_row]))

    if (is.na(longitude) || is.na(latitude)) {
        stop(
            sprintf(
                "Station row %s is missing valid longitude/latitude values.",
                station_row
            ),
            call. = FALSE
        )
    }

    list(
        station_row = station_row,
        station = stations[station_row, , drop = FALSE],
        longitude_col = longitude_col,
        latitude_col = latitude_col,
        longitude = longitude,
        latitude = latitude,
        station_location = c(longitude, latitude)
    )
}

resolve_time_buffer <- function(tag_delays,
                                     full_id = NULL,
                                     full_id_candidates = NULL,
                                     serial_col = c("Serial", "serial"),
                                     full_id_col = c("Full ID", "Full.ID", "full_id", "full.id"),
                                     nom_delay_col = c("Nom.Delay", "nom_delay", "Nom Delay", "nom.delay"),
                                     prompt_user = TRUE) {
    if (!is.data.frame(tag_delays) || !nrow(tag_delays)) {
        stop("`tag_delays` must be a data frame with at least one row.", call. = FALSE)
    }

    serial_col <- .pick_first_existing_col(tag_delays, serial_col, "serial")
    full_id_col <- .pick_first_existing_col(tag_delays, full_id_col, "Full ID")
    nom_delay_col <- .pick_first_existing_col(tag_delays, nom_delay_col, "Nom.Delay")

    delay_data <- tag_delays

    delay_data[[serial_col]] <- as.character(delay_data[[serial_col]])
    delay_data[[full_id_col]] <- as.character(delay_data[[full_id_col]])
    delay_data[[nom_delay_col]] <- suppressWarnings(as.numeric(delay_data[[nom_delay_col]]))

    delay_data <- delay_data[
        !is.na(delay_data[[full_id_col]]) & trimws(delay_data[[full_id_col]]) != "",
        ,
        drop = FALSE
    ]

    if (!is.null(full_id_candidates)) {
        full_id_candidates <- as.character(full_id_candidates)
        delay_data <- delay_data[delay_data[[full_id_col]] %in% full_id_candidates, , drop = FALSE]
    }

    if (!nrow(delay_data)) {
        stop("No tag programming rows remained after filtering `tag_delays`.", call. = FALSE)
    }

    delay_preview <- unique(
        delay_data[, c(full_id_col, serial_col, nom_delay_col), drop = FALSE]
    )

    rownames(delay_preview) <- NULL

    if (is.null(full_id)) {
        available_ids <- unique(delay_data[[full_id_col]])

        if (length(available_ids) == 1) {
            full_id <- available_ids[[1]]
        } else if (isTRUE(prompt_user) && interactive()) {
            print(delay_preview)

            selected_row <- as.integer(
                readline(
                    prompt = sprintf(
                        "Select the Full ID row to use for the GPS tracklog buffer [1-%s]: ",
                        nrow(delay_preview)
                    )
                )
            )

            if (length(selected_row) != 1 || is.na(selected_row) ||
                selected_row < 1 || selected_row > nrow(delay_preview)) {
                stop(
                    sprintf("Please select one row number between 1 and %s.", nrow(delay_preview)),
                    call. = FALSE
                )
            }

            full_id <- delay_preview[[full_id_col]][selected_row]
        } else {
            stop(
                paste(
                    "`full_id` must be supplied when multiple Full IDs are available.",
                    "Set `full_id` explicitly or use an interactive session to choose a row."
                ),
                call. = FALSE
            )
        }
    }

    selected_rows <- delay_data[delay_data[[full_id_col]] == full_id, , drop = FALSE]

    if (!nrow(selected_rows)) {
        stop(
            sprintf("Full ID '%s' was not found in `tag_delays`.", full_id),
            call. = FALSE
        )
    }

    selected_row <- selected_rows[1, , drop = FALSE]
    serial_value <- selected_row[[serial_col]][1]
    nom_delay <- selected_row[[nom_delay_col]][1]

    if (is.na(nom_delay)) {
        stop(
            sprintf("Full ID '%s' does not have a valid Nom.Delay value.", full_id),
            call. = FALSE
        )
    }

    shared_serial <- sum(delay_data[[serial_col]] == serial_value, na.rm = TRUE) > 1
    base_buffer <- nom_delay
    time_buffer <- if (shared_serial) 2 * base_buffer else base_buffer

    list(
        full_id = full_id,
        serial = serial_value,
        nom_delay = nom_delay,
        shared_serial = shared_serial,
        time_buffer = time_buffer,
        selected_programming = selected_row,
        programming_choices = delay_preview
    )
}
`%||%` <- function(a, b) {
    if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) {
        b
    } else {
        a
    }
}

.pick_ci <- function(nm, candidates) {
    nm_low <- tolower(nm)
    cand_low <- tolower(candidates)
    hit <- which(nm_low %in% cand_low)
    if (length(hit)) nm[hit[1]] else NA_character_
}

.parse_csv_datetime <- function(x, dayfirst = FALSE) {
    if (inherits(x, "POSIXt")) {
        return(lubridate::with_tz(x, "UTC"))
    }

    x <- as.character(x)

    try_list <- if (isTRUE(dayfirst)) {
        list(
            function(z) suppressWarnings(lubridate::dmy_hms(z, tz = "UTC", quiet = TRUE)),
            function(z) suppressWarnings(lubridate::dmy_hm(z, tz = "UTC", quiet = TRUE))
        )
    } else {
        list(
            function(z) suppressWarnings(lubridate::mdy_hms(z, tz = "UTC", quiet = TRUE)),
            function(z) suppressWarnings(lubridate::mdy_hm(z, tz = "UTC", quiet = TRUE))
        )
    }

    try_list <- c(
        try_list,
        list(
            function(z) suppressWarnings(lubridate::ymd_hms(z, tz = "UTC", quiet = TRUE)),
            function(z) {
                suppressWarnings(
                    lubridate::parse_date_time(
                        z,
                        orders = c(
                            "Y-m-d H:M:S", "Y/m/d H:M:S", "Y-m-d H:M", "Y/m/d H:M",
                            "m/d/Y H:M:S", "d/m/Y H:M:S",
                            "m/d/y H:M:S", "d/m/y H:M:S"
                        ),
                        tz = "UTC",
                        quiet = TRUE
                    )
                )
            }
        )
    )

    res <- NULL
    best_ok <- -1L
    for (f in try_list) {
        cand <- f(x)
        ok <- sum(!is.na(cand))
        if (ok > best_ok) {
            res <- cand
            best_ok <- ok
        }
        if (ok == length(x)) {
            break
        }
    }

    res
}

.dedupe_drift_tags <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(x)
    }
    paste0(unique(strsplit(x, ",", fixed = TRUE)[[1]]), collapse = ",")
}

.normalize_synth_inputs <- function(full_id, delay_sec, offset_sec, wins, gpx) {
    if (!nzchar(full_id)) return(NULL)
    if (is.null(wins) || !nrow(wins)) return(NULL)
    if (is.null(gpx) || !"time" %in% names(gpx)) return(NULL)

    delay_sec <- suppressWarnings(as.numeric(delay_sec))
    offset_sec <- suppressWarnings(as.numeric(offset_sec))

    if (!is.finite(delay_sec) || delay_sec <= 0) return(NULL)
    if (!is.finite(offset_sec) || offset_sec < 0) offset_sec <- 0
    offset_sec <- offset_sec %% delay_sec

    wins <- wins |>
        dplyr::filter(!is.na(start), !is.na(end), start < end) |>
        dplyr::arrange(start)
    if (!nrow(wins)) return(NULL)

    gpx <- gpx |>
        dplyr::filter(is.finite(as.numeric(lon)), is.finite(as.numeric(lat))) |>
        dplyr::mutate(time = as.POSIXct(time, tz = "UTC")) |>
        dplyr::arrange(time)
    if (!nrow(gpx)) return(NULL)

    list(
        full_id = full_id,
        delay_sec = delay_sec,
        offset_sec = offset_sec,
        wins = wins,
        gpx = gpx
    )
}

.parse_with_offset_to_utc <- function(x, offset_hours = 0, dayfirst = FALSE) {
    if (inherits(x, "POSIXt")) {
        t_naive <- lubridate::with_tz(x, "UTC")
    } else {
        xx <- as.character(x)

        tries <- if (isTRUE(dayfirst)) {
            list(
                function(z) suppressWarnings(lubridate::dmy_hms(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::dmy_hm(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::ymd_hms(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::ymd_hm(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::mdy_hms(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::mdy_hm(z, tz = "UTC", quiet = TRUE)),
                function(z) {
                    suppressWarnings(
                        lubridate::parse_date_time(
                            z,
                            orders = c(
                                "d/m/Y HMS", "d/m/Y HM", "d/m/y HMS", "d/m/y HM",
                                "Y-m-d HMS", "Y-m-d HM", "Y/m/d HMS", "Y/m/d HM",
                                "m/d/Y HMS", "m/d/Y HM", "m/d/Y I:M:S p", "m/d/Y I:M p"
                            ),
                            tz = "UTC",
                            quiet = TRUE
                        )
                    )
                }
            )
        } else {
            list(
                function(z) suppressWarnings(lubridate::ymd_hms(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::ymd_hm(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::mdy_hms(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::mdy_hm(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::dmy_hms(z, tz = "UTC", quiet = TRUE)),
                function(z) suppressWarnings(lubridate::dmy_hm(z, tz = "UTC", quiet = TRUE)),
                function(z) {
                    suppressWarnings(
                        lubridate::parse_date_time(
                            z,
                            orders = c(
                                "Y-m-d HMS", "Y-m-d HM", "Y/m/d HMS", "Y/m/d HM",
                                "m/d/Y HMS", "m/d/Y HM", "m/d/Y I:M:S p", "m/d/Y I:M p",
                                "d/m/Y HMS", "d/m/Y HM", "d/m/y HMS", "d/m/y HM"
                            ),
                            tz = "UTC",
                            quiet = TRUE
                        )
                    )
                }
            )
        }

        best <- NULL
        best_ok <- -1L
        for (f in tries) {
            cand <- f(xx)
            ok <- sum(!is.na(cand))
            if (ok > best_ok) {
                best <- cand
                best_ok <- ok
            }
            if (ok == length(xx)) {
                break
            }
        }
        t_naive <- best
    }

    t_utc <- t_naive - lubridate::dhours(as.numeric(offset_hours %||% 0))
    attr(t_utc, "tzone") <- "UTC"
    t_utc
}

.read_drift_windows_csv <- function(path, offset_hours = 0, dayfirst = FALSE) {
    df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

    pick <- function(cands) {
        cand <- intersect(cands, names(df))
        if (length(cand)) cand[1] else NA_character_
    }

    scol <- pick(c("StartTime", "start_time", "Start", "start", "starttime", "Begin", "begin"))
    ecol <- pick(c("EndTime", "end_time", "End", "end", "stoptime", "StopTime", "finish", "Finish"))
    lcol <- pick(c("Drift", "drift", "DriftNumber", "drift_number", "Label", "label", "ID", "id"))

    if (is.na(scol) || is.na(ecol)) {
        stop("CSV must have StartTime and EndTime columns.", call. = FALSE)
    }

    start_utc <- .parse_with_offset_to_utc(df[[scol]], offset_hours, dayfirst = dayfirst)
    end_utc <- .parse_with_offset_to_utc(df[[ecol]], offset_hours, dayfirst = dayfirst)
    label_id <- if (!is.na(lcol)) df[[lcol]] else seq_len(nrow(df))

    tibble::tibble(
        label_id = as.character(label_id),
        start = start_utc,
        end = end_utc,
        start_str = as.character(df[[scol]]),
        end_str = as.character(df[[ecol]]),
        utc_offset_hours = as.numeric(offset_hours)
    ) |>
        dplyr::filter(!is.na(start), !is.na(end), start < end) |>
        dplyr::arrange(start, end, label_id)
}

.recompute_drift_windows <- function(windows, offset_hours = 0, dayfirst = FALSE) {
    if (is.null(windows) || !nrow(windows)) return(windows)
    if (!all(c("start_str", "end_str") %in% names(windows))) {
        stop("windows must contain start_str and end_str columns.", call. = FALSE)
    }

    windows |>
        dplyr::mutate(
            start = .parse_with_offset_to_utc(start_str, offset_hours, dayfirst = dayfirst),
            end = .parse_with_offset_to_utc(end_str, offset_hours, dayfirst = dayfirst),
            utc_offset_hours = as.numeric(offset_hours)
        ) |>
        dplyr::filter(!is.na(start), !is.na(end), start < end) |>
        dplyr::arrange(start, end, label_id)
}

.read_gps_tracklog <- function(path, tracklog_offset_hours = 0, csv_dayfirst = FALSE) {
    ext <- tolower(tools::file_ext(path))

    if (ext == "gpx") {
        layers <- try(sf::st_layers(path)$name, silent = TRUE)
        if (inherits(layers, "try-error")) {
            stop("Could not read GPX. Is the file valid?", call. = FALSE)
        }

        preferred <- c("track_points", "tracks", "waypoints", "route_points", "routes")
        layer <- preferred[preferred %in% layers][1]
        if (!length(layer) || is.na(layer)) {
            stop(
                paste0("No suitable GPX layers found. Available: ", paste(layers, collapse = ", ")),
                call. = FALSE
            )
        }

        sfo <- sf::st_read(path, layer = layer, quiet = TRUE)
        coords <- sf::st_coordinates(sfo)
        df <- sf::st_drop_geometry(sfo)

        if (nrow(coords) == nrow(df)) {
            df$lon <- coords[, 1]
            df$lat <- coords[, 2]
            if (ncol(coords) >= 3 && !"ele" %in% names(df)) df$ele <- coords[, 3]
        }

        df$lon <- as.numeric(unlist(df$lon))
        df$lat <- as.numeric(unlist(df$lat))

        if ("time" %in% names(df)) {
            if (!inherits(df$time, "POSIXct")) {
                tt <- suppressWarnings(lubridate::ymd_hms(df$time, tz = "UTC", quiet = TRUE))
                bad <- is.na(tt) & !is.na(df$time)
                if (any(bad)) {
                    tt[bad] <- as.POSIXct(df$time[bad], format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
                }
                df$time <- tt
            } else {
                df$time <- lubridate::with_tz(df$time, "UTC")
            }

            off <- suppressWarnings(as.numeric(tracklog_offset_hours))
            if (is.na(off)) off <- 0
            if (off != 0) df$time <- df$time - lubridate::dhours(off)
        }

        return(
            df |>
                dplyr::select(
                    dplyr::any_of(c("time", "lon", "lat", "ele", "track_seg_id", "track_fid")),
                    dplyr::everything()
                )
        )
    }

    if (ext %in% c("csv", "txt")) {
        csv <- suppressWarnings(
            readr::read_csv(path, show_col_types = FALSE, progress = FALSE, guess_max = 200000)
        )

        if (!nrow(csv)) {
            stop("CSV appears empty.", call. = FALSE)
        }

        nm <- names(csv)
        tcol <- .pick_ci(nm, c("DateTime", "Date_Time", "Time", "Timestamp", "UTC", "Date & Time", "datetime", "date_time"))
        latc <- .pick_ci(nm, c("lat", "latitude", "Lat", "Latitude", "y", "lat_dd"))
        lonc <- .pick_ci(nm, c("long", "longitude", "lon", "lng", "Long", "Longitude", "x", "long_dd"))

        if (is.na(tcol)) {
            stop("CSV must contain a Date/Time column (e.g., DateTime, Time).", call. = FALSE)
        }
        if (is.na(latc)) {
            stop("CSV must contain a Latitude column (e.g., Lat, Latitude).", call. = FALSE)
        }
        if (is.na(lonc)) {
            stop("CSV must contain a Longitude column (e.g., Long, Lon, Longitude).", call. = FALSE)
        }

        tt <- .parse_csv_datetime(csv[[tcol]], dayfirst = isTRUE(csv_dayfirst))
        tt <- tt - lubridate::dhours(as.numeric(tracklog_offset_hours %||% 0))

        out <- tibble::tibble(
            time = tt,
            lon = suppressWarnings(as.numeric(csv[[lonc]])),
            lat = suppressWarnings(as.numeric(csv[[latc]])),
            track_seg_id = 1L,
            track_fid = 1L,
            track_seg_point_id = seq_along(tt)
        )

        if (sum(!is.na(out$time)) == 0) {
            stop(
                "Could not parse any times from the CSV. Try flipping day-first or adjusting the UTC offset.",
                call. = FALSE
            )
        }

        return(out)
    }

    stop("Unsupported file type. Please upload a .gpx or .csv.", call. = FALSE)
}

.annotate_gps_tracklog <- function(tracklog, drift_windows) {
    if (is.null(tracklog) || !nrow(tracklog)) return(tracklog)

    pts <- tracklog
    if (!"time" %in% names(pts)) pts$time <- NA
    if (!inherits(pts$time, "POSIXt")) pts$time <- as.POSIXct(pts$time, tz = "UTC")

    pts$in_window <- FALSE
    pts$drift_tag <- NA_character_

    win <- drift_windows
    if (!is.null(win) && nrow(win) > 0) {
        for (i in seq_len(nrow(win))) {
            st <- win$start[i]
            en <- win$end[i]
            hit <- !is.na(pts$time) & pts$time >= st & pts$time <= en
            pts$in_window <- pts$in_window | hit

            this_tag <- as.character(win$label_id[i])
            if (any(hit)) {
                prev <- pts$drift_tag[hit]
                pts$drift_tag[hit] <- ifelse(
                    is.na(prev) | prev == "",
                    this_tag,
                    paste0(prev, ",", this_tag)
                )
            }
        }

        pts$drift_tag <- vapply(pts$drift_tag, .dedupe_drift_tags, character(1))
    }

    pts |>
        dplyr::mutate(
            lon = round(lon, 4),
            lat = round(lat, 4)
        )
}

.filter_annotated_tracklog <- function(annotated_tracklog) {
    if (is.null(annotated_tracklog) || !nrow(annotated_tracklog)) return(annotated_tracklog)

    annotated_tracklog |>
        dplyr::filter(!is.na(in_window) & in_window) |>
        dplyr::select(time, lon, lat, drift_tag, dplyr::everything())
}

.generate_synthetic_transmissions <- function(full_id,
                                             delay_sec,
                                             max_gap_sec,
                                             wins,
                                             gpx,
                                             span_full = TRUE,
                                             snap_mode = c("interp", "nearest"),
                                             offset_sec = 0) {
    snap_mode <- match.arg(snap_mode)

    normalized <- .normalize_synth_inputs(
        full_id = full_id,
        delay_sec = delay_sec,
        offset_sec = offset_sec,
        wins = wins,
        gpx = gpx
    )
    if (is.null(normalized)) return(tibble::tibble())

    full_id <- normalized$full_id
    delay_sec <- normalized$delay_sec
    offset_sec <- normalized$offset_sec
    wins <- normalized$wins
    gpx <- normalized$gpx

    t_lo <- min(wins$start)
    t_hi <- max(wins$end)
    base_anchor <- t_lo + lubridate::dseconds(offset_sec)

    ts_fwd <- if (base_anchor <= t_hi) {
        seq(from = base_anchor, to = t_hi, by = delay_sec)
    } else {
        base_anchor[0]
    }

    ts_bwd <- if (t_lo <= base_anchor - delay_sec) {
        seq(from = t_lo, to = base_anchor - delay_sec, by = delay_sec)
    } else {
        base_anchor[0]
    }

    ts_all <- sort(unique(c(ts_fwd, ts_bwd)))

    if (!isTRUE(span_full)) {
        inside <- rep(FALSE, length(ts_all))
        for (i in seq_len(nrow(wins))) {
            inside <- inside | (ts_all >= wins$start[i] & ts_all <= wins$end[i])
        }
        ts_all <- ts_all[inside]
    }
    if (!length(ts_all)) return(tibble::tibble())

    if (identical(snap_mode, "interp")) {
        gmin <- min(gpx$time)
        gmax <- max(gpx$time)
        keep <- ts_all >= gmin & ts_all <= gmax
        ts_keep <- ts_all[keep]
        if (!length(ts_keep)) return(tibble::tibble())

        x <- as.numeric(gpx$time)
        xo <- as.numeric(ts_keep)
        lon_i <- stats::approx(x, gpx$lon, xout = xo, method = "linear", ties = "ordered")$y
        lat_i <- stats::approx(x, gpx$lat, xout = xo, method = "linear", ties = "ordered")$y

        out <- tibble::tibble(
            Time = as.POSIXct(ts_keep, tz = "UTC", origin = "1970-01-01"),
            Latitude = lat_i,
            Longitude = lon_i,
            `Full ID` = full_id,
            Model = "synthetic"
        )

        return(
            out |>
                dplyr::filter(is.finite(Latitude), is.finite(Longitude)) |>
                dplyr::arrange(Time)
        )
    }

    gpx_time_num <- as.numeric(gpx$time)
    ts_num <- as.numeric(ts_all)

    idxL <- findInterval(ts_num, gpx_time_num)
    idxR <- pmin(idxL + 1L, length(gpx_time_num))
    idxL[idxL < 1L] <- 1L

    gapL <- abs(gpx_time_num[idxL] - ts_num)
    gapR <- abs(gpx_time_num[idxR] - ts_num)
    useR <- gapR < gapL
    best <- ifelse(useR, idxR, idxL)

    gap_sec <- abs(gpx_time_num[best] - ts_num)
    ok <- gap_sec <= max_gap_sec
    if (!any(ok)) return(tibble::tibble())

    tibble::tibble(
        Time = as.POSIXct(ts_num[ok], tz = "UTC", origin = "1970-01-01"),
        Latitude = gpx$lat[best[ok]],
        Longitude = gpx$lon[best[ok]],
        `Full ID` = full_id,
        Model = "synthetic"
    ) |>
        dplyr::arrange(Time)
}





