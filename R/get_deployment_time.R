get_deplpoyment_time <- function(diag_fast, min_tilt = 0, max_tilt = 60, n) {
  diag_fast <- diag_fast %>%
    mutate(Tilt = as.numeric(`Tilt (deg)`))

  diag_fast <- diag_fast %>%
    group_by(Serial) %>%
    arrange(Time) %>%
    mutate(above_max_tilt = Tilt > max_tilt) %>%
    mutate(operational = {
      operational_vec <- rep(TRUE , n())
      above_max_tilt_vec <- above_max_tilt
      len <- length(operational_vec)
      for (i in seq_len(len)) {
        if (i <= len - n + 1 && all(above_max_tilt_vec[i:(i + n - 1)])) {
          operational_vec[i:(i + n -1)] <- FALSE
        }
      }
      operational_vec
    }) %>%
    ungroup()

  diag_fast <- diag_fast %>%
    mutate(group = with(rle(operational), rep(seq_along(lengths), lengths)))

  operational_periods <- diag_fast %>%
    filter(operational) %>%
    group_by(Serial, group) %>%
    summarize(start = min(Time), end = max(Time), .groups = 'drop') %>%
    mutate(duration = end - start) %>%
    mutate(duration_days = as.numeric(duration, units = "days")) %>%
    group_by(Serial) %>%
    arrange(desc(duration)) %>%
    slice(1)
  minTime <- max(operational_periods$start)
  maxTime <- min(operational_periods$end)

  return(list(minTime = minTime, maxTime = maxTime, operational_periods = operational_periods))
}
