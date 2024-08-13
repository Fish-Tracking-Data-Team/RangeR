
compile_drifts <- function(detection.data, transmit.data, time_buffer = 5, dist_threshold = 1200) {
  serial_values <- unique(detection.data$Serial)

  results <- purrr::map(serial_values, function(serial_value) {
    df_subset <- detection.data %>%
      filter(Serial == serial_value)

    fuzzy_left_join(transmit.data, df_subset,
                    by = c("Time" = "Time"),
                    match_fun = list(function(x, y) abs(difftime(x, y, units = "secs")) < time_buffer)) %>%
      dplyr::mutate(success = ifelse(!is.na(`Full ID.y`),1,0),
                    Serial = serial_value) %>%
      dplyr::select(Time.x,Serial.x,Serial.y,Serial,`Full ID.y`,distance, success)
  })

  detection_table <- bind_rows(results) %>%
    arrange(Serial, Time.x) %>%
    select(distance, success) %>%
    filter(distance <= dist_threshold)

  anchor <- data.frame(distance = rep(0.0,nrow(transmit.data)),
                       success = rep(1.0, nrow(transmit.data)))

  compiled_data <- bind_rows(detection_table, anchor)

  # logistic model
  lm <- glm(success ~  distance,  data = compiled_data, family=binomial)

  predictions <- data.frame(distance = seq(min(detection_table$distance), max(detection_table$distance), length.out = 500))
  predictions$prob <- predict(lm, newdata = predictions, type = "response")

  list(compiled_data = compiled_data, predictions = predictions)
}
