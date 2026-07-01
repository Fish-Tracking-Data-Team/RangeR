#' Calculate detection probability curves by tag model
#'
#' Fits logistic regression models of detection success versus distance for each
#' transmitter model and returns a combined plot and probability table.
#'
#' @param detection_data Data frame of detections with at least `Serial`,
#'   `Full ID`, and `Time` columns.
#' @param transmitters Data frame of transmitter metadata (for example from
#'   `transmitter_config()`) that includes `Full.ID` and `model`.
#' @param stations.red Data frame of receiver metadata with `Serial`, `Station`,
#'   and `distance` columns.
#' @param minTime,maxTime Start and end timestamps used to filter detections.
#' @param nom_delay Interval string passed to `lubridate::floor_date()` and used
#'   for padding (for example `"5 min"`).
#' @param prediction_distance_max Optional maximum distance in meters used when
#'   generating the prediction curve. Defaults to the maximum observed distance
#'   in the filtered detection data.
#' @return A `grob` produced by `gridExtra::grid.arrange()` containing the
#'   detection probability curve and a probability table.
#' @importFrom padr pad fill_by_value
#' @importFrom gridExtra grid.arrange tableGrob ttheme_default
#' @examples
#' \dontrun{
#' p <- calculate_range(detection_data, transmitters, stations.red,
#'                      minTime, maxTime, nom_delay = "5 min")
#' p
#' }
#' @export
  calculate_range <- function(detection_data,
                              transmitters,
                              stations.red,
                              minTime,
                              maxTime,
                              nom_delay,
                              prediction_distance_max = NULL) {
    # Prepare the data
    range_test_detection_data <- detection_data %>%
      filter(`Full ID` %in% transmitters$Full.ID) %>%
      filter(Time >= minTime & Time <= maxTime) %>%
      select(Serial, `Full ID`, Time) %>%
      mutate(Delay_Time = floor_date(Time, unit = nom_delay),
             `Full ID` = as.factor(`Full ID`)) %>%
      group_by(Serial, `Full ID`, Delay_Time) %>%
      summarise(count = n(), .groups = 'drop') %>%
      pad(interval = nom_delay, group = c('Serial','Full ID')) %>%
      fill_by_value() %>%
      mutate(count = pmin(pmax(count, 0), 1)) %>%
      left_join(select(stations.red, Serial, Station, distance), by = 'Serial') %>%
      left_join(transmitters, by = join_by(`Full ID` == Full.ID))

    observed_max_distance <- max(range_test_detection_data$distance, na.rm = TRUE)

    if (!is.null(prediction_distance_max)) {
      if (!is.numeric(prediction_distance_max) ||
          length(prediction_distance_max) != 1 ||
          !is.finite(prediction_distance_max)) {
        stop(
          "`prediction_distance_max` must be a single finite numeric value.",
          call. = FALSE
        )
      }

      if (prediction_distance_max < observed_max_distance) {
        stop(
          "`prediction_distance_max` must be greater than or equal to the maximum observed distance.",
          call. = FALSE
        )
      }
    }

    prediction_distance_max <- if (is.null(prediction_distance_max)) {
      observed_max_distance
    } else {
      prediction_distance_max
    }

    # Split the data by model
    data_list <- split(range_test_detection_data, range_test_detection_data$model)

    # Fit logistic regression models for each model
    models <- lapply(data_list, function(data) {
      glm(count ~ distance, data = data, family = binomial())
    })

    # Generate new data for predictions
    new_distances <- data.frame(distance = seq(0, prediction_distance_max, length.out = 500))
    predictions <- lapply(models, function(model) {
      predict(model, newdata = new_distances, type = "response")
    })

    # Combine predictions with new_distances
    predictions_df <- do.call(cbind, predictions)
    colnames(predictions_df) <- levels(range_test_detection_data$model)
    predictions_df <- cbind(new_distances, predictions_df)

    predictions_long <- predictions_df %>%
      tidyr::pivot_longer(-distance, names_to = "model", values_to = "predicted_count")
    #predictions_long <- pivot_longer(predictions_df, cols = -distance, names_to = "model", values_to = "predicted_count")

    # Calculate probabilities and distances
    p_values <- seq(0.1, 0.9, by = 0.1)
    all_models_df <- data.frame()
    for (model_name in names(models)) {
      model <- models[[model_name]]
      coefficients <- coef(model)
      beta_0 <- coefficients[1]
      beta_1 <- coefficients[2]
      x_values <- (log(p_values / (1 - p_values)) - beta_0) / beta_1
      rangeprobs <- data.frame(p_values, x_values)
      rangeprobs_red <- rangeprobs %>%
        filter(x_values >= 0) %>%
        mutate(`Distance (m)` = round(x_values, 0),
               Model = model_name) %>%
        rename(probability = p_values) %>%
        select(Model, probability, `Distance (m)`)
      all_models_df <- rbind(all_models_df, rangeprobs_red)
    }

    wide_df <- all_models_df %>%
      pivot_wider(names_from = Model, values_from = `Distance (m)`)


    prob_table <- tableGrob(wide_df, rows = NULL,
                       theme = ttheme_default(
                         core = list(fg_params = list(fontsize = 10)),
                         colhead = list(fg_params = list(fontsize = 10))))


    # Generate plot
    p <- ggplot() +
      geom_line(data = predictions_long, aes(x = distance, y = predicted_count, color = model)) +
      geom_point(data = all_models_df, aes(`Distance (m)`, probability, color = Model)) +
      geom_jitter(data = range_test_detection_data, aes(distance, count, color = model), width = 10, height = 0.02, alpha = 0.1) +
      labs(title = "Detection Probability Curve",
           x = "Distance (m)",
           y = "Detection probability",
           color = "Tag Model") +
      geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'black') +
      scale_y_continuous(limits = c(0,1.0), breaks = seq(0,1.0,0.1)) +
      theme(axis.line = element_line(colour = 'black'),
            panel.background = element_blank(),
            legend.position = c(0.9,0.9),
            axis.ticks.length = unit(0.1, "cm"),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())

    # p1 <- p + annotation_custom(grob = prob_table, xmin = 250, xmax = 380, ymin = -Inf, ymax = 1.6) +
    #   theme(plot.margin = unit(c(1, 4, 1, 1), "lines"))
    p1 <- grid.arrange(p, prob_table, widths = c(3/4, 1/4))

    return(p1)
  }
