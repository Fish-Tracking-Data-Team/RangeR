#' Read Innovasea transmitter specifications from an order sheet
#'
#' Imports a transmitter specification workbook, locates the tag-summary sheet,
#' standardizes column names when requested, and returns the full sheet plus
#' derived transmitter delay information.
#'
#' @param path Path to the Excel workbook containing transmitter specifications.
#' @param sheet_hint Character string used to identify the target worksheet.
#'   Matching is case-insensitive and tolerant of spacing differences.
#' @param id_col_candidates Candidate column names used to identify the
#'   transmitter `Full ID` field.
#' @param clean_names Logical; if `TRUE`, clean column names with
#'   `janitor::clean_names()` when available, otherwise use `make.names()`.
#' @param trim_chars Logical; if `TRUE`, trim leading and trailing whitespace
#'   from all character columns.
#' @param drop_missing_ids Logical; if `TRUE`, remove rows where the transmitter
#'   ID column is missing or blank.
#' @param output Output format. `"both"` returns a list containing the imported
#'   specification table and the derived delay table. `"specs"` returns only
#'   the imported sheet. `"delays"` returns only the derived delay table.
#'   `"sensors"` returns only the derived sensor calibration table.
#' @return Depending on `output`, either:
#'   a data frame of imported specifications,
#'   a data frame of derived transmitter delays,
#'   a data frame of derived sensor calibration values,
#'   or a list with `specs`, `delays`, and `sensors`.
#' @examples
#' \dontrun{
#' tag_specs <- innovasea_transmitter_spec(
#'   path = "./source_data/spec_sheets/OrderSheet_77720.xls",
#'   sheet_hint = "tag summary",
#'   drop_missing_ids = TRUE
#' )
#'
#' tag_delays <- tag_specs$delays
#' full_specs <- tag_specs$specs
#' sensors <- tag_specs$sensors
#' }
#' @export
innovasea_transmitter_spec <- function(
  path,
  sheet_hint = "tag summary",
  id_col_candidates = c(
    "vue.tag.id", "vue_tag_id", "vue.tagid", "vuetagid", "vue.tag.id."
  ),
  clean_names = TRUE,
  trim_chars = TRUE,
  drop_missing_ids = FALSE,
  output = c("both", "specs", "delays", "sensors")
) {
  output <- match.arg(output)

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }

  norm <- function(x) gsub("\\s+", " ", trimws(tolower(x)))
  pick_col <- function(nm, candidates, label) {
    hit <- which(norm(nm) %in% norm(candidates))
    if (length(hit) == 0) {
      stop(
        "Couldn't find the ", label, " column. Looked for any of: ",
        paste(candidates, collapse = ", "),
        "\nColumns present: ", paste(nm, collapse = ", ")
      )
    }
    nm[hit[1]]
  }
  as_numeric_safely <- function(x) suppressWarnings(as.numeric(as.character(x)))

  # Find the Tag Summary sheet, allowing for case and spacing differences.
  sheets <- readxl::excel_sheets(path)
  sheets_norm <- norm(sheets)
  hint_norm <- norm(sheet_hint)

  exact_hit <- which(sheets_norm == hint_norm)
  contains_hit <- which(grepl(hint_norm, sheets_norm, fixed = TRUE))
  starts_hit <- which(startsWith(sheets_norm, hint_norm))
  match_idx <- c(exact_hit, contains_hit, starts_hit)
  match_idx <- match_idx[!duplicated(match_idx)]

  if (length(match_idx) == 0) {
    stop(
      "Couldn't find a sheet matching '", sheet_hint,
      "'. Sheets present: ", paste(sheets, collapse = ", ")
    )
  }
  if (length(match_idx) > 1) {
    warning(
      "Multiple sheets matched '", sheet_hint, "': ",
      paste(sheets[match_idx], collapse = ", "),
      ". Using '", sheets[match_idx[1]], "'."
    )
  }
  target_sheet <- sheets[match_idx[1]]

  df <- readxl::read_excel(path, sheet = target_sheet, .name_repair = "unique")

  if (clean_names) {
    if (requireNamespace("janitor", quietly = TRUE)) {
      df <- janitor::clean_names(df)
    } else {
      names(df) <- make.names(names(df), unique = TRUE)
    }
  }

  if (trim_chars) {
    df <- dplyr::mutate(
      df,
      dplyr::across(dplyr::where(is.character), ~ trimws(.x))
    )
  }

  nm <- names(df)
  id_col <- pick_col(nm, id_col_candidates, "vue.tag.id")

  if (drop_missing_ids) {
    id_vals <- df[[id_col]]
    keep <- !(is.na(id_vals) | trimws(as.character(id_vals)) == "")
    df <- df[keep, , drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop(
      "No rows available after reading '", target_sheet,
      "' and applying filters."
    )
  }

  nm <- names(df)
  serial_col <- pick_col(
    nm,
    c("serial.no.", "serial_no", "serial.no", "serial number", "serial_number"),
    "serial number"
  )
  min_delay_col <- pick_col(
    nm,
    c(
      "step.1.min.delay..sec.",
      "step_1_min_delay_sec",
      "step.1.min.delay.sec.",
      "step 1 min delay sec"
    ),
    "step 1 minimum delay"
  )
  max_delay_col <- pick_col(
    nm,
    c(
      "step.1.max.delay..sec.",
      "step_1_max_delay_sec",
      "step.1.max.delay.sec.",
      "step 1 max delay sec"
    ),
    "step 1 maximum delay"
  )
  power_level <- pick_col(
    nm,
    c("step.1.power..l.h.",
      "step_1_power__l_h")
  )
  tag_model <- pick_col(
    nm,
    c("tag.family",
      "tag_family")
  )
  sensor_type <- pick_col(
    nm,
    c("sensor.type",
      "sensor_type")
  )
  slope <- pick_col(
    nm, 
    c("slope")
  )
  intercept <- pick_col(
    nm,
    c("intercept")
  )



  delay_df <- data.frame(
    Serial = df[[serial_col]],
    `Full ID` = df[[id_col]],
    Tag.Model = df[[tag_model]],
    Step.1.Power = df[[power_level]],
    Step.1.Min.Delay = as_numeric_safely(df[[min_delay_col]]),
    Step.1.Max.Delay = as_numeric_safely(df[[max_delay_col]]),
    stringsAsFactors = FALSE
  )
  delay_df$Nom.Delay <- rowMeans(
    cbind(
      delay_df$Step.1.Min.Delay,
      delay_df$Step.1.Max.Delay
    ),
    na.rm = FALSE
  )

  sensor_df <- data.frame(
    Serial = df[[serial_col]],
    `Full ID` = df[[id_col]],
    Tag.Model = df[[tag_model]],
    Sensor.Type = df[[sensor_type]],
    Slope = df[[slope]],
    Intercept = df[[intercept]] 

  )

  if (output == "specs") {
    return(df)
  }
  if (output == "delays") {
    return(delay_df)
  }
  if (output == "sensors") {
    return(sensor_df)
  }

  list(
    specs = df,
    delays = delay_df,
    sensors = sensor_df

  )
}
