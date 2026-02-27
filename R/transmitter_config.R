#' Build a transmitter configuration table
#'
#' Filters active transmitters at the reference station, computes nominal delay,
#' and attaches model metadata.
#'
#' @param cfg_transmitter Data frame of transmitter configuration records with
#'   `Serial`, `Full ID`, `Min Delay (s)`, `Max Delay (s)`, and `Power Level`.
#' @param stations Data frame of station metadata with `Serial` and `distance`.
#' @return A data frame with `Full.ID`, delay fields, `Power Level`, `nom_delay`,
#'   and `model`.
#' @examples
#' \dontrun{
#' transmitters <- transmitter_config(cfg_transmitter, stations)
#' }
#' @export
transmitter_config <- function(cfg_transmitter, stations){

transmitters <- cfg_transmitter %>%
  left_join(select(stations, Serial, distance), by='Serial') %>%
  filter(`Power Level` != 'DISABLED' & distance == 0) %>%
  group_by(Serial) %>%
  slice(n()) %>%
  select(`Full ID`, `Min Delay (s)`, `Max Delay (s)`, `Power Level`) %>%
  mutate(across(.cols = -c(`Full ID`, `Power Level`), .fns = as.numeric)) %>%
  rename(Full.ID = `Full ID`) %>%
  mutate(nom_delay = mean(`Min Delay (s)` : `Max Delay (s)`)) %>%
  mutate(nom_delay = paste0(ceiling(nom_delay / 60)," min"),
         model = factor(c(`Power Level`)))
return(transmitters)

}
