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
