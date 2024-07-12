library(dplyr)

get_metrics_list <- function(state = NULL, size = 'All', type = "Total sales, $") {
  m <- DATA %>%
    filter(
      case_when(state == 'ALL STATES' ~ county_name == 'All', T ~ !is.na(state_name)),
      case_when(!is.null(state) & state != 'ALL STATES' ~ state_name == state, T ~ !is.na(state_name)),
      case_when(!is.null(size) ~ farm_size == size, T ~ !is.na(state_name)),
      metric == type
    ) %>%
    select(short_desc) %>%
    distinct() %>%
    pull(short_desc)
  
  return(m)
  
}