library(dplyr)

get_metrics_list <- function(state = NULL, county = NULL, size = 'All') {
  
  m <- readRDS('data/finalized/ALL_DATA.rds') %>%
    filter(
      case_when(state == 'ALL STATES' ~ county_name == 'All', T ~ !is.na(state_name)),
      case_when(!is.null(state) & state != 'ALL STATES' ~ state_name == state, T ~ !is.na(state_name)),
      #case_when(!is.null(county) ~ county_name == county, T ~ !is.na(state_name))#,
      case_when(!is.null(size) ~ farm_size == size, T ~ !is.na(state_name))
    ) %>%
    select(short_desc) %>%
    distinct() %>%
    pull(short_desc)
  

  return(m)
  
}
