library(dplyr)

get_state_list <- function() {
  
  readRDS('data/finalized/ALL_DATA.rds') %>%
    select(state_name) %>%
    distinct() %>%
    pull(state_name)
  
}
