library(dplyr)

get_state_list <- function() {
  DATA %>%
    select(state_name) %>%
    distinct() %>%
    pull(state_name)
}
