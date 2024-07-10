library(DBI)
library(dplyr)
library(dbplyr)

get_metrics_list <- function(state = NULL, county = NULL, size = NULL) {
  
  mydb <- dbConnect(RSQLite::SQLite(), "census-db.sqlite")
  
  data_table <- tbl(mydb, "all_data")
  
  qry <- data_table %>%
    filter(
      case_when(state == 'ALL STATES' ~ county_name == 'All', T ~ !is.na(state_name)),
      case_when(!is.null(state) & state != 'ALL STATES' ~ state_name == state, T ~ !is.na(state_name)),
      case_when(!is.null(county) ~ county_name == county, T ~ !is.na(state_name)),
      case_when(!is.null(size) ~ farm_size == size, T ~ !is.na(state_name))
    ) %>%
    select(short_desc) %>%
    distinct()
    
  results <- qry %>% 
    collect() %>%
    pull(short_desc)
  
  dbDisconnect(mydb)
  
  return(results)
  
}
