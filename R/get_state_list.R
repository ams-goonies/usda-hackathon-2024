library(DBI)
library(dplyr)
library(dbplyr)

get_state_list <- function() {
  
  mydb <- dbConnect(RSQLite::SQLite(), "census-db.sqlite")
  
  data_table <- tbl(mydb, "all_data")
  
  qry <- data_table %>%
    select(state_name) %>%
    distinct() 
  
  states <- qry %>%
    collect() %>%
    pull(state_name)
  
  dbDisconnect(mydb)
  
  return(states)
  
}
