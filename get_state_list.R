library(DBI)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(odbc)
library(RODBC)


get_state_list <- function() {
  
  #print(paste0('R_CONFIG_ACTIVE=', Sys.getenv('R_CONFIG_ACTIVE')))
  #print(paste("DataSources:", odbc::odbcListDataSources()))
  #print(paste("odbcDrivers:", odbc::odbcListDrivers()))
  
  #if (Sys.getenv('R_CONFIG_ACTIVE') != 'shinyapps'){
  mydb <- dbConnect(RSQLite::SQLite(), dbname = "census-db.sqlite")
  # } else {
  #   mydb <- odbcDriverConnect("DRIVER=libsqliteodbc:amd64;Database=census-db.sqlite;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;")
  # }
  
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
