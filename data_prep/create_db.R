library(DBI)
library(dbplyr)

census_db <- dbConnect(RSQLite::SQLite(), "census_data.db")

# load our data then create db table from it
ALL_DATA <- readRDS('data/finalized/ALL_DATA.rds')
dbWriteTable(census_db, "all_data", ALL_DATA, append = FALSE)

# test er out
all_data <- tbl(census_db, "all_data")

lazy_query <- all_data %>%
  filter(
    state_name == 'KENTUCKY',
    farm_size == "Small"
  )

data_i_want <- lazy_query %>% collect()
