library(DBI)
library(dplyr)
library(dbplyr)
library(sf)

get_data <- function(state = NULL, county = NULL, desc = NULL, size = NULL) {
  
  mydb <- dbConnect(RSQLite::SQLite(), "census-db.sqlite")
  
  geoms_table <- tbl(mydb, "all_geoms")
  data_table <- tbl(mydb, "all_data")
  
  joined <- inner_join(
    geoms_table,
    data_table %>%
      filter(
        case_when(!is.null(state) ~ state_name == state, T ~ !is.na(state_name)),
        case_when(!is.null(county) ~ county_name == county, T ~ !is.na(state_name)),
        case_when(!is.null(desc) ~ short_desc == desc, T ~ !is.na(state_name)),
        case_when(!is.null(size) ~ farm_size == size, T ~ !is.na(state_name))
      ),
    by = c('state_name', 'county_name')
  )
  
  results <- joined %>% 
    collect()
  
  results_sf <- results %>%
    mutate(geometry = st_as_sfc(
      structure(as.character(geometry), 
                class = "WKB"), EWKB=TRUE)) %>%
    st_as_sf() %>%
    select(-geometry, geometry)
  
  dbDisconnect(mydb)
  
  return(results_sf)
  
}
