library(DBI)
library(dplyr)
library(dbplyr)
library(sf)

# create empty databse
mydb <- dbConnect(RSQLite::SQLite(), "census-db.sqlite")

# load geom data to add to db
ALL_GEOMS <- st_read('data/finalized/ALL_GEOS.shp') %>%
  rename(
    state_name = stat_nm,
    county_name = cnty_nm
  )

# write it to the db
sf::st_write(
  st_as_sf(ALL_GEOMS %>% sf::st_set_crs(4326)), 
  layer = "all_geoms",
  dsn = mydb, 
  driver = "SQLite",
  dataset_options=c("SPATIALITE=YES"),
  delete_dsn = FALSE
)

# check it out
RSQLite::dbListTables(mydb)

# now add tabular data
ALL_DATA <- readRDS('data/finalized/ALL_DATA.rds')
dbWriteTable(mydb, "all_data", ALL_DATA, append = FALSE)

# check it out again
RSQLite::dbListTables(mydb)

## add indexes to state_name and county_name for efficiency
index_statement <- 
"
CREATE INDEX state_idx 
ON all_data (state_name);
"
dbExecute(mydb, index_statement)

index_statement <- 
"
CREATE INDEX county_idx 
ON all_data (county_name);
"
dbExecute(mydb, index_statement)

index_statement <- 
"
CREATE INDEX state_g_idx 
ON all_geoms (state_name);
"
dbExecute(mydb, index_statement)

index_statement <- 
  "
CREATE INDEX county_g_idx 
ON all_geoms (county_name);
"
dbExecute(mydb, index_statement)

geoms_table <- tbl(mydb, "all_geoms")
data_table <- tbl(mydb, "all_data")

joined <- inner_join(
  geoms_table,
  data_table %>%
    filter(
      state_name == 'KENTUCKY',
      county_name != 'All',
      short_desc == "SOYBEANS - SALES, MEASURED IN $",
      farm_size == "All"
    ),
  by = c('state_name', 'county_name')
)

results <- joined %>% 
  collect()

results_sf <- results %>%
  mutate(geometry = st_as_sfc(structure(as.character(geometry), class =
                                "WKB"), EWKB=TRUE)) %>%
  st_as_sf()

plot(results_sf$geometry)

dbDisconnect(mydb)
  