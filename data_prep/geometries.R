library(tidyUSDA)
library(sf)
library(dplyr)

## state geometries
## doesn't matter what data we get - we'll get rid of everything but geos
d <- tidyUSDA::getQuickstat(
  domain='TOTAL',
  key = key <- Sys.getenv("AG_CENSUS_API_KEY"),
  program = 'CENSUS',
  data_item = 'CROP TOTALS - OPERATIONS WITH SALES',
  geographic_level = 'STATE',
  year = '2017',
  state = NULL,
  geometry = TRUE,
  lower48 = FALSE, 
  weighted_by_area = T)

state_geos <- d %>%
  mutate(county_name = "All") %>%
  select(state_name, county_name)

#saveRDS(state_geos, 'data/finalized/state_geometries.rds')


## county geometries
#state_geos <- readRDS('data/finalized/state_geometries.rds')
state_list <- state_geos %>%
  st_drop_geometry() %>%
  pull(state_name)

state_dfs <- list()

for (i in 1:length(state_list)) {
  print(paste0('Getting geometry data for ', state_list[i]))
  one_state <- tidyUSDA::getQuickstat(
    domain='TOTAL',
    county=NULL,
    key = key <- Sys.getenv("AG_CENSUS_API_KEY"),
    program = 'CENSUS',
    data_item = 'CROP TOTALS - OPERATIONS WITH SALES',
    geographic_level = 'COUNTY',
    year = '2017',
    state = state_list[i],
    geometry = TRUE,
    lower48 = FALSE, 
    weighted_by_area = T) %>%
    select(state_name, county_name)
  
  state_dfs[[i]] <- one_state
  
}

county_geos <- bind_rows(state_dfs)

ALL_GEOS <- bind_rows(state_geos, county_geos) %>%
  st_transform(4326)

st_write(ALL_GEOS, 'data/finalized/ALL_GEOS.shp')

