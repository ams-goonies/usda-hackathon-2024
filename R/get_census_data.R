library(dplyr)
library(sf)


get_census_data <- function(state = NULL, county = NULL, desc = NULL, size = NULL) {
  
  if (state != 'ALL STATES') {
    geoms_table <- GEOS %>% 
      filter(state_name == state)
  } else {
    geoms_table <- GEOS
  }
  
  joined <- geoms_table %>%
    merge(DATA, 
          by = c('state_name', 'county_name'))
  
  if (state == "ALL STATES") {
    joined <- joined %>% filter(county_name == 'All')
  } else if (state != 'ALL STATES' & !is.null(state)) {
    joined <- joined %>% filter(state_name == state)
  } else {
    joined <- joined %>% filter(county_name != "All")
  }
  
  if (!is.null(county)) {
    joined <- joined %>% filter(county_name == county)
  }
  
  if (!is.null(desc)) {
    joined <- joined %>% filter(short_desc == desc)
  }
  
  if (!is.null(county)) {
    joined <- joined %>% filter(county_name == county)
  }
  
  if (!is.null(size)) {
    joined <- joined %>% filter(farm_size == size)
  } else {
    joined <- joined %>% filter(farm_size == 'All')
    
  }
  
  return(joined)
  
}