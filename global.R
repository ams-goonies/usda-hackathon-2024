library(dplyr)
library(sf)
# 
# source('get_census_data2.R')
# source('get_metrics_list2.R')
# source('get_state_list2.R')

# Sys.setenv("LARES_FONT" = "ignore")
# library(sysfonts)
# sysfonts::font_add(family = 'Arial Narrow', regular = 'arialnarrow.ttf')
# library(systemfonts)
# systemfonts::register_font("Arial Narrow", plain = 'arialnarrow.ttf')

sf_use_s2(FALSE)

get_metrics_list <- function(state = NULL, size = 'All', type = "Total sales, $") {
  
  m <- readRDS('data/finalized/ALL_DATA2.rds') %>%
    filter(
      case_when(state == 'ALL STATES' ~ county_name == 'All', T ~ !is.na(state_name)),
      case_when(!is.null(state) & state != 'ALL STATES' ~ state_name == state, T ~ !is.na(state_name)),
      case_when(!is.null(size) ~ farm_size == size, T ~ !is.na(state_name)),
      metric == type
    ) %>%
    select(short_desc) %>%
    distinct() %>%
    pull(short_desc)
  
  
  return(m)
  
}


get_state_list <- function() {
  
  readRDS('data/finalized/ALL_DATA2.rds') %>%
    select(state_name) %>%
    distinct() %>%
    pull(state_name)
  
}


get_census_data <- function(state = NULL, county = NULL, desc = NULL, size = NULL) {
  
  if (state != 'ALL STATES') {
    geoms_table <- read_sf('data/finalized/all_geos.gpkg') %>% 
      filter(state_name == state)
  } else {
    geoms_table <- read_sf('data/finalized/all_geos.gpkg')
  }
  
  joined <- geoms_table %>%
    merge(readRDS('data/finalized/ALL_DATA2.rds'), 
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


ALL_STATES <- get_state_list()





