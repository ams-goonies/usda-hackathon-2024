library(dplyr)
library(sf)
# 
# source('get_census_data2.R')
# source('get_metrics_list2.R')
# source('get_state_list2.R')

sf_use_s2(FALSE)

get_metrics_list <- function(state = NULL, county = NULL, size = 'All') {
  
  m <- readRDS('data/finalized/ALL_DATA.rds') %>%
    filter(
      case_when(state == 'ALL STATES' ~ county_name == 'All', T ~ !is.na(state_name)),
      case_when(!is.null(state) & state != 'ALL STATES' ~ state_name == state, T ~ !is.na(state_name)),
      #case_when(!is.null(county) ~ county_name == county, T ~ !is.na(state_name))#,
      case_when(!is.null(size) ~ farm_size == size, T ~ !is.na(state_name))
    ) %>%
    select(short_desc) %>%
    distinct() %>%
    pull(short_desc)
  
  
  return(m)
  
}


get_state_list <- function() {
  
  readRDS('data/finalized/ALL_DATA.rds') %>%
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
    merge(readRDS('data/finalized/ALL_DATA.rds'), 
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

popup_creator <- function(d, state){
  return(
    ifelse(
      state == "ALL STATES",
      glue(
        "<b>{d$state_name}</b><br/>",
        "2017 value: {format(d$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
        "2022 value: {prettyNum(d$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
        "Change: {d$change_pct}%"),
      glue(
        "<b>{d$county_name}, {d$state_name}</b><br/>",
        "2017 value: {format(d$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
        "2022 value: {prettyNum(d$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
        "Change: {d$change_pct}%")
    )
  )
}


ALL_STATES <- get_state_list()





