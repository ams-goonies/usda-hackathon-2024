# Load all packages
library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(leaflet)
#library(tidyUSDA)
library(shinyWidgets)
library(sf)
library(DBI)
library(RSQLite)

sf_use_s2(FALSE)

CENSUS_DB <- dbConnect(RSQLite::SQLite(), "census_data.db")
ALL_DATA <- tbl(CENSUS_DB, "all_data")

ALL_GEOMS <- st_read('data/finalized/ALL_GEOS.shp') %>%
  rename(
    state_name = stat_nm,
    county_name = cnty_nm
  )

ALL_STATES <- ALL_DATA %>%
  select(state_name) %>%
  distinct() %>%
  pull(state_name)

STATE_METRICS <- ALL_DATA %>%
  filter(county_name == "All") %>%
  select(short_desc) %>%
  distinct() %>%
  pull(short_desc)

get_county_list <- function(state) {
  ALL_DATA %>% 
    filter(state_name == state) %>%
    select(county_name) %>%
    distinct() %>%
    pull(county_name)
}

# get_commodity_list <- function(state, county) {
#   test <- ALL_DATA %>%
#     filter(state_name == state,
#            county_name == county) %>%
#     select(short_desc)
#   
# }

get_metric_list <- function(state) {
  if (state != "All states") {
    ALL_DATA %>%
      filter(state_name == state) %>%
      select(short_desc) %>%
      distinct() %>%
      pull(short_desc)
  } else {
    ALL_DATA %>%
      filter(county_name == "All") %>%
      select(short_desc) %>%
      distinct() %>%
      pull(short_desc)
  }
  

}

# get_metric_list <- function(state, county, metrik) {
#   ALL_DATA %>% 
#     filter(state_name == state,
#            county_name == county,
#            short_desc == metrick) %>%
#     select(short_desc) %>%
#     distinct() %>%
#     pull(short_desc)
# }

# get_size_list <- function(state, county, metrick) {
#   ALL_DATA %>% 
#     filter(state_name == state,
#            county_name == county,
#            short_desc == metrick) %>%
#     select(farm_size) %>%
#     distinct() %>%
#     pull(farm_size)
# }


