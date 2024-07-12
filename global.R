library(dplyr)
library(sf)
# 
source('R/get_census_data.R')
source('R/get_metrics_list.R')
source('R/get_state_list.R')

sf_use_s2(FALSE)

DATA <- readRDS('data/finalized/ALL_DATA.rds')
D_INIT <- readRDS('data/finalized/d_init.rds')
GEOS <- read_sf('data/finalized/all_geos.gpkg')

ALL_STATES <- c("ARKANSAS", "SOUTH DAKOTA") #get_state_list()





