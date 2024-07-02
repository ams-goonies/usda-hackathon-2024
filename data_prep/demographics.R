library(tidyUSDA)
library(dplyr)
library(tidyr)
source('data_prep/helper_functions.R')

key <- Sys.getenv("AG_CENSUS_API_KEY")

items <- c('PRODUCERS, (ALL), FEMALE - NUMBER OF PRODUCERS',
           'PRODUCERS, (ALL), MALE - NUMBER OF PRODUCERS',
           'PRODUCERS, AGE 25 TO 34 - NUMBER OF PRODUCERS',
           'PRODUCERS, AGE 35 TO 44 - NUMBER OF PRODUCERS',
           'PRODUCERS, AGE 45 TO 54 - NUMBER OF PRODUCERS',
           'PRODUCERS, AGE 55 TO 64 - NUMBER OF PRODUCERS',
           'PRODUCERS, AGE 65 TO 74 - NUMBER OF PRODUCERS',
           'PRODUCERS, AGE GE 75 - NUMBER OF PRODUCERS',
           'PRODUCERS, AGE LT 25 - NUMBER OF PRODUCERS',
           'PRODUCERS, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF PRODUCERS',
           'PRODUCERS, ASIAN - NUMBER OF PRODUCERS',
           'PRODUCERS, BLACK OR AFRICAN AMERICAN - NUMBER OF PRODUCERS',
           'PRODUCERS, HISPANIC - NUMBER OF PRODUCERS',
           'PRODUCERS, MULTI-RACE - NUMBER OF PRODUCERS',
           'PRODUCERS, WHITE - NUMBER OF PRODUCERS',
           'PRODUCERS, MILITARY SERVICE, ACTIVE DUTY NOW OR IN THE PAST - NUMBER OF PRODUCERS',
           'PRODUCERS, MILITARY SERVICE, NEVER SERVED OR ONLY ON ACTIVE DUTY FOR TRAINING IN RESERVES OR NATIONAL GUARD - NUMBER OF PRODUCERS'
           )

groups <- c('OPERATORS', 'PRODUCERS')
  
get_demographic_data <- function(item, geographic_level = "STATE", state = NULL){
  demos <- tidyUSDA::getQuickstat(
    sector = 'DEMOGRAPHICS',
    key = key,
    program = 'CENSUS',
    geographic_level = geographic_level,
    data_item = item,
    year =  c('2017', '2022'),
    state = state) %>%
    filter(group_desc %in% groups)
                  
  if (length(demos$domaincat_desc[grepl("FARM SALES", demos$domaincat_desc)]) > 0) {
    demos <- demos %>% 
      filter(grepl('FARM SALES', domaincat_desc))
  } else {
      demos <- demos %>% mutate(farm_size = 'All')
      }
  
  if (geographic_level == 'STATE') {
    demos <- demos %>%
      mutate(county_name = "ALL"        )
    demos <- categorize_by_size(demos)
  }
  
  demos <- demos %>%
    select(
      state_name, county_name, short_desc, farm_size, year, Value
    ) %>%
    group_by(state_name, county_name, short_desc, farm_size, year) %>%
    summarize(Value = sum(Value), .groups = 'drop')

  totals <- demos %>%
    group_by(state_name, county_name, short_desc, year) %>%
    summarize(Value = sum(Value), .groups = 'drop') %>%
    mutate(farm_size = "All")
  
  demos <- bind_rows(demos, totals)
  
  demos <- demos %>%
    group_by(year) %>%
    mutate(row = row_number()) %>%
    pivot_wider(
      names_prefix = 'Value_',
      names_from = year, 
      values_from = Value
    ) %>%
    select(-row) %>%
    mutate(
      change = Value_2022 - Value_2017,
      change_pct = round(change / Value_2017 * 100, 2)) %>% 
    arrange(state_name, desc(farm_size))
  
  return(demos)
}

##################################
# get state level data
##################################
items_dfs <- list()

for (i in 1:length(items)){
  print(paste0("Getting '", items[i], "' data for all states."))
  d <- get_demographic_data(item = items[i])
  items_dfs[[i]] <- d
}

state_demographics <- bind_rows(items_dfs)
saveRDS(state_demographics, 'data/ready_for_app/state_demographics.rds')


###################################
# Get county level data
###################################
state_list <- readRDS('data/ready_for_app/state_sales_totals.rds') %>%
  pull(state_name)

# get state level data
full_state_dfs <- list()

for (i in 1:length(state_list)) {
  items_dfs <- list()
  for (j in 1:length(items)){
    print(paste0("Getting '", items[j], "' data for ", state_list[i]))
    d <- get_demographic_data(item = items[j], geographic_level = "COUNTY",
                              state = state_list[i])
    items_dfs[[j]] <- d
  }
  full_state_dfs[[i]] <- bind_rows(items_dfs)
}

county_demographics <- bind_rows(full_state_dfs)

saveRDS(county_demographics, 'data/ready_for_app/county_demographics.rds')



