
################################################################################
# This script is used to get demographic data at the state and county level,
# using the tidyUSDA package to hit the NASS API. Specific data queried were
# based on our focus on underserved communities.

# Authors: AMS Goonies Data Team
# Date: July 2024
################################################################################


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
      mutate(county_name = "All")
    demos <- categorize_by_size(demos)
  }
  
  demos <- demos %>%
    select(
      state_name, county_name, short_desc, farm_size, year, Value
    ) %>%
    group_by(state_name, county_name, short_desc, farm_size, year) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop')
  
  
  if (geographic_level == 'STATE') {
    totals <- demos %>%
      group_by(state_name, county_name, short_desc, year) %>%
      summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(farm_size = "All")
    
    demos <- bind_rows(demos, totals)
  }

  demos <- demos %>%
    group_by(year) %>%
    mutate(row = row_number()) %>%
    pivot_wider(
      names_prefix = 'Value_',
      names_from = year, 
      values_from = Value
    ) %>%
    select(-row)
  
  if (!'Value_2022' %in% unlist(colnames(demos))) {
    demos$Value_2022 <- NA
  } else if (!'Value_2017' %in% unlist(colnames(demos))) {
    demos$Value_2017 <- NA
  }
  
  demos <- demos %>%
    mutate(
      metric = "Number of producers",
      change = Value_2022 - Value_2017,
      change_pct = round(change / Value_2017 * 100, 2)) %>% 
    arrange(state_name, desc(farm_size)) %>%
    select(
      state_name, county_name, short_desc, farm_size, metric, 
      Value_2017, Value_2022, change, change_pct
    )
  
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

# fix something weird causing multiple incomplete rows for certain variables
state_demographics_fix <- state_demographics %>%
  group_by(state_name, county_name, short_desc, farm_size) %>%
  summarize(Value_2017 = sum(Value_2017, na.rm = TRUE),
            Value_2022 = sum(Value_2022, na.rm = TRUE),
            .groups = 'drop'
            ) %>%
  mutate(
    metric = "Number of producers",
    change = Value_2022 - Value_2017,
    change_pct = round(change / Value_2017 * 100, 2)) %>%
  select(
    state_name, county_name, short_desc, farm_size, metric, Value_2017,
    Value_2022, change, change_pct
  )
  arrange(state_name, desc(farm_size))
  
### fill in some missing rows with NAs
unique_cats <- unique(state_demographics_fix$short_desc)
unique_states <- unique(state_demographics_fix$state_name)
unique_sizes <- unique(state_demographics_fix$farm_size)

states <- rep(unique_states, length(unique_cats) * length(unique_sizes))
cats <- rep(unique_cats, length(unique_states) * length(unique_sizes))
sizes <- rep(unique_sizes, length(unique_states) * length(unique_cats))

all_combos <- data.frame(state_name = states) %>%
  arrange(state_name) %>%
  mutate(
    county_name = 'All',
    short_desc = cats
  ) %>%
  arrange(state_name, short_desc) %>%
  mutate(
    farm_size = sizes,
    metric = 'Number of producers',
    Value_2017 = NA,
    Value_2022 = NA,
    change = NA,
    change_pct = NA
  )

demographics_full <- bind_rows(state_demographics_fix, all_combos) %>%
  group_by(state_name, county_name, short_desc, farm_size, metric) %>%
  summarize(
    Value_2017 = sum(Value_2017, na.rm = TRUE),
    Value_2022 = sum(Value_2022, na.rm = TRUE),
    change = sum(change, na.rm = TRUE),
    change_pct = sum(change_pct, na.rm = TRUE),
    .groups = 'drop',
    across(c(Value_2017, Value_2022, change, change_pct), ~ na_if(., 0))
  )

saveRDS(demographics_full, 'data/ready_for_app/state_demographics.rds')


###################################
# Get county level data
###################################
state_list <- readRDS('data/finalized/state_sales_totals.rds') %>%
  select(state_name) %>%
  distinct() %>%
  pull(state_name)

#state_list <- state_list[12:13]

full_state_dfs <- list()

for (i in 1:length(state_list)) {
  items_dfs <- list()
  if (file.exists('data/ready_for_app/county_demographics.rds')) {
    existing <- readRDS('data/ready_for_app/county_demographics.rds')
  } else {
    existing <- data.frame(
      state_name = c('nothing man')
    )
  }
  if (state_list[i] %in% existing$state_name) {
    print(paste0("Skipping ", state_list[i]))
    next
  }
  for (j in 1:length(items)){
    print(paste0("Getting '", items[j], "' data for ", state_list[i]))
    d <- get_demographic_data(item = items[j], geographic_level = "COUNTY",
                              state = state_list[i])
    items_dfs[[j]] <- d
  }
  full_state_dfs[[i]] <- bind_rows(items_dfs)
  county_demographics <- bind_rows(full_state_dfs)
  
  if (file.exists('data/ready_for_app/county_demographics.rds')) {
    all <- bind_rows(county_demographics, existing) %>%
      distinct()
    saveRDS(all, 'data/ready_for_app/county_demographics.rds')
  } else {
    saveRDS(county_demographics, 'data/ready_for_app/county_demographics.rds')
  }
  
}

### fill in some missing rows with NAs
county_data <- readRDS('data/ready_for_app/county_demographics.rds')
states <- list()
all_counties <- list()

for (i in 1:length(unique(county_data$state_name))) {
  state <- unique(county_data$state_name)[i]
  counties_tigris <- toupper(tigris::list_counties(state)$county)
  counties_usda <- county_data %>%
    filter(state_name == state) %>%
    select(county_name) %>%
    distinct() %>%
    pull(county_name)
  counties_diff <- setdiff(counties_tigris, counties_usda)
  counties_full <- c(counties_usda, counties_diff)
  all_counties <- unlist(c(all_counties, counties_full))
  state_rep <- rep(state, length(counties_full))
  states <- unlist(c(states, state_rep))
}

unique_cats <- unique(county_data$short_desc)

states_full <- rep(states, length(unique_cats))
counties_full <- rep(all_counties, length(unique_cats))
cats <- rep(unique_cats, length(all_counties))

empties <- data.frame(
  state_name = states_full,
  county_name = counties_full) %>%
  arrange(state_name, county_name) %>%
  mutate(
    short_desc = cats,
    farm_size = "All",
    metric = "Number of producers",
    Value_2017 = NA,
    Value_2022 = NA,
    change = NA,
    change_pct = NA
  )

county_data_full <- bind_rows(county_data, empties) %>%
  group_by(state_name, county_name, short_desc, farm_size, metric) %>%
  summarize(
    Value_2017 = sum(Value_2017, na.rm = TRUE),
    Value_2022 = sum(Value_2022, na.rm = TRUE),
    change = sum(change, na.rm = TRUE),
    change_pct = sum(change_pct, na.rm = TRUE),
    across(c(Value_2017, Value_2022, change, change_pct), ~ na_if(., 0)),
    .groups = 'drop',
  )


saveRDS(county_demographics, 'data/ready_for_app/county_demographics.rds')



