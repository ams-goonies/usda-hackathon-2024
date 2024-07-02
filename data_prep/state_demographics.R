library(tidyUSDA)
library(dplyr)

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

small <- c(
  "FARM SALES: (LESS THAN 1,000 $)",
  "FARM SALES: (1,000 TO 2,499 $)",
  "FARM SALES: (2,500 TO 4,999 $)",
  "FARM SALES: (5,000 TO 9,999 $)",
  "FARM SALES: (10,000 TO 24,999 $)",
  "FARM SALES: (25,000 TO 49,999 $)",
  "FARM SALES: (50,000 TO 99,999 $)",
  "FARM SALES: (100,000 TO 249,999 $)"
)

not_small <- c(
  "FARM SALES: (250,000 TO 499,999 $)",
  "FARM SALES: (500,000 TO 999,999 $)",
  "FARM SALES: (1,000,000 OR MORE $)"
  )
  
get_demographic_data <- function(item, geographic_level = "STATE", state = NULL){
  demo_states <- tidyUSDA::getQuickstat(
    sector = 'DEMOGRAPHICS',
    group = 'PRODUCERS',
    key = key,
    program = 'CENSUS',
    geographic_level = geographic_level,
    data_item = item,
    year =  c('2017', '2022'),
    state = state,
    geometry = FALSE) %>%
    dplyr::filter(
      group_desc %in% groups,
      grepl('FARM SALES', domaincat_desc)
    ) %>%
    mutate(
      county_name = "ALL",
      year = as.character(year),
      small_farm = ifelse(domaincat_desc %in% small, TRUE, FALSE)
      ) %>%
    select(
      state_name, county_name, short_desc, small_farm, year, Value
    ) %>%
    pivot_wider(
      names_prefix = 'Value_',
      names_from = year, 
      values_from = Value
    )
  
  return(demo_states)
}


# get state level data
items_dfs <- list()

for (i in 1:length(items)){
  print(paste0("Getting '", items[i], "' data for all states."))
  d <- get_demographic_data(item = items[i])
  items_dfs[[i]] <- d
}

state_demographics <- bind_rows(items_dfs)
saveRDS(state_demographics, 'data/ready_for_app/state_demographics.rds')


# Get county level data
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



