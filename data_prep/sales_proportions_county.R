library(tidyUSDA)
library(tidyverse)
source('data_prep/helper_functions.R')


# Retrieving sales for commodities in CROPS and ANIMALS & PRODUCTS
get_commodity_sales_proportions <- function(geographic_level='STATE',
                                            state=NULL, 
                                            county=NULL) {
 
  sectors <- c("CROPS", "ANIMALS & PRODUCTS")
  commodities_sales <- tidyUSDA::getQuickstat(
    category='SALES',
    state = state,
    county = county,
    key = Sys.getenv("AG_CENSUS_API_KEY"),
    program = 'CENSUS',
    geographic_level = geographic_level,
    year = c('2022', '2017')) %>%
    filter(
      unit_desc == "$",
      sector_desc %in% sectors
      )

  if (geographic_level == 'STATE'){
    commodities_sales <- commodities_sales %>%
      mutate(county_name= "All") %>%
      filter(
        grepl("SALES, MEASURED IN", short_desc),
        grepl('FARM SALES:', domaincat_desc),
        !grepl('AND SALES:', domaincat_desc),
      ) %>%
      categorize_by_size()
  } else if (geographic_level == 'COUNTY') {
    commodities_sales$farm_size = 'All'
  }
  
  commodities_sales <- commodities_sales %>%
    group_by(state_name, county_name, short_desc, group_desc, commodity_desc,
             farm_size, year) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    select(year, state_name, county_name, group_desc, commodity_desc, 
           short_desc, Value, farm_size)
  
  sales_totals <- commodities_sales %>%
    group_by(year, state_name, county_name, group_desc, commodity_desc, short_desc) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(farm_size = "All")
  
  commodities_sales <- bind_rows(commodities_sales, sales_totals) %>%
    distinct()
  
  # Retrieving missing AQUACULTURE commodities from a separate category
  # note: in a separate pull as data exceeds API limit if category is not specified
  commodities_distr <- tidyUSDA::getQuickstat(
    category='SALES & DISTRIBUTION',
    state = state,
    county = county,
    key = Sys.getenv("AG_CENSUS_API_KEY"),
    program = 'CENSUS',
    geographic_level = geographic_level,
    year = c('2022', '2017')) %>%
    filter(
      grepl("SALES & DISTRIBUTION, MEASURED IN", short_desc),
      !grepl('AND SALES &', domaincat_desc),
      unit_desc == "$"
    )
    
  if (geographic_level == 'STATE'){
    commodities_distr <- commodities_distr %>%
      mutate(county_name= "All") %>%
      filter(
        #grepl("SALES, MEASURED IN", short_desc),
        grepl('FARM SALES:', domaincat_desc),
        !grepl('AND SALES:', domaincat_desc),
      ) %>%
      categorize_by_size()
  } else if (geographic_level == 'COUNTY') {
    commodities_distr$farm_size = 'All'
  }
  
  commodities_distr <- commodities_distr %>%
    group_by(state_name, county_name, short_desc, group_desc, commodity_desc,
             farm_size, year) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    select(year, state_name, county_name, group_desc, commodity_desc, 
           short_desc, Value, farm_size)
  
  distr_totals <- commodities_distr %>%
    group_by(year, state_name, county_name, group_desc, commodity_desc, short_desc) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(farm_size = "All")
  
  commodities_distr <- bind_rows(commodities_distr, distr_totals)
  
  # Combine all commodities into 1 dataset
  commodities <- rbind(commodities_sales, commodities_distr)
  
  # Retrieve commodity totals
  geo_totals <-  tidyUSDA::getQuickstat(
    group = 'INCOME',
    commodity = 'COMMODITY TOTALS',
    data_item = 'COMMODITY TOTALS - SALES, MEASURED IN $',
    category='SALES',
    #domain='TOTAL',
    key = Sys.getenv("AG_CENSUS_API_KEY"),
    program = 'CENSUS',
    geographic_level = geographic_level,
    state = state,
    county = county,
    year = c('2017','2022'))
  
  geo_totals <- geo_totals %>%
    filter(
    grepl("SALES, MEASURED IN", short_desc),
    grepl('FARM SALES:', domaincat_desc),
    !grepl('AND SALES:', domaincat_desc),
  )
  
  if (geographic_level == 'STATE'){
    geo_totals <- geo_totals %>%
      mutate(
        county_name = "All"
      )
  }
  
  geo_totals <- geo_totals %>%
    filter(
      grepl("SALES, MEASURED IN", short_desc),
      grepl('FARM SALES:', domaincat_desc),
      !grepl('AND SALES:', domaincat_desc),
    ) %>%
    categorize_by_size() %>%
    group_by(year, state_name, county_name, farm_size) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop')
  
  all_sizes <- geo_totals %>%      
    mutate(farm_size = "All") %>%
    group_by(year, state_name, county_name, farm_size) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop')
  
  geo_totals <- bind_rows(geo_totals, all_sizes)
  
  geo_totals <- geo_totals %>%
    select(year, state_name, county_name, farm_size, total = Value) %>%
    arrange(state_name, county_name, farm_size)
  
  # Merge individual and total commodity sales and pivot by year
  # note: NAs occur when information is unavailable for a commodity
  commodities_by_geo <- commodities %>% 
    left_join(geo_totals, by = c("state_name", "county_name", "year", "farm_size")) %>%
    distinct() %>%
    #drop_na(Value) %>%
    mutate(proportion = round(Value/total*100, 2)) %>%
    # group_by(year) %>%
    # mutate(row = row_number()) %>%
    pivot_wider(names_from = year, values_from = c(Value, total, proportion)) %>% 
    #select(-row) %>%
    mutate(
      metric = 'percent of total',
      #Value_2022 = round(Value_2022/total_2022 * 100, 2),
      #Value_2017 = round(Value_2017/total_2017 * 100, 2),
      change = proportion_2022 - proportion_2017,
      change_pct = ifelse((proportion_2017 != 0 & proportion_2022 != 0),
                          round(change / proportion_2017 * 100, 2), NA
                          )
      ) %>% 
    arrange(state_name, desc(proportion_2022))
  
  # ------------------------------------------------------------------------------
  # Removing the double-counted totals
  # reference: https://www.nass.usda.gov/Publications/AgCensus/2022/Full_Report/Volume_1,_Chapter_2_US_State_Level/st99_2_002_002.pdf
  reduced_commodities_by_geo <- commodities_by_geo %>% 
    filter(!grepl("TOTALS", group_desc),
           !(commodity_desc %in% c("CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS",
                                   "SHEEP & GOATS TOTALS",
                                   "HORTICULTURE TOTALS",
                                   "AQUACULTURE TOTALS")),
           !(short_desc %in% c("GRAIN, OTHER - SALES, MEASURED IN $",
                               "EQUINE, (HORSES & PONIES) & (MULES & BURROS & DONKEYS) - SALES, MEASURED IN $",
                               'FRUIT & TREE NUT TOTALS, (EXCL BERRIES) - SALES, MEASURED IN $'
                               ))
    )
  
  
  df <- reduced_commodities_by_geo %>%
    select(state_name, county_name, short_desc, farm_size, metric, 
           proportion_2017, proportion_2022, change, change_pct) %>%
    rename(
      Value_2017 = proportion_2017,
      Value_2022 = proportion_2022
    )
  
  return(df)

}



###########################################
# get state level data
###########################################
state_list <- readRDS('data/ready_for_app/state_sales_totals.rds') %>%
  pull(state_name)

state_dfs <- list()

for (i in 1:length(state_list)) {
  print(paste0("Getting commodity sales proportions data for ", state_list[i]))
  d <- get_commodity_sales_proportions(state = state_list[i])
  state_dfs[[i]] <- d
}

commodity_proportions <- bind_rows(state_dfs)

commodity_proportions <- commodity_proportions %>%
  mutate(
    pct_change = ifelse(change == 0, 0, 
                        ifelse(Value_2017 == 0, NA, change_pct))
  ) %>%
  select(-pct_change) %>%
  distinct()

## fill in all of the missing parts
unique_crops <- unique(commodity_proportions$short_desc)
unique_states <- unique(commodity_proportions$state_name)
unique_sizes <- unique(commodity_proportions$farm_size)

states <- rep(unique_states, length(unique_crops) * length(unique_sizes))
crops <- rep(unique_crops, length(unique_states) * length(unique_sizes))
sizes <- rep(unique_sizes, length(unique_states) * length(unique_crops))

all_combos <- data.frame(state_name = states) %>%
  arrange(state_name) %>%
  mutate(
    county_name = 'All',
    short_desc = crops
    ) %>%
  arrange(state_name, short_desc) %>%
  mutate(
    farm_size = sizes,
    metric = 'percent of total',
    Value_2017 = NA,
    Value_2022 = NA,
    change = NA,
    change_pct = NA
    )

commodity_proportions_full <- bind_rows(commodity_proportions, all_combos) %>%
  group_by(state_name, county_name, short_desc, farm_size, metric) %>%
  summarize(
    Value_2017 = sum(Value_2017, na.rm = TRUE),
    Value_2022 = sum(Value_2022, na.rm = TRUE),
    change = sum(change, na.rm = TRUE),
    change_pct = sum(change_pct, na.rm = TRUE),
    .groups = 'drop',
    across(c(Value_2017, Value_2022, change, change_pct), ~ na_if(., 0))
  )

saveRDS(commodity_proportions_full, 'data/ready_for_app/state_sales_proportions.rds')


###########################################
# get county level data
###########################################
state_list <- readRDS('data/finalized/state_sales_totals.rds') %>%
  select(state_name) %>%
  distinct() %>%
  pull(state_name)

#state_list <- state_list[12:13]

full_state_dfs <- list()

for (i in 1:length(state_list)) {
  #items_dfs <- list()
  if (file.exists('data/ready_for_app/county_sales_proportions.rds')) {
    existing <- readRDS('data/ready_for_app/county_sales_proportions.rds')
  } else {
    existing <- data.frame(
      state_name = c('nothing man')
    )
  }
  if (state_list[i] %in% existing$state_name) {
    print(paste0("Skipping ", state_list[i]))
    next
  }
  #for (j in 1:length(items)){
  print(paste0("Getting county-level sales proportions data for ", state_list[i]))
  d <- get_commodity_sales_proportions(geographic_level = "COUNTY",
                            state = state_list[i])
  #items_dfs[[j]] <- d
  #}
  full_state_dfs[[i]] <- d #bind_rows(items_dfs)
  county_proportions <- bind_rows(full_state_dfs)
  
  if (file.exists('data/ready_for_app/county_sales_proportions.rds')) {
    all <- bind_rows(county_proportions, existing) %>%
      distinct()
    saveRDS(all, 'data/ready_for_app/county_sales_proportions.rds')
  } else {
    saveRDS(county_proportions, 'data/ready_for_app/county_sales_proportions.rds')
  }
  
}
        