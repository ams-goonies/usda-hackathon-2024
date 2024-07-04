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
      mutate(county_name= "ALL") %>%
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
    summarize(Value = sum(Value), .groups = 'drop') %>%
    select(year, state_name, county_name, group_desc, commodity_desc, 
           short_desc, Value, farm_size)
  
  sales_totals <- commodities_sales %>%
    group_by(year, state_name, county_name, group_desc, commodity_desc, short_desc) %>%
    summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(farm_size = "All")
  
  commodities_sales <- bind_rows(commodities_sales, sales_totals)
  
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
    ) %>%
    categorize_by_size()
    
  if (geographic_level == 'STATE'){
    commodities_distr <- commodities_distr %>%
      mutate(county_name= "ALL") %>%
      filter(
        grepl("SALES, MEASURED IN", short_desc),
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
    domain='TOTAL',
    key = Sys.getenv("AG_CENSUS_API_KEY"),
    program = 'CENSUS',
    geographic_level = geographic_level,
    state = state,
    county = county,
    year = c('2017','2022'))
  
  if (geographic_level == 'STATE'){
    geo_totals$county_name <- 'ALL'
    geo_totals <- categorize_by_size(geo_totals)
  } else if (geographic_level == 'COUNTY') {
    geo_totals$farm_size <- "All"
  }
    
  geo_totals <- geo_totals %>%
    select(year, state_name, county_name, farm_size, total = Value) 
  
  # Merge individual and total commodity sales and pivot by year
  # note: NAs occur when information is unavailable for a commodity
  commodities_by_geo <- commodities %>% 
    left_join(geo_totals, by = c("state_name", "county_name", "year", "farm_size")) %>%
    distinct() %>%
    drop_na(Value) %>%
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
      pct_change = round(change / proportion_2017 * 100, 2)) %>% 
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
           !(short_desc %in% c("GRAIN - SALES, MEASURED IN $",
                               "EQUINE, (HORSES & PONIES) & (MULES & BURROS & DONKEYS) - SALES, MEASURED IN $",
                               "FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $"))
    )
  
  
  df <- reduced_commodities_by_geo %>%
    select(state_name, county_name, short_desc, farm_size, metric, 
           proportion_2017, proportion_2022, change, pct_change) %>%
    rename(
      Value_2017 = proportion_2017,
      Value_2022 = proportion_2022
    )
  
  return(df)

}

state_list <- readRDS('data/ready_for_app/state_sales_totals.rds') %>%
  pull(state_name)

###########################################
# get state level data
###########################################
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
                        ifelse(Value_2017 == 0, NaN, pct_change))
  )

saveRDS(commodity_proportions, 'data/ready_for_app/state_sales_proportions.rds')


###########################################
# get county level data
###########################################
state_list <- readRDS('data/ready_for_app/state_sales_totals.rds') %>%
  pull(state_name)



state_dfs <- list()

for (i in 1:2) {
  state <- state_list[i]
  counties <- tigris::list_counties(state)

  print(paste0("Getting county-level commodity sales proportions data for ", state_list[i]))
  d <- get_commodity_sales_proportions(state = state_list[i], 
                                       geographic_level = 'COUNTY')
  state_dfs[[i]] <- d
}

        