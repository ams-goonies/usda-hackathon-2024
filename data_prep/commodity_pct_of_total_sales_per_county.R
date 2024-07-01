library(tidyUSDA)
library(tidyverse)

ag_census_api_key <- Sys.getenv("AG_CENSUS_API_KEY")

# Retrieving sales for commodities in CROPS and ANIMALS & PRODUCTS
sectors <- c("CROPS", "ANIMALS & PRODUCTS")

get_commodity_sales <- function(
    apiKey=Sys.getenv("AG_CENSUS_API_KEY"),
    years=c('2017', '2022'),
    state=NULL
) {
  commodities_sales <- tidyUSDA::getQuickstat(
    category='SALES',
    domain='TOTAL',
    key = apiKey,
    program = 'CENSUS',
    geographic_level = 'COUNTY',
    year = years,
    state = state,
    geometry = FALSE,
    lower48 = FALSE,
    weighted_by_area = FALSE) %>%
    filter(
      grepl("SALES, MEASURED IN", short_desc),
      sector_desc %in% sectors,
      unit_desc == "$"
    ) %>%
    select(year, state_name, county_name, group_desc, commodity_desc, short_desc, Value)
  
  # Retrieving missing AQUACULTURE commodities from a separate category
  # note: in a separate pull as data exceeds API limit if category is not specified
  commodities_distr <- tidyUSDA::getQuickstat(
    category='SALES & DISTRIBUTION',
    domain='TOTAL',
    key = apiKey,
    program = 'CENSUS',
    geographic_level = 'COUNTY',
    year = years,
    state = state,
    geometry = FALSE,
    lower48 = FALSE, 
    weighted_by_area = FALSE) %>%
    filter(
      grepl("SALES & DISTRIBUTION, MEASURED IN", short_desc),
      sector_desc %in% sectors,
      unit_desc == "$"
    ) %>%
    select(year, state_name, county_name, group_desc, commodity_desc, short_desc, Value)
  
  # Combine all commodities into 1 dataset
  commodities <- rbind(commodities_sales, commodities_distr)
  
  # Retrieve commodity totals per state via query
  county_totals <-  tidyUSDA::getQuickstat(
    group = 'INCOME',
    commodity = 'COMMODITY TOTALS',
    data_item = 'COMMODITY TOTALS - SALES, MEASURED IN $',
    category='SALES',
    domain='TOTAL',
    key = apiKey,
    program = 'CENSUS',
    geographic_level = 'COUNTY',
    year = years,
    state = state,
    geometry = FALSE,
    lower48 = FALSE, 
    weighted_by_area = FALSE) %>%
    select(year, state_name, county_name, county_total = Value) 
  
  # Merge individual and total commodity sales and pivot by year
  # note: NAs occur when information is unavailable for a commodity
  commodities_by_county <- commodities %>% 
    left_join(county_totals, by = c("state_name", "county_name", "year")) %>% 
    pivot_wider(names_from = year, values_from = c(Value, county_total)) %>% 
    mutate(pct_of_total_2022 = round(Value_2022/county_total_2022 * 100, 2),
           pct_of_total_2017 = round(Value_2017/county_total_2017 * 100, 2),
           change = Value_2022 - Value_2017,
           change_pct = round(change / Value_2017 * 100, 2)) %>% 
    arrange(state_name, desc(pct_of_total_2022))
  
  # ------------------------------------------------------------------------------
  # Removing the double-counted totals
  # reference: https://www.nass.usda.gov/Publications/AgCensus/2022/Full_Report/Volume_1,_Chapter_2_US_State_Level/st99_2_002_002.pdf
  reduced_commodities_by_county <- commodities_by_county %>% 
    filter(!grepl("TOTALS", group_desc),
           !(commodity_desc %in% c("CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS",
                                   "SHEEP & GOATS TOTALS",
                                   "HORTICULTURE TOTALS",
                                   "AQUACULTURE TOTALS")),
           !(short_desc %in% c("GRAIN - SALES, MEASURED IN $",
                               "EQUINE, (HORSES & PONIES) & (MULES & BURROS & DONKEYS) - SALES, MEASURED IN $",
                               "FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $"))
    )
  
  county_commodity_sales <- reduced_commodities_by_county %>%
    #mutate(county_name = "ALL") %>%
    select(state_name, county_name, short_desc, Value_2017, Value_2022, change, change_pct)
  
  return(county_commodity_sales)
}


state_list <- readRDS('data/ready_for_app/state_sales_totals.rds') %>%
  pull(state_name)

# empty list to append state/county dataframes to
county_dfs <- list()

for (i in 1:length(state_list)) {
  print(paste0("Getting data for ", state_list[i]))
  d <- get_commodity_sales(state = state_list[i])
  county_dfs[[i]] <- d
}

county_commodity_sales <- bind_rows(county_dfs)

saveRDS(county_commodity_sales, 'data/ready_for_app/county_sales_bycommodity.rds')


#write.csv(reduced_commodities_by_state, 'data/commodity_pct_by_state_reduced.csv')
