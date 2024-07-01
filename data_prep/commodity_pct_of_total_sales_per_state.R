library(tidyUSDA)
library(tidyverse)

ag_census_api_key <- Sys.getenv("AG_CENSUS_API_KEY")

# Retrieving sales for commodities in CROPS and ANIMALS & PRODUCTS
sectors <- c("CROPS", "ANIMALS & PRODUCTS")
commodities_sales <- tidyUSDA::getQuickstat(
  #sector = NULL,
  category='SALES',
  domain='TOTAL',
  key = ag_census_api_key,
  program = 'CENSUS',
  geographic_level = 'STATE',
  year = c('2022', '2017'),
  geometry = FALSE,
  lower48 = FALSE, 
  weighted_by_area = FALSE) %>%
  filter(
    grepl("SALES, MEASURED IN", short_desc),
    sector_desc %in% sectors,
    unit_desc == "$"
  ) %>%
  select(year, state_name, group_desc, commodity_desc, short_desc, Value)

# Retrieving missing AQUACULTURE commodities from a separate category
  # note: in a separate pull as data exceeds API limit if category is not specified
commodities_distr <- tidyUSDA::getQuickstat(
  #sector = NULL,
  category='SALES & DISTRIBUTION',
  domain='TOTAL',
  key = ag_census_api_key,
  program = 'CENSUS',
  geographic_level = 'STATE',
  year = c('2022', '2017'),
  geometry = FALSE,
  lower48 = FALSE, 
  weighted_by_area = FALSE) %>%
  filter(
    grepl("SALES & DISTRIBUTION, MEASURED IN", short_desc),
    sector_desc %in% sectors,
    unit_desc == "$"
  ) %>%
  select(year, state_name, group_desc, commodity_desc, short_desc, Value)

# Combine all commodities into 1 dataset
commodities <- rbind(commodities_sales, commodities_distr)


# Retrieve commodity totals per state via query
state_totals <-  tidyUSDA::getQuickstat(
  group = 'INCOME',
  commodity = 'COMMODITY TOTALS',
  data_item = 'COMMODITY TOTALS - SALES, MEASURED IN $',
  category='SALES',
  domain='TOTAL',
  key = ag_census_api_key,
  program = 'CENSUS',
  geographic_level = 'STATE',
  year = c('2017','2022'),
  geometry = FALSE,
  lower48 = FALSE, 
  weighted_by_area = FALSE) %>%
  select(year, state_name, state_total = Value) 

# Merge individual and total commodity sales and pivot by year
  # note: NAs occur when information is unavailable for a commodity
commodities_by_state <- commodities %>% 
  left_join(state_totals, by = c("state_name", "year")) %>% 
  pivot_wider(names_from = year, values_from = c(Value, state_total)) %>% 
  mutate(pct_of_total_2022 = round(Value_2022/state_total_2022 * 100, 4),
         pct_of_total_2017 = round(Value_2017/state_total_2017 * 100, 4),
         change = Value_2022 - Value_2017,
         pct_change = round(change / Value_2017 * 100, 4)) %>% 
  arrange(state_name, desc(pct_of_total_2022))

#write.csv(commodities_by_state, 'data/commodity_pct_by_state')

# ------------------------------------------------------------------------------
# Removing the double-counted totals
  # reference: https://www.nass.usda.gov/Publications/AgCensus/2022/Full_Report/Volume_1,_Chapter_2_US_State_Level/st99_2_002_002.pdf
reduced_commodities_by_state <- commodities_by_state %>% 
  filter(!grepl("TOTALS", group_desc),
         !(commodity_desc %in% c("CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS",
                                 "SHEEP & GOAT TOTALS",
                                 "HORTICULTURE TOTALS",
                                 "AQUACULTURE TOTALS")),
         !(short_desc %in% c("GRAIN - SALES, MEASURED IN $",
                             "EQUINE, (HORSES & PONIES) & (MULES & BURROS & DONKEYS) - SALES, MEASURED IN $",
                             "FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $"))
         )

#write.csv(reduced_commodities_by_state, 'data/commodity_pct_by_state_reduced')