library(tidyUSDA)
library(dplyr)

akey <- Sys.getenv("AG_CENSUS_API_KEY")

small <- c(
  "FARM SALES: (1,000 TO 2,499 $)",
  "FARM SALES: (10,000 TO 19,999 $)",
  "FARM SALES: (10,000 TO 24,999 $)",
  "FARM SALES: (100,000 TO 249,999 $)",
  "FARM SALES: (2,500 TO 4,999 $)",
  "FARM SALES: (20,000 TO 24,999 $)",
  "FARM SALES: (25,000 TO 39,999 $)",
  "FARM SALES: (25,000 TO 49,999 $)",
  "FARM SALES: (40,000 TO 49,999 $)",
  "FARM SALES: (5,000 TO 9,999 $)",
  "FARM SALES: (50,000 TO 99,999 $)",
  "FARM SALES: (LESS THAN 1,000 $)"
)

not_small <- c(
  "FARM SALES: (1,000,000 OR MORE $)",
  "FARM SALES: (1,000,000 TO 2,499,999 $)",
  "FARM SALES: (2,500,000 TO 4,999,999 $)",
  "FARM SALES: (250,000 TO 499,999 $)",
  "FARM SALES: (5,000,000 OR MORE $)",
  "FARM SALES: (500,000 OR MORE $)",
  "FARM SALES: (500,000 TO 999,999 $)"
)

#Get state level sales 2022
state_sales_2017 <- tidyUSDA::getQuickstat(
  data_item = 'COMMODITY TOTALS - SALES, MEASURED IN $',
  category='SALES',
  domain = 'FARM SALES',
  key = akey,
  program = 'CENSUS',
  geographic_level = 'STATE',
  year = '2017') %>%
  mutate(
    farm_size = ifelse(domaincat_desc %in% small, "Small", "Not small")
  ) %>%
  select(state_name, short_desc, farm_size, Value) %>%
  rename(value_2017 = Value) %>%
  group_by(state_name, short_desc, farm_size) %>%
  summarise(value_2017 = sum(value_2017, na.rm = TRUE), .groups = 'drop')

# same for 2022
state_sales_2022 <- tidyUSDA::getQuickstat(
  data_item = 'COMMODITY TOTALS - SALES, MEASURED IN $',
  category='SALES',
  domain = 'FARM SALES',
  key = akey,
  program = 'CENSUS',
  geographic_level = 'STATE',
  year = '2022') %>%
  mutate(
    farm_size = ifelse(domaincat_desc %in% small, "Small", "Not small")
  ) %>%
  select(state_name, short_desc, farm_size, Value) %>%
  rename(value_2022 = Value) %>%
  group_by(state_name, short_desc, farm_size) %>%
  summarise(value_2022 = sum(value_2022, na.rm = TRUE), .groups = 'drop')

all_sales <- state_sales_2017 %>%
  merge(state_sales_2022, by = c('state_name', 'short_desc', 'farm_size'))

totals <- all_sales %>%
  group_by(state_name, short_desc) %>%
  summarize(
    value_2017 = sum(value_2017, na.rm = TRUE),
    value_2022 = sum(value_2022, na.rm = TRUE), 
    .groups = 'drop'
    ) %>%
  mutate(
    farm_size = "All")

all_sales <- bind_rows(all_sales, totals) %>%
  arrange(state_name, desc(farm_size)) %>%
  mutate(
    change = value_2022 - value_2017,
    change_pct = round(change / value_2017 * 100, 2),
    county_name = "ALL"
  ) %>%
  select(
    state_name, county_name, short_desc, farm_size, value_2017, value_2022,
    change, change_pct
  )

saveRDS(all_sales, 'data/ready_for_app/state_sales_totals.rds')

