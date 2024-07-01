library(tidyUSDA)
library(dplyr)

akey <- Sys.getenv("AG_CENSUS_API_KEY")

#Get state level sales 2022
state_sales_2017 <- tidyUSDA::getQuickstat(
  sector = 'CROPS',
  commodity='CROP TOTALS',
  category='SALES',
  domain = 'TOTAL',
  key = akey,
  program = 'CENSUS',
  geographic_level = 'STATE',
  year = '2017',
  geometry = FALSE,
  lower48 = FALSE,
  weighted_by_area = FALSE) %>%
  filter(
    grepl("SALES, MEASURED IN", short_desc),
    unit_desc == "$"
    ) %>%
  select(state_name, short_desc, Value) %>%
  rename(value_2017 = Value)

# same for 2017
state_sales_2022 <- tidyUSDA::getQuickstat(
  sector = 'CROPS',
  commodity='CROP TOTALS',
  category='SALES',
  domain = 'TOTAL',
  key = akey,
  program = 'CENSUS',
  geographic_level = 'STATE',
  year = '2022',
  geometry = FALSE,
  lower48 = FALSE,
  weighted_by_area = FALSE) %>%
  filter(
    grepl("SALES, MEASURED IN", short_desc),
    unit_desc == "$"
  ) %>%  
  select(state_name, short_desc, Value) %>%
  rename(value_2022 = Value)

all_sales <- state_sales_2017 %>%
  merge(state_sales_2022, by = c('state_name', 'short_desc')) %>%
  mutate(
    change = value_2022 - value_2017,
    change_pct = round(change / value_2017 * 100, 2),
    county_name = "ALL"
  )

saveRDS(all_sales, 'data/ready_for_app/state_sales_totals.rds')

