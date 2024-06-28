library(tidyverse)
library(tidyUSDA)
library(sf)

key <- Sys.getenv("AG_CENSUS_API_KEY")

# Retrieving Total Crop Sales by County
crop_totals_2022 <- tidyUSDA::getQuickstat(
  sector='CROPS',
  group="CROP TOTALS",
  commodity='CROP TOTALS',
  category='SALES',
  domain='TOTAL',
  county=NULL,
  key = key,
  program = 'CENSUS',
  data_item = "CROP TOTALS - SALES, MEASURED IN $",
  geographic_level = 'COUNTY',
  year = '2022',
  state = NULL,
  geometry = F,
  lower48 = F) %>%
  select(state_name, county_name, short_desc, Value) %>%
  rename(value_2022 = Value)

crop_totals_2017 <- tidyUSDA::getQuickstat(
  sector='CROPS',
  group="CROP TOTALS",
  commodity='CROP TOTALS',
  category='SALES',
  domain=NULL,
  county=NULL,
  key = key,
  program = 'CENSUS',
  data_item = "CROP TOTALS - SALES, MEASURED IN $",
  geographic_level = 'COUNTY',
  year = '2017',
  state = NULL,
  geometry = F,
  lower48 = F) %>%
  select(state_name, county_name, short_desc, Value) %>%
  rename(value_2017 = Value)

crop_totals <- crop_totals_2017 %>%
  merge(crop_totals_2022, by = c('state_name', 'county_name', 'short_desc')) %>%
  mutate(
    change = value_2022 - value_2017,
    change_pct = round(change / value_2017 * 100, 2)
    )

#st_write(crop_totals,"Crop Totals By County.geojson", row.names = FALSE)
saveRDS(crop_totals, 'data/ready_for_app/crop_totals_by_county.rds')
