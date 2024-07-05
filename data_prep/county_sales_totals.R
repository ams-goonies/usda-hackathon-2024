library(dplyr)
library(tidyUSDA)
library(tigris)

key <- Sys.getenv("AG_CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

# Retrieving Total Crop Sales by County
crop_totals_2022 <- tidyUSDA::getQuickstat(
  sector='CROPS',
  group="CROP TOTALS",
  commodity='CROP TOTALS',
  category='SALES',
  domain='TOTAL',
  key = key,
  program = 'CENSUS',
  data_item = "CROP TOTALS - SALES, MEASURED IN $",
  geographic_level = 'COUNTY',
  year = '2022',
  geometry = FALSE,
  lower48 = FALSE) %>%
  select(state_name, county_name, short_desc, Value) %>%
  rename(Value_2022 = Value)

crop_totals_2017 <- tidyUSDA::getQuickstat(
  sector='CROPS',
  group="CROP TOTALS",
  commodity='CROP TOTALS',
  category='SALES',
  key = key,
  program = 'CENSUS',
  data_item = "CROP TOTALS - SALES, MEASURED IN $",
  geographic_level = 'COUNTY',
  year = '2017',
  geometry = FALSE,
  lower48 = FALSE) %>%
  select(state_name, county_name, short_desc, Value) %>%
  rename(Value_2017 = Value)

crop_totals <- crop_totals_2017 %>%
  merge(crop_totals_2022, by = c('state_name', 'county_name', 'short_desc')) %>%
  mutate(
    farm_size = "All",
    metric = "Total sales, $",
    change = Value_2022 - Value_2017,
    change_pct = round(change / Value_2017 * 100, 2)
    ) %>%
  select(
    state_name, county_name, short_desc, farm_size, metric, Value_2017,
    Value_2022, change, change_pct
  )

# missing county data?
tigris::list_counties("ALABAMA")

### fill in some missing rows with NAs
states <- list()
all_counties <- list()

for (i in 1:length(unique(crop_totals$state_name))) {
  state <- unique(crop_totals$state_name)[i]
  counties <- toupper(tigris::list_counties(state)$county)
  all_counties <- unlist(c(all_counties, counties))
  state_rep <- rep(state, length(counties))
  states <- unlist(c(states, state_rep))
}

empties <- data.frame(
  state_name = states,
  county_name = all_counties) %>%
  mutate(
    short_desc = unique(crop_totals$short_desc),
    farm_size = unique(crop_totals$farm_size),
    metric = unique(crop_totals$metric),
    Value_2017 = NA,
    Value_2022 = NA,
    change = NA,
    change_pct = NA
  )

county_sales_full <- bind_rows(crop_totals, empties) %>%
  group_by(state_name, county_name, short_desc, farm_size, metric) %>%
  summarize(
    Value_2017 = sum(Value_2017, na.rm = TRUE),
    Value_2022 = sum(Value_2022, na.rm = TRUE),
    change = sum(change, na.rm = TRUE),
    change_pct = sum(change_pct, na.rm = TRUE),
    across(c(Value_2017, Value_2022, change, change_pct), ~ na_if(., 0)),
    .groups = 'drop',
  )

saveRDS(county_sales_full, 'data/ready_for_app/county_sales_totals.rds')
