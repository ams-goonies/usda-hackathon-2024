akey <- Sys.getenv("AG_CENSUS_API_KEY")

#Get state level sales 2022

state_sales <- tidyUSDA::getQuickstat(
  sector = 'CROPS',
  #group = c('FIELD CROPS', 'FRUIT & TREE NUTS', 'VEGETABLES'),
  commodity='CROP TOTALS',
  category='SALES',
  #domain='SALES & DISTRIBUTION',
  #county=NULL,
  key = akey,
  program = 'CENSUS',
  #data_item = 'CROP TOTALS - SALES, MEASURED in $',
  geographic_level = 'STATE',
  year = '2022',
  #state = 'ALABAMA',
  geometry = FALSE,
  lower48 = FALSE,
  weighted_by_area = FALSE) %>%
  filter(grepl("SALES, MEASURED IN", short_desc))

cleandata <- select(state_sales, "Value", "state_alpha")
cleandata <- na.omit(cleandata)
total_sales <- cleandata %>% group_by(state_alpha)  %>%
  summarize(total_sales = sum(Value))

View(total_sales)

