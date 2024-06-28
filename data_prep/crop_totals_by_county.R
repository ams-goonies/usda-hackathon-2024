library(tidyverse)
library(tidyUSDA)
library(sf)

# Retrieving Total Crop Sales by County
crop_totals <- tidyUSDA::getQuickstat(
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
  year = '2022',
  state = NULL,
  geometry = T,
  lower48 = F)

#st_write(crop_totals,"Crop Totals By County.geojson", row.names = FALSE)
