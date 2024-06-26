library(tidyverse)
library(readxl)
library(sf)
library(leaflet)

all_zips <- read_sf('data/tl_2020_us_zcta510/tl_2020_us_zcta510.shp') %>%
  st_transform(4326)

dci <- read_xlsx('data/DCI_2016_to_2020.xlsx')

# Function to add leading 0 to 4-digit zip values in DCI data
add_leading_zero <- function(x) {
  # Check if the number has 4 digits
  ifelse(nchar(x) == 4, paste0("0", x), x)
}

# apply function to fips column
dci$Zipcode <- sapply(dci$Zipcode, add_leading_zero)

# outer join em
d <- all_zips %>%
  rename('Zipcode' = ZCTA5CE10) %>%
  merge(dci, by = 'Zipcode', all = TRUE) %>%
  rename(
    'Quintile' = `Quintile (5=Distressed)`) %>%
  select(
    Zipcode, `Distress Score`, Quintile, State, `State Abbreviation`
  ) %>%
  #st_simplify() %>% 
  #st_transform(4326) %>%
  mutate(Quintile = ifelse(!is.na(Quintile), as.character(Quintile), "No data"))

# export
#sf::st_write(d, dsn = "dci_clean.geojson")

my_map <- leaflet(options = leafletOptions(minZoom = 3)) %>%
  #addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setMaxBounds(
    lng1 = -127.8,
    lat1 = 52.5,
    lng2 = -63.8,
    lat2 = 21.4)

my_map %>%
  addPolygons(data = d)
