library(tidyverse)
library(readxl)
library(sf)
library(sp)
library(leaflet)
library(leafgl)
library(rgdal)
#library(spData)
#library(raster)
library(geodata)


pj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

all_zips <- read_sf('data/tl_2020_us_zcta510/tl_2020_us_zcta510.shp') %>%
  st_transform(pj) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.01) %>%
  st_cast('MULTIPOLYGON') %>%
  st_cast('POLYGON')

all_zips$area <- as.numeric(st_area(all_zips))

all_zips <- all_zips %>%
  group_by(GEOID10) %>%
  filter(area == max(area))

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
  mutate(Quintile = ifelse(!is.na(Quintile), as.character(Quintile), "No data")) 
  
# my_map <- leaflet(options = leafletOptions(minZoom = 3)) %>%
#   #addTiles() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   setMaxBounds(
#     lng1 = -127.8,
#     lat1 = 52.5,
#     lng2 = -63.8,
#     lat2 = 21.4)
# 
# my_map %>%
#   addPolygons(
#     data = head(all_zips, 2500),
#     color = 'darkblue',
#     weight = 0.5,
#     fillColor = 'pink',
#     fillOpacity = 0.5
#     )

# county level choropleth for single state
or <- d %>%
  filter(State == "Oregon") %>%
  mutate(Quintile = as.factor(Quintile))

pal <- colorFactor("YlOrRd", domain = or$Quintile)

labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  or$Zipcode, or$Quintile
) %>% lapply(htmltools::HTML)

leaflet() %>%
  #setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = or,
    fillColor = ~pal(Quintile),
    weight = 2,
    opacity = 1,
    color = "transparent",
    #dashArray = "3",
    fillOpacity = 0.6,
    # highlightOptions = highlightOptions(
    #   weight = 5,
    #   color = "#666",
    #   dashArray = "",
    #   fillOpacity = 0.7,
    #   bringToFront = TRUE)
    # ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = c(1,2,3,4,5), opacity = 0.7, title = "DCI Quintile",
            position = "bottomright")

 #saveRDS(d, 'data/dci_county.rds') 

#####################
d_load <- readRDS('data/dci_county.rds')
d_state <- d_load %>%
  st_drop_geometry() %>%
  ungroup() %>%
  dplyr::group_by(State) %>%
  summarize(mean = mean(`Distress Score`, na.rm = TRUE)) %>%

data(us_states)

s <- us_states %>%
  rename(State = NAME) %>%
  dplyr::select(State) %>%
  merge(d_state, by = 'State') %>%
  st_transform(4326)



#saveRDS(s, 'data/dci_state_mean.rds') 

