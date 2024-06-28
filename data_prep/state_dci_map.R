library(sf)
library(dplyr)
library(spData)
library(leaflet)

sf_use_s2(FALSE)

d_load <- readRDS('data/ready_for_app/dci_county.rds')

d_state <- d_load %>%
  st_drop_geometry() %>%
  ungroup() %>%
  dplyr::group_by(State) %>%
  summarize(mean = mean(`Distress Score`, na.rm = TRUE))
  
data(us_states)

s <- us_states %>%
  rename(State = NAME) %>%
  dplyr::select(State) %>%
  merge(d_state, by = 'State') %>%
  st_transform(4326) %>%
  st_as_sf()

state_pal <- colorNumeric("YlGnBu", domain = s$mean)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = s,
    fillColor = ~state_pal(mean),
    fillOpacity = 0.7,
    color = 'transparent'
  )

# saveRDS(s, "data/ready_for_app/dci_state_mean.rds")