library(leaflet)
library(dplyr)
library(sf)
library(leafgl)

# imagine this is what is selected by a shiny app filter. Change it to any state.
STATE <- "Iowa"

# county level choropleth for single state
one_state <- readRDS("data/ready_for_app/dci_county.rds") %>%
  filter(
    !is.na(State),
    State == STATE
    ) %>%
  mutate(Quintile = as.factor(Quintile))

pal <- colorFactor("YlOrRd", domain = one_state$Quintile)

labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  one_state$Zipcode, one_state$Quintile
) %>% lapply(htmltools::HTML)

leaflet() %>%
  addPolygons(
    data = one_state,
    fillColor = ~pal(Quintile),
    weight = 2,
    opacity = 1,
    color = "transparent",
    #dashArray = "3",
    fillOpacity = 0.6,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = c(1,2,3,4,5), opacity = 0.7, title = "DCI Quintile",
            position = "bottomright")