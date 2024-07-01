library(shiny)
library(leaflet)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output, session) {

    state_pal <- colorNumeric("YlGnBu", domain = dci_state$mean)
    
    output$mapview <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          data = dci_state,
          fillColor = ~state_pal(mean),
          fillOpacity = 0.7,
          color = 'transparent',
          group = 'states'
        )
    })
    
    observeEvent(input$stateSelector, {
      state <- dci_state %>%
        filter(State == input$stateSelector)
      
      leafletProxy('mapview') %>%
        clearGroup('states') %>%
        addPolygons(
          data = state,
          fillColor = 'pink',
          fillOpacity = 0.7,
          color = 'transparent',
          group = 'states'
        )
        

    })
    
    

}
