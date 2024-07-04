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
      
      if (input$stateSelector != "(No state selected)"){
        state <- dci_state %>%
          filter(State == input$stateSelector)
        
        bb <- as.numeric(st_bbox(state))
        
        leafletProxy('mapview') %>%
          clearGroup('states') %>%
          fitBounds(
            lng1 = bb[1],
            lat1 = bb[2],
            lng2 = bb[3],
            lat2 = bb[4]
              ) %>%
          addPolygons(
            data = state,
            fillColor = 'pink',
            fillOpacity = 0.7,
            color = 'transparent',
            group = 'states'
          )
      }

    })
    
    output$fake_map <- renderPlot({
      # Sample data set
      set.seed(1)
      df <- data.frame(x = LETTERS[1:10],
                       y = sample(20:35, 10, replace = TRUE))
      
      ggplot(df, aes(x = x, y = y)) +
        geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                     color = "gray", lwd = 1) +
        geom_point(size = 7.5, pch = 21, bg = 4, col = 1) +
        geom_text(aes(label = y), color = "white", size = 3) +
        scale_x_discrete(labels = paste0("G_", 1:10)) +
        coord_flip() +
        theme_minimal()
      
    })
    
    

}
