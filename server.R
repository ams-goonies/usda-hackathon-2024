library(shiny)
library(leaflet)
library(dplyr)
library(reactable)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  observe(
    updateSelectInput(
      session,
      "metricSelector",
      choices = get_metric_list(input$stateSelector)
    )
  )
  
  # build reactive SQL query iteratively
  d <- reactive({

    if (input$stateSelector == "All states") {
      query <- ALL_DATA %>%
        filter(
          county_name == "All")
    } else {
      query <- ALL_DATA %>%
        filter(
          county_name != "All",
          state_name == input$stateSelector
        )
    }
    
    query <- query %>%
      filter(short_desc == input$metricSelector,
             farm_size == input$sizeSelector)
    
    # execute query
    d <- query %>% collect()
    
    ALL_GEOMS %>%
      merge(d, by = c("state_name", "county_name"))
  })
  
  output$stateSelected <- renderText({
    input$stateSelector
  })
  
  output$metricSelected <- renderText({
    input$metricSelector
  })
  
  output$sizeSelected <- renderText({
    input$sizeSelector
  })
  
  output$dataDims <- renderText({
    dim(d())
  })
  
  
  output$mapview <- renderLeaflet({
    #browser()
    d <- d()
    # map_pal_17 <- colorNumeric("YlGnBu", domain = d$Value_2017)
    # map_pal_22 <- colorNumeric("plasma", domain = d$Value_2017)
    # map_pal_delta <- colorNumeric("viridis", domain = d$Value_2017)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-95.7129, 37.0902, zoom = 4)
  })

  observeEvent(d(), {
    map_pal_17 <- colorNumeric("YlGnBu", domain = d()$Value_2017)
    map_pal_22 <- colorNumeric("YlGnBu", domain = d()$Value_2022)
    map_pal_delta <- colorNumeric("viridis", domain = d()$change_pct)
    
    m <- leafletProxy('mapview') %>%
      clearGroup('states') %>%
      addPolygons(
        data = d(),
        fillColor = ~map_pal_17(Value_2017),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 3,
        group = '2017 Data'
      ) %>%
      addPolygons(
        data = d(),
        fillColor = ~map_pal_22(Value_2022),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 3,
        group = '2022 Data'
      ) %>%
      addPolygons(
        data = d(),
        fillColor = ~map_pal_delta(change_pct),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 3,
        group = 'Change, 2017-2022'
      ) %>%
      addLayersControl(
        baseGroups = c("2017 Data", "2022 Data", "Change, 2017-2022"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    if (input$stateSelector == "All states"){
      m %>% setView(-95.7129, 37.0902, zoom = 4)

    } else {
      bb <- as.numeric(st_bbox(d()))
      m %>%
        fitBounds(
          lng1 = bb[1],
          lat1 = bb[2],
          lng2 = bb[3],
          lat2 = bb[4]
        )
    }
  })
  
  
  # output$fake_map <- renderPlot({
  #   # Sample data set
  #   set.seed(1)
  #   df <- data.frame(x = LETTERS[1:10],
  #                    y = sample(20:35, 10, replace = TRUE))
  #   
  #   ggplot(df, aes(x = x, y = y)) +
  #     geom_segment(aes(x = x, xend = x, y = 0, yend = y),
  #                  color = "gray", lwd = 1) +
  #     geom_point(size = 7.5, pch = 21, bg = 4, col = 1) +
  #     geom_text(aes(label = y), color = "white", size = 3) +
  #     scale_x_discrete(labels = paste0("G_", 1:10)) +
  #     coord_flip() +
  #     theme_minimal()
  #   
  # })
  
  
  output$data_table <- renderReactable({
    format_color <- function(value) {
      if (is.na(value)) return(NULL)
      
      if (value > 0) {
        color <- "rgba(0, 255, 0, 0.2)" 
      } else if (value < 0) {
        color <- "rgba(255, 0, 0, 0.2)"
      } else {
        color <- "transparent"
      }
      
      list(backgroundColor = color)
    }
    
    reactable(d() %>% st_drop_geometry(), 
              columns = list(
                state_name = colDef(name = "State"),
                short_desc = colDef(name = "Demographic"),
                farm_size = colDef(name = "Farm Size"),
                Value_2017 = colDef(
                  name = "2017 Sales",
                  format = colFormat(prefix = "$", separators = TRUE, digits = 0)
                ),
                Value_2022 = colDef(
                  name = "2022 Sales",
                  format = colFormat(prefix = "$", separators = TRUE, digits = 0)
                ),
                change_pct = colDef(
                  name = "Change in Sales",
                  cell = function(value) {
                    if (is.na(value)) return(NA)
                    sprintf("%.2f%%", value)
                  },
                  style = function(value) format_color(value),
                  align = "right"
                )
              )
    )  })
    
    

}
