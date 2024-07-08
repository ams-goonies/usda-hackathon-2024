library(shiny)
library(leaflet)
library(dplyr)
library(reactable)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # observe(
  #   updateSelectInput(
  #     session, 
  #     "commoditySelector", 
  #     choices = get_commodity_list(input$stateSelector, input$countySelector)
  #   )
  # )
  
  observe(
    updateSelectInput(
      session,
      "metricSelector",
      choices = get_metric_list(input$stateSelector)
    )
  )
  
  # observe(
  #   if (input$stateSelector == "(No state selected)") {
  #     updateSelectInput(
  #       session, 
  #       "sizeSelector", 
  #       choices = c("Small", "Not small", "All")
  #     )
  #   } else {
  #     updateSelectInput(
  #       session, 
  #       "sizeSelector", 
  #       choices = c("All")
  #     )
  #   }
  # )

  d <- reactive({

    if (input$stateSelector == "All states") {
      d <- ALL_DATA %>%
        filter(
          county_name == "All")
    } else {
      d <- ALL_DATA %>%
        filter(
          county_name != "All",
          state_name == input$stateSelector
        )
    }
    
    # if (input$countySelector != "(select a state above to view counties)") {
    #   d <- d %>%
    #     filter(county_name == input$countySelector)
    # }
    
    # if (input$metricSelector == "(select a state above to view metrics)") {
    #   d <- d %>%
    #     filter(
    #       short_desc = "CROP TOTALS - SALES, MEASURED IN $"
    #     )
    # } else {
    d <- d %>%
      filter(short_desc == input$metricSelector,
             farm_size == input$sizeSelector) 
    
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
      setView(-95.7129, 37.0902, zoom = 4) %>%
      addPolygons(
        data = d,
        fillColor = 'transparent',
        color = 'darkred',
        weight = 2,
        group = 'states'
      )
  })

  observeEvent(d(), {
    map_pal_17 <- colorNumeric("YlGnBu", domain = d()$Value_2017)
    map_pal_22 <- colorNumeric("YlGnBu", domain = d()$Value_2017)
    map_pal_delta <- colorNumeric("viridis", domain = d()$change_pct)
    
    m <- leafletProxy('mapview') %>%
      clearGroup('states') %>%
      addPolygons(
        data = d(),
        fillColor = ~map_pal_17(Value_2017),
        fillOpacity = 0.7,
        color = 'transparent',
        group = '2017 Data'
      ) %>%
      addPolygons(
        data = d(),
        fillColor = ~map_pal_22(Value_2017),
        fillOpacity = 0.7,
        color = 'transparent',
        group = '2022 Data'
      ) %>%
      addPolygons(
        data = d(),
        fillColor = ~map_pal_delta(change_pct),
        fillOpacity = 0.7,
        color = 'transparent',
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
  
  
  output$data_table <- renderReactable({
    reactable(d())
  })
    
    

}
