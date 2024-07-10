library(shiny)
library(leaflet)
library(dplyr)
library(dbplyr)
library(reactable)
library(paletteer)
library(glue)
library(scales)
library(hrbrthemes)
library(plotly)

function(input, output, session) {
  
  
  output$map_header <- renderUI({
    extent <- ifelse(input$stateSelector == "ALL STATES", "per state",
                     paste("per county in", input$stateSelector))
    print(paste("Total", input$metricSelector, extent))
  })
  
  output$plot_header <- renderUI({
    extent <- ifelse(input$stateSelector == "ALL STATES", "states",
                     paste("counties in", input$stateSelector))
    print(paste("15", extent, "with the greatest change in", 
                input$metricSelector, ", 2017-2022"))
  })
  
  output$table_header <- renderUI({
    extent1 <- ifelse(input$stateSelector == "ALL STATES", "State-level",
                     "County-level")
    extent2 <- ifelse(input$stateSelector == 'ALL STATES', "", 
                      paste(" for", input$stateSelector))
    
    print(paste0(input$metricSelector, ": ", extent1, " data for 2017 and 2022 ", 
                 extent2))
  })
  
  observeEvent(input$stateSelector,{
    updateSelectInput(
      session,
      "metricSelector",
      choices = get_metrics_list(input$stateSelector)
    )
  })

  d <- eventReactive(
    c(input$stateSelector,
      input$metricSelector,
      input$sizeSelector), {
        
    req(input$stateSelector)
    req(input$metricSelector)
    req(input$sizeSelector)
    
    get_census_data(
      state = input$stateSelector,
      #county = input$countySelector,
      desc = input$metricSelector,
      size = input$sizeSelector
    )
  })
  
  pal17 <- reactive({
    colorNumeric("YlGnBu", domain = d()$Value_2017)
  })
  
  pal22 <- reactive({
    colorNumeric("YlGnBu", domain = d()$Value_2022)
  })
  
  pal_delta <- reactive({
    delta_colors <- as.vector(paletteer_d("MexBrewer::Revolucion"))
    colorNumeric(palette = delta_colors, domain = d()$change_pct)
  })
  
  pal_delta_rev <- reactive({
    delta_colors <- as.vector(paletteer_d("MexBrewer::Revolucion"))
    colorNumeric(palette = delta_colors, domain = d()$change_pct,
                 reverse = TRUE)
  })
  
  output$mapview <- renderLeaflet({
    pal17_init <- colorNumeric("YlGnBu", domain = D_INIT$Value_2017)
    pal22_init <- colorNumeric("YlGnBu", domain = D_INIT$Value_2022)
    delta_colors <- as.vector(paletteer_d("MexBrewer::Revolucion"))
    pal_delta_init <- colorNumeric(palette = delta_colors, domain = D_INIT$change_pct)
    pal_delta_init_rev <- colorNumeric(
      palette = delta_colors,
      domain = D_INIT$change_pct,
      reverse = TRUE
      )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-95.7129, 37.0902, zoom = 4) %>%
      # addPolygons(
      #   data = D_INIT,
      #   fillColor = ~pal17_init(Value_2017),
      #   fillOpacity = 0.7,
      #   color = 'white',
      #   opacity = 1,
      #   weight = 1,
      #   group = '2017 Data'
      # ) %>%
      # addPolygons(
      #   data = D_INIT,
      #   fillColor = ~pal22_init(Value_2022),
      #   fillOpacity = 0.7,
      #   color = 'white',
      #   opacity = 1,
      #   weight = 1,
      #   group = '2022 Data'
      # ) %>%
      addPolygons(
        data = D_INIT,
        fillColor = ~pal_delta_init(change_pct),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 1,
        group = 'Change, 2017-2022',
        popup = glue(
          "<b>{D_INIT$state_name}</b><br/>",
          "2017 value: {format(D_INIT$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
          "2022 value: {prettyNum(D_INIT$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
          "Change: {D_INIT$change_pct}%") %>%
          lapply(htmltools::HTML)
      ) %>%
      addLegend("bottomright", pal = pal_delta_init_rev, values = D_INIT$change_pct,
                title = "Change, 2017-2022",
                labFormat = labelFormat(
                  suffix = "%",
                  transform = function(x) sort(x, decreasing = TRUE)
                  ),
                opacity = 1,
                group = "Change, 2017-2022"
                ) %>%
      addLayersControl(
        baseGroups = c("Change, 2017-2022", "2017 Data", "2022 Data"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  observeEvent(d(), {
    map_pal_17 <- pal17()
    map_pal_22 <- pal22()
    map_pal_delta <- pal_delta()
    pal_delta_init_rev <- pal_delta_rev()
    
    m <- leafletProxy('mapview') %>%
      clearGroup("Change, 2017-2022") %>%
      clearControls() %>%
      # addPolygons(
      #   data = d(),
      #   fillColor = ~map_pal_17(Value_2017),
      #   fillOpacity = 0.7,
      #   color = 'white',
      #   opacity = 1,
      #   weight = 3,
      #   group = '2017 Data'
      # ) %>%
      # addPolygons(
      #   data = d(),
      #   fillColor = ~map_pal_22(Value_2022),
      #   fillOpacity = 0.7,
      #   color = 'white',
      #   opacity = 1,
      #   weight = 3,
      #   group = '2022 Data'
      # ) %>%
      addPolygons(
        data = d(),
        fillColor = ~map_pal_delta(change_pct),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 1,
        group = 'Change, 2017-2022',
        popup = glue(
          "<b>{d()$state_name}</b><br/>",
          "2017 value: {format(d()$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
          "2022 value: {prettyNum(d()$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
          "Change: {d()$change_pct}%") %>%
          lapply(htmltools::HTML)
      ) %>%
      addLegend("bottomright", pal = pal_delta_init_rev, values = d()$change_pct,
                title = "Change, 2017-2022",
                labFormat = labelFormat(
                  suffix = "%",
                  transform = function(x) sort(x, decreasing = TRUE)
                ),
                opacity = 1,
                group = "Change, 2017-2022"
      ) %>%
      addLayersControl(
        baseGroups = c("Change, 2017-2022", "2017 Data", "2022 Data"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    if (input$stateSelector == "ALL STATES"){
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
  
  
  output$lollipop <- renderPlot({
    n <- 15
    
    d <- d() %>%
      arrange(Value_2022) %>%
      slice(1:n) %>%
      mutate(state_name = factor(state_name, levels = state_name))
    
    sub_label <- paste("TOP", n, 
                       #D_INIT$short_desc[1],
                       input$metricSelector,
                       ifelse(
                         input$stateSelector == "ALL STATES", 
                         "THE US", input$stateSelector), "FROM 2017 TO 2022")
    
    y_label <- "Sales, Measured in $"
    x_label1 <- "Sales in 2017: $"
    x_label2 <- "Sales in 2022: $"
    
    lollipop <- ggplot(d) +
      geom_point(
        aes(
          x = state_name,
          y = Value_2022,
          color = change_pct#,
          #text = paste(x_label1, comma(Value_2022), sep = "")
        ),
        size = 4
      ) +
      geom_point(
        aes(
          x = state_name,
          y = Value_2017,
          color=change_pct#,
          #text = paste(x_label1, comma(Value_2017), sep = "")
        ),
        size = 2
      ) +
      geom_segment(
        aes(
          x = state_name,
          xend = state_name,
          y = Value_2017,
          yend = Value_2022,
          color = change_pct#,
          # text = paste(
          #   short_desc, "\nPercentage Change:", sprintf("%.2f%%", change_pct))
        ),
        linewidth = 1.5
      ) +
      scale_color_gradient2(low = "darkred", mid = "#FFFF33", high = "darkgreen", midpoint = 0)  +
      coord_flip() +
      #labs(title = paste("YEAR OVER YEAR CHANGE:", sub_label)) +
      xlab("") +
      ylab(y_label) +
      scale_y_continuous(
        labels = label_number(scale_cut = cut_short_scale())
      ) +
      theme_ipsum() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank())
    
    lollipop
    #ggplotly(lollipop, tooltip = c("x", 'y', 'color')) %>% config(displayModeBar = FALSE)# +
      #layout(hovermode = "y unified")

  })
  
  
  output$table <- renderReactable({
    
    d <- d() %>%
      st_drop_geometry() %>%
      arrange(desc(Value_2022))
    
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
    
    # d <- d() %>%
    #   st_drop_geometry() %>%
    #   arrange(desc(Value_2022))
    
    reactable(
      d, 
      columns = list(
        state_name = colDef(name = "State"),
        short_desc = colDef(show = FALSE),
        metric = colDef(show = FALSE),
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
    )  
  })
}
