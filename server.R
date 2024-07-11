library(shiny)
library(leaflet)
library(dplyr)
#library(dbplyr)
library(reactable)
library(paletteer)
library(glue)
library(scales)
library(hrbrthemes)
library(htmltools)
library(ggiraph)
library(plotly)
library(fontawesome)

function(input, output, session) {
  
  # debugging help
  output$stateSelected <- renderText(input$stateSelector)
  output$catSelected <- renderText(input$categorySelector)
  output$metricSelected <- renderText(input$metricSelector)
  output$sizeSelected <- renderText(input$sizeSelector)
  
  metrics_available <- reactiveVal(
    get_metrics_list(
      state = "ALL STATES",
      size = "All"
    )
  )
  
  observe({
    metrics_available(
      get_metrics_list(
        state = ifelse(isTruthy(input$stateSelector), input$stateSelector, "ALL STATES"),
        size = ifelse(isTruthy(input$sizeSelector), input$sizeSelector, "All"),
        type = ifelse(isTruthy(input$categorySelector), input$categorySelector, "Total sales, $")
        )
      )
  })
  
  observe({
    updateSelectInput(
      session,
      "metricSelector",
      choices = metrics_available()
    )
  })
  
  output$map_header <- renderUI({
    extent <- ifelse(input$stateSelector == "ALL STATES", "per state",
                     paste("per county in", input$stateSelector))
    paste("Total", input$metricSelector, extent)
  })
  
  output$plot_header <- renderUI({
    extent <- ifelse(input$stateSelector == "ALL STATES", "states",
                     paste("counties in", input$stateSelector))
    paste("10", extent, "with the greatest change in", 
                input$metricSelector, ", 2017-2022")
  })
  
  output$table_header <- renderUI({
    extent1 <- ifelse(input$stateSelector == "ALL STATES", "State-level",
                     "County-level")
    extent2 <- ifelse(input$stateSelector == 'ALL STATES', "", 
                      paste(" for", input$stateSelector))
    
    paste0(input$metricSelector, ": ", extent1, " data for 2017 and 2022 ", 
                 extent2)
  })
  
  
  d <- reactiveVal(readRDS('data/finalized/d_init.rds'))
  
  observe({
    state <- ifelse(isTruthy(input$stateSelector) == FALSE, 'ALL STATES', 
                    input$stateSelector)
    print(paste("state:", state))
    metric <- ifelse(isTruthy(input$metricSelector) == FALSE, 
                     "PRODUCERS, (ALL), FEMALE - NUMBER OF PRODUCERS", 
                     input$metricSelector)
    print(paste("metric:", metric))
    
    size <- ifelse(isTruthy(input$sizeSelector) == FALSE, 'All', 
                   input$sizeSelector)
    
    size <- ifelse(input$sizeSelector != TRUE, "All", "Small")
    #print(size)
    print(paste("size:", size))
    
    d(
      get_census_data(
        state = state,
        county = NULL,
        desc = metric,
        size = size
      ) %>%
        filter(!is.infinite(Value_2017),
               !Value_2017 %in% c("Inf", "-Inf"), 
               !is.infinite(Value_2022),
               !Value_2017 %in% c("Inf", "-Inf"), 
               !is.infinite(change_pct),
               !change_pct %in% c("Inf", "-Inf"), 
        )
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

    d <- d() %>%
      mutate(
        change_html = ifelse(
          change > 0,
          paste0("fa-arrow-up", as.character(change_pct)),
          paste0("fa-arrow-down", as.character(change_pct))
        )
      )
    
    pal17_init <- colorNumeric("YlGnBu", domain = d$Value_2017)
    pal22_init <- colorNumeric("YlGnBu", domain = d$Value_2022)
    delta_colors <- as.vector(paletteer_d("MexBrewer::Revolucion"))
    pal_delta_init <- colorNumeric(palette = delta_colors, 
                                   domain = d$change_pct)
    pal_delta_init_rev <- colorNumeric(
      palette = delta_colors,
      domain = d$change_pct,
      reverse = TRUE
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-95.7129, 37.0902, zoom = 4) %>%
      # addPolygons(
      #   data = d,
      #   fillColor = ~pal17_init(Value_2017),
      #   fillOpacity = 0.7,
      #   color = 'white',
      #   opacity = 1,
      #   weight = 1,
      #   group = '2017 Data',
      #   popup = glue(
      #     ifelse(
      #       input$stateSelector!="ALL STATES",
      #       "<b>{d()$county_name} COUNTY, {d()$state_name}</b><br/>",
      #       "<b>{d()$state_name}</b><br/>"
      #     ),
      #     "2017 value: {format(d()$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
      #     "2022 value: {prettyNum(d()$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
      #     "Change: {d()$change_pct}%") %>%
      #     lapply(htmltools::HTML)   
      #   ) %>%
      # addPolygons(
      #   data = d,
      #   fillColor = ~pal22_init(Value_2022),
      #   fillOpacity = 0.7,
      #   color = 'white',
      #   opacity = 1,
      #   weight = 1,
      #   group = '2022 Data',
      #   popup = glue(
      #     ifelse(
      #       input$stateSelector!="ALL STATES",
      #       "<b>{d()$county_name} COUNTY, {d()$state_name}</b><br/>",
      #       "<b>{d()$state_name}</b><br/>"
      #     ),
      #     "2017 value: {format(d()$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
      #     "2022 value: {prettyNum(d()$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
      #     "Change: {d()$change_pct}%") %>%
      #     lapply(htmltools::HTML)      
      #   ) %>%
      addPolygons(
        data = d,
        fillColor = ~pal_delta_init(change_pct),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 1,
        group = 'Change, 2017-2022',
        popup = glue(
          ifelse(
            input$stateSelector!="ALL STATES",
            "<b>{d()$county_name} COUNTY, {d()$state_name}</b><br/>",
            "<b>{d()$state_name}</b><br/>"
          ),
          "2017 value: {format(d()$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
          "2022 value: {prettyNum(d()$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
          "Change: {d()$change_html}%") %>%
          lapply(htmltools::HTML)      
        ) %>%
      addLegend("bottomright", 
                pal = pal_delta_init_rev,
                values = d$change_pct,
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
    
    d <- d() %>%
      mutate(
        change_html = ifelse(
          change > 0,
          paste(tags$span(fa("arrow-up", fill = "#244E57FF")), as.character(change_pct)),
          paste(tags$span(fa("arrow-down", fill = "#AF2213FF")), as.character(change_pct))
        )
      )
    
    m <- leafletProxy('mapview') %>%
      clearGroup("Change, 2017-2022") %>%
      clearControls() %>%
      addPolygons(
        data = d,
        fillColor = ~map_pal_17(Value_2017),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 3,
        group = '2017 Data',
        popup = glue(
          ifelse(
            input$stateSelector!="ALL STATES",
            "<b>{d$county_name} COUNTY, {d$state_name}</b><br/>",
            "<b>{d$state_name}</b><br/>"
          ),
          "2017 value: {format(d$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
          "2022 value: {prettyNum(d$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
          'Change: {d$change_html}%') %>%
          lapply(htmltools::HTML)
        ) %>%
      addPolygons(
        data = d,
        fillColor = ~map_pal_22(Value_2022),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 3,
        group = '2022 Data',
        popup = glue(
          ifelse(
            input$stateSelector!="ALL STATES",
            "<b>{d$county_name} COUNTY, {d$state_name}</b><br/>",
            "<b>{d$state_name}</b><br/>"
          ),
          "2017 value: {format(d$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
          "2022 value: {prettyNum(d$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
          "Change: {d$change_html}%") %>%
          lapply(htmltools::HTML)
        ) %>%
      addPolygons(
        data = d,
        fillColor = ~map_pal_delta(change_pct),
        fillOpacity = 0.7,
        color = 'white',
        opacity = 1,
        weight = 1,
        group = 'Change, 2017-2022',
        popup = glue(
          ifelse(
            input$stateSelector!="ALL STATES",
            "<b>{d$county_name} COUNTY, {d$state_name}</b><br/>",
            "<b>{d$state_name}</b><br/>"
            ),
          "2017 value: {format(d$Value_2017, big.mark = ",", scientific = FALSE)}<br/>",
          "2022 value: {prettyNum(d$Value_2022, big.mark = ",", scientific = FALSE)}<br/>",
          "Change: {d$change_html}%") %>%
          lapply(htmltools::HTML)
      ) %>%
      addLegend("bottomright", pal = pal_delta_init_rev, 
                values = d()$change_pct,
                title = "Change, 2017-2022",
                labFormat = labelFormat(
                  suffix = "%",
                  transform = function(x) sort(x, decreasing = TRUE)
                ),
                opacity = 1,
                group = "Change, 2017-2022"
      ) %>%
      addLayersControl(
        position = 'bottomleft',
        baseGroups = c("Change, 2017-2022", "2017 Data", "2022 Data"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    if (input$stateSelector == "ALL STATES"){
      m %>% setView(-95.7129, 37.0902, zoom = 4)

    } else {
      bb <- as.numeric(st_bbox(d))
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
    n <- 10
    
    d <- d() %>%
      arrange(desc(abs(change_pct))) %>%
      slice(1:n) %>%
      arrange(abs(change_pct))
    
    if (input$stateSelector == "ALL STATES"){
      d <- d %>% 
        mutate(
          xvar = factor(state_name, levels = state_name),
          state_name = factor(state_name, levels = state_name)
          )
    } else {
      d <- d %>% 
        mutate(
          xvar = factor(county_name, levels = county_name),
          county_name = factor(county_name, levels = county_name)
          )
    }
# 
#     y_label <- ifelse(grepl("SALES", input$metricSelector), 
#                       "Sales, Measured in $", "Number of producers")
#     x_label1 <- ifelse(grepl("SALES", input$metricSelector), 
#                        "Sales in 2017: $", "Number in 2017") 
#     x_label2 <- ifelse(grepl("SALES", input$metricSelector), 
#                        "Sales in 2022: $", "Number in 2022")
    
    lollipop <- ggplot(d) +
      geom_segment(
        aes(
          x = xvar,
          xend = xvar,
          y = 0,
          yend = change_pct,
        ),
        color = 'grey45',
        linewidth = 1.5
      ) +
      geom_segment(
        aes(
          x = xvar,
          xend = xvar,
          y = change_pct*0.99,
          yend = change_pct,
          color = change_pct
        ),
        arrow = arrow(
          length = unit(0.6, "cm"),
          type = 'closed'
        ),
        linewidth = .5
      ) +
      geom_hline(yintercept = 0, color = 'grey60', linewidth = 0.8) +
      scale_color_gradient2(
        low = "#AF2213FF", mid = "#F0F6EBFF", high = "#244E57FF", midpoint = 0) +
      # scale_color_gradientn(
      #   colors = as.vector(paletteer_d("MexBrewer::Revolucion"))#,
      #   #values = d$change_pct
      # ) +
      coord_flip() +
      xlab("") +
      ylab("") +
      scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        #labels = label_number(scale_cut = cut_short_scale()),
        expand = c(0, 0)
      ) +
      theme_ipsum() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(
              color = "grey89", size = 0.8),
            panel.border = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(
              size = 14, family = "Arial Narrow", margin = margin(t = 10)),
            axis.text.y = element_text(
              size = 14, family = "Arial Narrow", margin = margin(r = 10)),
            plot.margin = margin(0)
            )
    
    lollipop

  })
  
  
  output$table <- renderReactable({
    
    d <- d() %>%
      st_drop_geometry() %>%
      arrange(desc(Value_2022))
    
    if(input$stateSelector == "ALL STATES") {
      d <- d %>% select(-county_name) %>%
        rename(State = state_name)
    } else {
      d <- d %>%
        select(-state_name) %>%
        rename(County = county_name)
    }
    
    orange_pal <- function(x) {
      if (!is.na(x)) {
        rgb(
          colorRamp(
            c("#D9792EFF", "#F0F6EBFF", "#368990FF"))(x), 
          maxColorValue = 255)
      } else {
        'grey'
      }
    }
    
    reactable(
      d, 
      columns = list(
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
        change = colDef(
          name = "Change",
          format = colFormat(prefix = "$", separators = TRUE, digits = 0),
          style = function(value) {
            if(is.na(value)){
              color <- 'grey85'
            } else {
              normalized <- (value - min(d$change, na.rm = TRUE)) / (max(d$change, na.rm = TRUE) - min(d$change, na.rm = TRUE))
              color <- orange_pal(normalized)
            }
            list(background = color)
          }
        ),
        change_pct = colDef(
          name = "Change in Sales",
          cell = function(value) {
            if (is.na(value)) return(NA)
            sprintf("%.2f%%", value)
          },
          style = function(value) {
            if(is.na(value)){
              color <- 'grey85'
            } else {
              normalized <- (value - min(d$change_pct, na.rm = TRUE)) / (max(d$change_pct, na.rm = TRUE) - min(d$change_pct, na.rm = TRUE))
              color <- orange_pal(normalized)
            }
            list(background = color)
          },
          align = "right"
        )
      )
    )  
  })
}
