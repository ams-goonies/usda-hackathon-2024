import("shiny")
import("DT")
import("plotly")
import("modules")
import("stats")
import("utils")
import("shinydashboard")
import("dplyr")
import("echarts4r")
import("echarts4r.maps")
import("leaflet")

export("ui")
export("init_server")

CONSTS <- use("constants/constants.R")


ui <- function(id) {
  ns <- NS(id)

  box(
    title = "Destination Map",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    #echarts4rOutput(ns("mapview"), height = "100%")
    leafletOutput(ns("mapview2"), height = "100%")
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session, state) {
  ns <- session$ns
  state <- reactive({state()})
    # bubble_data <- CONSTS$APP_DATA$bubble_data
  # state_data <- CONSTS$APP_DATA$state_data %>%
  #   ungroup() %>% 
  #   transmute(state, state_full, value = total.shipments)
  
  # state_label <- function(visible = FALSE) {
  #   list(
  #     show = visible,
  #     backgroundColor = CONSTS$COLORS$secondary,
  #     borderRadius = 4,
  #     borderWidth = 0,
  #     color = CONSTS$COLORS$white,
  #     padding = c(7, 14)
  #   )
  # }
  
  state_pal <- colorNumeric("YlGnBu", domain = CONSTS$dci_state$mean)
  
  output$mapview2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = CONSTS$dci_state,
        fillColor = ~state_pal(mean),
        fillOpacity = 0.7,
        color = 'transparent',
        group = 'states'
      )
  })
  
  observeEvent(input$stateSelector, {
    leafletProxy('mapview2', session) %>%
      clearGroup('states')
    
  })
  
  
  
  # output$mapview <- renderEcharts4r({
  #   state_data %>%
  #     e_charts(state_full) %>%
  #     em_map("USA") %>%
  #     e_map(
  #       value,
  #       map = "USA",
  #       name = "State total shipments",
  #       roam = TRUE,
  #       scaleLimit = list(min = 2, max = 8),
  #       itemStyle = list(
  #         areaColor = CONSTS$COLORS$ash_light,
  #         borderColor = CONSTS$COLORS$white,
  #         borderWidth = 0.5
  #       ),
  #       emphasis = list(
  #         label = state_label(),
  #         itemStyle = list(areaColor = CONSTS$COLORS$primary)
  #       ),
  #       select = list(
  #         label = state_label(visible = TRUE),
  #         itemStyle = list(areaColor = CONSTS$COLORS$primary)
  #       )
  #     ) %>%
  #     e_visual_map(
  #       value,
  #       inRange = list(color = c(CONSTS$COLORS$ash_light, CONSTS$COLORS$secondary))
  #     ) %>%
  #     e_tooltip(
  #       trigger = "item",
  #       borderWidth = 0,
  #       extraCssText = "box-shadow: 0 3px 12px rgba(0,0,0,0.2);"
  #     )
  # })
  
  # output$mapview <- renderLeaflet({
  #   
  # })
}
