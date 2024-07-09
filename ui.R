library(shiny)
library(leaflet)
library(bslib)
library(reactable)


ui <- page_fillable(
  
  title = "USDA Ag Census Data Explorer",
  
  layout_columns(
    card(
      card_header("User Inputs"),
      textOutput('dataDims'),
      
      selectInput(
        inputId = "stateSelector",
        label = "State:",
        choices = c(ALL_STATES, "All states"),
        selected = "All states"
      ),
      
      textOutput('stateSelected'),
      # selectInput(
      #   inputId = "countySelector",
      #   label = "County:",
      #   choices = c("(select a state above to view counties)")
      # ),
      # selectInput(
      #   inputId = "commoditySelector",
      #   label = "Commodity:",
      #   choices = c("(select a state above to view commodities)")
      # ),
      selectInput(
        inputId = "metricSelector",
        label = "Metric to view:",
        choices = STATE_METRICS
      ),
      textOutput('metricSelected'),
      
      # selectInput(
      #   inputId = "demographicSelector",
      #   label = "Demographic metric:",
      #   choices = c("(select a state above to view demographics)")
      # ),
      selectInput(
        inputId = "sizeSelector",
        label = "Farm size:",
        choices = c("Small", "Not small", "All"),
        selected = 'All'
      ),
      textOutput('sizeSelected')
    ),
    card(
      card_header("Map"),
      leafletOutput('mapview')
    ),
    col_widths = c(4, 8)
  ),
  layout_columns(
    card(
      card_header("Plot"),
      plotOutput('lollipop')
      ),
    card(
      card_header("Table"),
      reactableOutput('table')
      ),
    col_widths = c(4, 8)
  )
  
)

