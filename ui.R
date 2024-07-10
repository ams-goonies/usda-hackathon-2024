library(shiny)
library(leaflet)
library(bslib)
library(reactable)
library(plotly)
library(shinyWidgets)


ui <- page_fillable(
  
  includeCSS("www/my_style.css"),
  
  h2("USDA Ag Census Data Explorer"),
  
  layout_columns(
    
    card(
      class = 'input-card',
      #height = 450,
      card_header(
        class = 'input-card'
        ),

      selectInput(
        inputId = "stateSelector",
        label = "State:",
        choices = c("ALL STATES", ALL_STATES),
        selected = "ALL STATES",
        width = '100%'
      ),
      
      #textOutput('stateSelected'),
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
        choices = c("PRODUCERS, (ALL), FEMALE - NUMBER OF PRODUCERS"),
        width = '100%'
      ),
      #textOutput('metricSelected'),
      
      # selectInput(
      #   inputId = "demographicSelector",
      #   label = "Demographic metric:",
      #   choices = c("(select a state above to view demographics)")
      # ),
      # selectInput(
      #   inputId = "sizeSelector",
      #   label = "Farm size:",
      #   choices = c("Small", "Not small", "All"),
      #   selected = 'All',
      #   width = '100%'
      # ),
      
      awesomeRadio(
        inputId = "sizeSelector",
        label = "Filter by farm size:", 
        choices = c("All farms" = 'All',
                    "Small farms only (sales <= $250k/yr.)" = 'Small'
                    ),
        selected = "All",
        status = "warning"
      )#,
      
      # actionButton(
      #   inputId = "updateButton",
      #   label = "GET THE DATA"
      # )
      #textOutput('sizeSelected')
    ),
    card(
      #height = 450,
      full_screen = TRUE,
      card_header(
        uiOutput('map_header')
        ),
      card_body(
        leafletOutput('mapview')
      )
    ),
    col_widths = c(5, 7),
    height = '50%'
  ),
  layout_columns(
    card(
      card_header(
        uiOutput('plot_header')
        ),
      plotOutput('lollipop')
      ),
    card(
      card_header(
        uiOutput('table_header')
        
        ),
      reactableOutput('table')
      ),
    col_widths = c(5, 7),
    height = '50%'
  )
  
)

