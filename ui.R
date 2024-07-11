library(shiny)
library(leaflet)
library(bslib)
library(reactable)
library(plotly)
library(shinyWidgets)
library(ggiraph)


ui <- page_fillable(
  theme = bslib::bs_theme(bootswatch = "yeti", font_scale = 0.9),
  
  includeCSS("www/my_style.css"),
  
  h3(HTML('<img src="usda-logo-color.svg" height="50" style="padding-right:20px" >AgCensus Data Explorer')),
  
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
      textOutput('stateSelected'),
      
      radioGroupButtons(
        inputId = "categorySelector",
        label = "Data category:", 
        choices = c(
          "Sales totals" = "Total sales, $", 
          "Sales proportions" = "percent of total", 
          "Demographics" = "Number of producers"),
        #status = "info",
        width = '100%'
      ),
      textOutput('catSelected'),

      selectInput(
        inputId = "metricSelector",
        label = "Metric to view:",
        choices = c("COMMODITY TOTALS - SALES, MEASURED IN $"),
        width = '100%'
      ),
      textOutput('metricSelected'),
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
      
      materialSwitch(
        inputId = "sizeSelector",
        label = "Show results for small farms only", 
        status = "primary",
        right = TRUE,
        value = FALSE
      ),
      textOutput('sizeSelected')
      
      # awesomeRadio(
      #   inputId = "sizeSelector",
      #   label = "Filter by farm size:", 
      #   choices = c("All farms" = 'All',
      #               "Small farms only (sales <= $250k/yr.)" = 'Small'
      #               ),
      #   selected = "All",
      #   status = "warning"
      # )#,
      
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
      plotOutput('lollipop', width = "100%")
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

