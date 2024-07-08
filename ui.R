library(shiny)
library(leaflet)
library(bslib)


ui <- page_fillable(
  
  title = "USDA Ag Census Data Explorer",
  
  layout_columns(
    card(
      card_header("User Inputs"),
      selectInput(
        inputId = "stateSelector",
        label = "Choose a state:",
        choices = c(dci_state$State, "(No state selected)"),
        selected = "(No state selected)"
      ),
      selectInput(
        inputId = "countySelector",
        label = "Choose a county:",
        choices = c("(select a state above to view counties)")#,#c(dci_state$State, "(No state selected)"),
        #selected = "(No state selected)"
      ),
      selectInput(
        inputId = "metricSelector",
        label = "Metric to view:",
        choices = c("Total sales",
                    "Acres in production",
                    "Number of farms"
        )#,#c(dci_state$State, "(No state selected)"),
        #selected = "(No state selected)"
      ),
      selectInput(
        inputId = "demographicSelector",
        label = "Demographic metric:",
        choices = c("Female", "Veteran", "Native American")#,#c(dci_state$State, "(No state selected)"),
        #selected = "(No state selected)"
      )
      
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
      plotOutput('fake_map')
      ),
    card(card_header("Table")),
    col_widths = c(4, 8)
  )
  
)

