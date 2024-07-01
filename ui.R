library(shiny)
library(leaflet)

fluidPage(
    # Application title
    titlePanel("Goonies hackathon testing app"),

    sidebarLayout(
      sidebarPanel(
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

    mainPanel(
        leafletOutput('mapview')
    )
  )
)
