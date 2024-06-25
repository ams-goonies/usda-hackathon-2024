import("shiny")

export("ui")
export("init_server")

CONSTS <- use("constants/constants.R")

ui <- function(id) {
  ns <- NS(id)

  metrics <- CONSTS$metrics_list

  # Map list of available metrics to UI elements
  lapply(seq_along(1:length(metrics)), function(index) {
    div (
      class = paste0("box box-primary metric metric-", index),
      div(class = "icon"),
      div(
        class = "value",
        tags$label(metrics[[index]]$label),
        textOutput(ns(paste0("metricsbox", index)))
      )
    )
  })
}

init_server <- function(id) {
  callModule(server, id, id)
}

server <- function(input, output, session, id) {
  ns <- session$ns
  
  output$metricsbox1 <- renderText({ CONSTS$metrics_list$shipments$value })
  output$metricsbox2 <- renderText({ CONSTS$metrics_list$weight$value })
  output$metricsbox3 <- renderText({ CONSTS$metrics_list$locations$value })
  output$metricsbox4 <- renderText({ CONSTS$metrics_list$shipments_day$value })
}
