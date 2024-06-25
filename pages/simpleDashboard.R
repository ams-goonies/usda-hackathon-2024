# Simple dashboard tab containing three modules

simpleDashboardTab <- tags$div(
  class = "main-content-grid simple-grid",
  global_metrics_view$ui("global_metrics_simple_view"),
  div(
    class = "table-grid-wrapper",
    state_view$ui("state_simple_view")
  ),
  div(
    class = "barchart-grid-wrapper",
    city_view$ui("city_simple_view")
  )
)
