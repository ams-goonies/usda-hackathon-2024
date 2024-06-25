# Advanced dashboard tab containing all modules

advancedDashboardTab <- tags$div(
  class = "main-content-grid advanced-grid",
  global_metrics_view$ui("global_metrics_advanced_view"),
  div(
    class = "map-grid-wrapper",
    map_view$ui("map_advanced_view")
  ),
  div(
    class = "table-grid-wrapper",
    state_view$ui("state_advanced_view")
  ),
  div(
    class = "barchart-grid-wrapper",
    city_view$ui("city_advanced_view")
  ),
  local_metrics_view$ui("local_metrics_advanced_view")
)
