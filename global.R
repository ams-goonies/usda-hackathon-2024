# Load all packages
library(modules)
library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(sass)
library(echarts4r)
library(echarts4r.maps)
library(htmlwidgets)
library(leaflet)

# Compile sass to css
sass(
  sass::sass_file("styles/main.scss"),
  cache = NULL,
  options = sass_options(output_style = "compressed"),
  output = "www/css/sass.min.css"
)

# Modules
map_view <- use("modules/map_view2.R")
state_view <- use("modules/state_view.R")
city_view <- use("modules/city_view.R")
global_metrics_view <- use("modules/global_metrics_view.R")
local_metrics_view <- use("modules/local_metrics_view.R")

# Tab pages
#source("pages/simpleDashboard.R")
source("pages/advancedDashboard.R")
source("pages/contact.R")
