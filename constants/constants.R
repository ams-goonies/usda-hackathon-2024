import("shiny")
import("dplyr")

APP_DATA <- readRDS("./data/DATA_FAKE.rds")
APP_TITLE <- "Ag Census Data Explorer"
APP_TIME_RANGE <- ""
APP_VERSION <- ""

ag_census_api_key <- Sys.getenv("AG_CENSUS_API_KEY")

usda_website <- "https://www.usda.gov/"
ams_website <- "https://www.ams.usda.gov/"
ams_name <- "USDA-AMS"

COLORS <- list(
  white = "#FFF",
  black = "#112f4e",
  primary = "#002D72", #"#0099F9",
  secondary = "#005440",#15354A",
  ash = "#9bd4cf",
  ash_light = "#e0f7f6"
)

metrics_list <- list(
  shipments = list(
    label = "Total shipments",
    value = paste(sum(APP_DATA$state_data$total.shipments), "trips")
  ),
  weight = list(
    label = "Total weight",
    value = paste(format(sum(APP_DATA$state_data$total.weight), big.mark = ","), "tons")
  ),
  locations = list(
    label = "Number of locations",
    value = paste(sum(APP_DATA$state_data$number.of.locations), "locations")
  ),
  shipments_day = list(
    label = "Shipments per day",
    value = paste(sum(APP_DATA$state_data$shipments.day), "trips")
  )
)

usda_logo <- tags$a(
  href = usda_website,
  target = "_blank",
  rel = "nofollow noreferrer",
  class = "logo-link",
  img(src = "images/usda-logo-color.svg", class = "logo-img", alt = "USDA Logo")
)

usda_footer <- tags$h3(
  class = "footer-heading",
  tags$span("This application built by cool people from"),
  tags$a(
    class = "footer-link",
    href = ams_website,
    target = "_blank",
    rel = "nofollow noreferrer",
    ams_name
  )
)

# Get county level sales 2022
sales <- tidyUSDA::getQuickstat(
  sector = 'CROPS',
  #group = c('FIELD CROPS', 'FRUIT & TREE NUTS', 'VEGETABLES'),
  commodity='CROP TOTALS',
  category='SALES',
  #domain='SALES & DISTRIBUTION',
  #county=NULL,
  key = ag_census_api_key,
  program = 'CENSUS',
  #data_item = 'CROP TOTALS - SALES, MEASURED in $',
  geographic_level = 'COUNTY',
  year = '2022',
  #state = 'ALABAMA',
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = TRUE) %>%
  filter(grepl("SALES, MEASURED IN", short_desc))
