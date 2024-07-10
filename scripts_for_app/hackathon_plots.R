library(dplyr)
library(reactable)

state_demographics <- readRDS('data/finalized/ALL_DATA.rds') %>%
  filter(county_name == "All")

demdata <- state_demographics %>%
  select(state_name, short_desc, farm_size, Value_2017, Value_2022, change_pct)

demdata$change_pct <- as.numeric(demdata$change_pct)

format_color <- function(value) {
  if (is.na(value)) return(NULL)
  
  if (value > 0) {
    color <- "rgba(0, 255, 0, 0.2)" 
  } else if (value < 0) {
    color <- "rgba(255, 0, 0, 0.2)"
  } else {
    color <- "transparent"
  }
  
  list(backgroundColor = color)
}

reactable(demdata, 
          columns = list(
            state_name = colDef(name = "State"),
            short_desc = colDef(name = "Demographic"),
            farm_size = colDef(name = "Farm Size"),
            Value_2017 = colDef(
              name = "2017 Sales",
              format = colFormat(prefix = "$", separators = TRUE, digits = 0)
            ),
            Value_2022 = colDef(
              name = "2022 Sales",
              format = colFormat(prefix = "$", separators = TRUE, digits = 0)
            ),
            change_pct = colDef(
              name = "Change in Sales",
              cell = function(value) {
                if (is.na(value)) return(NA)
                sprintf("%.2f%%", value)
              },
              style = function(value) format_color(value),
              align = "right"
            )
          )
)