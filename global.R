
# source helper functions/scripts
file_names <- list.files("R", pattern = "\\.R$", full.names =  TRUE)
lapply(file_names, source)

sf_use_s2(FALSE)

ALL_STATES <- get_state_list()

# initial state upon loading
D_INIT <- get_census_data(
  state = 'ALL STATES',
  county = NULL,
  desc = 'PRODUCERS, (ALL), FEMALE - NUMBER OF PRODUCERS',
  size = 'All'
)

# MAP_PALS <- function(d) {
#   return(
#     list(
#       colorNumeric("YlGnBu", domain = d()$Value_2017),
#       colorNumeric("YlGnBu", domain = d()$Value_2022),
#       colorNumeric("viridis", domain = d()$change_pct),
#     )
#   )
# }


