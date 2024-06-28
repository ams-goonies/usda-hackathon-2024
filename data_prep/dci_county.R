library(sp)
library(dplyr)
library(sf)
library(readxl)

pj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

unzip(zipfile = 'data/raw/tl_2020_us_zcta510.zip', exdir = 'data/raw/')

all_zips <- read_sf('data/raw/tl_2020_us_zcta510.shp') %>%
  st_transform(pj) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.01) %>%
  st_cast('MULTIPOLYGON') %>%
  st_cast('POLYGON')

# Need to deal with multi-part polygons that got split into two individual polygons
# Find area, group by zip, keep only the larger if there are multiple
# units don't matter, only magnitude because it's relative
all_zips$area <- as.numeric(st_area(all_zips)) 

all_zips <- all_zips %>%
  group_by(GEOID10) %>%
  filter(area == max(area))

dci <- read_xlsx('data/raw/DCI_2016_to_2020.xlsx')

# Function to add leading 0 to 4-digit zip values in DCI data
add_leading_zero <- function(x) {
  # Check if the number has 4 digits
  ifelse(nchar(x) == 4, paste0("0", x), x)
}

# apply function to Zipcode column
dci$Zipcode <- sapply(dci$Zipcode, add_leading_zero)

# outer join em
d <- all_zips %>%
  rename('Zipcode' = ZCTA5CE10) %>%
  merge(dci, by = 'Zipcode', all = TRUE) %>%
  rename(
    'Quintile' = `Quintile (5=Distressed)`) %>%
  select(
    Zipcode, `Distress Score`, Quintile, State, `State Abbreviation`
  ) %>%
  mutate(Quintile = ifelse(!is.na(Quintile), as.character(Quintile), "No data"))

#saveRDS(d, 'data/ready_for_app/dci_county.rds')

