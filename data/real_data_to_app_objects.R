library(dplyr)
DATA_CITY <- utils::read.csv("./src/app/data/DATA_CITY_REAL.csv", stringsAsFactors = FALSE)
# randomisation part
set.seed(1234)
data_random <- function(vector_name, data = DATA_CITY, random_min = 0.9, random_max = 1.1, round_precision = 0) {
  (data[[vector_name]] * runif(length(data[[vector_name]]), random_min, random_max)) %>% round(., round_precision)
}
DATA_CITY$weight <- data_random("weight")
DATA_CITY$freight <- data_random("freight", round_precision = 2)
DATA_CITY$shipments <- data_random("shipments")

# STATE VIEW
state_data <- DATA_CITY %>%
  group_by(city) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(c("state", "state_full", "shipments", "weight")) %>%
  group_by(state, state_full) %>%
  add_tally() %>%
  summarise(total.shipments = sum(shipments),
            total.weight = sum(weight),
            number.of.locations = mean(n)) %>%
  arrange(desc(total.shipments)) %>%
  mutate(shipments.day = round(total.shipments/30, 1))


# MAP VIEW
bubble_data <- DATA_CITY %>%
  select(c("city", "lat", "lon", "shipments", "state")) %>%
  group_by(city) %>%
  filter(row_number() == 1)
names(bubble_data) <- c("name", "lat", "lon", "z", "state")

map_state_data <- DATA_CITY %>%
  group_by(city) %>%
  filter(row_number() == 1) %>%
  select(c("state", "shipments")) %>%
  group_by(state) %>%
  summarise(total.shipments = sum(shipments))

# CITY VIEW
city_data <- DATA_CITY %>%
  group_by(city) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(c("city", "state", "shipments", "weight")) %>%
  arrange(desc(shipments)) %>%
  mutate(shipments.day = round(shipments/30, 1))


APP_DATA <- list(state_data = state_data,
                 bubble_data = bubble_data,
                 map_state_data = map_state_data,
                 city_data = city_data)

saveRDS(APP_DATA, "./src/app/data/DATA_FAKE.rds")
