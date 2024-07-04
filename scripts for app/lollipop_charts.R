# Lollipop Charts

library(extrafont)
#loadfonts(device="win")
#extrafont::font_import()
#loadfonts(); windowsFonts()
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(scales)

# Commodities -----------------------------------------------------------------------------------------------------
reduced_commodities_by_state <- read.csv(#enter path here)
commodities_by_state <- read.csv(#enter path here)

# Read input
state_input <- "FLORIDA"
#state_input <- NA
n <- 10

# Apply input filter
if (!is.na(state_input)) {
  # Filter by state
  commodities_filtered <- reduced_commodities_by_state %>% 
    filter(state_name == state_input)
  
  omitted <- commodities_filtered %>% 
    filter(is.na(Value_2022) | is.na(Value_2017)) %>% 
    select(short_desc) %>% 
    unlist() %>% 
    paste(collapse = "; ")
  if(omitted == "") {omitted <- "none"}
  
  commodities_filtered <- commodities_filtered %>% 
    na.omit() %>% 
    slice(1:n)
  
} else {
  # Retrieving national totals (by summing ANIMAL + CROP TOTALS for accuracy)
  total_sales <- commodities_by_state %>% 
    filter(grepl("TOTALS", group_desc)) %>% 
    select(Value_2017, Value_2022) %>% 
    colSums()
  
  # Aggregating all states
  commodities_filtered <- reduced_commodities_by_state %>% 
    group_by(short_desc) %>% 
    mutate(Value_2022 = sum(Value_2022, na.rm = TRUE),
           Value_2017 = sum(Value_2017, na.rm = TRUE)) %>%
    slice(1) %>% 
    select(short_desc, Value_2017, Value_2022) %>% 
    mutate(pct_total_2017 = round(Value_2017 / total_sales[1] * 100,2),
           pct_total_2022 = round(Value_2022 / total_sales[2] * 100,2)) 
  
  omitted <- commodities_filtered %>% 
    filter(Value_2017 == 0 | Value_2022 == 0) %>% 
    select(short_desc) %>% 
    unlist() %>% 
    paste(collapse = "; ")
  
  commodities_filtered <- commodities_filtered %>% 
    filter(Value_2017 > 0, Value_2022 > 0) %>% 
    mutate(change_pct = round((Value_2022 - Value_2017)/Value_2017 * 100, 4)) %>% 
    ungroup() %>% 
    arrange(desc(Value_2022)) %>% 
    slice(1:n)
}

# Year over year change
commodities_change <- ggplot(commodities_filtered) +
  geom_segment(aes(x=reorder(short_desc, Value_2022), xend=short_desc, y=Value_2017, yend=Value_2022, color = change_pct), 
              linewidth = 1.5) +
  scale_color_gradient2(low = "darkred", mid = "gold", high = "darkgreen", midpoint = 0) +
  geom_point(aes(x=short_desc, y=Value_2017, color=change_pct), size=4, shape = 1, show.legend = TRUE) +
  geom_point(aes(x=short_desc, y=Value_2022, color=change_pct), size=4, show.legend = TRUE)  +
  coord_flip()+
  labs(title = "Year Over Year Change",
       subtitle = paste("TOP", n, "COMMODITY SALES IN", ifelse(is.na(state_input), "THE US", state_input), "FROM 2017 TO 2022")) +
  xlab("") +
  ylab("Sales, Measured in $") +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale())
  ) + 
  theme_ipsum() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank())
commodities_change


# Percentage Change Chart
commodities_pct <- ggplot(commodities_filtered, aes(x=reorder(short_desc, Value_2022), y=change_pct)) +
  geom_hline(yintercept = 0, lty=2, lwd = 1, alpha = 0.25) +
  geom_segment(aes(x=short_desc, xend=short_desc, y=0, yend=change_pct, color=change_pct), linewidth = 3) +
  scale_color_gradient2(low = "darkred", mid = "light yellow", high = "darkgreen", midpoint = 0) +
  geom_point(aes(color=change_pct), size=5) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  coord_flip() +
  labs(title = "Year Over Year Percentage Change",
       subtitle = paste("TOP", n, "COMMODITY SALES IN", ifelse(is.na(state_input), "THE US", state_input))) +
  xlab("") +
  ylab("Percentage Change") +
  expand_limits(y = c(-50, 50))
commodities_pct

# Add footnote for omitted data items
cat("The following data items were omitted due to either hidden or missing information in one or both years:", omitted)


# Demographics  ------------------------------------------------------------------------------------
demo <- readRDS(#enter path here)

# Fixing the missing values 
states <- NULL
for (i in unique(demo$state_name)) {
  states <- append(states, rep(i, 3*length(unique(demo$short_desc))))
}
complete <- data.frame(
  state_name = states,
  farm_size = rep(c(rep("Small", length(unique(demo$short_desc))),
                    rep("Not small", length(unique(demo$short_desc))),
                    rep("All", length(unique(demo$short_desc)))), 50), 
  short_desc = rep(unique(demo$short_desc), 50*3)
)

demo <- complete %>% 
  left_join(demo, by = c("state_name", "farm_size", "short_desc")) %>% 
  mutate_at(c("Value_2017","Value_2022","change","change_pct"), ~replace_na(.,0))

# Shortening labels
demo <- demo %>% 
  mutate(short_desc = vapply(short_desc, function(x){
    x <- gsub("[(]ALL), ","",x)
    x <- strsplit(x, "PRODUCERS, ")[[1]][2]
    x <- strsplit(x, " - NUMBER OF PRODUCERS")[[1]][1]
    x
  }, "abc")) %>% 
  mutate(short_desc = case_match(
    short_desc,
    "MILITARY SERVICE, NEVER SERVED OR ONLY ON ACTIVE DUTY FOR TRAINING IN RESERVES OR NATIONAL GUARD" ~ "MILITARY (RESERVES/GUARD)",
    "MILITARY SERVICE, ACTIVE DUTY NOW OR IN THE PAST" ~ "MILITARY (ACTIVE OR PAST)",
    "AGE LT 25" ~ "AGE UNDER 25",
    "AGE GE 75" ~ "AGE 75+",
    .default = short_desc
  ))

# Manually ordering labels for y-axis
sex <- c("FEMALE", "MALE")
race <- c("WHITE", "HISPANIC", "BLACK OR AFRICAN AMERICAN", "ASIAN", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTI-RACE")
military <- c("MILITARY (RESERVES/GUARD)", "MILITARY (ACTIVE OR PAST)")
age <- c("AGE UNDER 25", "AGE 25 TO 34", "AGE 35 TO 44", "AGE 45 TO 54", "AGE 55 TO 64", "AGE 65 TO 74", "AGE 75+")
all <- c(sex, race, age, military)

# Accepting inputs
state_input <- "ALASKA"
#state_input <- NA
size_input <- "Not small"

# Applying filters
if (!is.na(state_input)) {
  # filtered to a state
  demo_filtered <- demo %>% 
    filter(state_name == state_input,
           farm_size == size_input)
} else {
  # filtered to the entire nation
  
  # get total number of producers by adding males & females
  total_number <- demo %>% 
    filter(farm_size == size_input,
           short_desc %in% sex) %>% 
    select(Value_2017, Value_2022) %>% 
    colSums()

  demo_filtered <- demo %>% 
    filter(farm_size == size_input) %>% 
    group_by(short_desc) %>% 
    mutate(Value_2022 = sum(Value_2022, na.rm = TRUE),
           Value_2017 = sum(Value_2017, na.rm = TRUE)) %>%
    slice(1) %>% 
    select(short_desc, Value_2017, Value_2022) %>% 
    mutate(pct_total_2017 = round(Value_2017 / total_number[1] * 100,2),
           pct_total_2022 = round(Value_2022 / total_number[2] * 100,2)) %>% 
    filter(Value_2017 > 0, Value_2022 > 0) %>% 
    mutate(change_pct = round((Value_2022 - Value_2017)/Value_2017 * 100, 4))
}

# Lollipop Chart with Year Over Year Changes
demographics_change <- ggplot(demo_filtered) +
  geom_segment(aes(x=factor(short_desc, levels = rev(all)), xend=short_desc, y=Value_2017, yend=Value_2022, color = change_pct),
                linewidth = 1.5) +
  geom_point(aes(x=short_desc, y=Value_2017, color=change_pct), shape = 1, size=4) +
  geom_point(aes(x=short_desc, y=Value_2022, color=change_pct), size=4) +
  coord_flip()+
  labs(title = "Year Over Year Change",
       subtitle = paste("DEMOGRAPHICS IN", ifelse(is.na(state_input), "THE US", state_input), "FROM 2017 TO 2022")) +
  theme_ipsum() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("Number of Producers") +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  scale_color_gradient2(low = "darkred", mid = "gold", high = "darkgreen", midpoint = 0)
demographics_change


# Lollipop Chart with Percentage Change Only
demographics_pct <- ggplot(demo_filtered, aes(x=short_desc, y=change_pct)) +
  geom_hline(yintercept = 0, lty=2, lwd = 1, alpha = 0.25) +
  geom_segment(aes(x=factor(short_desc, levels = rev(all)), xend=short_desc, y=0, yend=change_pct, color=change_pct), 
              linewidth = 1.5) +
  geom_point(aes(color=change_pct), size=4) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Year Over Year Change",
       subtitle = paste(ifelse(is.na(state_input), "US", state_input), "DEMOGRAPHICS, PCT CHANGE FROM 2017 TO 2022")) +
  coord_flip() +
  xlab("") +
  ylab("Percentage Change of Number of Producers") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_gradient2(low = "darkred", mid = "gold", high = "darkgreen", midpoint = 0) +
  expand_limits(y = c(-50, 50))
demographics_pct

