# Lollipop Charts

library(extrafont)
#loadfonts(device="win")
#extrafont::font_import()
#loadfonts(); windowsFonts()
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(scales)
library(plotly)

demo <- readRDS("C:/Users/Stella.Koh/USDA/AMS Grants Division (GD) - Data Room/Division Wide/Hackathon 2024/ready_for_app/state_demographics.rds")
reduced_commodities_by_state <- read.csv('C:/Users/Stella.Koh/OneDrive - USDA/2022 Ag Census/reduced_commodity_pct_by_state.csv')

lollipop_chart <- function(variable = "commodities", state_input = NA, size_input = "All") {
  # variable: commodities, demographics
  # state_input: state name or NA (for US total)
  # size_input: All, Not small, Small (for demographics only)
  
  if (variable == "commodities") { 
    # Filtering commodities dataset
    
    if (!is.na(state_input)) {
      
      # Filters by state
      filtered <- reduced_commodities_by_state %>% 
        filter(state_name == state_input)

      # Record omitted data
      omitted <- filtered %>% 
        filter(is.na(Value_2022) | is.na(Value_2017)) %>% 
        select(short_desc) %>% 
        unlist() %>% 
        paste(collapse = "; ")
      if(omitted == "") {omitted <- "none"}
      
      filtered <- filtered %>% 
        na.omit() %>% 
        slice(1:n)
      
    } else {
      
      demo <- readRDS("C:/Users/Stella.Koh/USDA/AMS Grants Division (GD) - Data Room/Division Wide/Hackathon 2024/ready_for_app/state_demographics.rds")
      
      # Aggregating all states to get national totals
      filtered <- reduced_commodities_by_state %>% 
        group_by(short_desc) %>% 
        mutate(Value_2022 = sum(Value_2022, na.rm = TRUE),
               Value_2017 = sum(Value_2017, na.rm = TRUE)) %>%
        slice(1) %>% 
        select(short_desc, Value_2017, Value_2022) 
      
      # Recording omitted data
      omitted <- filtered %>% 
        filter(Value_2017 == 0 | Value_2022 == 0) %>% 
        select(short_desc) %>% 
        unlist() %>% 
        paste(collapse = "; ")
      
      # Finish filtering data
      filtered <- filtered %>% 
        filter(Value_2017 > 0, Value_2022 > 0) %>% 
        mutate(change_pct = round((Value_2022 - Value_2017)/Value_2017 * 100, 4)) %>% 
        ungroup() %>% 
        arrange(desc(Value_2022)) %>% 
        slice(1:n)
    }
    
    # Lollipop chart -- unique elements
    lollipop <- ggplot(filtered) +
      geom_segment(aes(x=reorder(short_desc, Value_2022), xend=short_desc, y=Value_2017, yend=Value_2022, color = change_pct), linewidth = 1.5)
    sub_label <- paste("TOP", n, "COMMODITY SALES IN", ifelse(is.na(state_input), "THE US", state_input), "FROM 2017 TO 2022")
    y_label <- "Sales, Measured in $"
    
  } else {
    
    # Filtering demographics dataset
    
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
    
    # Applying filters
    if (!is.na(state_input)) {
      filtered <- demo %>% 
        filter(state_name == state_input,
               farm_size == size_input) 

      # Record omitted data
      omitted <- filtered %>% 
        filter(is.na(change_pct)) %>% 
        select(short_desc) %>% unlist() %>% 
        paste(collapse = "; ")
      
      filtered <- filtered %>% 
        mutate_at(c("Value_2017","Value_2022","change","change_pct"), ~replace_na(.,0))
    } else {
      filtered <- demo %>% 
        filter(farm_size == size_input) %>% 
        group_by(short_desc) %>% 
        mutate(Value_2022 = sum(Value_2022, na.rm = TRUE),
               Value_2017 = sum(Value_2017, na.rm = TRUE)) %>%
        slice(1) %>% 
        select(short_desc, Value_2017, Value_2022) %>% 
        filter(Value_2017 > 0, Value_2022 > 0) %>% 
        mutate(change_pct = round((Value_2022 - Value_2017)/Value_2017 * 100, 4))
    }
    
    lollipop <- ggplot(filtered) +
      geom_segment(aes(x=factor(short_desc, levels = rev(all)), xend=short_desc, y=Value_2017, yend=Value_2022, color = change_pct), linewidth = 1.5)
    sub_label <- paste(ifelse(is.na(state_input), "US", state_input), "DEMOGRAPHICS, PCT CHANGE FROM 2017 TO 2022")
    y_label <- "Number of Producers"
  }
  
  # Adding on to lollipop chart
  lollipop <- lollipop +
    scale_color_gradient2(low = "darkred", mid = "#FFFF33", high = "darkgreen", midpoint = 0) +
    geom_point(aes(x=short_desc, y=Value_2017, color=change_pct), size=4, shape = '|', show.legend = TRUE) +
    geom_point(aes(x=short_desc, y=Value_2022, color=change_pct), size=4, show.legend = TRUE)  +
    coord_flip()+
    labs(title = "Year Over Year Change",
         subtitle = sub_label) +
    xlab("") +
    ylab(y_label) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale())
    ) + 
    theme_ipsum() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank())
  lollipop
  
  # To Add: information icon detailing omitted variables
  # paste("The following variables were omitted due to missing information:", omitted)
}

