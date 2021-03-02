#!/usr/bin/env Rscript

library(readxl)
library(tidyverse)
library(ggpubr)
# library(here)
library(oildata)
library(extrafont)

# Read pre-2008 data
# data <- read_xlsx(here("data", "api_spilldata.xlsx"))
data <- read_xlsx("data/api_spilldata.xlsx")
data <- drop_na(data)
data <- select(data, -c(...2, original))
data <- pivot_longer(data, -year, names_to = "commodity")

recode_key = c(spill_crude_per_barrel_mile = "Crude", 
               spill_refined_per_barrel_mile = "Refined", 
               spill_total_per_barrel_mile = "Total")
data <- data %>%
    filter(commodity %in% names(recode_key)) %>%
    mutate(commodity = recode(commodity, !!! recode_key))

# Read new data
data_new <- pipelines_ungrouped %>%
    filter(on_offshore == "onshore") %>%
    filter(commodity %in% c("crude", "rpp")) %>%
    select(ID, year, commodity, incidents_volume, estimate_volume_all) %>%
    group_by(year, commodity) %>%
    summarize(incidents_volume = sum(incidents_volume, na.rm = T),
              estimate_volume_all = sum(estimate_volume_all, na.rm = T)) %>%
    pivot_wider(names_from = commodity, 
                values_from = c(incidents_volume, estimate_volume_all)) %>%
    mutate(incidents_volume_total = incidents_volume_crude + incidents_volume_rpp, 
           estimate_volume_all_total = estimate_volume_all_crude + 
               estimate_volume_all_rpp) %>%
    pivot_longer(matches("crude$|rpp$|total$"),
                 names_to = c(".value", "commodity"),
                 names_pattern = "(.*)_(.*)") %>%
    mutate(value = incidents_volume / estimate_volume_all * 1e9)

recode_key_2 <- c(crude = "Crude", 
                  rpp = "Refined", 
                  total = "Total")
data_new <- mutate(data_new, commodity = recode(commodity, !!! recode_key_2))

# Merge new & old
data_new$Source = "New"
data$Source = "Historic"
data_combined <- bind_rows(data_new, data)

# Plot\
this_theme <- theme(strip.text = element_text(size=12, family = "Times New Roman"), 
                    axis.title = element_text(size=12, family = "Times New Roman"),
                    plot.caption = element_text(size=12, family = "Times New Roman", hjust=0))
                    
caption = "Blue line:\t\t\t\t\t\t\t\t   Quadratic curve of best fit, with confidence interval.

Source (new):\t\t\t\t\t\t\t\t\t\t   https://github.com/julianbarg/oildata

Source (historic): http://www.api.org/environment-health-and-safety/clean-water/oil-spill-prevention-\n\t\t\t\tand-response/~/media/93371EDFB94C4B4D9C6BBC766F0C4A40.ashx, p. 38"

data_combined %>%
    filter(!(Source =="New" & year <= 2007)) %>%
    filter(commodity == "Refined") %>%
    ggplot(aes(x=year, y=value)) +
    facet_wrap(~ commodity) +
    geom_line() +
    geom_point(aes(shape=Source), size=2) +
    geom_point(aes(shape=Source, color=Source), size=1) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
    labs(x = "Year", 
         y = "Bbl spilled per Billion Barrel-Miles Transport", 
         caption = caption) +
    theme_minimal()

# ggsave(here("illustrations", "pipeline_spill_trend.png"), width = 10, height = 5)
ggsave("illustrations/pipeline_spill_trend.png", width = 10, height = 5)
