# Load necessary packages
library(tidyverse)

# Load necessary data
load("analyses/prepped_data_bed.RData")

# Table S1
# Coast and Strait 1989-2021
kelppl_tempfp %>% 
  filter(region!="Cherry Point AR") %>% 
  filter(region!="Smith and Minor Island AR") %>% 
  filter(region!="Cypress Island AR") %>% 
  group_by(region,Year) %>% 
  summarize(bed_area = sum(cover)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarize(min_bed_area = min(bed_area),
            mean_bed_area = mean(bed_area),
            sd_bed_area = sd(bed_area),
            max_bed_area = max(bed_area),
            fold_diff = max_bed_area/min_bed_area)

# Aquatic Reserves 2011-2021
kelppl_tempfp %>% 
  filter(region!="Coast") %>% 
  filter(region!="Eastern Strait") %>% 
  filter(region!="Western Strait") %>% 
  filter(Year > 2010) %>% 
  group_by(region,Year) %>% 
  summarize(bed_area = sum(cover)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarize(min_bed_area = min(bed_area),
            mean_bed_area = mean(bed_area),
            sd_bed_area = sd(bed_area),
            max_bed_area = max(bed_area),
            fold_diff = max_bed_area/min_bed_area)
