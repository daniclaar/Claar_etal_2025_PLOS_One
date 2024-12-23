# Load necessary packages
library(tidyverse)

# Load necessary data
load("analyses/prepped_data_ca.RData")


# Table S2
# Coast and Strait 1989-2021
kelpca_tempfp %>% 
  filter(region!="Cherry Point AR") %>% 
  filter(region!="Smith and Minor Island AR") %>% 
  filter(region!="Cypress Island AR") %>% 
  group_by(region,Year) %>% 
  summarize(ca_area = sum(cover)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarize(min_ca_area = min(ca_area),
            mean_ca_area = mean(ca_area),
            sd_ca_area = sd(ca_area),
            max_ca_area = max(ca_area),
            fold_diff = max_ca_area/min_ca_area)

# Aquatic Reserves 2011-2021
kelpca_tempfp %>% 
  filter(region!="Coast") %>% 
  filter(region!="Eastern Strait") %>% 
  filter(region!="Western Strait") %>% 
  filter(Year > 2010) %>% 
  group_by(region,Year) %>% 
  summarize(ca_area = sum(cover)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarize(min_ca_area = min(ca_area),
            mean_ca_area = mean(ca_area),
            sd_ca_area = sd(ca_area),
            max_ca_area = max(ca_area),
            fold_diff = max_ca_area/min_ca_area)

