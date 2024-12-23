# Load packages
library(tidyverse)

# Load data
load("analyses/prepped_data_ca.RData")
load("analyses/prepped_data_bed.RData")

### Table 1 ##
kelpca_tempf %>% 
  filter(Year>2010&Year<2019) %>% 
  group_by(region,Year) %>% 
  mutate(cov_tot=sum(cover)) %>% 
  select(region,Year,cov_tot) %>% 
  ungroup(Year) %>% 
  summarize(max_can=max(cov_tot),
            min_can=min(cov_tot),
            mean_can=mean(cov_tot),
            sd_can=sd(cov_tot),
            fold=max_can/min_can)

#####
# kelppl_tempfp1118 %>%
#   group_by(Year,region) %>%
#   summarize(tot_cov=sum(cover)) %>%
#   group_by(region) %>%
#   summarize(max_can=max(tot_cov),
#             min_can=min(tot_cov),
#             mean_can=mean(tot_cov),
#             sd_can=sd(tot_cov),
#             fold=max_can/min_can)
# 
# 
# kelpca_tempfp %>% 
#   group_by(Year,region) %>% 
#   summarize(tot_cov=sum(cover)) %>% 
#   group_by(region) %>% 
#   summarize(max_can=max(tot_cov),
#             min_can=min(tot_cov),
#             mean_can=mean(tot_cov),
#             sd_can=sd(tot_cov),
#             fold=max_can/min_can)
# 
# 
# kelppl_tempfp %>%
#   group_by(Year,region) %>%
#   summarize(tot_cov=sum(cover)) %>%
#   group_by(region) %>%
#   summarize(max_can=max(tot_cov),
#             min_can=min(tot_cov),
#             mean_can=mean(tot_cov),
#             sd_can=sd(tot_cov),
#             fold=max_can/min_can)
# 
