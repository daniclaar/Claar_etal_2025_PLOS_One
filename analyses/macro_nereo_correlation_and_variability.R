# Load packages
library(tidyverse)
library(car)

# Load data
load("analyses/prepped_data_ca_ma_ne.RData")

macro <- kelpca_ma_tempfp %>% 
  select(c(map_index,Year_seas,cover))

nereo <- kelpca_ne_tempfp %>% 
  select(c(map_index,Year_seas,cover))

bothsp <- full_join(macro,nereo,by=c("map_index","Year_seas"),
                    suffix = c("_macro", "_nereo")) %>% 
  filter(!is.na(cover_macro)) %>% 
  filter(!is.na(cover_nereo)) 

# Correlation test included in manuscript:
cor.test(bothsp$cover_macro,bothsp$cover_nereo)

####
# Test for normality
shapiro.test(bothsp$cover_macro) # very not normal
shapiro.test(bothsp$cover_nereo) # very not normal
# visualize normality
hist(bothsp$cover_macro) # didn't really even need the test
hist(bothsp$cover_nereo) # didn't really even need the test

bothspl <- pivot_longer(bothsp,names_to = "species", values_to = "cover",
             cols=c("cover_macro","cover_nereo"))

# Test for homogeneity of variances
fligner.test(cover ~ species,data = bothspl)
# Chose this test because it is the most robust against departures from normality
# From Rdocumentation.org, fligner.test references Conover, Johnson & Johnson

###############################

bothsp1118 <- bothsp %>% 
  filter(Year_seas>2010 & Year_seas < 2019)
cor.test(bothsp1118$cover_macro,bothsp1118$cover_nereo)

####
# Test for normality
shapiro.test(bothsp1118$cover_macro) # very not normal
shapiro.test(bothsp1118$cover_nereo) # very not normal
# visualize normality
hist(bothsp1118$cover_macro) # didn't really even need the test
hist(bothsp1118$cover_nereo) # didn't really even need the test

bothsp1118l <- pivot_longer(bothsp1118,names_to = "species", values_to = "cover",
                        cols=c("cover_macro","cover_nereo"))

# Check Mean percentage of maximum canopy area observed (2011-2018)
mean(kelpca_ma_tempfp1118$ca_permax,na.rm = TRUE)
mean(kelpca_ne_tempfp1118$ca_permax,na.rm = TRUE)
sd(kelpca_ma_tempfp1118$ca_permax,na.rm = TRUE)
sd(kelpca_ne_tempfp1118$ca_permax,na.rm = TRUE)

kelpca_ne_ma_tempfp1118 <- rbind(kelpca_ne_tempfp1118,kelpca_ma_tempfp1118)

# Test for homogeneity of variances
leveneTest(ca_permax ~ kelp_sp,data = kelpca_ne_ma_tempfp1118)
fligner.test(ca_permax ~ kelp_sp,data = kelpca_ne_ma_tempfp1118) # same result, not reported in ms

# Are the two species different?
wilcox.test(ca_permax ~ kelp_sp,data = kelpca_ne_ma_tempfp1118) # yes
