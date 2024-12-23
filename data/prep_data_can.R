## Load packages and data
library(tidyverse)
library(wesanderson)

# Kelp data
kelp_ar_s_l <- read.csv("data/kelp_data_AQRES.csv",row.names = "X")
kelp_s_l <- read.csv("data/kelp_data_COSTR.csv",row.names = "X.1") %>% 
  select(-X)
#Temperature data
COSTR_AQRES_sst <- read.csv("data/NOAA_sst_5km_COSTR.csv") %>% 
  select(-X)
COSTR_AQRES_ssta <- read.csv("data/NOAA_ssta_5km_COSTR.csv") %>% 
  select(-X)

## Set colors
reg_cols <- c("Eastern Strait"=wes_palette("FantasticFox1", 10, type = "continuous")[3], 
              "Western Strait"=wes_palette("FantasticFox1", 10, type = "continuous")[4],
              "Coast"=wes_palette("FantasticFox1", 10, type = "continuous")[5],
              "Smith and Minor Island AR"=wes_palette("FantasticFox1", 10, type = "continuous")[2],
              "Cypress Island AR"="#DB6413",
              "Cherry Point AR"=wes_palette("FantasticFox1", 10, type = "continuous")[10])
reg_cols_cont <- wes_palette("FantasticFox1", 150, type = "continuous")

###############
COSTR_AQRES_ssta$COSTR_map_index_CPAR_1_1.1 <- COSTR_AQRES_ssta$COSTR_map_index_CPAR_1_1.4
COSTR_AQRES_ssta$COSTR_map_index_CPAR_1_1.2 <- COSTR_AQRES_ssta$COSTR_map_index_CPAR_1_1.4
COSTR_AQRES_ssta$COSTR_map_index_CPAR_1_1.3 <- COSTR_AQRES_ssta$COSTR_map_index_CPAR_1_1.4

COSTR_AQRES_sst$COSTR_map_index_CPAR_1_1.1 <- COSTR_AQRES_sst$COSTR_map_index_CPAR_1_1.4
COSTR_AQRES_sst$COSTR_map_index_CPAR_1_1.2 <- COSTR_AQRES_sst$COSTR_map_index_CPAR_1_1.4
COSTR_AQRES_sst$COSTR_map_index_CPAR_1_1.3 <- COSTR_AQRES_sst$COSTR_map_index_CPAR_1_1.4


COSTR_AQRES_ssta_long <- COSTR_AQRES_ssta %>% 
  pivot_longer(cols=c(1:99,101:103),names_to="location",values_to="tempa") %>% 
  rename(t=date)
COSTR_AQRES_sst_long <- COSTR_AQRES_sst %>% 
  pivot_longer(cols=c(1:98,100:102),names_to="location",values_to="temp") %>% 
  rename(t=date)

save(COSTR_AQRES_sst_long, file = "data/COSTR_AQRES_sst_long.RData")
save(COSTR_AQRES_ssta_long, file = "data/COSTR_AQRES_ssta_long.RData")

COSTR_AQRES_sst_ssta <- left_join(COSTR_AQRES_sst_long,
                                  COSTR_AQRES_ssta_long,by=c("t","location")) %>% 
  mutate(clim=temp-tempa)
write.csv(COSTR_AQRES_sst_ssta,"data/COSTR_AQRES_sst_ssta.csv",row.names = FALSE)

COSTR_AQRES_sst_ssta_mon <- COSTR_AQRES_sst_ssta %>% 
  mutate(Date = strftime(t, "%Y-%m")) %>% # extract the year-month
  mutate(Year = strftime(t, "%Y")) %>%  # extract the year
  mutate(Month=as.numeric(substr(Date,6,7))) %>% # extract the month
  group_by(Date,location) %>%  # group by month and zone
  mutate(days_over_15C_TF = case_when(is.na(temp) ~ NA, # calculate monthly days>15C
                                      temp > 15 ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(days_over_17C_TF = case_when(is.na(temp) ~ NA, # calculate monthly days>17C
                                      temp > 17 ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(days_over_clim_TF = case_when(is.na(temp) ~ NA, # calculate monthly days>clim
                                       tempa > 0 ~ 1,
                                       TRUE ~ 0)) %>% 
  mutate(days_over_climplus1_TF = case_when(is.na(temp) ~ NA, # calculate monthly days>clim+1
                                            tempa > 1 ~ 1,
                                            TRUE ~ 0)) %>% 
  mutate(days_under_clim_TF = case_when(is.na(temp) ~ NA, # calculate monthly days<clim
                                        tempa < 0 ~ 1,
                                        TRUE ~ 0)) %>% 
  mutate(days_na_TF = case_when(is.na(temp) ~ 1, # calculate number days with NA
                                TRUE ~ 0)) %>% 
  reframe(month = Month,
            year = Year,
            mean_temp=mean(temp,na.rm=TRUE), # calc monthly mean temp
            days_over_15C=sum(days_over_15C_TF,na.rm=TRUE),
            days_over_17C=sum(days_over_17C_TF,na.rm=TRUE),
            max_temp=max(temp,na.rm=TRUE),
            min_temp=min(temp,na.rm=TRUE),
            mean_tempa=mean(tempa,na.rm=TRUE),
            max_tempa=max(tempa,na.rm=TRUE),
            min_tempa=min(tempa,na.rm=TRUE),
            mean_clim=mean(clim,na.rm=TRUE),
            days_over_clim=sum(days_over_clim_TF,na.rm=TRUE),
            days_under_clim=sum(days_under_clim_TF,na.rm=TRUE),
            days_over_climplus1=sum(days_over_climplus1_TF,na.rm=TRUE)) %>% 
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA)

COSTR_AQRES_sst_ssta_mon_mmt <- COSTR_AQRES_sst_ssta_mon %>% # calculate the max monthly temp for last year
  mutate(Year_seas=case_when(month==12 ~ as.numeric(year)+1,
                             month==1 ~ as.numeric(year),
                             month==2 ~ as.numeric(year),
                             month==3 ~ as.numeric(year),
                             month==4 ~ as.numeric(year),
                             month==5 ~ as.numeric(year),
                             month==6 ~ as.numeric(year),
                             month==7 ~ as.numeric(year),
                             month==8 ~ as.numeric(year),
                             month==9 ~ as.numeric(year)+1,
                             month==10 ~ as.numeric(year)+1,
                             month==11 ~ as.numeric(year)+1)) %>%   
  group_by(Year_seas,location) %>% # group by season for preceding year
  summarize(max_mean_temp=max(mean_temp,na.rm=TRUE)) %>% # calc max mean temp for last year
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA)


COSTR_AQRES_sst_ssta_seas <- COSTR_AQRES_sst_ssta %>% 
  mutate(Date = strftime(t, "%Y-%m")) %>% 
  mutate(Year = as.numeric(strftime(t, "%Y"))) %>% 
  mutate(Month=as.numeric(substr(Date,6,7))) %>% 
  mutate(season=case_when(Month==12 ~ "winter",
                          Month==1 ~ "winter",
                          Month==2 ~ "winter",
                          Month==3 ~"spring",
                          Month==4 ~"spring",
                          Month==5 ~"spring",
                          Month==6 ~"summer",
                          Month==7 ~"summer",
                          Month==8 ~"summer",
                          Month==9~"fall",
                          Month==10~"fall",
                          Month==11~"fall")) %>% 
  mutate(Year_seas=case_when(Month==12 ~ Year+1,
                             Month==1 ~ Year,
                             Month==2 ~ Year,
                             Month==3 ~ Year,
                             Month==4 ~ Year,
                             Month==5 ~ Year,
                             Month==6 ~ Year,
                             Month==7 ~ Year,
                             Month==8 ~ Year,
                             Month==9 ~ Year+1,
                             Month==10 ~ Year+1,
                             Month==11 ~ Year+1)) %>%   
  group_by(season,Year_seas,location) %>% 
  mutate(days_over_15C_TF = case_when(is.na(temp) ~ NA,
                                      temp > 15 ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(days_over_17C_TF = case_when(is.na(temp) ~ NA,
                                      temp > 17 ~ 1,
                                      TRUE ~ 0)) %>%   
  mutate(days_over_clim_TF = case_when(is.na(tempa) ~ NA,
                                       tempa > 0 ~ 1,
                                       TRUE ~ 0)) %>%   
  mutate(days_over_climplus1_TF = case_when(is.na(tempa) ~ NA,
                                            tempa > 1 ~ 1,
                                            TRUE ~ 0)) %>%    
  mutate(days_under_clim_TF = case_when(is.na(tempa) ~ NA,
                                        tempa < 0 ~ 1,
                                        TRUE ~ 0)) %>%  
  mutate(days_na_TF = case_when(is.na(temp) ~ 1,
                                TRUE ~ 0)) %>% 
  summarize(mean_temp=mean(temp, na.rm=TRUE),
            days_na = sum(days_na_TF, na.rm=TRUE),
            days_over_15C=sum(days_over_15C_TF, na.rm=TRUE),
            days_over_17C=sum(days_over_17C_TF, na.rm=TRUE),
            max_temp=max(temp,na.rm = TRUE),
            min_temp=min(temp,na.rm = TRUE),
            mean_tempa=mean(tempa,na.rm = TRUE),
            max_tempa=max(tempa,na.rm = TRUE),
            min_tempa=min(tempa,na.rm = TRUE),
            mean_clim=mean(clim,na.rm = TRUE),
            days_over_clim=sum(days_over_clim_TF, na.rm=TRUE),
            days_under_clim=sum(days_under_clim_TF, na.rm=TRUE),
            days_over_climplus1=sum(days_over_climplus1_TF, na.rm=TRUE)) %>% 
  mutate(map_index=gsub("COSTR_map_index_","",location)) %>% 
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA)


COSTR_AQRES_sst_ssta_seas_w0 <- COSTR_AQRES_sst_ssta_seas %>% 
  pivot_wider(id_cols = c(Year_seas,location),names_from=c(season),
              values_from=c(mean_temp:days_over_climplus1)) %>% 
  mutate(map_index=gsub("COSTR_map_index_","",location)) %>% 
  mutate(Year_seas=as.integer(Year_seas))

COSTR_AQRES_sst_ssta_seas_w <- left_join(COSTR_AQRES_sst_ssta_seas_w0,COSTR_AQRES_sst_ssta_mon_mmt,by=c("Year_seas","location"))

#################

kelp_s_lca<-kelp_s_l %>% #dplyr::select(-c(X)) %>% 
  filter(kelp_sp=="tot_can")

kelp_ar_s_lca <- kelp_ar_s_l %>% 
  filter(kelp_sp=="tot_ne_can") %>% 
  mutate(kelp_sp=gsub("_ne", "", kelp_sp))

kelp_s_lca_all <- rbind(kelp_s_lca,kelp_ar_s_lca) %>% 
  rename(Year=year_) %>% 
  mutate(Year_seas=Year) %>% 
  mutate(map_index=gsub("-","_",map_index)) %>% 
  mutate(map_index=gsub(",","_",map_index))

coastonly <- c(unique(kelp_s_lca_all$map_index[kelp_s_lca_all$region=="Coast"]),"16.4")

#######

kelpca_temp <- left_join(kelp_s_lca_all, COSTR_AQRES_sst_ssta_seas_w,
                         by = c("map_index","Year_seas"))

nofk <- c("COSTR_map_index_24.1","COSTR_map_index_24.2", 
          "COSTR_map_index_25.1", "COSTR_map_index_26.1", 
          "COSTR_map_index_27.1", "COSTR_map_index_28.1",
          "COSTR_map_index_29.1", "COSTR_map_index_30.1",
          "COSTR_map_index_31.1", "COSTR_map_index_32.1",
          "COSTR_map_index_33.1", "COSTR_map_index_34.1",
          "COSTR_map_index_35.1", "COSTR_map_index_36.1",
          "COSTR_map_index_37.1", "COSTR_map_index_38.1", 
          "COSTR_map_index_39.1", "COSTR_map_index_40.1",
          "COSTR_map_index_41.1")

kelpca_tempf <- kelpca_temp %>% 
  filter(!location %in% nofk)

kelpca_tempfp <- kelpca_tempf %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  right_join(.,kelpca_tempf,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax == 1 ~ 0.99,
                                TRUE ~ ca_permax))
kelpca_tempfp_0 <- kelpca_tempfp


kelpca_tempfp1118_0 <- kelpca_tempfp %>% 
  filter(Year>2010&Year<2019) %>% 
  mutate(map_index=as.factor(map_index)) %>% 
  mutate(region=as.factor(region)) 
###
kelpca_tempfp1118 <- kelpca_tempfp1118_0 %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  left_join(.,kelpca_tempfp1118_0,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca.x) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax > 0.999 ~ 0.99,
                                TRUE ~ ca_permax))

kelpca_tempfp1118 <- kelpca_tempfp1118 %>% 
  filter(max_ca.x>0) %>%  # remove all map indices that have 0 floating kelp during 11-18
  filter(map_index!=2.1) %>% # floating kelp only present in one year
  filter(map_index!=4.1) %>% # floating kelp only present in one year
  filter(map_index!=20.1) %>% # floating kelp only present in two years
  filter(map_index!=23.2) %>% # floating kelp only present in three years
  rowwise() %>% 
  mutate(MAX_tempa=max(c(max_tempa_fall,max_tempa_spring,
                         max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(MEAN_tempa=mean(c(max_tempa_fall,max_tempa_spring,
                           max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(SUM_daysoverclim=(days_over_clim_fall+days_over_clim_spring+
                             days_over_clim_summer+days_over_clim_winter))%>% 
  mutate(SUM_daysoverclimplus1=(days_over_climplus1_fall+days_over_climplus1_spring+
                             days_over_climplus1_summer+days_over_climplus1_winter))%>% 
  mutate(SUM_daysover15C=(days_over_15C_fall+days_over_15C_spring+
                                  days_over_15C_summer+days_over_15C_winter))%>%   
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA) %>% 
  filter(!is.na(map_index)) %>% 
  filter(!is.na(max_mean_temp))
###

kelpca_tempfp <- kelpca_tempfp_0 %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  left_join(.,kelpca_tempfp_0,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca.x) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax > 0.999 ~ 0.99,
                                TRUE ~ ca_permax))

kelpca_tempfp <- kelpca_tempfp %>% 
  filter(max_ca.x>0) %>%  # remove all map indices that have 0 floating kelp 
  rowwise() %>% 
  mutate(MAX_tempa=max(c(max_tempa_fall,max_tempa_spring,
                         max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(MEAN_tempa=mean(c(max_tempa_fall,max_tempa_spring,
                           max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(SUM_daysoverclim=(days_over_clim_fall+days_over_clim_spring+
                             days_over_clim_summer+days_over_clim_winter))%>% 
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA) %>% 
  filter(!is.na(map_index)) %>% 
  filter(!is.na(max_mean_temp))

###
save(file="analyses/prepped_data_ca.RData",list=c("kelpca_tempfp1118","kelpca_tempfp",
                                                  "reg_cols_cont",
                                                  "reg_cols", 'kelpca_tempf'))
######################################################################
#################

kelp_s_lca_ne<-kelp_s_l %>% 
  filter(kelp_sp=="tot_ne_can") %>% 
  rename(Year_seas=year_) %>% 
  mutate(map_index=as.character(map_index))
kelp_s_lca_ma<-kelp_s_l %>% 
  filter(kelp_sp=="tot_ma_can") %>% 
  rename(Year_seas=year_) %>% 
  mutate(map_index=as.character(map_index))

#######
kelpca_ne_temp <- left_join(kelp_s_lca_ne, COSTR_AQRES_sst_ssta_seas_w, 
                         by = c("map_index","Year_seas"))
kelpca_ma_temp <- left_join(kelp_s_lca_ma, COSTR_AQRES_sst_ssta_seas_w, 
                            by = c("map_index","Year_seas"))

nofk <- c("COSTR_map_index_24.1","COSTR_map_index_24.2", 
          "COSTR_map_index_25.1", "COSTR_map_index_26.1", 
          "COSTR_map_index_27.1", "COSTR_map_index_28.1",
          "COSTR_map_index_29.1", "COSTR_map_index_30.1",
          "COSTR_map_index_31.1", "COSTR_map_index_32.1",
          "COSTR_map_index_33.1", "COSTR_map_index_34.1",
          "COSTR_map_index_35.1", "COSTR_map_index_36.1",
          "COSTR_map_index_37.1", "COSTR_map_index_38.1", 
          "COSTR_map_index_39.1", "COSTR_map_index_40.1",
          "COSTR_map_index_41.1")

kelpca_ne_tempf <- kelpca_ne_temp %>% 
  filter(!location %in% nofk)
kelpca_ma_tempf <- kelpca_ma_temp %>% 
  filter(!location %in% nofk)

kelpca_ne_tempfp <- kelpca_ne_tempf %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  right_join(.,kelpca_ne_tempf,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax == 1 ~ 0.99,
                                TRUE ~ ca_permax))
kelpca_ne_tempfp_0 <- kelpca_ne_tempfp

kelpca_ma_tempfp <- kelpca_ma_tempf %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  right_join(.,kelpca_ma_tempf,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax == 1 ~ 0.99,
                                TRUE ~ ca_permax))
kelpca_ma_tempfp_0 <- kelpca_ma_tempfp


kelpca_ne_tempfp1118_0 <- kelpca_ne_tempfp %>% 
  rename(Year=Year_seas) %>% 
  filter(Year>2010&Year<2019) %>% 
  mutate(map_index=as.factor(map_index)) %>% 
  mutate(region=as.factor(region)) 
kelpca_ma_tempfp1118_0 <- kelpca_ma_tempfp %>% 
  rename(Year=Year_seas) %>% 
  filter(Year>2010&Year<2019) %>% 
  mutate(map_index=as.factor(map_index)) %>% 
  mutate(region=as.factor(region)) 


###
kelpca_ne_tempfp1118 <- kelpca_ne_tempfp1118_0 %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  left_join(.,kelpca_ne_tempfp1118_0,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca.x) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax > 0.999 ~ 0.99,
                                TRUE ~ ca_permax))
kelpca_ma_tempfp1118 <- kelpca_ma_tempfp1118_0 %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  left_join(.,kelpca_ma_tempfp1118_0,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca.x) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax > 0.999 ~ 0.99,
                                TRUE ~ ca_permax))

kelpca_ne_tempfp1118 <- kelpca_ne_tempfp1118 %>% 
  filter(max_ca.x>0) %>%  # remove all map indices that have 0 floating kelp during 11-18
  filter(map_index!=2.1) %>% # floating kelp only present in one year
  filter(map_index!=4.1) %>% # floating kelp only present in one year
  filter(map_index!=20.1) %>% # floating kelp only present in two years
  filter(map_index!=23.2) %>% # floating kelp only present in three years
  rowwise() %>% 
  mutate(MAX_tempa=max(c(max_tempa_fall,max_tempa_spring,
                         max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(MEAN_tempa=mean(c(max_tempa_fall,max_tempa_spring,
                           max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(SUM_daysoverclim=(days_over_clim_fall+days_over_clim_spring+
                             days_over_clim_summer+days_over_clim_winter))%>% 
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA) %>% 
  filter(!is.na(map_index)) %>% 
  filter(!is.na(max_mean_temp))

kelpca_ma_tempfp1118 <- kelpca_ma_tempfp1118 %>% 
  filter(max_ca.x>0) %>%  # remove all map indices that have 0 floating kelp during 11-18
  filter(map_index!=2.1) %>% # floating kelp only present in one year
  filter(map_index!=4.1) %>% # floating kelp only present in one year
  filter(map_index!=20.1) %>% # floating kelp only present in two years
  filter(map_index!=22.1) %>% # floating kelp only present in one years
  filter(map_index!=23.1) %>% # floating kelp only present in three years
  filter(map_index!=23.2) %>% # floating kelp only present in three years
  rowwise() %>% 
  mutate(MAX_tempa=max(c(max_tempa_fall,max_tempa_spring,
                         max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(MEAN_tempa=mean(c(max_tempa_fall,max_tempa_spring,
                           max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(SUM_daysoverclim=(days_over_clim_fall+days_over_clim_spring+
                             days_over_clim_summer+days_over_clim_winter))%>% 
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA) %>% 
  filter(!is.na(map_index)) %>% 
  filter(!is.na(max_mean_temp))

###
kelpca_ne_tempfp <- kelpca_ne_tempfp_0 %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  left_join(.,kelpca_ne_tempfp_0,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca.x) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax > 0.999 ~ 0.99,
                                TRUE ~ ca_permax))
kelpca_ma_tempfp <- kelpca_ma_tempfp_0 %>% 
  group_by(map_index) %>% 
  summarize(max_ca = max(cover,na.rm=TRUE)) %>% 
  left_join(.,kelpca_ma_tempfp_0,by="map_index") %>% 
  mutate(ca_permax = cover/max_ca.x) %>% 
  mutate(ca_permaxt = case_when(ca_permax == 0 ~ 0.01,
                                ca_permax > 0.999 ~ 0.99,
                                TRUE ~ ca_permax))

kelpca_ne_tempfp <- kelpca_ne_tempfp %>% 
  filter(max_ca.x>0) %>%  # remove all map indices that have 0 floating kelp 
  rowwise() %>% 
  mutate(MAX_tempa=max(c(max_tempa_fall,max_tempa_spring,
                         max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(MEAN_tempa=mean(c(max_tempa_fall,max_tempa_spring,
                           max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(SUM_daysoverclim=(days_over_clim_fall+days_over_clim_spring+
                             days_over_clim_summer+days_over_clim_winter))%>% 
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA) %>% 
  filter(!is.na(map_index)) %>% 
  filter(!is.na(max_mean_temp))

kelpca_ma_tempfp <- kelpca_ma_tempfp %>% 
  filter(max_ca.x>0) %>%  # remove all map indices that have 0 floating kelp 
  rowwise() %>% 
  mutate(MAX_tempa=max(c(max_tempa_fall,max_tempa_spring,
                         max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(MEAN_tempa=mean(c(max_tempa_fall,max_tempa_spring,
                           max_tempa_winter,max_tempa_summer),na.rm=TRUE)) %>% 
  mutate(SUM_daysoverclim=(days_over_clim_fall+days_over_clim_spring+
                             days_over_clim_summer+days_over_clim_winter))%>% 
  replace(.==Inf,NA) %>% 
  replace(.==-Inf,NA) %>% 
  filter(!is.na(map_index)) %>% 
  filter(!is.na(max_mean_temp))

###
save(file="analyses/prepped_data_ca_ma_ne.RData",list=c("kelpca_ne_tempfp1118","kelpca_ne_tempfp",
                                                        "kelpca_ma_tempfp1118","kelpca_ma_tempfp",
                                                        "kelpca_ne_tempf","kelpca_ma_tempf",
                                                  "reg_cols_cont", "reg_cols"))
