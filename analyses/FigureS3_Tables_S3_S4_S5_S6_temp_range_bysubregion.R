library(tidyverse)
library(patchwork)

load("data/NOAA_sst_5km_COSTR.RData")
load(file="analyses/prepped_data_ca.RData")

p_CherryPoint_all <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=14),axis.text.x = element_text(size=14))+
  geom_hline(yintercept = 14,color="darkgray")+
  geom_hline(yintercept = 15,color="goldenrod2")+
  geom_hline(yintercept = 16,color="darkorange")+
  geom_hline(yintercept = 17,color="firebrick")+
  geom_hline(yintercept = 18,color="darkred")+
  geom_line(aes(x=date, y=COSTR_map_index_CPAR_1_1.4),
            color="black")+
  ggtitle("Cherry Point")+
  scale_y_continuous(name="SST (°C)",limits=c(6,18),
                     breaks = c(6,8,10,12,14,16,18))+
  scale_x_datetime(limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                   expand=c(0,0))

p_CypressIsland_all <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=14),axis.text.x = element_text(size=14))+
  geom_hline(yintercept = 14,color="darkgray")+
  geom_hline(yintercept = 15,color="goldenrod2")+
  geom_hline(yintercept = 16,color="darkorange")+
  geom_hline(yintercept = 17,color="firebrick")+
  geom_hline(yintercept = 18,color="darkred")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.1),
                 color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.2),
                color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.3),
                color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.4),
                color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.5),
                color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.7),
                color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.8),
                color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.9),
                color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_CIAR_1_1.6),
            color="black")+
  ggtitle("Cypress Island")+
  scale_y_continuous(name="SST (°C)",limits=c(6,18),
                     breaks = c(6,8,10,12,14,16,18))+
  scale_x_datetime(limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                   expand=c(0,0))


p_SmithMinor_all <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=14),axis.text.x = element_text(size=14))+
  geom_hline(yintercept = 14,color="darkgray")+
  geom_hline(yintercept = 15,color="goldenrod2")+
  geom_hline(yintercept = 16,color="darkorange")+
  geom_hline(yintercept = 17,color="firebrick")+
  geom_hline(yintercept = 18,color="darkred")+
  geom_line(aes(x=date, y=COSTR_map_index_SIAR_1_1.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_SIAR_1_1.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_SIAR_1_1.4),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_SIAR_2_2.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_SIAR_2_2.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_SIAR_3_3.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_SIAR_3_3.2),
            color="gray")+
geom_line(aes(x=date, y=COSTR_map_index_SIAR_1_1.1),
          color="black")+
  ggtitle("Smith & Minor Islands")+
  scale_y_continuous(name="SST (°C)",limits=c(6,18),
                     breaks = c(6,8,10,12,14,16,18))+
  scale_x_datetime(limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                   expand=c(0,0))


p_EStrait_all <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_text(size=18),
        axis.title.x=element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=14),axis.text.x = element_text(size=14))+
  geom_hline(yintercept = 14,color="darkgray")+
  geom_hline(yintercept = 15,color="goldenrod2")+
  geom_hline(yintercept = 16,color="darkorange")+
  geom_hline(yintercept = 17,color="firebrick")+
  geom_hline(yintercept = 18,color="darkred")+
  geom_line(aes(x=date, y=COSTR_map_index_1.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_1.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_2.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_2.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_2.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_2.4),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_2.5),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_3.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_3.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_4.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_4.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_5.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_5.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_6.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_6.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_7.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_7.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_8.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_8.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_8.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_8.4),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_4.2),
            color="black")+
  ggtitle("Eastern Strait")+
  scale_y_continuous(name="SST (°C)",limits=c(6,18),
                     breaks = c(6,8,10,12,14,16,18))+
  scale_x_datetime(limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                   expand=c(0,0))


p_WStrait_all <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=14),axis.text.x = element_text(size=14))+
  geom_hline(yintercept = 14,color="darkgray")+
  geom_hline(yintercept = 15,color="goldenrod2")+
  geom_hline(yintercept = 16,color="darkorange")+
  geom_hline(yintercept = 17,color="firebrick")+
  geom_hline(yintercept = 18,color="darkred")+
  geom_line(aes(x=date, y=COSTR_map_index_9.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_9.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_9.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_10.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_10.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_10.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_10.4),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_10.5),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_10.6),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_11.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_11.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_11.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_13.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_13.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_14.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_15.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_15.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_15.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_15.4),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_16.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_16.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_12.1),
            color="black")+
  ggtitle("Western Strait")+
  scale_y_continuous(name="SST (°C)",limits=c(6,18),
                     breaks = c(6,8,10,12,14,16,18))+
  scale_x_datetime(limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                   expand=c(0,0))

p_Coast_all <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=14),axis.text.x = element_text(size=14))+
  geom_hline(yintercept = 14,color="darkgray")+
  geom_hline(yintercept = 15,color="goldenrod2")+
  geom_hline(yintercept = 16,color="darkorange")+
  geom_hline(yintercept = 17,color="firebrick")+
  geom_hline(yintercept = 18,color="darkred")+
  geom_line(aes(x=date, y=COSTR_map_index_16.3),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_16.4),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_17.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_17.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_18.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_18.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_19.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_20.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_20.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_21.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_21.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_22.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_22.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_23.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_23.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_25.1),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_25.2),
            color="gray")+
  geom_line(aes(x=date, y=COSTR_map_index_17.3),
            color="black")+
  ggtitle("Coast")+
  scale_y_continuous(name="SST (°C)",limits=c(6,18),
                     breaks = c(6,8,10,12,14,16,18))+
  scale_x_datetime(limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                   expand=c(0,0))

# Make jpeg
jpeg(filename="figures/FigureS3_SST_byregion.jpeg",width=14,height=14,units="in",res=300)
p_CherryPoint_all + p_CypressIsland_all + p_SmithMinor_all +  p_EStrait_all + p_WStrait_all + p_Coast_all +   plot_layout(nrow=6)
dev.off()

##############

# Table S3 - For representative zones
kelpca_tempfp1118 %>% 
  filter(map_index==17.3) %>% # Coast
  select(map_index,Year,max_mean_temp,MAX_tempa,SUM_daysoverclim)

kelpca_tempfp1118 %>% 
  filter(map_index==12.1) %>% # Western Strait
  select(map_index,Year,max_mean_temp,MAX_tempa,SUM_daysoverclim)

kelpca_tempfp1118 %>% 
  filter(map_index==4.2) %>% # Eastern Strait
  select(map_index,Year,max_mean_temp,MAX_tempa,SUM_daysoverclim)

kelpca_tempfp1118 %>% 
  filter(map_index=="SIAR_1_1.1") %>% # Smith and Minor
  select(map_index,Year,max_mean_temp,MAX_tempa,SUM_daysoverclim)

kelpca_tempfp1118 %>% 
  filter(map_index=="CIAR_1_1.6") %>% # Smith and Minor
  select(map_index,Year,max_mean_temp,MAX_tempa,SUM_daysoverclim)

kelpca_tempfp1118 %>% 
  filter(map_index=="CPAR_1_1.4") %>% # Smith and Minor
  select(map_index,Year,max_mean_temp,MAX_tempa,SUM_daysoverclim)


temp_summaries <- kelpca_tempfp1118 %>% 
  select(c(region,map_index,Year,max_mean_temp,MAX_tempa,SUM_daysoverclim)) %>% 
  group_by(region,Year) %>% 
  summarize(MAX_max_mean_temp=max(max_mean_temp),
            MEAN_max_mean_temp=mean(max_mean_temp),
            MIN_max_mean_temp=min(max_mean_temp),
            MAX_MAX_tempa=max(MAX_tempa),
            MEAN_MAX_tempa=mean(MAX_tempa),
            MIN_MAX_tempa=min(MAX_tempa),
            MAX_SUM_daysoverclim=max(SUM_daysoverclim),
            MEAN_SUM_daysoverclim=mean(SUM_daysoverclim),
            MIN_SUM_daysoverclim=min(SUM_daysoverclim))

# Table S4
temp_summaries_max_mean_temp <- temp_summaries %>% 
  select(region, Year, 
         MEAN_max_mean_temp,MIN_max_mean_temp, MAX_max_mean_temp)

# Table S5
temp_summaries_max_tempa <- temp_summaries %>% 
  select(region, Year, 
         MEAN_MAX_tempa, MIN_MAX_tempa, MAX_MAX_tempa)

# Table S6
temp_summaries_SUM_daysoverclim <- temp_summaries %>% 
  select(region, Year, 
         MEAN_SUM_daysoverclim, MIN_SUM_daysoverclim, MAX_SUM_daysoverclim)
