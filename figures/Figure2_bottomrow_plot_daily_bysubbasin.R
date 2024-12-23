# Load necessary libraries
library(tidyverse)
library(patchwork)

# Load data
load("data/NOAA_ssta_5km_COSTR.RData")

# Make plots
p_CherryPoint <- COSTR_AQRES_ssta %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_point(aes(x=date, y=COSTR_map_index_CPAR_1_1.4,
                 color=COSTR_map_index_CPAR_1_1.4))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_hline(yintercept = 1,color="goldenrod2")+
  geom_hline(yintercept = 2,color="darkorange")+
  geom_hline(yintercept = 3,color="firebrick3")+
  geom_hline(yintercept = 4,color="darkred")+
  ggtitle("Cherry Point")+ylim(c(-3.5,5.5))+
  scale_color_viridis_c(option="inferno",direction=-1,limits=c(0,5.5))
  
p_Cypress <- COSTR_AQRES_ssta %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_point(aes(x=date, y=COSTR_map_index_CIAR_1_1.6,
                 color=COSTR_map_index_CIAR_1_1.6))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_hline(yintercept = 1,color="goldenrod2")+
  geom_hline(yintercept = 2,color="darkorange")+
  geom_hline(yintercept = 3,color="firebrick3")+
  geom_hline(yintercept = 4,color="darkred")+
  ggtitle("Cypress Island")+ylim(c(-3.5,5.5))+
  scale_color_viridis_c(option="inferno",direction=-1,limits=c(0,5.5))

p_SmithMinor <- COSTR_AQRES_ssta %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_point(aes(x=date, y=COSTR_map_index_SIAR_1_1.1,
                 color=COSTR_map_index_SIAR_1_1.1))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_hline(yintercept = 1,color="goldenrod2")+
  geom_hline(yintercept = 2,color="darkorange")+
  geom_hline(yintercept = 3,color="firebrick3")+
  geom_hline(yintercept = 4,color="darkred")+
  ggtitle("Smith & Minor")+ylim(c(-3.5,5.5))+
  scale_color_viridis_c(option="inferno",direction=-1,limits=c(0,5.5))

p_Coast <- COSTR_AQRES_ssta %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        axis.title.y=element_text(size=18),
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))+
  geom_point(aes(x=date, y=COSTR_map_index_17.3,
                 color=COSTR_map_index_17.3))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_hline(yintercept = 1,color="goldenrod2")+
  geom_hline(yintercept = 2,color="darkorange")+
  geom_hline(yintercept = 3,color="firebrick3")+
  geom_hline(yintercept = 4,color="darkred")+
  ggtitle("Coast")+ylim(c(-3.5,5.5))+
  scale_color_viridis_c(option="inferno",direction=-1,limits=c(0,5.5))+
  scale_y_continuous(limits=c(-3,5.5),breaks=c(-2,0,2,4),
                     name = "Temperature anomaly (°C)")
  
p_EStrait <- COSTR_AQRES_ssta %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_point(aes(x=date, y=COSTR_map_index_4.2,
                 color=COSTR_map_index_4.2))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_hline(yintercept = 1,color="goldenrod2")+
  geom_hline(yintercept = 2,color="darkorange")+
  geom_hline(yintercept = 3,color="firebrick3")+
  geom_hline(yintercept = 4,color="darkred")+
  ggtitle("Eastern Strait")+ylim(c(-3.5,5.5))+
  scale_color_viridis_c(option="inferno",direction=-1,limits=c(0,5.5))

p_WStrait <- COSTR_AQRES_ssta %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_point(aes(x=date, y=COSTR_map_index_12.1,
                 color=COSTR_map_index_12.1))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_hline(yintercept = 1,color="goldenrod2")+
  geom_hline(yintercept = 2,color="darkorange")+
  geom_hline(yintercept = 3,color="firebrick3")+
  geom_hline(yintercept = 4,color="darkred")+
  ggtitle("Western Strait")+ylim(c(-3.5,5.5))+
  scale_color_viridis_c(option="inferno",direction=-1,limits=c(0,5.5))

#####
daily_ssta_bysubbasin_2011_2018 <- p_Coast+
  scale_x_datetime(name="Year",
                   limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                   expand=c(0,0))+
  p_WStrait+scale_x_datetime(name="Year",
                             limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                                      as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                             expand=c(0,0))+
  p_EStrait+scale_x_datetime(name="Year",
                             limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                                      as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                             expand=c(0,0))+
  p_SmithMinor+ scale_x_datetime(name="Year",
                                limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                                         as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                                expand=c(0,0))+
  p_Cypress+ scale_x_datetime(name="Year",
                              limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                                      as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                              expand=c(0,0))+
  p_CherryPoint+  scale_x_datetime(name="Year",
                                   limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"), 
                                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                                   expand=c(0,0))+
  plot_layout(ncol=6) 

#####
jpeg(filename="figures/Figure2_bottomrow_daily_ssta_bysubbasin_2011_2018.jpg",
     width = 15, height=4,units="in",res=300)
daily_ssta_bysubbasin_2011_2018
dev.off()
