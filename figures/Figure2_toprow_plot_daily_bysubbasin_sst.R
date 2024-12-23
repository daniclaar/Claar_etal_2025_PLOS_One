# Load necessary libraries
library(tidyverse)
library(patchwork)
library(scales)

# Load data
load("data/NOAA_sst_5km_COSTR.RData")

# Make plots
p_CherryPoint <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_rect(aes(xmin = as.POSIXct("2014-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2014-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_rect(aes(xmin = as.POSIXct("2017-06-01",format="%Y-%m-%d"),
                xmax = as.POSIXct("2017-09-31",format="%Y-%m-%d"),
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_point(aes(x=date, y=COSTR_map_index_CPAR_1_1.4,color=COSTR_map_index_CPAR_1_1.4))+
  geom_hline(yintercept = 12,color="darkgray")+
  geom_hline(yintercept = 14,color="goldenrod2")+
  geom_hline(yintercept = 15,color="darkorange")+
  geom_hline(yintercept = 16,color="firebrick")+
  geom_hline(yintercept = 17,color="darkred")+
  ggtitle("Cherry Point AR")+ylim(c(5,19))+
  scale_color_viridis_c(option="turbo",direction=1,limits=c(5,19))+
  scale_x_datetime(limits=c(as.POSIXct("1989-01-01",format="%Y-%m-%d"),
       as.POSIXct("2021-10-01",format="%Y-%m-%d")),expand=c(0,0))

p_CypressIsland <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_rect(aes(xmin = as.POSIXct("2014-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2014-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_rect(aes(xmin = as.POSIXct("2017-06-01",format="%Y-%m-%d"),
                xmax = as.POSIXct("2017-09-31",format="%Y-%m-%d"),
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_point(aes(x=date, y=COSTR_map_index_CIAR_1_1.6,color=COSTR_map_index_CIAR_1_1.6))+
  geom_hline(yintercept = 12,color="darkgray")+
  geom_hline(yintercept = 14,color="goldenrod2")+
  geom_hline(yintercept = 15,color="darkorange")+
  geom_hline(yintercept = 16,color="firebrick")+
  geom_hline(yintercept = 17,color="darkred")+
  ggtitle("Cypress Island AR")+ylim(c(5,19))+
  scale_color_viridis_c(option="turbo",direction=1,limits=c(5,19))+
  xlim(as.POSIXct("1989-01-01",format="%Y-%m-%d"),
       as.POSIXct("2021-10-01",format="%Y-%m-%d"))

p_Coast <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        axis.title.y=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.title.x=element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))+
  geom_rect(aes(xmin = as.POSIXct("2014-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2014-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_rect(aes(xmin = as.POSIXct("2017-06-01",format="%Y-%m-%d"),
                xmax = as.POSIXct("2017-09-31",format="%Y-%m-%d"),
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_point(aes(x=date, y=COSTR_map_index_17.3,color=COSTR_map_index_17.3))+
  geom_hline(yintercept = 12,color="darkgray")+
  geom_hline(yintercept = 14,color="goldenrod2")+
  geom_hline(yintercept = 15,color="darkorange")+
  geom_hline(yintercept = 16,color="firebrick")+
  geom_hline(yintercept = 17,color="darkred")+
  ggtitle("Coast")+
  scale_y_continuous(limits=c(5,19),breaks=c(6,8,10,12,14,16,18),
                     name = "Temperature (°C)")+
  scale_color_viridis_c(option="turbo",direction=1,limits=c(5,19))+
  scale_x_datetime(name="Year",limits=c(as.POSIXct("1989-01-01",format="%Y-%m-%d"),
                            as.POSIXct("2021-10-01",format="%Y-%m-%d")))

p_EStrait <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_rect(aes(xmin = as.POSIXct("2014-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2014-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_rect(aes(xmin = as.POSIXct("2016-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2016-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_point(aes(x=date, y=COSTR_map_index_4.2,color=COSTR_map_index_4.2))+
  geom_hline(yintercept = 12,color="darkgray")+
  geom_hline(yintercept = 14,color="goldenrod2")+
  geom_hline(yintercept = 15,color="darkorange")+
  geom_hline(yintercept = 16,color="firebrick")+
  geom_hline(yintercept = 17,color="darkred")+
  ggtitle("Eastern Strait")+ylim(c(5,19))+
  scale_color_viridis_c(option="turbo",direction=1,limits=c(5,19))+
  xlim(as.POSIXct("1989-01-01",format="%Y-%m-%d"),
       as.POSIXct("2021-10-01",format="%Y-%m-%d"))

p_SMI <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_rect(aes(xmin = as.POSIXct("2014-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2014-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_rect(aes(xmin = as.POSIXct("2016-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2016-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_point(aes(x=date, y=COSTR_map_index_SIAR_1_1.1,color=COSTR_map_index_SIAR_1_1.1))+
  geom_hline(yintercept = 12,color="darkgray")+
  geom_hline(yintercept = 14,color="goldenrod2")+
  geom_hline(yintercept = 15,color="darkorange")+
  geom_hline(yintercept = 16,color="firebrick")+
  geom_hline(yintercept = 17,color="darkred")+
  ggtitle("Smith & Minor Island AR")+ylim(c(5,19))+
  scale_color_viridis_c(option="turbo",direction=1,limits=c(5,19))+
  xlim(as.POSIXct("1989-01-01",format="%Y-%m-%d"),
       as.POSIXct("2021-10-01",format="%Y-%m-%d"))

p_WStrait <- COSTR_AQRES_sst %>% 
  ggplot()+theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank(),
        axis.title.x=element_text(size=18),plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),axis.text.x = element_text(size=14))+
  geom_rect(aes(xmin = as.POSIXct("2014-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2014-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_rect(aes(xmin = as.POSIXct("2015-06-01",format="%Y-%m-%d"), 
                xmax = as.POSIXct("2015-09-31",format="%Y-%m-%d"), 
                ymin = -Inf, ymax = Inf), fill="lightgray", alpha = 0.1) +
  geom_point(aes(x=date, y=COSTR_map_index_12.1,color=COSTR_map_index_12.1))+
  geom_hline(yintercept = 12,color="darkgray")+
  geom_hline(yintercept = 14,color="goldenrod2")+
  geom_hline(yintercept = 15,color="darkorange")+
  geom_hline(yintercept = 16,color="firebrick")+
  geom_hline(yintercept = 17,color="darkred")+
  ggtitle("Western Strait")+ylim(c(5,19))+
  scale_color_viridis_c(option="turbo",direction=1,limits=c(5,19))+
  xlim(as.POSIXct("1989-01-01",format="%Y-%m-%d"),
       as.POSIXct("2021-10-01",format="%Y-%m-%d"))

#####
daily_sst_bysubbasin_2011_2018 <- p_Coast+
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
  p_SMI+scale_x_datetime(name="Year",
                         limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"),
                                  as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                         expand=c(0,0))+
  p_CypressIsland+scale_x_datetime(name="Year",
                                   limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"),
                                            as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                                   expand=c(0,0))+
  p_CherryPoint+scale_x_datetime(name="Year",
                                 limits=c(as.POSIXct("2011-01-01",format="%Y-%m-%d"),
                                          as.POSIXct("2019-01-01",format="%Y-%m-%d")),
                                 expand=c(0,0))+
  plot_layout(ncol=6)


jpeg(filename="figures/Figure2_toprow_daily_sst_bysubbasin_2011_2018.jpg",
     width = 15, height=4,units="in",res=300)
daily_sst_bysubbasin_2011_2018
dev.off()

