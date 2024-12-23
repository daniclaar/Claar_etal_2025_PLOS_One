# Load necessary libraries
library(tidyverse)
library(patchwork)

# Load data
load("analyses/prepped_data_ca_ma_ne.RData")

# Prep data
kelpca_ne_ma_tempfp <- rbind(kelpca_ne_tempfp,kelpca_ma_tempfp)

kelpca_ne_tempfp1118_WST <- kelpca_ne_tempfp1118 %>% 
  filter(region=="Western Strait")
kelpca_ne_tempfp1118_NCO <- kelpca_ne_tempfp1118 %>% 
  filter(region=="Coast")

kelpca_ma_tempfp1118_WST <- kelpca_ma_tempfp1118 %>% 
  filter(region=="Western Strait")
kelpca_ma_tempfp1118_NCO <- kelpca_ma_tempfp1118 %>% 
  filter(region=="Coast")

# Run models
mb1neWST <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_ne_tempfp1118_WST)
summary(mb1neWST) 
mb1neNCO <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_ne_tempfp1118_NCO)
summary(mb1neNCO) 

mb2neWST <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_ne_tempfp1118_WST)
summary(mb2neWST)
mb2neNCO <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_ne_tempfp1118_NCO)
summary(mb2neNCO) 

mb3neWST <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_ne_tempfp1118_WST)
summary(mb3neWST) 
mb3neNCO <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_ne_tempfp1118_NCO)
summary(mb3neNCO) 

####
mb1maWST <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_ma_tempfp1118_WST)
summary(mb1maWST) 
mb1maNCO <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_ma_tempfp1118_NCO)
summary(mb1maNCO) 

mb2maWST <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_ma_tempfp1118_WST)
summary(mb2maWST)
mb2maNCO <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_ma_tempfp1118_NCO)
summary(mb2maNCO) 

mb3maWST <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_ma_tempfp1118_WST)
summary(mb3maWST) 
mb3maNCO <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_ma_tempfp1118_NCO)
summary(mb3maNCO) 

# Compare models
AIC(mb1neWST,mb2neWST,mb3neWST) 
AIC(mb1neNCO,mb2neNCO,mb3neNCO) 

AIC(mb1maWST,mb2maWST,mb3maWST) 
AIC(mb1maNCO,mb2maNCO,mb3maNCO) 

# Compare variability
sd(kelpca_ma_tempfp1118_NCO$cover)/mean(kelpca_ma_tempfp1118_NCO$cover)
sd(kelpca_ne_tempfp1118_NCO$cover)/mean(kelpca_ne_tempfp1118_NCO$cover)
sd(kelpca_ma_tempfp1118_WST$cover)/mean(kelpca_ma_tempfp1118_WST$cover)
sd(kelpca_ne_tempfp1118_WST$cover)/mean(kelpca_ne_tempfp1118_WST$cover)

kelpca_ma_tempfp_NCO <- kelpca_ma_tempfp %>% filter(region=="Coast")
kelpca_ma_tempfp_WST <- kelpca_ma_tempfp %>% filter(region=="Western Strait")
kelpca_ne_tempfp_NCO <- kelpca_ne_tempfp %>% filter(region=="Coast")
kelpca_ne_tempfp_WST <- kelpca_ne_tempfp %>% filter(region=="Western Strait")

sd(kelpca_ma_tempfp_NCO$cover,na.rm = TRUE)/mean(kelpca_ma_tempfp_NCO$cover,na.rm = TRUE)
sd(kelpca_ne_tempfp_NCO$cover,na.rm = TRUE)/mean(kelpca_ne_tempfp_NCO$cover,na.rm = TRUE)
sd(kelpca_ma_tempfp_WST$cover,na.rm = TRUE)/mean(kelpca_ma_tempfp_WST$cover,na.rm = TRUE)
sd(kelpca_ne_tempfp_WST$cover,na.rm = TRUE)/mean(kelpca_ne_tempfp_WST$cover,na.rm = TRUE)

# Plot models
# Nereocystis
p_mb1ne<- ggplot(mapping = aes(x = max_mean_temp, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_ne_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_ne_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb1neWST, kelpca_ne_tempfp1118_WST)),
            data = kelpca_ne_tempfp1118_WST,color=reg_cols[2],lwd=1.5,lty="twodash") +
  geom_line(mapping = aes(y = predict(mb1neNCO, kelpca_ne_tempfp1118_NCO)),
            data = kelpca_ne_tempfp1118_NCO,color=reg_cols[3],lwd=1.5)+
  scale_x_continuous(name="Mean SST of the warmest month (C)",breaks = c(12,14,16))+
  scale_y_continuous(name="Percent of maximum canopy area",limits=c(0,1))

p_mb2ne<-ggplot(mapping = aes(x = MAX_tempa, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_ne_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_ne_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb2neWST, kelpca_ne_tempfp1118_WST)),
            data = kelpca_ne_tempfp1118_WST,color=reg_cols[2],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb2neNCO, kelpca_ne_tempfp1118_NCO)),
            data = kelpca_ne_tempfp1118_NCO,color=reg_cols[3],lwd=1.5)+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(name="Maximum temperature anomaly (C)",
                     breaks=c(0,1,2,3,4,5),limits=c(0,5))+
  scale_y_continuous(limits=c(0,1))

p_mb3ne<-ggplot(mapping = aes(x = SUM_daysoverclim, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_ne_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_ne_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb3neWST, kelpca_ne_tempfp1118_WST)),
            data = kelpca_ne_tempfp1118_WST,color=reg_cols[2],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb3neNCO, kelpca_ne_tempfp1118_NCO)),
            data = kelpca_ne_tempfp1118_NCO,color=reg_cols[3],lwd=1.5,lty="longdash")+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(name="Days above climatology",
                     breaks=c(0,100,200,300),limits=c(0,366))+
  scale_y_continuous(limits=c(0,1))

####
# Macrocystis
p_mb1ma<- ggplot(mapping = aes(x = max_mean_temp, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_ma_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_ma_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb1maWST, kelpca_ma_tempfp1118_WST)),
            data = kelpca_ma_tempfp1118_WST,color=reg_cols[2],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb1maNCO, kelpca_ma_tempfp1118_NCO)),
            data = kelpca_ma_tempfp1118_NCO,color=reg_cols[3],lwd=1.5)+
  scale_x_continuous(name="Mean SST of the warmest month (C)",breaks = c(12,14,16))+
  scale_y_continuous(name="Percent of maximum canopy area",limits=c(0,1))

p_mb2ma<-ggplot(mapping = aes(x = MAX_tempa, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_ma_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_ma_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb2maWST, kelpca_ma_tempfp1118_WST)),
            data = kelpca_ma_tempfp1118_WST,color=reg_cols[2],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb2maNCO, kelpca_ma_tempfp1118_NCO)),
            data = kelpca_ma_tempfp1118_NCO,color=reg_cols[3],lwd=1.5)+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(name="Maximum temperature anomaly (C)",
                     breaks=c(0,1,2,3,4,5),limits=c(0,5))+
  scale_y_continuous(limits=c(0,1))

p_mb3ma<-ggplot(mapping = aes(x = SUM_daysoverclim, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_ma_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_ma_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb3maWST, kelpca_ma_tempfp1118_WST)),
            data = kelpca_ma_tempfp1118_WST,color=reg_cols[2],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb3maNCO, kelpca_ma_tempfp1118_NCO)),
            data = kelpca_ma_tempfp1118_NCO,color=reg_cols[3],lwd=1.5,lty="longdash")+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(name="Days above climatology",
                     breaks=c(0,100,200,300),limits=c(0,366))+
  scale_y_continuous(limits=c(0,1))


jpeg(filename="figures/Figure3DEFGHI_betareg_1118nema_mapindex.jpg",width=9,height=8,units="in",res=300)
(p_mb1ne+p_mb2ne+p_mb3ne)/(p_mb1ma+p_mb2ma+p_mb3ma)
dev.off()

pdf(file="figures/Figure3DEFGHI_betareg_1118nema_mapindex.pdf",width=9,height=7)
(p_mb1ne+p_mb2ne+p_mb3ne)/(p_mb1ma+p_mb2ma+p_mb3ma)
dev.off()
