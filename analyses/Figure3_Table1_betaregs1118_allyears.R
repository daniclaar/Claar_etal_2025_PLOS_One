# Load necessary libraries
library(tidyverse)
library(betareg)
library(lmtest)
library(patchwork)

# Load data
load(file="analyses/prepped_data_ca.RData")

#####
# Prepare data
kelpca_tempfp1118_WST <- kelpca_tempfp1118 %>% 
  filter(region=="Western Strait")
kelpca_tempfp1118_EST <- kelpca_tempfp1118 %>% 
  filter(region=="Eastern Strait")
kelpca_tempfp1118_NCO <- kelpca_tempfp1118 %>% 
  filter(region=="Coast")
kelpca_tempfp1118_SMI <- kelpca_tempfp1118 %>% 
  filter(region=="Smith and Minor Island AR")
kelpca_tempfp1118_CYP <- kelpca_tempfp1118 %>% 
  filter(region=="Cypress Island AR")
kelpca_tempfp1118_CHP <- kelpca_tempfp1118 %>% 
  filter(region=="Cherry Point AR")

# Run models
mb1WST <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp1118_WST)
summary(mb1WST) 
mb1EST <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp1118_EST)
summary(mb1EST) 
mb1NCO <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp1118_NCO)
summary(mb1NCO) 
mb1SMI <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp1118_SMI)
summary(mb1SMI) 
mb1CYP <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp1118_CYP)
summary(mb1CYP) 
mb1CHP <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp1118_CHP)
summary(mb1CHP) 

mb2WST <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp1118_WST)
summary(mb2WST)
mb2EST <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp1118_EST)
summary(mb2EST)
mb2NCO <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp1118_NCO)
summary(mb2NCO) 
mb2SMI <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp1118_SMI)
summary(mb2SMI)
mb2CYP <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp1118_CYP)
summary(mb2CYP)
mb2CHP <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp1118_CHP)
summary(mb2CHP) 

####
mb3WST <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp1118_WST)
summary(mb3WST) 
mb3EST <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp1118_EST)
summary(mb3EST)
mb3NCO <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp1118_NCO)
summary(mb3NCO) 
mb3SMI <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp1118_SMI)
summary(mb3SMI)  
mb3CYP <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp1118_CYP)
summary(mb3CYP) 
mb3CHP <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp1118_CHP)
summary(mb3CHP) 

####
mb4WST <- betareg(ca_permaxt~SUM_daysoverclimplus1,link="cauchit",data=kelpca_tempfp1118_WST)
summary(mb4WST) 
mb4EST <- betareg(ca_permaxt~SUM_daysoverclimplus1,link="cauchit",data=kelpca_tempfp1118_EST)
summary(mb4EST)
mb4NCO <- betareg(ca_permaxt~SUM_daysoverclimplus1,link="cauchit",data=kelpca_tempfp1118_NCO)
summary(mb4NCO) 
mb4SMI <- betareg(ca_permaxt~SUM_daysoverclimplus1,link="cauchit",data=kelpca_tempfp1118_SMI)
summary(mb4SMI)  
mb4CYP <- betareg(ca_permaxt~SUM_daysoverclimplus1,link="cauchit",data=kelpca_tempfp1118_CYP)
summary(mb4CYP) 
mb4CHP <- betareg(ca_permaxt~SUM_daysoverclimplus1,link="cauchit",data=kelpca_tempfp1118_CHP)
summary(mb4CHP) 

####
mb5WST <- betareg(ca_permaxt~SUM_daysover15C,link="cauchit",data=kelpca_tempfp1118_WST)
summary(mb5WST) 
mb5EST <- betareg(ca_permaxt~SUM_daysover15C,link="cauchit",data=kelpca_tempfp1118_EST)
summary(mb5EST)
mb5NCO <- betareg(ca_permaxt~SUM_daysover15C,link="cauchit",data=kelpca_tempfp1118_NCO)
summary(mb5NCO) 
mb5SMI <- betareg(ca_permaxt~SUM_daysover15C,link="cauchit",data=kelpca_tempfp1118_SMI)
summary(mb5SMI)  
mb5CYP <- betareg(ca_permaxt~SUM_daysover15C,link="cauchit",data=kelpca_tempfp1118_CYP)
summary(mb5CYP) 
mb5CHP <- betareg(ca_permaxt~SUM_daysover15C,link="cauchit",data=kelpca_tempfp1118_CHP)
summary(mb5CHP) 

# Compare models
AIC(mb1WST,mb2WST,mb3WST,mb4WST,mb5WST) 
AIC(mb1EST,mb2EST,mb3EST,mb4EST) 
AIC(mb1NCO,mb2NCO,mb3NCO,mb4NCO,mb5NCO) 
AIC(mb1SMI,mb2SMI,mb3SMI,mb4SMI) 
AIC(mb1CYP,mb2CYP,mb3CYP,mb4CYP,mb5CYP) 
AIC(mb1CHP,mb2CHP,mb3CHP,mb4CHP,mb5CHP) 

# Breusch-Pagan test against heteroskedasticity.
bptest(mb1NCO) 
bptest(mb2NCO)
bptest(mb3NCO)
bptest(mb4NCO)
bptest(mb5NCO)

bptest(mb1WST) 
bptest(mb2WST)
bptest(mb3WST)
bptest(mb4WST)
bptest(mb5WST)

bptest(mb1EST) 
bptest(mb2EST)
bptest(mb3EST)
bptest(mb4EST)
#bptest(mb5EST)

bptest(mb1SMI) 
bptest(mb2SMI)
bptest(mb3SMI)
bptest(mb4SMI)
#bptest(mb5SMI)

bptest(mb1CYP) 
bptest(mb2CYP)
bptest(mb3CYP)
bptest(mb4CYP)
bptest(mb5CYP)

bptest(mb1CHP) 
bptest(mb2CHP)
bptest(mb3CHP)
bptest(mb4CHP)
bptest(mb5CHP)

# Variance function misspecification test (conceptually similar to testing for overdispersion)
mb1NCO_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp1118_NCO)
lrtest(mb1NCO, mb1NCO_phi)
mb2NCO_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp1118_NCO)
lrtest(mb2NCO, mb2NCO_phi)
mb3NCO_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp1118_NCO)
lrtest(mb3NCO, mb3NCO_phi)
mb4NCO_phi <- betareg(ca_permaxt ~ SUM_daysoverclimplus1 | SUM_daysoverclimplus1, link = "cauchit", data = kelpca_tempfp1118_NCO)
lrtest(mb4NCO, mb4NCO_phi)
mb5NCO_phi <- betareg(ca_permaxt ~ SUM_daysover15C | SUM_daysover15C, link = "cauchit", data = kelpca_tempfp1118_NCO)
lrtest(mb5NCO, mb5NCO_phi)

mb1WST_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp1118_WST)
lrtest(mb1WST, mb1WST_phi)
mb2WST_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp1118_WST)
lrtest(mb2WST, mb2WST_phi)
mb3WST_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp1118_WST)
lrtest(mb3WST, mb3WST_phi)
mb4WST_phi <- betareg(ca_permaxt ~ SUM_daysoverclimplus1 | SUM_daysoverclimplus1, link = "cauchit", data = kelpca_tempfp1118_WST)
lrtest(mb4WST, mb4WST_phi)
mb5WST_phi <- betareg(ca_permaxt ~ SUM_daysover15C | SUM_daysover15C, link = "cauchit", data = kelpca_tempfp1118_WST)
lrtest(mb5WST, mb5WST_phi)

mb1EST_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp1118_EST)
lrtest(mb1EST, mb1EST_phi)
mb2EST_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp1118_EST)
lrtest(mb2EST, mb2EST_phi)
mb3EST_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp1118_EST)
lrtest(mb3EST, mb3EST_phi)
mb4EST_phi <- betareg(ca_permaxt ~ SUM_daysoverclimplus1 | SUM_daysoverclimplus1, link = "cauchit", data = kelpca_tempfp1118_EST)
lrtest(mb4EST, mb4EST_phi)
# mb5EST_phi <- betareg(ca_permaxt ~ SUM_daysover15C | SUM_daysover15C, link = "cauchit", data = kelpca_tempfp1118_EST)
# lrtest(mb5EST, mb5EST_phi)

mb1SMI_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp1118_SMI)
lrtest(mb1SMI, mb1SMI_phi)
mb2SMI_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp1118_SMI)
lrtest(mb2SMI, mb2SMI_phi)
mb3SMI_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp1118_SMI)
lrtest(mb3SMI, mb3SMI_phi)
mb4SMI_phi <- betareg(ca_permaxt ~ SUM_daysoverclimplus1 | SUM_daysoverclimplus1, link = "cauchit", data = kelpca_tempfp1118_SMI)
lrtest(mb4SMI, mb4SMI_phi)
# mb5SMI_phi <- betareg(ca_permaxt ~ SUM_daysover15C | SUM_daysover15C, link = "cauchit", data = kelpca_tempfp1118_SMI)
# lrtest(mb5SMI, mb5SMI_phi)

mb1CYP_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp1118_CYP)
lrtest(mb1CYP, mb1CYP_phi)
mb2CYP_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp1118_CYP)
lrtest(mb2CYP, mb2CYP_phi)
mb3CYP_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp1118_CYP)
lrtest(mb3CYP, mb3CYP_phi)
mb4CYP_phi <- betareg(ca_permaxt ~ SUM_daysoverclimplus1 | SUM_daysoverclimplus1, link = "cauchit", data = kelpca_tempfp1118_CYP)
lrtest(mb4CYP, mb4CYP_phi)
mb5CYP_phi <- betareg(ca_permaxt ~ SUM_daysover15C | SUM_daysover15C, link = "cauchit", data = kelpca_tempfp1118_CYP)
lrtest(mb5CYP, mb5CYP_phi)

mb1CHP_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp1118_CHP)
lrtest(mb1CHP, mb1CHP_phi)
mb2CHP_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp1118_CHP)
lrtest(mb2CHP, mb2CHP_phi)
mb3CHP_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp1118_CHP)
lrtest(mb3CHP, mb3CHP_phi)
mb4CHP_phi <- betareg(ca_permaxt ~ SUM_daysoverclimplus1 | SUM_daysoverclimplus1, link = "cauchit", data = kelpca_tempfp1118_CHP)
lrtest(mb4CHP, mb4CHP_phi)
mb5CHP_phi <- betareg(ca_permaxt ~ SUM_daysover15C | SUM_daysover15C, link = "cauchit", data = kelpca_tempfp1118_CHP)
lrtest(mb5CHP, mb5CHP_phi)

### Plot models
#mb1
p_mb11118<- ggplot(mapping = aes(x = max_mean_temp, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[6], fill = reg_cols[6], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_CHP) +
  geom_point(size = 1.5, color = reg_cols[5], fill = reg_cols[5], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_CYP) +
  geom_point(size = 1.5, color = reg_cols[4], fill = reg_cols[4], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_SMI) +
  geom_point(size = 1.5, color = reg_cols[1], fill = reg_cols[1],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_EST) +
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb1CHP, kelpca_tempfp1118_CHP)),
            data = kelpca_tempfp1118_CHP,color=reg_cols[6],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb1CYP, kelpca_tempfp1118_CYP)),
            data = kelpca_tempfp1118_CYP,color=reg_cols[5],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb1SMI, kelpca_tempfp1118_SMI)),
            data = kelpca_tempfp1118_SMI,color=reg_cols[4],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb1EST, kelpca_tempfp1118_EST)),
            data = kelpca_tempfp1118_EST,color=reg_cols[1],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb1WST, kelpca_tempfp1118_WST)),
            data = kelpca_tempfp1118_WST,color=reg_cols[2],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb1NCO, kelpca_tempfp1118_NCO)),
            data = kelpca_tempfp1118_NCO,color=reg_cols[3],lwd=1.5)+
  scale_x_continuous(name="Mean SST of the warmest month (C)")+
  scale_y_continuous(name="Percent of maximum canopy area")


#mb2
p_mb21118<-ggplot(mapping = aes(x = MAX_tempa, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[6], fill = reg_cols[6], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_CHP) +
  geom_point(size = 1.5, color = reg_cols[5], fill = reg_cols[5], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_CYP) +
  geom_point(size = 1.5, color = reg_cols[4], fill = reg_cols[4], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_SMI) +
  geom_point(size = 1.5, color = reg_cols[1], fill = reg_cols[1],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_EST) +
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb2CHP, kelpca_tempfp1118_CHP)),
            data = kelpca_tempfp1118_CHP,color=reg_cols[6],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb2CYP, kelpca_tempfp1118_CYP)),
            data = kelpca_tempfp1118_CYP,color=reg_cols[5],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb2SMI, kelpca_tempfp1118_SMI)),
            data = kelpca_tempfp1118_SMI,color=reg_cols[4],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb2EST, kelpca_tempfp1118_EST)),
            data = kelpca_tempfp1118_EST,color=reg_cols[1],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb2WST, kelpca_tempfp1118_WST)),
            data = kelpca_tempfp1118_WST,color=reg_cols[2],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb2NCO, kelpca_tempfp1118_NCO)),
            data = kelpca_tempfp1118_NCO,color=reg_cols[3],lwd=1.5)+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(name="Maximum temperature anomaly (C)")


#mb3
p_mb31118<-ggplot(mapping = aes(x = SUM_daysoverclim, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[6], fill = reg_cols[6], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_CHP) +
  geom_point(size = 1.5, color = reg_cols[5], fill = reg_cols[5], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_CYP) +
  geom_point(size = 1.5, color = reg_cols[4], fill = reg_cols[4], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_SMI) +
  geom_point(size = 1.5, color = reg_cols[1], fill = reg_cols[1],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_EST) +
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp1118_NCO) +
  geom_line(mapping = aes(y = predict(mb3CHP, kelpca_tempfp1118_CHP)),
            data = kelpca_tempfp1118_CHP,color=reg_cols[6],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb3CYP, kelpca_tempfp1118_CYP)),
            data = kelpca_tempfp1118_CYP,color=reg_cols[5],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb3SMI, kelpca_tempfp1118_SMI)),
            data = kelpca_tempfp1118_SMI,color=reg_cols[4],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb3EST, kelpca_tempfp1118_EST)),
            data = kelpca_tempfp1118_EST,color=reg_cols[1],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb3WST, kelpca_tempfp1118_WST)),
            data = kelpca_tempfp1118_WST,color=reg_cols[2],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb3NCO, kelpca_tempfp1118_NCO)),
            data = kelpca_tempfp1118_NCO,color=reg_cols[3],lwd=1.5,lty="longdash")+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(name="Days above climatology")


# p_mb1+p_mb2+p_mb3


jpeg(filename="figures/Figure3ABC_betareg_1118_mapindex.jpg",width=9,height=4,units="in",res=300)
p_mb11118+p_mb21118+p_mb31118
dev.off()

pdf(file="figures/Figure3ABC_betareg_1118_mapindex.pdf",width=9,height=4)
p_mb11118+p_mb21118+p_mb31118
dev.off()


###############
#All years
# Prepare data
kelpca_tempfp <- kelpca_tempfp %>% 
  mutate(map_index=as.factor(map_index)) %>% 
  mutate(region=as.factor(region)) 

kelpca_tempfp_WST <- kelpca_tempfp %>% 
  filter(region=="Western Strait")
kelpca_tempfp_EST <- kelpca_tempfp %>% 
  filter(region=="Eastern Strait")
kelpca_tempfp_NCO <- kelpca_tempfp %>% 
  filter(region=="Coast")

# Run models
mb1WST <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp_WST)
summary(mb1WST) 
mb1EST <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp_EST)
summary(mb1EST) 
mb1NCO <- betareg(ca_permaxt~max_mean_temp,link="cauchit",data=kelpca_tempfp_NCO)
summary(mb1NCO) 

mb2WST <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp_WST)
summary(mb2WST)
mb2EST <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp_EST)
summary(mb2EST)
mb2NCO <- betareg(ca_permaxt~MAX_tempa,link="cauchit",data=kelpca_tempfp_NCO)
summary(mb2NCO) 

mb3WST <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp_WST)
summary(mb3WST) 
mb3EST <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp_EST)
summary(mb3EST)
mb3NCO <- betareg(ca_permaxt~SUM_daysoverclim,link="cauchit",data=kelpca_tempfp_NCO)
summary(mb3NCO) 


# Compare models
AIC(mb1WST,mb2WST,mb3WST) 
AIC(mb1EST,mb2EST,mb3EST) 
AIC(mb1NCO,mb2NCO,mb3NCO) 

##########
# Breusch-Pagan test against heteroskedasticity.
bptest(mb1NCO) 
bptest(mb2NCO)
bptest(mb3NCO)

bptest(mb1WST) 
bptest(mb2WST)
bptest(mb3WST)

# Variance function misspecification test (conceptually similar to testing for overdispersion)
mb1NCO_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp_NCO)
lrtest(mb1NCO, mb1NCO_phi)
mb2NCO_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp_NCO)
lrtest(mb2NCO, mb2NCO_phi)
mb3NCO_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp_NCO)
lrtest(mb3NCO, mb3NCO_phi)

mb1WST_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp_WST)
lrtest(mb1WST, mb1WST_phi)
mb2WST_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp_WST)
lrtest(mb2WST, mb2WST_phi)
mb3WST_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp_WST)
lrtest(mb3WST, mb3WST_phi)

mb1EST_phi <- betareg(ca_permaxt ~ max_mean_temp | max_mean_temp, link = "cauchit", data = kelpca_tempfp_EST)
lrtest(mb1EST, mb1EST_phi)
mb2EST_phi <- betareg(ca_permaxt ~ MAX_tempa | MAX_tempa, link = "cauchit", data = kelpca_tempfp_EST)
lrtest(mb2EST, mb2EST_phi)
mb3EST_phi <- betareg(ca_permaxt ~ SUM_daysoverclim | SUM_daysoverclim, link = "cauchit", data = kelpca_tempfp_EST)
lrtest(mb3EST, mb3EST_phi)



##########

### Plot models
#mb1
p_mb1all<- ggplot(mapping = aes(x = max_mean_temp, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[1], fill = reg_cols[1],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_EST) +
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_NCO) +
  geom_line(mapping = aes(y = predict(mb1EST, kelpca_tempfp_EST)),
            data = kelpca_tempfp_EST,color=reg_cols[1],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb1WST, kelpca_tempfp_WST)),
            data = kelpca_tempfp_WST,color=reg_cols[2],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb1NCO, kelpca_tempfp_NCO)),
            data = kelpca_tempfp_NCO,color=reg_cols[3],lwd=1.5)+
  scale_x_continuous(name="Mean SST of the warmest month (C)")+
  scale_y_continuous(name="Percent of maximum canopy area")

#mb2
p_mb2all<- ggplot(mapping = aes(x = MAX_tempa, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[1], fill = reg_cols[1],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_EST) +
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_NCO) +
  geom_line(mapping = aes(y = predict(mb2EST, kelpca_tempfp_EST)),
            data = kelpca_tempfp_EST,color=reg_cols[1],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb2WST, kelpca_tempfp_WST)),
            data = kelpca_tempfp_WST,color=reg_cols[2],lwd=1.5) +
  geom_line(mapping = aes(y = predict(mb2NCO, kelpca_tempfp_NCO)),
            data = kelpca_tempfp_NCO,color=reg_cols[3],lwd=1.5)+
  scale_x_continuous(name="Maximum temperature anomaly (C)")+
  scale_y_continuous(name="Percent of maximum canopy area")+  
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())


#mb3
p_mb3all<- ggplot(mapping = aes(x = SUM_daysoverclim, y = ca_permaxt)) +
  theme_classic()+
  geom_point(size = 1.5, color = reg_cols[1], fill = reg_cols[1],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_EST) +
  geom_point(size = 1.5, color = reg_cols[2], fill = reg_cols[2],
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_WST) +
  geom_point(size = 1.5, color = reg_cols[3], fill = reg_cols[3], 
             shape = 21, alpha=0.3,
             data = kelpca_tempfp_NCO) +
  geom_line(mapping = aes(y = predict(mb3EST, kelpca_tempfp_EST)),
            data = kelpca_tempfp_EST,color=reg_cols[1],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb3WST, kelpca_tempfp_WST)),
            data = kelpca_tempfp_WST,color=reg_cols[2],lwd=1.5,lty="longdash") +
  geom_line(mapping = aes(y = predict(mb3NCO, kelpca_tempfp_NCO)),
            data = kelpca_tempfp_NCO,color=reg_cols[3],lwd=1.5)+
  scale_x_continuous(name="Days above climatology")+
  scale_y_continuous(name="Percent of maximum canopy area")+  
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())

jpeg(filename="figures/Figure3DEF_betareg_all_mapindex.jpg",
     width=9,height=4,units="in",res=300)
p_mb1all+p_mb2all+p_mb3all
dev.off()

pdf(file="figures/Figure3DEF_betareg_all_mapindex.pdf",width=9,height=4)
p_mb1all+p_mb2all+p_mb3all
dev.off()

##########
# Make figure 3 with all panels

jpeg(filename="figures/Figure3_betareg_all_mapindex.jpg",
     width=9,height=8,units="in",res=300)
(p_mb11118+p_mb21118+p_mb31118)/(p_mb1all+p_mb2all+p_mb3all)
dev.off()

pdf(file="figures/Figure3_betareg_all_mapindex.pdf",width=9,height=8)
(p_mb11118+p_mb21118+p_mb31118)/(p_mb1all+p_mb2all+p_mb3all)
dev.off()


