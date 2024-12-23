## Load packages and data
library(tidyverse)
library(wesanderson)
library(patchwork)

load("analyses/prepped_data_ca.RData")


## Plot prep
kelpca_tempfp1118_y <- kelpca_tempfp1118 %>% 
  group_by(Year) %>% 
  summarize(cov_mean = mean(cover,na.rm=TRUE),
            cov_tot = sum(cover)) 

kelpca_tempfp1118_r <- kelpca_tempfp1118 %>% 
  group_by(region,Year) %>% 
  summarize(cov_mean = mean(cover,na.rm=TRUE),
            cov_tot = sum(cover)) %>% 
  mutate(cov_tomax=cov_mean-max(cov_mean))

kelpca_tempfp1118_r_ES <- kelpca_tempfp1118_r %>% filter(region=="Eastern Strait")
kelpca_tempfp1118_r_WS <- kelpca_tempfp1118_r %>% filter(region=="Western Strait")
kelpca_tempfp1118_r_OC <- kelpca_tempfp1118_r %>% filter(region=="Coast")
kelpca_tempfp1118_r_SM <- kelpca_tempfp1118_r %>% filter(region=="Smith and Minor Island AR")
kelpca_tempfp1118_r_CI <- kelpca_tempfp1118_r %>% filter(region=="Cypress Island AR")
kelpca_tempfp1118_r_CP <- kelpca_tempfp1118_r %>% filter(region=="Cherry Point AR")

kelpca_tempfp1118_r_pre_mean <- kelpca_tempfp1118 %>% 
  group_by(region,Year) %>% 
  mutate(cov_tot=sum(cover)) %>% 
  select(region,Year,cov_tot) %>% 
  filter(Year==2011|Year==2012|Year==2013) %>% 
  ungroup(Year) %>% 
  summarize(pre_mean=mean(cov_tot),
            pre_sd=sd(cov_tot),
            pre_sd_perc=pre_sd/pre_mean*100)

kelpca_tempfp1118_r2 <- kelpca_tempfp1118_r %>% 
  left_join(., kelpca_tempfp1118_r_pre_mean, by="region") %>% 
  mutate(cov_to_pre_mean = cov_tot-pre_mean) %>% 
  mutate(cov_to_perc_pre_mean = (cov_tot-pre_mean)/pre_mean)


# Figure 1 D
p1 <- kelpca_tempfp1118_r2 %>% 
  mutate(region = factor(x = region, levels = c("Cherry Point AR","Cypress Island AR",
                                                "Smith and Minor Island AR",
                                                "Eastern Strait", "Western Strait",
                                                "Coast"))) %>%
  ggplot()+theme_classic()+theme(legend.position=c(0.3,0.8))+
  annotate("rect", xmin=2013.75, xmax=2014.25, ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_hline(yintercept=0)+
  geom_line(aes(x=Year, y=cov_to_perc_pre_mean*100, color=region),linewidth=1)+
  geom_point(aes(x=Year, y=cov_to_perc_pre_mean*100, color=region),size=2)+
  ylim(-100,125)+
  scale_color_manual(values=reg_cols,name="Region")+
  scale_x_continuous(breaks=c(2011, 2012,2013,2014,2015,2016, 2017, 2018),
                     limits=c(2011,2018),expand=c(0,0.1))+
  scale_y_continuous(limits=c(-100,125),expand=c(0,0))+
  ylab("Percent difference from mean (2011-2013)")
p1

# Figure 1 E
p2 <- kelpca_tempfp1118_r2 %>% 
  filter(region=="Eastern Strait"|region=="Western Strait"|region=="Coast")  %>% 
  ggplot()+theme_classic()+theme(legend.position="none")+
  geom_line(aes(x=Year,y=cov_tot,color=region),linewidth=1)+
  geom_point(aes(x=Year,y=cov_tot,color=region),size=1.5)+
  labs(y="Total Cover (ha)")+
  scale_y_continuous(limits=c(0,1200),expand=c(0,0))+
  scale_x_continuous(limits=c(2011,2018), expand=c(0,0))+
  scale_color_manual(values=reg_cols)+
  geom_hline(yintercept = kelpca_tempfp1118_r_pre_mean$pre_mean[2],color=reg_cols[3])+ # coast
  geom_ribbon(aes(x=Year,ymin=kelpca_tempfp1118_r_pre_mean$pre_mean[2]-kelpca_tempfp1118_r_pre_mean$pre_sd[2],
                  ymax=kelpca_tempfp1118_r_pre_mean$pre_mean[2]+kelpca_tempfp1118_r_pre_mean$pre_sd[2]),
              fill=reg_cols[3],alpha=0.2)+
  geom_hline(yintercept = kelpca_tempfp1118_r_pre_mean$pre_mean[6],color=reg_cols[2])+  #est
  geom_ribbon(aes(x=Year,ymin=kelpca_tempfp1118_r_pre_mean$pre_mean[6]-kelpca_tempfp1118_r_pre_mean$pre_sd[6],
                  ymax=kelpca_tempfp1118_r_pre_mean$pre_mean[6]+kelpca_tempfp1118_r_pre_mean$pre_sd[6]),
              fill=reg_cols[2],alpha=0.2)+
  geom_hline(yintercept = kelpca_tempfp1118_r_pre_mean$pre_mean[4],color=reg_cols[1])+#wst
  geom_ribbon(aes(x=Year,ymin=kelpca_tempfp1118_r_pre_mean$pre_mean[4]-kelpca_tempfp1118_r_pre_mean$pre_sd[4],
                  ymax=kelpca_tempfp1118_r_pre_mean$pre_mean[4]+kelpca_tempfp1118_r_pre_mean$pre_sd[4]),
              fill=reg_cols[1],alpha=0.2)+
  geom_vline(xintercept = 2015,color=reg_cols[1])+
  geom_vline(xintercept = 2014.98,color=reg_cols[2])+
  geom_vline(xintercept = 2015.02,color=reg_cols[3])
p2

# Figure 1 F
p3 <- kelpca_tempfp1118_r2 %>% 
  filter(region=="Smith and Minor Island AR"|region=="Cherry Point AR"|region=="Cypress Island AR")%>% 
  ggplot()+theme_classic()+theme(legend.position="none")+
  geom_line(aes(x=Year,y=cov_tot,color=region),linewidth=1)+
  geom_point(aes(x=Year,y=cov_tot,color=region),size=1.5)+
  labs(y="Total Cover (ha)")+
  scale_color_manual(values=reg_cols)+
  scale_x_continuous(limits=c(2011,2018),expand=c(0,0))+
  scale_y_continuous(limits=c(0,150),expand=c(0,0))+
  geom_hline(yintercept = kelpca_tempfp1118_r_pre_mean$pre_mean[1],color=reg_cols[6])+ # cherry
  geom_ribbon(aes(x=Year,ymin=kelpca_tempfp1118_r_pre_mean$pre_mean[1]-kelpca_tempfp1118_r_pre_mean$pre_sd[1],
                  ymax=kelpca_tempfp1118_r_pre_mean$pre_mean[1]+kelpca_tempfp1118_r_pre_mean$pre_sd[1]),
              fill=reg_cols[6],alpha=0.2)+
  geom_hline(yintercept = kelpca_tempfp1118_r_pre_mean$pre_mean[3],color=reg_cols[5])+  # cypress
  geom_ribbon(aes(x=Year,ymin=kelpca_tempfp1118_r_pre_mean$pre_mean[3]-kelpca_tempfp1118_r_pre_mean$pre_sd[3],
                  ymax=kelpca_tempfp1118_r_pre_mean$pre_mean[3]+kelpca_tempfp1118_r_pre_mean$pre_sd[3]),
              fill=reg_cols[5],alpha=0.2)+
  geom_hline(yintercept = kelpca_tempfp1118_r_pre_mean$pre_mean[5],color=reg_cols[4])+  #smith
  geom_ribbon(aes(x=Year,ymin=kelpca_tempfp1118_r_pre_mean$pre_mean[5]-kelpca_tempfp1118_r_pre_mean$pre_sd[5],
                  ymax=kelpca_tempfp1118_r_pre_mean$pre_mean[5]+kelpca_tempfp1118_r_pre_mean$pre_sd[5]),
              fill=reg_cols[4],alpha=0.2)+
  geom_vline(xintercept = 2016,color=reg_cols[4])+
  geom_vline(xintercept = 2016.98,color=reg_cols[6])+
  geom_vline(xintercept = 2017.02,color=reg_cols[5])
p3


jpeg(filename = "figures/Figure1_DEF.jpg",width=8, height=5,units="in",res=300)
p1+(p2/p3)+plot_layout(widths=c(1.5,1))
dev.off()

pdf(file = "figures/Figure1_DEF.pdf",width=8, height=5)
p1+(p2/p3)+plot_layout(widths=c(1.5,1))
dev.off()

#### Table 1 columns 6 & 7
# Pre-MHW ‘baseline’ mean canopy area ± s.d. (ha) (2011-2013)
kelpca_tempfp1118_r_pre_mean %>% 
  select(-pre_sd_perc)
  
kelpca_tempfp1416_r2 <- kelpca_tempfp1118_r2 %>% 
  filter(Year>2013 & Year < 2017) %>% 
  select(region, Year,cov_to_perc_pre_mean) %>% 
  group_by(region) %>% 
  filter(cov_to_perc_pre_mean==min(cov_to_perc_pre_mean))



##### Figure S1
p5 <- kelpca_tempfp %>% 
  group_by(Year,region) %>% 
  summarize(cov_mean = mean(cover,na.rm=TRUE),
            cov_tot = sum(cover)) %>% 
  ggplot()+theme_classic()+theme(legend.position="none")+
  geom_vline(xintercept=2014,lwd=2,color="gray")+
  geom_line(aes(x=Year,y=cov_tot,color=region),size=1)+
  labs(y="Total Cover (ha)")+
  scale_x_continuous(expand=c(0,0),limits = c(1989,2018),
                     breaks=c(1990,1995,2000,2005,2010,2015))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=reg_cols)
p5


p6 <- kelpca_tempfp %>% 
  group_by(Year,region) %>% 
  summarize(cov_mean = mean(cover,na.rm=TRUE),
            cov_tot = sum(cover)) %>% 
  filter(region=="Smith and Minor Island AR"|region=="Cypress Island AR"|region=="Cherry Point AR")  %>% 
  filter(Year!=2010) %>% 
  ggplot()+theme_classic()+theme(legend.position="none")+
  geom_vline(xintercept=2014,lwd=2,color="gray")+
  geom_line(aes(x=Year,y=cov_tot,color=region),size=1)+
  labs(y="Total Cover (ha)")+xlim(2011,2021)+
  scale_x_continuous(expand=c(0,0),limits = c(2011,2018),
                     breaks=c(2012,2014,2016,2018))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=reg_cols)+
  facet_wrap("region",nrow = 3,scales = "free",strip.position = "right")
p6



jpeg(filename = "figures/FigureS1_floatingkelp_tot_cov_alltime.jpg",width=10, height=6,
     units="in",res=300)
p5+p6+plot_layout(ncol=2,widths=c(2,1))
dev.off()
