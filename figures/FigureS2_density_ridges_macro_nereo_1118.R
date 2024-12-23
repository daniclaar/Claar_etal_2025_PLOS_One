# Load necessary packages
library(tidyverse)
library(ggridges)

# Load data
load("analyses/prepped_data_ca_ma_ne.RData")

# Prep data
kelpca_ne_ma_tempfp1118 <- rbind(kelpca_ne_tempfp1118,kelpca_ma_tempfp1118)
kelpca_ne_ma_tempfp <- rbind(kelpca_ne_tempfp,kelpca_ma_tempfp)

# Make plot
p1 <- ggplot(aes(x=ca_permaxt,y=kelp_sp, height=stat(density)), 
       data = kelpca_ne_ma_tempfp1118)+
  geom_density_ridges(stat = "density",alpha=0.7)+
  theme_minimal()+guides(fill="none")+
  scale_x_continuous(name="Percent of maximum canopy area")+
  scale_y_discrete(name="Density distribution (2011-2018)",labels=c("Macrocystis","Nereocystis"))

# Make jpeg
jpeg(filename="figures/FigureS2_density_ridges_macro_nereo_1118.jpg",width=9,height=4,units="in",res=300)
p1
dev.off()

# Extract mean and s.d. for each species
kelpca_ne_ma_tempfp1118 %>% 
  group_by(kelp_sp) %>% 
  summarize(mean=mean(ca_permaxt),
            sd=sd(ca_permaxt))
