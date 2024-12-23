# Load necessary packages
library(ape)
library(tidyverse)

# Load data
load("analyses/prepped_data_ca.RData")
site_zone_latlon <- read.csv("data/site_zone_lat_lon.csv")


sites <- site_zone_latlon[57:139,] %>% 
  rename(map_index=unit) %>% 
  mutate(map_index=gsub("-","_",map_index)) %>% 
  mutate(map_index=gsub(",","_",map_index))

covs <- kelpca_tempfp1118 %>% 
  select(c(map_index,Year,ca_permaxt,region)) 

sites_covs <- left_join(covs,sites,by = "map_index") %>% 
  filter(!is.na(lat)) %>% 
  group_by(region,map_index,lat,lon) %>% 
  summarize(ca_permaxt=mean(ca_permaxt))

sites_covs_NCO <- sites_covs %>% filter(region=="Coast")
site.dists.NCO <- as.matrix(dist(cbind(sites_covs_NCO$lat, sites_covs_NCO$lon)))
site.dists.NCO.inv <- 1/site.dists.NCO
diag(site.dists.NCO.inv) <- 0
Moran.I(sites_covs_NCO$ca_permaxt, site.dists.NCO.inv)

sites_covs_WST <- sites_covs %>% filter(region=="Western Strait")
site.dists.WST <- as.matrix(dist(cbind(sites_covs_WST$lat, sites_covs_WST$lon)))
site.dists.WST.inv <- 1/site.dists.WST
diag(site.dists.WST.inv) <- 0
Moran.I(sites_covs_WST$ca_permaxt, site.dists.WST.inv)

sites_covs_EST <- sites_covs %>% filter(region=="Eastern Strait")
site.dists.EST <- as.matrix(dist(cbind(sites_covs_EST$lat, sites_covs_EST$lon)))
site.dists.EST.inv <- 1/site.dists.EST
diag(site.dists.EST.inv) <- 0
Moran.I(sites_covs_EST$ca_permaxt, site.dists.EST.inv)

sites_covs_SMI <- sites_covs %>% filter(region=="Smith and Minor Island AR")
site.dists.SMI <- as.matrix(dist(cbind(sites_covs_SMI$lat, sites_covs_SMI$lon)))
site.dists.SMI.inv <- 1/site.dists.SMI
diag(site.dists.SMI.inv) <- 0
Moran.I(sites_covs_SMI$ca_permaxt, site.dists.SMI.inv)

sites_covs_CYP <- sites_covs %>% filter(region=="Cypress Island AR")
site.dists.CYP <- as.matrix(dist(cbind(sites_covs_CYP$lat, sites_covs_CYP$lon)))
site.dists.CYP.inv <- 1/site.dists.CYP
diag(site.dists.CYP.inv) <- 0
Moran.I(sites_covs_CYP$ca_permaxt, site.dists.CYP.inv)

sites_covs_CHP <- sites_covs %>% filter(region=="Cherry Point AR")
site.dists.CHP <- as.matrix(dist(cbind(sites_covs_CHP$lat, sites_covs_CHP$lon)))
site.dists.CHP.inv <- 1/site.dists.CHP
diag(site.dists.CHP.inv) <- 0
Moran.I(sites_covs_CHP$ca_permaxt, site.dists.CHP.inv)

####
# All years for Coast and Western Strait

covsA <- kelpca_tempfp %>% 
  select(c(map_index,Year,ca_permaxt,region)) 

sites_covsA <- left_join(covsA,sites,by = "map_index") %>% 
  filter(!is.na(lat)) %>% 
  group_by(region,map_index,lat,lon) %>% 
  summarize(ca_permaxt=mean(ca_permaxt))

sites_covs_NCOA <- sites_covsA %>% filter(region=="Coast")
site.dists.NCOA <- as.matrix(dist(cbind(sites_covs_NCOA$lat, sites_covs_NCOA$lon)))
site.dists.NCO.invA <- 1/site.dists.NCOA
diag(site.dists.NCO.invA) <- 0
Moran.I(sites_covs_NCOA$ca_permaxt, site.dists.NCO.invA)

sites_covs_WSTA <- sites_covsA %>% filter(region=="Western Strait")
site.dists.WSTA <- as.matrix(dist(cbind(sites_covs_WSTA$lat, sites_covs_WSTA$lon)))
site.dists.WST.invA <- 1/site.dists.WSTA
diag(site.dists.WST.invA) <- 0
Moran.I(sites_covs_WSTA$ca_permaxt, site.dists.WST.invA)
