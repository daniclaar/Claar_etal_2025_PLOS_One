### Extract temperature anomaly data ###
source("analyses/NOAA_daily_extract_SSTA.R")
  # data_dir <- "D:/NOAA_DHW_daily/ssta/" # PUT YOUR LOCATION OF THE NOAA DATA HERE
  #read.csv("data/EStr_datatable.csv")
  #read.csv("data/WStr_datatable.csv")
  #read.csv("data/Coast_datatable.csv")
  #read.csv("data/AQRES_datatable.csv")
  #save(COSTR_AQRES_ssta, file="data/NOAA_ssta_5km_COSTR.RData")

### Extract temperature data ###
source("analyses/NOAA_daily_extract_SST.R")
  #data_dir <- "D:/NOAA_DHW_daily/sst/" # PUT YOUR LOCATION OF THE NOAA DATA HERE
  #read.csv("data/EStr_datatable.csv")
  #read.csv("data/WStr_datatable.csv")
  #read.csv("data/Coast_datatable.csv")
  #read.csv("data/AQRES_datatable.csv")
  #save(COSTR_AQRES_sst, file="data/NOAA_sst_5km_COSTR.RData")

### Prepare kelp and temp data ###
source("data/prep_data_can.R")
# Kelp data
  #kelp_ar_s_l <- read.csv("data/kelp_data_AQRES.csv",row.names = "X")
  #kelp_s_l <- read.csv("data/kelp_data_COSTR.csv",row.names = "X.1") 
#Temperature data
  #COSTR_AQRES_sst <- read.csv("data/NOAA_sst_5km_COSTR.csv") 
  #COSTR_AQRES_ssta <- read.csv("data/NOAA_ssta_5km_COSTR.csv") 
  #save(file="analyses/prepped_data_ca.RData",list=c("kelpca_tempfp1118","kelpca_tempfp","reg_cols_cont","reg_cols", 'kelpca_tempfp'))
  #save(file="analyses/prepped_data_ca_ma_ne.RData",list=c("kelpca_ne_tempfp1118","kelpca_ne_tempfp", "kelpca_ma_tempfp1118","kelpca_ma_tempfp","reg_cols_cont", "reg_cols"))
source("data/prep_data_bed.R")
  # Kelp data
    #kelp_ar_s_l <- read.csv("data/kelp_data_AQRES.csv",row.names = "X")
    #kelp_s_l <- read.csv("data/kelp_data_COSTR.csv",row.names = "X.1") 
  #Temperature data
    #COSTR_AQRES_sst <- read.csv("data/NOAA_sst_5km_COSTR.csv") 
    #COSTR_AQRES_ssta <- read.csv("data/NOAA_ssta_5km_COSTR.csv") 
  #save(file="analyses/prepped_data_bed.RData",list=c(""reg_cols","reg_cols_cont","kelppl_tempfp1118"))

### Table 1 ###
source("analyses/Table1_kelp_min_max_mean.R")
  #load("analyses/prepped_data_ca.RData")
  #load("analyses/prepped_data.RData")

### Figure 1DEF & Figure S1 ###
source("figures/Figure1_DEF_FigureS1_floating_kelp_plots.R")
  #load("analyses/prepped_data_ca.RData")
  #"figures/Figure1_DEF.jpg"
  #"figures/Figure1_DEF.pdf"
  #"figures/FigureS1_floatingkelp_tot_cov_alltime.jpg"
  
### Figure 2 (lower panel) ###
source("figures/Figure2_bottomrow_plot_daily_bysubbasin.R") 
  #load("data/NOAA_ssta_5km_COSTR.RData")
  #"figures/Figure2_bottomrow_daily_ssta_bysubbasin_2011_2018.jpg"

### Figure 2 (upper panel) ###
source("figures/Figure2_toprow_plot_daily_bysubbasin_sst.R")
  #load("data/NOAA_sst_5km_COSTR.RData")
  #"figures/Figure2_toprow_daily_sst_bysubbasin_2011_2018.jpg"

### Beta-regression models 2011-2018 and Figure 3ABC ###
source("analyses/Figure3_Table1_betaregs1118_allyears.R")
  #load(file="analyses/prepped_data_ca.RData")
  #"figures/Figure3ABC_betareg_1118_mapindex.jpg"
  #"figures/Figure3ABC_betareg_1118_mapindex.pdf"
  #"figures/Figure3DEF_betareg_all_mapindex.jpg"
  #"figures/Figure3DEF_betareg_all_mapindex.pdf"
  #"figures/Figure3_betareg_all_mapindex.jpg" 
  #"figures/Figure3_betareg_all_mapindex.pdf" # edited in Illustrator to add panel letters, then use in manuscript

### Beta-regression models by species 2011-2018 and Figure 3DEFGHI ###
source("analyses/Figure3DEFGHI_TableS8_ne_and_ma.R") 
  # load("analyses/prepped_data_ca_ma_ne.RData")
  # "figures/Figure3DEFGHI_betareg_1118nema_mapindex.jpg"
  # "figures/Figure3DEFGHI_betareg_1118nema_mapindex.pdf"
  # Numbers for Table S8

### Plot density ridges by kelp species
source("figures/FigureS2_density_ridges_macro_nereo_1118.R")
  #load("analyses/prepped_data_ca_ma_ne.RData")
  #"figures/FigureS2_density_ridges_macro_nereo_1118.jpg"

### Floating kelp bed area for all years
source("analyses/TableS1_bedarea_allyears.R")
  #load("analyses/prepped_data_bed.RData")
  # Numbers for Table S1

### Floating kelp canopy area for all years
source("analyses/TableS2_canopyarea_allyears.R")
  #load("analyses/prepped_data_ca.RData")
  # Numbers for Table S2

### Temperature Supplemental plots and tables 
source("analyses/FigureS3_Tables_S3_S4_S5_S6_temp_range_bysubregion")
  #load("data/NOAA_sst_5km_COSTR.RData")
  #load(file="analyses/prepped_data_ca.RData")
  #"figures/FigureS3_SST_byregion.jpeg"
  #Numbers for Table S3, Table S4, Table S5, Table S6

### Testing for spatial autocorrelation within sub-regions ###
source("analyses/test_moransi.R")
  #load("analyses/prepped_data_ca.RData")
  #read.csv("data/site_zone_lat_lon.csv")
  #Check values manually, all p>0.05

