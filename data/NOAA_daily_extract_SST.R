# Load necessary packages
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
require(svMisc)
library(tidyverse)

# Make a list of files to extract data from
data_dir <- "data/" # PUT YOUR LOCATION OF THE NOAA DATA HERE 
# (If I am doing years worth of daily data, I typically keep the full data 
# set outside of the github folder, because it is very large)
files=list.files(data_dir,full.names = TRUE,pattern = ".nc$")

## Test on one file to make sure it works:
## I recommend doing this to make sure the function below will work 
# the way you expect it to. You'll have to change the file name/path and 
# change "analyzed_sst" to whatever the DHW variable name is
testfile <- paste0(data_dir,"coraltemp_v3.1_19880101.nc") # This is a file you expect to be in your data directory. Change the name in quotes if you don't expect to have this particular file for testing
nc <- nc_open(testfile)
sst_full <- ncvar_get( nc, "analysed_sst")
# get longitude and latitude
lon <- ncvar_get(nc,"lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(nc,"lat")
nlat <- dim(lat)
head(lat)
nc_close(nc) # THIS IS IMPORTANT, OR YOU WILL CORRUPT YOUR FILES

#######
# Read in csvs with metadata for map indices in each data set
# Input csv(s) need(s) to have: MAP_INDEX_ (this could be the 
# site code or name), Longitude, and Latitude
EStr_dt <- read.csv("data/EStr_datatable.csv") %>% 
  mutate(MAP_INDEX_ = as.character(round(MAP_INDEX_,1)))
WStr_dt <- read.csv("data/WStr_datatable.csv") %>% 
  mutate(MAP_INDEX_ = as.character(round(MAP_INDEX_,1)))
Coast_dt <- read.csv("data/Coast_datatable.csv") %>% 
  mutate(MAP_INDEX_ = as.character(round(MAP_INDEX_,1)))
AQRES_dt <- read.csv("data/AQRES_datatable.csv")

# Make AQRES data table match the others
# Ignore this if you have consistent csv files
AQRES_dt <- AQRES_dt[c(1:2,5:6,3,8,7)]
AQRES_dt <- AQRES_dt %>% 
  rename(MAP_INDEX_=map_index) %>% 
  rename(Shape_Length=SHAPE_Length) %>% 
  rename(Shape_Area=SHAPE_Area) %>% 
  rename(Longitude=longitude) %>% 
  rename(Latitude=latitude) %>% 
  mutate(sub_basin=NA) %>% 
  mutate(reach=NA) %>% 
  mutate(map_index=NA) %>% 
  mutate(MAP_INDEX_ = gsub("-","_",MAP_INDEX_)) %>% 
  mutate(MAP_INDEX_ = gsub(",","_",MAP_INDEX_))


# Bind data tables together
# Only necessary if you have multiple csvs
COSTR_AQRES_dt <- rbind(EStr_dt,WStr_dt,Coast_dt,AQRES_dt) 

######
# Extract values by lat/lon, except you have to figure out which indices 
# relate to each lat and lon

# Make empty arrays
LonLatIdx <- array(dim=c(nrow(COSTR_AQRES_dt),2))
LonLatVal <- array(dim=c(nrow(COSTR_AQRES_dt),2))

# Fill the arrays by finding the closest lat/lon from the NOAA data
for (j in 1:nrow(COSTR_AQRES_dt)) {
  LonLatVal[j,1] <- lon[which.min(abs(COSTR_AQRES_dt$Longitude[j]-lon))]
  LonLatVal[j,2] <- lat[which.min(abs(COSTR_AQRES_dt$Latitude[j]-lat))]
  LonLatIdx[j,1] <- which(lon==LonLatVal[j,1])
  LonLatIdx[j,2] <- which(lat==LonLatVal[j,2])
}

# Calculate minimum and maximums lat/lons
minlon <- min(LonLatIdx[,1])
maxlon <- max(LonLatIdx[,1])
minlat <- min(LonLatIdx[,2])
maxlat <- max(LonLatIdx[,2])

minlatlon <- c(minlon,minlat)
maxlatlon <- c(maxlon,maxlat)

maxlatlon-minlatlon # 42 unique lons, 52 unique lats

# All lat/lon indices
all_lonidx <- seq(from=minlatlon[1],to=maxlatlon[1])
all_latidx <- seq(from=minlatlon[2],to=maxlatlon[2])

# List all files
sstlist <- list()
for (i in files) {
  # Open the netcdf file
  nc <- nc_open(i)
  sstlist[[i]] <- ncvar_get( nc,
                              "analysed_sst")[min(LonLatIdx[,1]):max(LonLatIdx[,1]), # This extracts sst values within the bounding box of your chosen lat/lons
                                              min(LonLatIdx[,2]):max(LonLatIdx[,2])]
  # Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
  nc_close(nc)
  print(i)
  Sys.sleep(0.01)
  flush.console()
}  

# Extract the dimensions
numr <- nrow(as.data.frame(sstlist)) # lon
numc <- ncol(as.data.frame(sstlist)) # lat
numd <- length(files) # number of days

# Convert to an array
sst_array <- array(unlist(sstlist),dim=c(numr,numc,numd))
# sst_array <- sst_array[,,,1,drop=TRUE] if there's an extra dimension, use this to drop it

LonLatIdx2 <- LonLatIdx
LonLatIdx2[,1] <- LonLatIdx[,1]-min(LonLatIdx[,1])
LonLatIdx2[,2] <- LonLatIdx[,2]-min(LonLatIdx[,2])

# Create individual objects for each map index
for(i in 1:nrow(LonLatIdx2)){
  assign(paste0("COSTR_AQRES_map_index_",COSTR_AQRES_dt$MAP_INDEX_[i]), 
         sst_array[LonLatIdx2[i,1],LonLatIdx2[i,2],])
}

# Remove unneeded map indices
rm(COSTR_AQRES_map_index_19.1,COSTR_AQRES_map_index_CPAR_1_1.1,
   COSTR_AQRES_map_index_CPAR_1_1.2,COSTR_AQRES_map_index_CPAR_1_1.3,
   COSTR_AQRES_map_index_41.1)

## Combine individual map indices into one data frame, and format columns
COSTR_AQRES_sst <- data.frame(mget(ls(pattern='COSTR_AQRES_map_index')))
COSTR_AQRES_sst$date <- gsub(glue("{data_dir}coraltemp_v3.1_"),"",names(sstlist))
COSTR_AQRES_sst$date <- gsub(".nc","",COSTR_AQRES_sst$date)
COSTR_AQRES_sst$date <- as.POSIXct(COSTR_AQRES_sst$date,format="%Y%m%d")

plot(COSTR_AQRES_sst$date,COSTR_AQRES_sst$COSTR_map_index_CPAR_1_1.4)


#Save RData file for downstream use
save(COSTR_AQRES_sst, file="data/NOAA_sst_5km_COSTR.RData")
