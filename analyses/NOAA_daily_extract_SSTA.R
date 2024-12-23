# Load necessary packages
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
require(svMisc)

# Make a list of files to extract data from
data_dir <- "D:/NOAA_DHW_daily/ssta/" # PUT YOUR LOCATION OF THE NOAA DATA HERE
files=list.files(data_dir,full.names = TRUE)

## Test on one file to make sure it works:
## I recommend doing this to make sure the function below will work the way you expect it to. You'll have to change the file name/path and change "analyzed_sst" to whatever the DHW variable name is
testfile <- paste0(data_dir,"ct5km_ssta_v3.1_19900917.nc") # This is a file you expect to be in your data directory. Change the name in quotes if you don't expect to have this particular file for testing
nc <- nc_open(testfile)
ssta_full <- ncvar_get( nc, "sea_surface_temperature_anomaly")
# get longitude and latitude
lon <- ncvar_get(nc,"lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(nc,"lat")
nlat <- dim(lat)
head(lat)
nc_close(nc) # THIS IS IMPORTANT, OR YOU WILL CORRUPT YOUR FILES

#######
EStr_dt <- read.csv("data/EStr_datatable.csv")
WStr_dt <- read.csv("data/WStr_datatable.csv")
Coast_dt <- read.csv("data/Coast_datatable.csv")
AQRES_dt <- read.csv("data/AQRES_datatable.csv")

AQRES_dt <- AQRES_dt[c(1:2,5:6,3,8,7)]

AQRES_dt <- AQRES_dt %>% 
  rename(MAP_INDEX_=map_index) %>% 
  rename(Shape_Length=SHAPE_Length) %>% 
  rename(Shape_Area=SHAPE_Area) %>% 
  rename(Longitude=longitude) %>% 
  rename(Latitude=latitude) %>% 
  mutate(sub_basin=NA) %>% 
  mutate(reach=NA) %>% 
  mutate(map_index=NA)
  

COSTR_AQRES_dt <- rbind(EStr_dt,WStr_dt,Coast_dt,AQRES_dt)

######
LonLatIdx <- array(dim=c(nrow(COSTR_AQRES_dt),2))
LonLatVal <- array(dim=c(nrow(COSTR_AQRES_dt),2))

for (j in 1:nrow(COSTR_AQRES_dt)) {
  LonLatVal[j,1] <- lon[which.min(abs(COSTR_AQRES_dt$Longitude[j]-lon))]
  LonLatVal[j,2] <- lat[which.min(abs(COSTR_AQRES_dt$Latitude[j]-lat))]
  LonLatIdx[j,1] <- which(lon==LonLatVal[j,1])
  LonLatIdx[j,2] <- which(lat==LonLatVal[j,2])
}

minlon <- min(LonLatIdx[,1])
maxlon <- max(LonLatIdx[,1])
minlat <- min(LonLatIdx[,2])
maxlat <- max(LonLatIdx[,2])

minlatlon <- c(minlon,minlat)
maxlatlon <- c(maxlon,maxlat)


maxlatlon-minlatlon # 42 unique lons, 52 unique lats

all_lonidx <- seq(from=minlatlon[1],to=maxlatlon[1])
all_latidx <- seq(from=minlatlon[2],to=maxlatlon[2])

# this makes an empty dataframe where rows are lon and columns are lat
test <- data.frame(matrix(nrow=43,ncol=53))
rownames(test) <- all_lonidx
colnames(test) <- all_latidx

sstalist <- list()
for (i in files) {
  # Open the netcdf file
  nc <- nc_open(i)
  sstalist[[i]] <- ncvar_get( nc,
                              "sea_surface_temperature_anomaly")[1105:1147,823:875]
  # Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
  nc_close(nc)
  print(i)
  Sys.sleep(0.01)
  flush.console()
} # this currently works!

#save(sstalist,file="data/sstalist.RData")
#load(file="data/sstalist.RData")

# sstalist dims are [1:43, 1:53, 1:12419] which is [lon, lat, date]

ssta_array <- array(unlist(sstalist),dim=c(43,53,12419))

LonLatIdx2 <- LonLatIdx
LonLatIdx2[,1] <- LonLatIdx[,1]-1105
LonLatIdx2[,2] <- LonLatIdx[,2]-823

COSTR_AQRES_dt$MAP_INDEX_[1:82] <- round(as.numeric(COSTR_AQRES_dt$MAP_INDEX_[1:82]),2)
COSTR_AQRES_dt$MAP_INDEX_ <- gsub("-","_",COSTR_AQRES_dt$MAP_INDEX_)
COSTR_AQRES_dt$MAP_INDEX_ <- gsub(",","_",COSTR_AQRES_dt$MAP_INDEX_)

for(i in 1:nrow(LonLatIdx2)){
  assign(paste0("COSTR_map_index_",COSTR_AQRES_dt$MAP_INDEX_[i]), 
         ssta_array[LonLatIdx2[i,1],LonLatIdx2[i,2],])
}

rm(COSTR_map_index_19.1,COSTR_map_index_CPAR_1_1.1,COSTR_map_index_CPAR_1_1.2,COSTR_map_index_CPAR_1_1.3)

COSTR_AQRES_ssta <- data.frame(mget(ls(pattern='COSTR_map_index')))
COSTR_AQRES_ssta$date <- gsub("D:/NOAA_DHW_daily/ssta/ct5km_ssta_v3.1_","",names(sstalist))
COSTR_AQRES_ssta$date <- gsub(".nc","",COSTR_AQRES_ssta$date)
COSTR_AQRES_ssta$date <- as.POSIXct(COSTR_AQRES_ssta$date,format="%Y%m%d")

plot(COSTR_AQRES_ssta$date,COSTR_AQRES_ssta$COSTR_map_index_10.1)


#Save RData file for downstream use
save(COSTR_AQRES_ssta, file="data/NOAA_ssta_5km_COSTR.RData")
