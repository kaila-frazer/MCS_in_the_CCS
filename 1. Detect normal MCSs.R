# This code was originally written by Heather Welch. Kaila Frazer has tweaked it and updated the commenting.

#### Libraries ####

library(glue)
library(ncdf4)
library(dplyr)
library(ggplot2)
library(heatwaveR)
library(cowplot)
library(data.table)
library(tidyverse)
library(foreach)
library(raster)
library(doParallel, quietly = TRUE)

#### Global objects ####

model = "gfdl" # Change this for each run

outdir="your directory" # Update with your personal directory
nc=glue("/Downloads/sst_daily_roms_{model}_1980_2100.nc") # Downscaled climate projections from Pozo Buil et al. 2021. For the paper, I ran this code three times (once with each of the GFDL, HAD, and IPSL models).

# Gather SST from the netCDF
varname="sst"
nc.data=nc_open(nc)
lat <- ncvar_get(nc.data,'lat'); lat <- lat[1,] # Get longitudes
lon <- ncvar_get(nc.data,'lon'); lon <- lon[,1] # Get latitudes
yr <- ncvar_get(nc.data,'year'); mth <- ncvar_get(nc.data,'month'); day <- ncvar_get(nc.data,'day') # Get time
tim <- as.POSIXct(paste(yr,mth,day,sep='-'),tz='UTC') # Make dates

#### Create a unique identifier raster - one time run ####

data.var.grid  <-  ncvar_get(nc.data,varname,start=c(1,1,1), # Start extracting at the first column (lon), row (lat), and the first time slot
                              count=c(-1,-1,1),verbose=FALSE) # Keep extracting for all of the lons (-1 means get everything), all of the lats, just 1 time slot

dat1=list()
dat1$x=lon
dat1$y=lat
dat1$z=t(data.var.grid)

template <-raster(
  dat1$z,
  xmn=range(dat1$x)[1], xmx=range(dat1$x)[2],
  ymn=range(dat1$y)[1], ymx=range(dat1$y)[2], 
  crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
) %>% flip() # This is one layer from the netcdf now

num=template@data@values %>% na.omit(.) %>% length() # How many pixels w data the ROMS fields have
random_id=seq(1:num)
template[values(template)>=0]=random_id # Now each cell in the template has a unique identifier
writeRaster(template,glue("{outdir}/roms_unique_id")) # This will be your master key. Only write it out once, and then use the same template for all of the other climate models 

random_id = raster(glue("{outdir}/roms_unique_id.grd")) # After you make the master key once, read it back in here.

master=rasterToPoints(random_id) %>% as.data.frame() %>% 
  rename(id=layer) %>% 
  rename(lon=x) %>% 
  rename(lat=y)

#### Detect MCSs ####

# Set up for the parallel process
start=Sys.time()
detectCores() # Don't use more cores than you have, and probably dont use all your cores, e.g. if you have 6 use 3-4
registerDoParallel(10) # This is where you input the number of cores to use

ROMS_mcs=foreach(i=1:length(random_id),
        .packages = c("heatwaveR","glue","ncdf4","tidyverse"),.verbose=T, 
        .export = ls(globalenv()),
        .combine = c("rbind")) %dopar% {
 
   print(glue("Pixel id {random_id[i]}"))
   inpts=master %>% filter(id==random_id[i])
  
  tryCatch(
    expr ={
      
      c <- which.min(abs(lon-inpts$lon)) # Which column of data to get
      r <- which.min(abs(lat-inpts$lat)) # Which row of data to get
      data.var.point  <-  ncvar_get(nc.data,varname,start=c(c,r,1), # Start extracting as c, r, and the first time slot
                                      count=c(1,1,-1), # Keep extracting for 1 column (lon), 1 row (lat), all of the time slots (-1 means get everything)
                                    verbose=FALSE) # Now we've extracted SST data across the whole time series for this pixel
      
      df=data.frame(t=as.Date(tim),temp=data.var.point)
      
      # Detect MCSs using heatwaveR software
      clim=ts2clm(data=df,climatologyPeriod=c("1980-01-01", "2009-12-31"), pctile=10) # Set the climatology period and tell heatwaveR you want to be detecting cold-spells, not heatwaves
      mcs <- detect_event(clim, coldSpells=T, as.Date(tim))
      mcs <- dplyr::select(mcs[["event"]], event_no, date_start, date_peak, date_end, intensity_mean, intensity_max, intensity_cumulative, rate_onset, rate_decline) %>% 
        mutate(pixel_id=inpts$id,
               lon=inpts$lon,
               lat=inpts$lat,
               climatology="traditional") # Clean up the output dataframe
      return(mcs)
    },
    
    error = function(e){
      message(glue("No data"))
      print(e)
    }
  )
        }

end=Sys.time()
dif=end-start
print(dif)

write.csv(ROMS_mcs,glue("{outdir}/MCSs Detected/mcs_{model}_normal.csv")) # Save final result