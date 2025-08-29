# This code was originally written by Heather Welch. Kaila Frazer has tweaked it and updated the commenting.

#### Libraries and data ####

library(ncdf4)
  library(dplyr)
  library(ggplot2)
  library(heatwaveR)
  library(cowplot)
  library(data.table)
  library(rgeos)
  library(sf)
  library(tidyr)
  library(ggpubr)
  library(raster)
  library(glue)
  library(rts)
  library(patchwork)
  library(glue)
  library(tidyverse)
  library(foreach)
  library(parallel)
  library(doParallel, quietly = TRUE)

#### Global objects ####

model = "gfdl" # Change this for each run

outdir="your directory" # Update with your personal directory
nc=glue("/Downloads/sst_daily_roms_{model}_1980_2100.nc") # Downscaled climate projections from Pozo Buil et al. 2021. For the paper, I ran this code three times (once with each of the GFDL, HAD, and IPSL models).

# Load SST
sstVar <- ncvar_get(nc, 'sst')
# Load lat and lon values
lat <- ncvar_get(nc,'lat'); lat <- lat[1,]
lon <- ncvar_get(nc,'lon'); lon <- lon[,1]
yr <- ncvar_get(nc,'year'); mth <- ncvar_get(nc,'month'); day <- ncvar_get(nc,'day') # Get the time components
tim <- as.POSIXct(paste(yr,mth,day,sep='-'),tz='UTC') # Clean up times

# Load unique identifier raster made in 1. Detect normal MCSs.R
template = raster(glue("{outdir}/roms_unique_id.grd"))

master=rasterToPoints(template) %>% as.data.frame() %>% 
  rename(id=layer) %>% 
  rename(lon=x) %>% 
  rename(lat=y)
random_id=seq(1:nrow(master))

#### Detect detrended MCSs ####

# Set up the parallel process
start=Sys.time()
detectCores() # Same message from Heather as previous script: don't use more cores than you have, and probably dont use all your cores, e.g. if you have 6 use 3-4
registerDoParallel(15)

ROMS_mcs=foreach(i=1:nrow(master),
                 .packages = c("heatwaveR","glue","ncdf4","tidyverse"),.verbose=T, 
                 .export = ls(globalenv()),
                 .combine = c("rbind")) %dopar% {
                   
                   print(glue("Pixel id {random_id[i]}"))
                   inpts=master %>% filter(id==random_id[i])
                   
                   tryCatch(
                     expr ={
                       
                       c <- which.min(abs(lon-inpts$lon)) # Which column of data to get
                       r <- which.min(abs(lat-inpts$lat)) # Which row of data to get
                       data.var.point  <-  ncvar_get(nc,"sst",start=c(c,r,1), # Start extracting as c, r, and the first time slot
                                                     count=c(1,1,-1), # Keep extracting for 1 column (lon), 1 row (lat), all of the time slots (-1 means get everything)
                                                     verbose=FALSE) # Now we've extracted SST data across the whole time series for this pixel
                       
                       df=data.frame(t=as.Date(tim),temp=data.var.point) # Time series
                       df$doy <- as.integer(yday(df$t)) # Set day of year (doy). This is where this code begins to split with the normal MCS detection methods

                       # Define a function that finds daily trends
                       day_split <- function(doyid, ...) {
                         # Filter the time series to a given day
                         dayts <- filter(df, doy==doyid)
                         # Set up the days for quadratic equation
                         dayts$dayid <- seq(1,nrow(dayts))
                         dayts$daysquare <- dayts$dayid^2
                         # Find a quadratic model
                         model <- lm(temp ~ dayid + daysquare, data=dayts)
                         # Predict the model
                         preddata <- mutate(dayts, pred=predict(model, dayts))
                         return(preddata)
                       }
                       
                       # And apply the function
                       ts.df.trended.daily <- as.data.frame(do.call(rbind, (lapply(seq(1,366), FUN=day_split))))
                       ts.df.trended.daily <- ts.df.trended.daily[order(ts.df.trended.daily$t),] # Reorder by date
                       ts.df.trended.daily$detrended <- ts.df.trended.daily$temp - ts.df.trended.daily$pred # Detrend the data
                       # Edit the time series so heatwaveR likes it
                       ts.detrended.daily <- dplyr::select(ts.df.trended.daily, t, detrended) %>%
                         rename(temp=detrended)
                       
                       # Generate a climatology from the SST anomalies
                       clim <- ts2clm(ts.detrended.daily, climatologyPeriod=c("1980-01-01", "2100-12-31"), pctile=10, smoothPercentile=F, roundClm=F)
                       
                       # Detect detrended events
                       mcss <- detect_event(clim, coldSpells=T)
                       # Create a nice dataframe of MCSs
                       mcs <- dplyr::select(mcss[["event"]], event_no, date_start, date_peak, date_end, intensity_mean, intensity_max, intensity_cumulative, rate_onset, rate_decline) %>%
                         mutate(pixel_id=inpts$id,
                                   lon=inpts$lon,
                                   lat=inpts$lat,
                                   climatology="quadratic")
                       
                       return(mcs)          },
                     
                     error = function(e){
                       message(glue("No data"))
                       print(e)
                     }
                   )
                 }

end=Sys.time()
dif=end-start
print(dif)

write.csv(ROMS_mcs,glue("{outdir}/MCSs Detected/mcs_{model}_detrended.csv"))