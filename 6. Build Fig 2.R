### 1. Libraries and data ####

library(ncdf4)
library(dplyr)
library(ggplot2)
library(heatwaveR)
library(lubridate)
library(patchwork)   
library(ragg)

# Open nc
# Access netCDF of downscaled sea surface temperature data. This data was published in Pozo Buil et al. (2021) and is available upon request from the corresponding author of that paper. 
nc = nc_open('/Volumes/One Touch/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/SST netCDFs/sst_gfdl.nc')
# Extract spatiotemporal data from nc
lon = ncvar_get(nc, "lon")[,1]; lat = ncvar_get(nc, "lat")[1,]
sst = ncvar_get(nc, "sst")
yr <- ncvar_get(nc,'year'); mth <- ncvar_get(nc,'month'); day <- ncvar_get(nc,'day')
tim <- as.POSIXct(paste(yr,mth,day,sep='-'),tz='UTC') %>% as.Date()
# Create a time series for a cell in MB - lat[68] = 36.7 and lon[121] = -122.0 and (36.7,-122.0) is a point in MB
ts = data.frame(t=tim,sst=sst[68,121,])

#### 2. Detect MCSs traditionally ####

# Climatology
clim_normal = ts2clm(ts, y=sst, climatologyPeriod=c("1980-01-01", "2009-12-31"), pctile=10)
# Detection
mcs_normal = detect_event(clim_normal, coldSpells=T, y=sst)
# Dataframe from detection
mcs_normal = as.data.frame(mcs_normal[["event"]])

#### 3. Detect MCSs detrended ####

ts$doy <- as.integer(yday(ts$t)) # Set day of year (doy)

# Define a function that finds daily trends
day_split <- function(doyid, ...) {
  # Filter the time series to a given day
  dayts <- filter(ts, doy==doyid)
  # Set up the days for quadratic equation
  dayts$dayid <- seq(1,nrow(dayts))
  dayts$daysquare <- dayts$dayid^2
  # Find a quadratic model
  model <- lm(sst ~ dayid + daysquare, data=dayts)
  # Predict the model
  preddata <- mutate(dayts, pred=predict(model, dayts))
  return(preddata)
}

# And apply the function
ts.trended <- as.data.frame(do.call(rbind, (lapply(seq(1,366), FUN=day_split))))
ts.trended <- ts.trended[order(ts.trended$t),] # Reorder by date
ts.trended$detrended <- ts.trended$sst - ts.trended$pred # Detrend the data
# Edit the time series so heatwaveR likes it
ts.detrended <- dplyr::select(ts.trended, t, detrended) %>%
  rename(temp=detrended)

# Generate a climatology from the SST anomalies
clim_detr <- ts2clm(ts.detrended, climatologyPeriod=c("1980-01-01", "2100-12-31"), pctile=10, smoothPercentile=F, roundClm=F)

# Detect events
mcs_detr <- detect_event(clim_detr, coldSpells=T)
# Create a nice dataframe of MCSs
mcs_detr <- dplyr::select(mcs_detr[["event"]])

#### 4. Prepare to plot ####

# Set theme pre-plotting
theme <- theme_set(theme_classic())
theme_set(theme(axis.title=element_text(size=10,face="bold"), axis.text=element_text(size=8), legend.text=element_text(size=8), plot.title=element_text(size=12, hjust=0,face="bold"),legend.title=element_text(size=10,face="bold"),panel.border=element_rect(color="black",fill=NA,linewidth=1),axis.line=element_blank()))

# Saving all as 1000 x 500

#### 5. Plot traditional MCS detection ####

# # Slice the clim
trad.slice = slice(clim_normal, which(clim_normal$t=="1990-01-01"):which(clim_normal$t=="1993-12-31"))
# 
# # Plot traditional MCSs
# ggplot(data=trad.slice, aes(x=t)) +
#   geom_line(aes(y=sst,colour="Observed\ntemperature"))+
#   geom_line(aes(y = seas, colour = "Baseline\n"))+
#   geom_line(aes(y = thresh, colour = "Cold-spell\nthreshold"))+
#   geom_flame(data = trad.slice, aes(y = thresh, y2 = sst, fill="Cold-spell"))+
#   scale_colour_manual("", values=c("Observed\ntemperature"="grey70","Baseline\n" = "black", "Cold-spell\nthreshold" =  "#1f9e89"))+ #, "Cold-spell\nthreshold" =  "#1f9e89"
#   scale_fill_manual("", values=c("Cold-spell"="#3e4989"))+
#   scale_x_date(date_labels="%Y-%m")+
#   theme_classic()+theme_get()+
#   theme(legend.position="right")+
#   ylab("Sea Surface Temperature (°C)")+xlab("Date(Year-Month)") +
#   ylim(c(-2.5,20.5))

#### 6. Plot detrended MCS detection ####

# # Slice the clim
detr.slice = slice(clim_detr, which(clim_detr$t=="1990-01-01"):which(clim_detr$t=="1993-12-31")) %>% rename(sst=temp)
# 
# # Plot detrended  MCSs
# ggplot(data=detr.slice, aes(x=t)) +
#   geom_line(aes(y=sst,colour="Observed\ntemperature"))+
#   geom_line(aes(y = seas, colour = "Baseline\n"))+
#   geom_line(aes(y = thresh, colour = "Cold-spell\nthreshold"))+
#   geom_flame(data = detr.slice, aes(y = thresh, y2 = sst, fill="Cold-spell"))+
#   scale_colour_manual("", values=c("Observed\ntemperature"="coral3","Baseline\n" = "black", "Cold-spell\nthreshold" =  "#1f9e89"))+ #, "Cold-spell\nthreshold" =  "#1f9e89"
#   scale_fill_manual("", values=c("Cold-spell"="#3e4989"))+
#   scale_x_date(date_labels="%Y-%m")+
#   theme_classic()+theme_get() +
#   theme(legend.position="right")+
#   ylab("Sea Surface Temperature (°C)")+xlab("Date(Year-Month)") +
#   ylim(c(-2.5,20.5))

#### 7. Plot SST ####
  
# Slice to time period of interest
#sst.slice <- slice(ts, which(ts$t=="1990-01-01"):which(ts$t=="1993-12-31"))

# Plot SST and anomalies together
a <- ggplot(data=ts.trended, aes(x=t)) +
  geom_line(aes(y=sst, color="Modeled SST")) +
  geom_line(aes(y=detrended, color="Detrended SSTa")) +
  scale_color_manual("", values=c("Modeled SST"="grey70", "Detrended SSTa"="#bc3754")) +
  scale_x_date(date_labels="%Y-%m") +
  theme_classic() + theme_get() +
  theme(legend.position="top", legend.background = element_rect(color="black"))+
  #guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  ylab("Sea Surface Temperature (°C)") + xlab("Date (Year-Month)") + ggtitle("A. 120 Years of Sea Surface Temperature")

#### 8. All together now? ####

b<- ggplot() +
    geom_line(data=trad.slice, aes(x=t, y=sst,colour="Modeled SST"))+
    geom_line(data=trad.slice, aes(x=t, y = seas, colour = "Baseline"))+
    geom_line(data=trad.slice, aes(x=t, y = thresh, colour = "Cold-spell threshold"))+
    geom_flame(data = trad.slice, aes(x=t,y = thresh, y2 = sst, fill="Cold-spell"))+
    geom_line(data=detr.slice, aes(x=t, y=sst, color="Detrended SSTa"))+
    geom_line(data=detr.slice, aes(x=t, y=seas, color="Baseline"))+
    geom_line(data=detr.slice, aes(x=t, y=thresh, color="Cold-spell threshold"))+
    geom_flame(data=detr.slice,aes(x=t,y=thresh,y2=sst,fill="Cold-spell"))+
    scale_colour_manual("", values=c("Modeled SST"="grey70","Baseline" = "black", "Cold-spell threshold" =  "#21918c", "Detrended SSTa"="#bc3754"))+ #, "Cold-spell\nthreshold" =  "#1f9e89"
    scale_fill_manual("", values=c("Cold-spell"="#3b528b"))+
    scale_x_date(date_labels="%Y-%m")+
    theme_classic()+theme_get()+
    theme(legend.position="top", legend.background=element_rect(color="black"), legend.box="vertical")+
    guides(color=guide_legend(nrow=2,byrow=TRUE)) +
    ylab("Sea Surface Temperature (°C)")+xlab("Date (Year-Month)") + ggtitle("B. Example Marine Cold-spell Detection")

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("Fig2.tiff", width = 7, height = 7, units = "in", res = 300)

a / b # 800 x 900 is good with lowres save image

dev.off()
