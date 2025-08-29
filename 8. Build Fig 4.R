#### 1. Libraries and data ####

library(dplyr)
library(glue)
library(lubridate)
library(ggplot2)
library(tidyterra)
library(terra)
library(patchwork)

# Load NMS SpatVectors
uqid <- rast("{GitHub directory}/roms_unique_id.grd")
# Load shapefiles of the NMSs
cb <- vect("{GitHub directory}/NMS Shapefiles/cb/cbnms_py.shp")
mb <- vect("{GitHub directory}/NMS Shapefiles/mb/mbnms_py.shp")
ch <- vect("{GitHub directory}/NMS Shapefiles/ch/chnms_py.shp")
ci <- vect("{GitHub directory}/NMS Shapefiles/ci/cinms_py.shp")
gf <- vect("{GitHub directory}/NMS Shapefiles/gf/gfnms_py.shp")

# Use SpatVectors of NMSs to find the cell ids within the NMSs
cb_cells <- terra::extract(uqid, cb) %>% na.omit() %>% select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="cb")
mb_cells <- terra::extract(uqid, mb) %>% na.omit() %>% select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="mb")
ch_cells <- terra::extract(uqid, ch) %>% na.omit() %>% select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="ch")
ci_cells <- terra::extract(uqid, ci) %>% na.omit() %>% select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="ci")
gf_cells <- terra::extract(uqid, gf) %>% na.omit() %>% select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="gf")
nms_cells <- rbind(cb_cells, mb_cells, ch_cells, ci_cells, gf_cells)

# Find the sizes of each NMS
sanctuary_sizes = data.frame(sanctuary=c("cb", "ci", "ch", "mb", "gf"), sizes=c(nrow(cb_cells), nrow(ci_cells), nrow(ch_cells), nrow(mb_cells), nrow(gf_cells)))

# Make an N/S ordered dataframe of NMS locations
sanctuary_locs <- data.frame(sanctuary=c("gf", "cb", "mb", "ch", "ci"), loc=c(1,2,3,4,5))

# Gather detrended MCSs
gfdl_mcs_detr = read.csv("{GitHub directory}/MCSs Detected/mcs_gfdl_detrended.csv") %>% right_join(nms_cells)
had_mcs_detr = read.csv("{GitHub directory}/MCSs Detected/mcs_had_detrended.csv") %>% right_join(nms_cells)
ipsl_mcs_detr = read.csv("{GitHub directory}/MCSs Detected/mcs_ipsl_detrended.csv") %>% right_join(nms_cells)

# Gather normal MCSs
gfdl_mcs_norm = read.csv("{GitHub directory}/MCSs Detected/mcs_gfdl_normal.csv") %>% filter(climatology=="traditional") %>% right_join(nms_cells)
had_mcs_norm = read.csv("{GitHub directory}/MCss Detected/mcs_had_normal.csv") %>% filter(climatology=="traditional") %>% right_join(nms_cells)
ipsl_mcs_norm = read.csv("{GitHub directory}/MCSs Detected/mcs_ipsl_normal.csv") %>% filter(climatology=="traditional") %>% right_join(nms_cells)

#### 2. Extract decadal statistics from MCS .csvs ####

sanctuary_names = data.frame(sanct_id=c("cb", "ci", "ch", "mb", "gf"), sanct_name=c("Cordell Bank", "Channel Islands", "Chumash Heritage", "Monterey Bay", "Greater Farallones"))

# Define a function to find stats for one csv
nms_mcs = function(csv) {
  csv = mutate(csv, decade=floor(year(date_start)/10)*10) %>% # Add decade column
    mutate(duration=difftime(date_end, date_start, unit="days")) %>% # Add duration column
    left_join(sanctuary_sizes %>% rename(sanct_id=sanctuary)) %>% # Add sanctuary sizes
    left_join(sanctuary_names)
  stats = group_by(csv, sanct_name, decade) %>%
    summarise(intensity_mean=mean(intensity_mean), days=sum(duration)/10, duration_mean=mean(duration), mean_yearly_n=n()/10, sizes=mean(sizes))
  stats$days = stats$days/stats$sizes
  stats$adj_n = stats$mean_yearly_n/stats$sizes
  # Add stats for decades with no MCSs
  for (sanct in c("Cordell Bank", "Channel Islands", "Chumash Heritage", "Monterey Bay", "Greater Farallones")) {
    for (decade in c(1980, 1990, 2000, 2010, 2020, 2040, 2050, 2060, 2070, 2080, 2090)) {
      if(length(which(filter(stats, sanct_name==sanct)$decade==decade))==0) {
        stats=rbind(stats, data.frame(sanct_name=sanct, decade=as.numeric(decade), intensity_mean=as.double(0), days=as.duration(0), duration_mean=as.duration(0), mean_yearly_n=as.double(0)))
      }
    }
  }
  # Return the data
  return(stats)
}

# Call the function for normal MCSs
gfdl_stats_norm = nms_mcs(gfdl_mcs_norm)
had_stats_norm = nms_mcs(had_mcs_norm)
ipsl_stats_norm = nms_mcs(ipsl_mcs_norm)
# Combine models
stats_norm = full_join(gfdl_stats_norm, had_stats_norm) %>% full_join(ipsl_stats_norm)
stats_norm[is.na(stats_norm)] <- 0 # Replace NAs
# Final summary
stats_norm = group_by(stats_norm, sanct_name, decade) %>% summarise(intensity_mean=mean(intensity_mean), days_mean=duration(mean(days), "seconds"), duration_mean=duration(mean(duration_mean), "seconds"), mean_yearly_n=mean(adj_n))
# Set an order of sanctuaries for plotting later
stats_norm$order = factor(stats_norm$sanct_name, ordered=T, levels=c("Channel Islands", "Chumash Heritage", "Monterey Bay", "Cordell Bank", "Greater Farallones"))

# Call the function for detrended MCSs
gfdl_stats_detr = nms_mcs(gfdl_mcs_detr)
had_stats_detr = nms_mcs(had_mcs_detr)
ipsl_stats_detr = nms_mcs(ipsl_mcs_detr)
# Combine models
stats_detr = full_join(gfdl_stats_detr, had_stats_detr) %>% full_join(ipsl_stats_detr)
stats_detr[is.na(stats_detr)] <- 0 # Replace NAs
# Final summary
stats_detr = group_by(stats_detr, sanct_name, decade) %>% summarise(intensity_mean=mean(intensity_mean), days=mean(days), duration_mean=mean(duration_mean), mean_yearly_n=mean(adj_n))
# Set an order of sanctuaries for plotting later
stats_detr$order = factor(stats_detr$sanct_name, ordered=T, levels=c("Channel Islands", "Chumash Heritage", "Monterey Bay", "Cordell Bank", "Greater Farallones"))
stats_detr = filter(stats_detr, decade != "2100")

#### 3. Prepare to plot ####

# Set theme pre-plotting
theme_set(theme(axis.title=element_text(size=20), axis.text=element_text(size=16), legend.text=element_text(size=16), legend.title=element_text(size=16), plot.title=element_text(size=20)))

# Make normal plots
n_days = ggplot(stats_norm) + geom_tile(aes(x=decade, y=order, fill=days_mean/86400)) + scale_fill_viridis_c("Mean cold\nspell days/year", option="G", direction=-1, limits=c(0,51), na.value="white")+ylab("Sanctuary")+xlab("")+ggtitle("A. Fixed Cold Spell Days")+theme_classic()+theme_get()+theme(legend.position="right", axis.text.y = element_text(angle = 25, margin=margin(r=10)), axis.text.x = element_blank())

n_duration = ggplot(stats_norm) + geom_tile(aes(x=decade, y=order, fill=duration_mean/86400)) + scale_fill_viridis_c("Mean\nduration\n(days)",option="D", direction=-1, na.value="white", lim=c(0.001,20))+ylab("Sanctuary")+xlab("")+ggtitle("C. Fixed Cold Spell Duration")+theme_classic()+theme_get()+theme(legend.position="right", axis.text.y=element_text(angle=25, margin=margin(r=10)), axis.text.x=element_blank()) # For this one I divide the duration (in seconds) by the number of seconds per day
n_intensity = ggplot(stats_norm)+geom_tile(aes(x=decade,y=order,fill=intensity_mean))+scale_fill_viridis_c("Mean temp\nanomaly (°C)",option="C",na.value="white", lim=c(-2.5,-0.001))+ylab("Sanctuary")+xlab("Decade")+ggtitle("E. Fixed Cold Spell Intensity")+theme_classic()+theme_get()+ scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080))+theme(legend.position="right",axis.text.y=element_text(angle=25, margin=margin(r=10)), axis.text.x=element_text(angle=25, margin=margin(t=8)))

# Make detrended plots
d_days = ggplot(stats_detr) + geom_tile(aes(x=decade, y=order, fill=days)) + scale_fill_viridis_c("Mean cold\nspell days/year", option="G", limits=c(0,51), direction=-1, na.value="white")+ylab("")+xlab("")+ggtitle("B. Detrended Cold Spell Days")+theme_classic()+theme_get()+theme(legend.position="right", axis.text.y = element_blank(), axis.text.x = element_blank())

d_duration = ggplot(stats_detr) + geom_tile(aes(x=decade, y=order, fill=duration_mean)) + scale_fill_viridis_c("Mean\nduration\n(days)",option="D", direction=-1, na.value="white",lim=c(0.001,20))+ylab("")+xlab("")+ggtitle("D. Detrended Cold Spell Duration")+theme_classic()+theme_get()+theme(legend.position="right", axis.text.y=element_blank(), axis.text.x=element_blank())

d_intensity = ggplot(stats_detr)+geom_tile(aes(x=decade,y=order,fill=intensity_mean))+scale_fill_viridis_c("Mean temp\nanomaly (°C)",option="C",na.value="white", lim=c(-2.5,-0.001))+ylab("")+xlab("Decade")+ggtitle("F. Detrended Cold Spell Intensity")+theme_classic()+theme_get()+ scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080))+theme(legend.position="right",axis.text.y=element_blank(), axis.text.x=element_text(angle=25, margin=margin(t=8)))

# Final plot
(n_days+d_days)/(n_duration+d_duration)/(n_intensity+d_intensity) + plot_layout(guides="collect")
