#### 1. Libraries and data ####

library(terra)
library(viridis)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(glue)
library(dplyr)
library(lubridate)
library(vroom)
library(ragg)

# Load pixel ID data
pixel_rast = rast('/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/roms_unique_id.grd') %>% as.data.frame(xy=T) %>% rename(pixel_id=layer)

# Define a function to find the number of MCSs detected in each cell for different time periods and detection methods
n_mcs = function(period, method, ...) { # period is future or present, method is traditional or detrended
  get_csv = function(model, ...) {
    # Load relevant CSVs and filter to time period
    if (method == "traditional") {
      csv = vroom(glue("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/MCSs Detected/mcs_{model}_normal.csv")) %>% filter(climatology=="traditional") # mcss for everywhere
    }
    if (method == "detrended") {
      csv = vroom(glue("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/MCSs Detected/mcs_{model}_detrended.csv"))
    }
    csv$duration = difftime(csv$date_end, csv$date_start)
    if (period == "present") {
      csv = filter(csv, date_start >= "1980-01-01", date_end <= "2010-01-01") %>% group_by(pixel_id) %>% summarise(days=sum(duration)/30) %>% right_join(pixel_rast) #n=n(),
    }
    if (period == "future") {
      csv = filter(csv, date_start >= "2070-01-01") %>% group_by(pixel_id) %>% summarise(days=sum(duration)/30) %>% right_join(pixel_rast) #n=n(), is in summarise when we're trying to get n
    }
    csv$days[is.na(csv$days)] = as.duration(0) # trying this method since i need to convert type
    #csv[is.na(csv)] = 0 # Replace NAs with zeros - important for averaging
    return(csv)
  }
  gfdl = get_csv("gfdl") %>% rename(gfdl_days=days) # Access GFDL data # when getting n also add gfld_n = n and so on for the rest
  #gfdl_rast_n = as_spatraster(select(gfdl, x, y, gfdl_n)) # Save a GFDL SpatRaster
  gfdl_rast_days = as_spatraster(select(gfdl, x, y, gfdl_days)) # Save a GFDL n days Spatraster
  had = get_csv("had") %>% rename(had_days=days) # Access HAD data
  #had_rast_n = as_spatraster(select(had, x, y, had_n)) # Save a HAD SpatRaster
  had_rast_days = as_spatraster(select(had, x, y, had_days))
  ipsl = get_csv("ipsl") %>% rename(ipsl_days=days) # Access IPSL data
  #ipsl_rast_n = as_spatraster(select(ipsl, x, y, ipsl_n)) # Save an IPSL SpatRaster
  ipsl_rast_days = as_spatraster(select(ipsl, x, y, ipsl_days))
  all = full_join(gfdl, had) %>% full_join(ipsl) # Join the model data
  #all$n = rowMeans(select(all, c(gfdl_n, had_n, ipsl_n))) # Take the mean n MCSs for three models
  all$gfdl_days = as.numeric(all$gfdl_days); all$had_days = as.numeric(all$had_days); all$ipsl_days = as.numeric(all$ipsl_days)
  all$days = rowMeans(select(all, c(gfdl_days, had_days, ipsl_days)))
  #all_rast_n = as_spatraster(select(all, x, y, n)) # Save a SpatRaster of mean n MCSs
  all_rast_days = as_spatraster(select(all, x, y, days))
  return(c(gfdl_rast_days, had_rast_days, ipsl_rast_days, all_rast_days)) # Return the SpatRasters
}

# Call the function to get SpatRasters - call[[1]] will always be the GFDL SpatRaster, [[2]] will be HAD, [[3]] will be IPSL, and [[4]] will be mean
pres_norm_days = n_mcs("present", "traditional")
fut_norm_days = n_mcs("future", "traditional")
pres_detr_days = n_mcs("present", "detrended")
fut_detr_days = n_mcs("future", "detrended")

#### 2. Plot ####

# Collect NMS SpatVectors
cb <- vect("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/cb/cbnms_py.shp")
mb <- vect("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/mb/mbnms_py.shp")
ch <- vect("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/ch/chnms_py.shp")
ci <- vect("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/ci/cinms_py.shp")
gf <- vect("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/gf/gfnms_py.shp")

# # Get bathymetry data
# z <- rast("C:/Users/kaila/Desktop/Marine Cold-Spell Manuscript/Figures/Fig 3/z.grd")
# shelf <- as.contour(z, levels=c(-200))
# isobath <- as.contour(z, levels=c(-2000))

# Set theme pre-plotting
theme <- theme_set(theme_classic())
theme_set(theme(axis.title=element_text(size=10,face="bold"), axis.text=element_text(size=8), legend.text=element_text(size=8, margin=margin(l=8)), legend.key.size=unit(1,"cm"), legend.title=element_text(size=10,face="bold"), plot.title=element_text(size=12,face="bold",hjust=0), axis.text.x=element_text(margin=margin(t=8)),panel.border=element_rect(color="black",fill=NA,linewidth=1),axis.line=element_blank()))


pn_plot = ggplot() + geom_spatraster(data=pres_norm_days[[4]]) + coord_sf() + 
  scale_fill_viridis_b("Mean cold-spell\ndays per year\n", na.value="white", limits=c(0,50), direction=-1, option="G",n.breaks=10) + #scale was 14,50
  geom_spatvector(data=cb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ci, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=gf, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=mb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ch, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  labs(title="A. 1980-2009", y="Fixed Method") + theme_classic() + theme_get() + theme(axis.text.x=element_blank())

fn_plot = ggplot() + geom_spatraster(data=fut_norm_days[[4]]) + coord_sf() + scale_fill_viridis_b("Mean cold-spell\ndays per year\n", na.value="white",limits=c(0,50), direction=-1, option="G",n.breaks=10) + #previously no scale
  geom_spatvector(data=cb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ci, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=gf, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=mb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ch, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  labs(title="B. 2070-2099", y="") + theme_classic() + theme_get() + theme(axis.text.y=element_blank(), axis.text.x=element_blank())

pd_plot = ggplot() + geom_spatraster(data=pres_detr_days[[4]]) + coord_sf() + scale_fill_viridis_b("Mean cold-spell\ndays per year\n", limits=c(0,50), na.value="white", direction=-1, option="G", n.breaks=10) + #scale was 14,50
  geom_spatvector(data=cb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ci, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=gf, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=mb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ch, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  labs(title="C. 1980-2009", y="Detrended Method") + theme_classic() + theme_get()

fd_plot = ggplot() + geom_spatraster(data=fut_detr_days[[4]]) + coord_sf() + scale_fill_viridis_b("Mean cold-spell\ndays per year\n", limits=c(0,50), na.value="white", direction=-1, option="G", n.breaks=10) + #scale was 14,50
  geom_spatvector(data=cb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ci, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=gf, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=mb, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  geom_spatvector(data=ch, fill=NA, linewidth=.6, show.legend=F, aes(color="")) + 
  labs(title="D. 2070-2099", y="") + theme_classic() + theme_get() + theme(axis.text.y=element_blank())

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("Fig3.tiff", width = 7, height = 7, units = "in", res = 300)

((pn_plot / pd_plot) | (fn_plot / fd_plot)) + plot_layout(guides="collect") # 770 x 690 is good

dev.off()

#### 3. Supplemental model-by-model plots ####

pn_gfdl = ggplot() + geom_spatraster(data=pres_norm_days[[1]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G", n.breaks=6) + labs(title="C. GFDL", ylab="") + theme_classic() + theme_get() + theme(axis.text.x=element_blank(), axis.text.y=element_blank())

pn_had = ggplot() + geom_spatraster(data=pres_norm_days[[2]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G", n.breaks=6) + labs(title="A. HAD",y="Fixed Present\n(1980-2009)") + theme_classic() + theme_get() + theme(axis.text.x=element_blank())

pn_ipsl = ggplot() + geom_spatraster(data=pres_norm_days[[3]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="B. IPSL",y="") + theme_classic() + theme_get() + theme(axis.text.x=element_blank(), axis.text.y=element_blank())

fn_gfdl = ggplot() + geom_spatraster(data=fut_norm_days[[1]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="I.") + theme_classic() + theme_get() + theme(axis.text.x=element_blank(), axis.text.y=element_blank())

fn_had = ggplot() + geom_spatraster(data=fut_norm_days[[2]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="G.", y="Fixed Future\n(2070-2099)") + theme_classic() + theme_get() + theme(axis.text.x=element_blank())

fn_ipsl = ggplot() + geom_spatraster(data=fut_norm_days[[3]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="H.") + theme_classic() + theme_get() + theme(axis.text.x=element_blank(), axis.text.y=element_blank())

pd_gfdl = ggplot() + geom_spatraster(data=pres_detr_days[[1]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="F.", y="") + theme_classic() + theme_get() + theme(axis.text.x=element_blank(), axis.text.y=element_blank())

pd_had = ggplot() + geom_spatraster(data=pres_detr_days[[2]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="D.", y="Detrended Present\n(1980-2009)") + theme_classic() + theme_get() + theme(axis.text.x=element_blank())

pd_ipsl = ggplot() + geom_spatraster(data=pres_detr_days[[3]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="E.",y="") + theme_classic() + theme_get() + theme(axis.text.x=element_blank(), axis.text.y=element_blank())

fd_gfdl = ggplot() + geom_spatraster(data=fut_detr_days[[1]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="L.") + theme_classic() + theme_get() + theme(axis.text.y=element_blank())

fd_had = ggplot() + geom_spatraster(data=fut_detr_days[[2]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="J.", y="Detrended Future\n(2070-2099)") + theme_classic() + theme_get()

fd_ipsl = ggplot() + geom_spatraster(data=fut_detr_days[[3]]) + coord_sf() + scale_fill_viridis_b("Mean cold-\nspell days\nper year", na.value="white", limits=c(0, 60), direction=-1, option="G",n.breaks=6) + labs(title="K.") + theme_classic() + theme_get() + theme(axis.text.y=element_blank())

ragg::agg_tiff("S4Fig.tiff", width = 7, height = 7, units = "in", res = 300)

((pn_had / pd_had / fn_had / fd_had) | (pn_ipsl / pd_ipsl / fn_ipsl / fd_ipsl) | (pn_gfdl / pd_gfdl / fn_gfdl / fd_gfdl)) + plot_layout(guides="collect") # 550 x 700 is good

dev.off()

#### 4. Pull some stats ####

pres_norm_d = terra::global(pres_norm_days, "mean", na.rm=T)
fut_norm_d = terra::global(fut_norm_days, "mean", na.rm=T)
pres_detr_d = terra::global(pres_detr_days, "mean", na.rm=T)
fut_detr_d = terra::global(fut_detr_days, "mean", na.rm=T)
