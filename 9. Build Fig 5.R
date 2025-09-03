library(glue)
library(terra)
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(cowplot)
library(magick)
library(ragg)
setwd("/Users/kailafrazer/")

#### Average current habitat ####

#setwd("Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/Species Projections/")
# #set present and future same as the time ranges I use to determine cold-spells for this figure
# present <- seq(as.Date("1980-01-01"), as.Date("2010-01-01"), by="+1 day")
# present.summer <- Filter(function(date) {month(date) %in% c(7, 8, 9, 10)}, present)
# future <- seq(as.Date("2070-01-01"), as.Date("2099-12-31"), by="+1 day")
# future.summer <- Filter(function(date) {month(date) %in% c(7, 8, 9, 10)}, future)
# 
# # Write a function to find the average habitat for a species in a time period
# mean_habitat <- function(species, time, ...) {
#   one_model <- function(model, ...) {
#     print(glue("starting on {model} for {time} {species}"))
#     if (time=="present") {fnames <- sapply(present.summer, function(date, ...) {glue("{model}_{species}/{model}_{species}_{date}_mean.grd")})}
#     if (time=="future") {fnames <- sapply(future.summer, function(date, ...) {glue("{model}_{species}/{model}_{species}_{date}_mean.grd")})}
#     return(mean(rast(fnames)))
#   }
#   model_means <- lapply(c("GFDL", "HAD", "IPSL"), one_model)
#   return(mean(rast(model_means)))
# }
# 
# system.time(blwh_present <- mean_habitat("blwh", "present"))
# system.time(blwh_future <- mean_habitat("blwh", "future"))
# system.time(lbst_present <- mean_habitat("lbst", "present"))
# system.time(lbst_future <- mean_habitat("lbst", "future"))
# 
# setwd("/Users/kailafrazer/")
# # Save work
# writeRaster(blwh_present, "Desktop/MCS/Honors/Habitat/blwh_mean_present_habitat_30.tif", overwrite=T)
# writeRaster(blwh_future, "Desktop/MCS/Honors/Habitat/blwh_mean_future_habitat_30.tif", overwrite=T)
# writeRaster(lbst_present, "Desktop/MCS/Honors/Habitat/lbst_mean_present_habitat_30.tif", overwrite=T)
# writeRaster(lbst_future, "Desktop/MCS/Honors/Habitat/lbst_mean_future_habitat_30.tif", overwrite=T)

#### Plot #### 

# Read in data created and saved in the first part of this file
blwh_present <- rast("Desktop/MCS/Honors/Habitat/blwh_mean_present_habitat_30.tif")
blwh_future <- rast("Desktop/MCS/Honors/Habitat/blwh_mean_future_habitat_30.tif")
lbst_present <- rast("Desktop/MCS/Honors/Habitat/lbst_mean_present_habitat_30.tif")
lbst_future <- rast("Desktop/MCS/Honors/Habitat/lbst_mean_future_habitat_30.tif")

lbst_threshold <- 0.501748 # Threshold found in earlier script
blwh_threshold <- 0.5033957 # Threshold found in earlier script

blwh_presc <- as.contour(blwh_present, levels=c(blwh_threshold)); blwh_futc <- as.contour(blwh_future, levels=c(blwh_threshold)); lbst_presc <- as.contour(lbst_present, levels=c(lbst_threshold)); lbst_futc <- as.contour(lbst_future, levels=c(lbst_threshold))

# Collect NMS SpatVectors

cb <- vect("Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/cb/cbnms_py.shp")
mb <- vect("Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/mb/mbnms_py.shp")
ch <- vect("Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/ch/chnms_py.shp")
ci <- vect("Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/ci/cinms_py.shp")
gf <- vect("Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/gf/gfnms_py.shp")

# Set theme pre-plotting
theme <- theme_set(theme_classic())
theme_set(theme(axis.title=element_text(size=10,face="bold"), axis.text=element_text(size=8), legend.text=element_text(size=8), legend.title=element_text(size=10,face="bold"), plot.title=element_text(size=12,face="bold",hjust=0),panel.grid.major=element_blank(),panel.border=element_rect(color="black",fill=NA,linewidth=1),axis.line=element_blank()))

whale <- ggdraw() + draw_image("Desktop/MCS/blwh silhouette fancy.jpg")
turtle <- ggdraw() + draw_image("Desktop/MCS/lbst silhouette.jpg")

blwh_presp <- ggplot() + geom_spatraster(data=blwh_present) + 
  geom_spatvector(data=blwh_presc, color="#FFFFFF", fill=NA, linewidth=.4) +
  #geom_spatvector(data=blwh_presc, mapping=aes(color="Core Habitat -\n>0.50 for Whales\n>0.45 for Turtles"), fill=NA, linewidth=.8) + 
  coord_sf() + 
  #scale_color_manual("", values=c("Core Habitat -\n>0.50 for Whales\n>0.45 for Turtles" = "#cf4446")) + 
  scale_fill_viridis_c("Habitat\nSuitability", na.value="white", limits=c(0,1)) + labs(title="A. 1980-2009") +theme_classic()+theme_get()+theme(axis.text.x=element_blank())+inset_element(whale, left=.6, bottom=.65, right=.9, top=.95)

blwh_futp <- ggplot() + geom_spatraster(data=blwh_future) + 
  geom_spatvector(data=blwh_futc, color="#FFFFFF", fill=NA, linewidth=.4) +
  coord_sf() + 
  scale_fill_viridis_c("Habitat\nSuitability", na.value="white", limits=c(0,1)) + 
  labs(title="B. 2070-2099") +theme_classic()+theme_get()+theme(legend.position="none",axis.text=element_blank()) + inset_element(whale, left=.6, bottom=.65, right=.9, top=.95)

blwh_anomp <- ggplot() + 
  geom_spatraster(data=(blwh_future-blwh_present)) + coord_sf() +
  scale_fill_gradient2("Suitability\nDifference", high="#31688e", low="#cf4446", mid="white", midpoint=0, na.value="white", limits=c(-.2, .3)) + 
  geom_spatvector(data=cb, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=ci, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=mb, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=ch, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=gf, color="grey30", fill=NA, linewidth=.4)+
  labs(title="C. Difference") +theme_classic()+theme_get()+theme(axis.text=element_blank())+inset_element(whale, left=.6, bottom=.65, right=.9, top=.95)

lbst_presp <- ggplot() + geom_spatraster(data=lbst_present) + geom_spatvector(data=lbst_presc, color="#FFFFFF", fill=NA, linewidth=.4) + coord_sf() + scale_fill_viridis_c("Habitat\nSuitability",na.value="white", limits=c(0,1)) + 
  labs(title="D. 1980-2009") + theme_classic()+theme(legend.position="none")+theme_get()+ inset_element(turtle, left=.6, bottom=.65, right=.9, top=.95)

lbst_futp <- ggplot() + geom_spatraster(data=lbst_future) + geom_spatvector(data=lbst_futc, color="#FFFFFF", fill=NA, linewidth=.4) + coord_sf() + scale_fill_viridis_c("Habitat\nSuitability",na.value="white", limits=c(0,1)) + 
  labs(title="E. 2070-2099") + theme_classic()+ theme_get() + theme(legend.position="none",axis.text.y=element_blank()) +
  inset_element(turtle, left=.6, bottom=.65, right=.9, top=.95)

lbst_anomp <- ggplot() + geom_spatraster(data=(lbst_future-lbst_present)) + coord_sf() + 
  geom_spatvector(data=cb, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=ci, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=mb, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=ch, color="grey30", fill=NA, linewidth=.4) + geom_spatvector(data=gf, color="grey30", fill=NA, linewidth=.4)+scale_fill_gradient2("Suitability\nDifference", high="#31688e", low="#cf4446", mid="white", midpoint=0, na.value="white", limits=c(-.2, .3)) + labs(title="F. Difference") + theme(legend.position="none") + theme_classic()+theme_get()+theme(axis.text.y=element_blank())+inset_element(turtle, left=.6, bottom=.65, right=.9, top=.95)

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("Fig5.tiff", width = 7.5, height = 5.6, units = "in", res = 300)

(blwh_presp | blwh_futp | blwh_anomp) / (lbst_presp | lbst_futp | lbst_anomp) + plot_layout(guides="collect",axes="collect") # saving 1500x1000 pixels is good to keep W visible on x-axis

dev.off()
