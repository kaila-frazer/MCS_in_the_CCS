#### Libraries ####
library(dplyr)
library(vroom)
library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)

#### Gather data ####

# Original satellite tracks
blwh_tracks <- readRDS("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/BLWH Tracks.rds")[[1]] %>% filter(presabs==1)
lbst_tracks <- vroom("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/LBST Tracks.csv")

# Read in predicted, suitable, historical habitat for the species created in the first part of the "9. Build Fig 5.R" script
blwh_present <- rast("/Users/kailafrazer/Desktop/MCS/Honors/Habitat/blwh_mean_present_habitat_30.tif")
lbst_present <- rast("/Users/kailafrazer/Desktop/MCS/Honors/Habitat/lbst_mean_present_habitat_30.tif")

# Convert the tracks to SpatVectors
blwh_tracks_vect <- as_spatvector(blwh_tracks, geom=c("lon", "lat"), crs="WGS84")
lbst_tracks_vect <- as_spatvector(lbst_tracks, geom=c("lon", "lat"), crs="WGS84")
# Crop lbst_tracks_vect to the domain of the California Current
lbst_tracks_vect <- crop(lbst_tracks_vect, lbst_present)

#### Plot ####

# Set theme
theme <- theme_set(theme_classic())
theme_set(theme(axis.title=element_text(size=10,face="bold"), axis.text=element_text(size=8), legend.text=element_text(size=8), legend.title=element_text(size=10,face="bold"), plot.title=element_text(size=12,face="bold",hjust=0),panel.grid.major=element_blank(),panel.border=element_rect(color="black",fill=NA,linewidth=1),axis.line=element_blank()))

# Plot

blwh<- ggplot() + geom_spatraster(data=blwh_present) + 
  geom_spatvector(data=blwh_tracks_vect, fill=NA, shape=".", 
                  size=.1, aes(color="Animal\nTrack")) +
  scale_color_manual("", values=c("Animal\nTrack"="#F8766D"), 
                     guide = guide_legend(override.aes = list(shape=16, size = 3))) +
  coord_sf() + 
  scale_fill_viridis_c("Habitat\nSuitability", na.value="white", limits=c(0,1)) + 
  labs(title="Blue Whale Suitability and Tracks") +
  theme_classic() + theme_get()

lbst <- ggplot() + geom_spatraster(data=lbst_present) + 
  geom_spatvector(data=lbst_tracks_vect, fill=NA, shape=".", 
                  size=.1, aes(color="Animal\nTrack")) +
                    scale_color_manual("", values=c("Animal\nTrack"="#F8766D"), 
                                       guide = guide_legend(override.aes = list(shape=16, size = 3))) +
  coord_sf() + 
  scale_fill_viridis_c("Habitat\nSuitability", na.value="white", limits=c(0,1)) + 
  labs(title="Leatherback Suitability and Tracks") +
  theme_classic() + theme_get() + theme(axis.text.y=element_blank())

# Take a look!
blwh
lbst

# Save arranged plots

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("S7Fig.tiff", width = 7, height = 4, units = "in", res = 300)

(blwh | lbst) + plot_layout(guides="collect")

dev.off()
