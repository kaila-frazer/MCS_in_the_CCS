library(dplyr)
library(ggplot2)
library(sf)
library(cowplot)
library(rnaturalearth)
library(terra)
library(tidyterra)
library(ragg)

us <- ne_countries(continent="north america", returnclass="sf", scale="medium")

# Set your working directory
mb <- st_read("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/mb/mbnms_py.shp")
ci <- st_read("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/ci/cinms_py.shp")
ch <- st_read("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/ch/chnms_py.shp")
gf <- st_read("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/gf/gfnms_py.shp")
cb <- st_read("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/cb/cbnms_py.shp")

# Get bathymetry data
# Access a raster file of bathymetry data for the California Current on ERDDAP via https://www.ncei.noaa.gov/erddap/index.html 
z <- rast("/Users/kailafrazer/Desktop/MCS/Marine Cold-Spell Manuscript/Original Submission Figures/Fig 3/z.grd") 
shelf <- as.contour(z, levels=c(-200))
isobath <- as.contour(z, levels=c(-2000))

theme <- theme_set(theme_classic())
theme_set(theme(plot.title=element_text(size=12, hjust=0,face="bold"), axis.title=element_text(size=10,face="bold"), legend.title=element_text(size=10,face="bold"), legend.text=element_text(size=10), axis.text=element_text(size=8), panel.background=element_rect(fill="lightblue"), panel.grid.major=element_blank(), panel.border = element_rect(colour = "black", fill=NA, linewidth=1)))


calimap <- ggplot()+
  geom_sf(data=us, aes(fill="\n6. Land\n")) +
  geom_spatvector(data=shelf, fill=NA, linewidth=.8, aes(color="200m Shelf Break")) +
  geom_spatvector(data=isobath, fill=NA, linewidth=.8, aes(color="2000m Isobath")) +
  geom_sf(data=mb, aes(fill="\n3. Monterey Bay (MB)\n"))+
  geom_sf(data=ci, aes(fill="\n5. Channel Islands (CI)\n"))+
  geom_sf(data=gf, aes(fill="\n1. Greater Farallones (GF)\n"))+
  geom_sf(data=cb, aes(fill="\n2. Cordell Bank (CB)\n"))+
  geom_sf(data=ch, aes(fill="\n4. Chumash Heritage (CH)\n"))+
  coord_sf(xlim=c(-118,-130), ylim=c(32,44))+
  scale_color_manual("Bathymetry", values=c("NMS" = "#ffc3b1", "200m Shelf Break" = "lightblue4", "2000m Isobath" = "blue4")) +
  scale_fill_manual("National Marine Sanctuaries", values=c("\n6. Land\n" = "white", "\n1. Greater Farallones (GF)\n"="#fde725", "\n2. Cordell Bank (CB)\n"="#5ec962", "\n3. Monterey Bay (MB)\n"="#21918c", "\n4. Chumash Heritage (CH)\n"="#3b528b", "\n5. Channel Islands (CI)\n"="#440154"))+xlab("Longitude") + ylab("Latitude")+
  theme(axis.text.x=element_text(margin=margin(t=5)), legend.background = element_rect(color="black"))+guides(linetype = guide_legend(override.aes = list(fill = "white")))


americamap <- ggplot(data=us)+geom_sf(fill="white")+coord_sf(xlim=c(-175,-20), expand=T)+geom_rect(xmin=-118, xmax=-130, ymin=32, ymax=44, fill=NA, color="black", linewidth=0.6)+theme(axis.text=element_blank(), axis.ticks=element_blank())

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("Fig1.tiff", width = 7, height = 7, units = "in", res = 300)

ggdraw(calimap) + draw_plot(americamap, x=.39, y=.69, width=.25, height=.20) # 800 x 500 pixels at 72 dpi

dev.off()

