library(dplyr)
library(ggplot2)
library(sf)
library(cowplot)
library(rnaturalearth)

us <- ne_countries(continent="north america", returnclass="sf", scale="medium")

# Set your working directory
#setwd("C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/") 
mb <- st_read("{GitHub directory}/NMS Shapefiles/mb/mbnms_py.shp")
ci <- st_read("{GitHub directory}/NMS Shapefiles/ci/cinms_py.shp")
ch <- st_read("{GitHub directory}/NMS Shapefiles/ch/chnms_py.shp")
gf <- st_read("{GitHub directory}/NMS Shapefiles/gf/gfnms_py.shp")
cb <- st_read("{GitHub directory}/NMS Shapefiles/cb/cbnms_py.shp")

# Get bathymetry data
# Access a raster file of bathymetry data for the California Current on ERDDAP via https://www.ncei.noaa.gov/erddap/index.html 
#z <- rast("C:/Users/kaila/Desktop/Marine Cold-Spell Manuscript/Figures/Fig 3/z.grd") 
shelf <- as.contour(z, levels=c(-200))
isobath <- as.contour(z, levels=c(-2000))

theme <- theme_set(theme_classic())
theme_set(theme(plot.title=element_text(size=20, hjust=0), axis.title=element_text(size=20), legend.title=element_text(size=16), legend.text=element_text(size=12), axis.text=element_text(size=16), panel.background=element_rect(fill="lightblue"), panel.grid.major=element_blank()))


calimap <- ggplot()+
  geom_sf(data=us, aes(fill="\nLand\n")) +
  geom_spatvector(data=shelf, fill=NA, linewidth=.8, aes(color="200m Shelf Break")) +
  geom_spatvector(data=isobath, fill=NA, linewidth=.8, aes(color="2000m Isobath")) +
  geom_sf(data=mb, aes(fill="\n3. Monterey Bay (MB)\n"))+
  geom_sf(data=ci, aes(fill="\n5. Channel Islands (CI)\n"))+
  geom_sf(data=gf, aes(fill="\n1. Greater Farallones (GF)\n"))+
  geom_sf(data=cb, aes(fill="\n2. Cordell Bank (CB)\n"))+
  geom_sf(data=ch, aes(fill="\n4. Chumash Heritage (CH)\n"))+
  coord_sf(xlim=c(-118,-130), ylim=c(32,44))+
  scale_color_manual("Bathymetry", values=c("NMS" = "#ffc3b1", "200m Shelf Break" = "lightblue4", "2000m Isobath" = "blue4")) +
  scale_fill_manual("National Marine Sanctuaries", values=c("\nLand\n" = "white", "\n1. Greater Farallones (GF)\n"="#fde725", "\n2. Cordell Bank (CB)\n"="#5ec962", "\n3. Monterey Bay (MB)\n"="#21918c", "\n4. Chumash Heritage (CH)\n"="#3b528b", "\n5. Channel Islands (CI)\n"="#440154"))+labs(xlab="Longitude", ylab="Latitude")+
  theme(axis.text.x=element_text(angle=45,margin=margin(t=5)), legend.background = element_rect(color="black"))+guides(linetype = guide_legend(override.aes = list(fill = "white")))


americamap <- ggplot(data=us)+geom_sf(fill="white")+coord_sf(xlim=c(-175,-20), expand=T)+geom_rect(xmin=-118, xmax=-130, ymin=32, ymax=44, fill=NA, color="black", linewidth=0.6)+theme(axis.text=element_blank(), axis.ticks=element_blank())

ggdraw(calimap) + draw_plot(americamap, x=.37, y=.78, width=.18, height=.20) # 800 x 500 pixels