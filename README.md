# Marine cold-spells in the California Current System
This repository contains code used for analysis for a paper currently in review.

## Build Fig 1
Code to create Fig 1, which displays a map of the California Current System and its National Marine Sanctuaries.

Inputs required:
To run this code, you'll need the five NMS Shapefiles included in this repository. You'll also need to download a raster file of bathymetry data for the California Current via the ERDDAP server (https://www.ncei.noaa.gov/erddap/index.html). 

## Build Fig 2
Fig 2 creates a graphical representation of how marine cold-spells are detected in the paper.

Inputs required:
To make this figure, you'll need a sea surface temperature time series. Here, we use dynamically downscaled sea surface temperature data which was published in Pozo Buil et al. (2021) and is available upon request from Mercedes Pozo Buil.

## Build Fig 3
Please note that the data processing required for this figure may take a long time (days on a normal computer).

Inputs required:
This figure requires datasets of predicted and projected marine cold-spells in the California Current, which are stored in this repository in the "MCSs Detected" folder. Additionally, this script requires the roms_unique_id.grd file and the five NMS Shapefiles included in this repository.

## Data availability:
All scripts use previously published data. For x, y, and z, download from ERDDAP. For a, contact This Person. For b, contact This Person.

Please contact Kaila Frazer (kaila.frazer@unh.edu) for information or feedback.
