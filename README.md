# Marine cold-spells in the California Current System
This repository contains code used for analysis for a paper currently in review. Each R script contributing to the paper is described below.

#### Key to acronyms:

* MCS = Marine cold-spell
* NMS = National Marine Sanctuary

## 1. Detect normal MCSs
Detect marine cold-spells using a historical climatology.

We run this code three times, once for each of the three downscaled climate models. This code produces a unique identifier raster which should only be produced once; after running the code the first time, skip that step. Please note that this script is computationally intensive to run (allocate a day to run on a supercomputer).

Inputs required:
To make this figure, you'll need a netCDF of sea surface temperature data. Here, we use dynamically downscaled sea surface temperature data which was published in Pozo Buil et al. 2021.

## 2. Detect detrended MCSs
This script is very similar to the previous one (we also run it three times for three models), but it detects marine cold-spells from a detrended temperature time series.

Inputs required:
To make this figure, you'll need a netCDF of sea surface temperature data. Here, we use dynamically downscaled sea surface temperature data which was published in Pozo Buil et al. 2021.

## 3. Calculate habitat thresholds
Calculate a threshold for core species habitat using the dismo() package in R. We chose the equal sensitivity and specificity threshold.

Inputs required:
Calculating the threshold requires access to the original species distribution models as well as the original data. We've included the accurate thresholds where necessary in the code going forward, so accessing these models isn't necessary to get most of the rest of the code to work.

## 4. Measure habitat
Here, we create dataframes of the impacts of marine cold-spells on two species' suitable habitat within California's National Marine Sanctuaries. Please note that this script is computationally intensive to run (allocate half a day to run on a supercomputer).

Inputs required:
This code requires many raster files of habitat suitability. These files are publicly available on ERDDAP (https://www.ncei.noaa.gov/erddap/index.html). It also requires the five NMS Shapefiles and unique raster key included in this repository. You'll also need dataframes of marine cold-spells detected in the system, which can be created from the first two scripts in this repository.

## 5. Build Fig 1
Code to create Fig 1, which displays a map of the California Current System and its National Marine Sanctuaries.

Inputs required:
To run this code, you'll need the five shapefiles included in this repository. You'll also need to download a raster file of bathymetry data for the California Current via the ERDDAP server (https://www.ncei.noaa.gov/erddap/index.html). 

## 6. Build Fig 2
Fig 2 creates a graphical representation of how marine cold-spells are detected in the paper.

Inputs required:
To make this figure, you'll need a sea surface temperature time series. Here, we use dynamically downscaled sea surface temperature data which was published in Pozo Buil et al. 2021.

## 7. Build Fig 3 and supplementary Fig 4
Build a figure displaying typical marine cold-spells occuring per year across the California Current. The supplementary figure breaks down the results by climate model. Please note that the data processing required for this figure may take a long time (a day on a normal computer).

Inputs required:
This figure requires datasets of predicted and projected marine cold-spells in the California Current created in the first two scripts in this repository. Additionally, this script requires the unique raster key file and the five NMS Shapefiles included in this repository.

## 8. Build Fig 4
Build a figure to display changes in marine cold-spells within California's National Marine Sanctuaries.

Inputs required:
This figure requires datasets of marine cold-spell occurrences created in the first two scripts. These datasets are also available in the repository.

## 9. Build Fig 5
Build a figure that shows changes from the historical period to the current period in species habitat across the California Current.

Inputs required:
This code requires many raster files of habitat suitability. These files are publicly available on ERDDAP (https://www.ncei.noaa.gov/erddap/index.html).

## Build Fig 6 and supplementary Fig 5
Fig 6 shows how the study species responded to marine cold-spells across National Marine Sanctuaries and through time. Supplementary Fig 5 shows the standard deviations of the species' responses. This code also takes the averages of each species' responses to marine cold-spells; these average numbers are mentioned in the paper.

Inputs required:
These figures require the dataframes of species' responses to marine cold-spells generated in the Measure Habitat.R script.

## Build Fig 7
Build a figure comparing species distribution model response curves with modeled sea surface temperature data. The goal of this figure is to understand the mechanisms behind modeled species' response to marine cold-spells.

*Inputs required:*
This figure requires the NMS shapefiles and unique raster key included in this repository. You'll also need access to the original species distribution models (we use models built by Abrahms et al. 2019 and Lezama Ochoa et al. 2024) and dynamically downscaled sea surface temperature data published in Pozo Buil et al. 2021.

Please contact Kaila Frazer (kaila.frazer@unh.edu) for information or feedback.
