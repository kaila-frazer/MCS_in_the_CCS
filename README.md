# TODO

## Compile working R scripts into GitHub folder
## Combine MCS files if possible
## Clarify outputs in MCS code
## Add MCS files to GitHub
## Edit how MCS files get called, especially normal MCSs in habitat detection file and for the MCS stats in sanctuaries figure
## Combine habitat threshold files
## Skim code for each figure, editing inputs and outputs
## Final read over

# Marine cold-spells in the California Current System
This repository contains code used for analysis for a paper currently in review. Each R script contributing to the paper is described below. The scripts should be run in the order in which they are presented below.

## Detect marine cold-spells
**** Please note that this script is computationally intensive to run (allocate a day to run on a supercomputer).

Inputs required:
****

## Calculate habitat threshold
****

Inputs required:
****

## Measure habitat
**** the impacts of marine cold-spells on two species' suitable. Please note that this script is computationally intensive to run (allocate half a day to run on a supercomputer).

Inputs required:
This code requires many raster files of habitat suitability. These files are publicly available on ERDDAP (https://www.ncei.noaa.gov/erddap/index.html). It also requires the five NMS Shapefiles included in this repository. You'll also need dataframes of marine cold-spells detected in the system, which can be created via the Detect marine cold-spells.R script.

## Build Fig 1
Code to create Fig 1, which displays a map of the California Current System and its National Marine Sanctuaries.

Inputs required:
To run this code, you'll need the five NMS Shapefiles included in this repository. You'll also need to download a raster file of bathymetry data for the California Current via the ERDDAP server (https://www.ncei.noaa.gov/erddap/index.html). 

## Build Fig 2
Fig 2 creates a graphical representation of how marine cold-spells are detected in the paper.

Inputs required:
To make this figure, you'll need a sea surface temperature time series. Here, we use dynamically downscaled sea surface temperature data which was published in Pozo Buil et al. (2021) and is available upon request from Mercedes Pozo Buil.

## Build Fig 3
Please note that the data processing required for this figure may take a long time (a day on a normal computer).

Inputs required:
This figure requires datasets of predicted and projected marine cold-spells in the California Current, which are stored in this repository in the "MCSs Detected" folder. Additionally, this script requires the roms_unique_id.grd file and the five NMS Shapefiles included in this repository.

## Build Fig 4 (**and supps?)
****

Inputs required:
****

## Build Fig 5 (**and supps?)
****

Inputs required:
****

## Build Fig 6 and Supplementary Fig 5
Fig 6 shows how the study species responded to marine cold-spells across National Marine Sanctuaries and through time. Supplementary Fig 5 shows the standard deviations of the species' responses. This code also takes the averages of each species' responses to marine cold-spells; these average numbers are mentioned in the paper.

Inputs required:
These figures require the dataframes of species' responses to marine cold-spells generated in the Measure Habitat.R script.

## Build Fig 7 (**and supps?)
****

Inputs required:
****

Please contact Kaila Frazer (kaila.frazer@unh.edu) for information or feedback.
