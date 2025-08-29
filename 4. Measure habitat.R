# This code was developed by Kaila Frazer. This script in particular contains a lot of references to file paths. I opted to include my actual file paths in the script because I think the actual paths read more clearly than dummy paths. The only places I edited file paths was for files which are actually included in the GitHub repository. Please be sure to edit the file paths according to your machine before running.

#0. Libraries ####

library(vroom)
library(terra)
library(dplyr)
library(glue)
library(foreach)
library(doParallel)
library(lubridate)

#1. Save mean habitat rasters - one time run ####

# Create a function to save mean rasters given month, decade, and model
create_mean_monthly_rasters <- function(decade, month, model) { # must enter month as two digits. model must be all caps
  # Get lists of files
  if (model == "IPSL") {
    blwh_list <- list.files(glue("C:/Users/kaila/Dropbox/Species_projections/kaila_working_feb7/IPSL_blwh/"), pattern=glue("IPSL_blwh_{decade}-{month}-.._mean.grd"))}
  if (model != "IPSL") {
    blwh_list <- list.files(glue("C:/Users/kaila/Dropbox/Species_projections/{model}_blwh/"), pattern=glue("{model}_blwh_{decade}-{month}-.._mean.grd"))
  }
  lbst_list <- list.files(glue("C:/Users/kaila/Dropbox/Species_projections/{model}_lbst/"), pattern=glue("{model}_lbst_{decade}-{month}-.._mean.grd"))

  # Stack and average the rasters
  if (model == "IPSL") {
    blwh_mean <- rast(glue("C:/Users/kaila/Dropbox/Species_projections/kaila_working_feb7/IPSL_blwh/{blwh_list}")) %>% mean()
  }
  if (model != "IPSL") {
    blwh_mean <- rast(glue("C:/Users/kaila/Dropbox/Species_projections/{model}_blwh/{blwh_list}")) %>% mean()
  }
  lbst_mean <- rast(glue("C:/Users/kaila/Dropbox/Species_projections/{model}_lbst/{lbst_list}")) %>% mean()

  # Write rasters
  writeRaster(blwh_mean, glue("C:/Users/kaila/Dropbox/Kaila_newMCSs/Species Projections/blwh_month-means/{model}_blwh_{decade}-{month}_mean.grd"), overwrite=T)
  writeRaster(lbst_mean, glue("C:/Users/kaila/Dropbox/Kaila_newMCSs/Species Projections/lbst_month-means/{model}_lbst_{decade}-{month}_mean.grd"), overwrite=T)
  # Check in on the user :)
  print(glue("Saved rasters for {decade} {month} {model}."))
}

decades <- c("1980", "1990", "2000", "2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090")
months <- c("06", "07", "08", "09", "10")
models <- c("GFDL", "HAD", "IPSL")
combinations <- expand.grid(decades, months, models)
system.time(do.call(mapply, c("create_mean_monthly_rasters", unname(as.list(combinations)))))

#2. Parallel process to get monthly mean habitat - one time run ####

# Establish a dataframe of the combinatons I'm interested in
decades <- c("1980", "1990", "2000", "2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090")
months <- c("06", "07", "08", "09", "10")
models <- c("GFDL", "HAD", "IPSL")
combinations <- expand.grid(models, decades, months)
rm(decades); rm(months); rm(models)

# Set up the parallel process
start=Sys.time()
detectCores()
registerDoParallel(2)

# Execute parallel process
NMS_contemporaneous_habitat=foreach(i=1:nrow(combinations), # nrow(combinations) is 180; try starting with 10
                                    .packages = c("terra", "glue", "tidyverse", "foreach"),.verbose=T,
                                    .export = ls(globalenv()), # I think this exports all the objects I have in R into the parallel process, so make sure my environment is clean before starting this
                                    .combine = c("rbind")) %dopar% {
                                      # Establish conditions for the combination
                                      input=combinations[i,]
                                      model=as.character(input$Var1)
                                      decade=as.character(input$Var2)
                                      month=as.character(input$Var3)

                                      # Establish conditions for the function runs
                                      # Establish necessary conditions to pass into the parallel process
                                      # Set a CRS example to project the NMS shapefile
                                      crs_ex <- rast("C:/Users/kaila/Dropbox/Kaila_newMCSs/Species Projections/GFDL_blwh/GFDL_blwh_1980-01-01_mean.grd")
                                      # Set thresholds
                                      blwh_threshold <- 0.5033957
                                      lbst_threshold <- 0.501748

                                      # Define functions
                                      # Create a function to output mean habitat for each sanctuary
                                      find_mean_sanctuary_habitat <- function(sanctuary, ...) { # sanctuary should be "gf", "cb", "mb", "ch", or "ci"
                                        # Open and project shapefile for the sanctuary
                                        sanct_shape <- vect(glue("/NMS Shapefiles/{sanctuary}/{sanctuary}nms_py.shp")) %>% project(crs(crs_ex))

                                        # Crop rasts to sanctuary
                                        blwh_sanct <- blwh_mean %>% mask(sanct_shape) %>% crop(sanct_shape)
                                        lbst_sanct <- lbst_mean %>% mask(sanct_shape) %>% crop(sanct_shape)

                                        # Find core habitat
                                        blwh_hab <- length(which(values(blwh_sanct) >= blwh_threshold))
                                        lbst_hab <- length(which(values(lbst_sanct) >= lbst_threshold))

                                        return(c(blwh_hab, lbst_hab))
                                      }

                                      # Establish conditions for function call
                                      # Read in averaged habitat rasters for the whole CCS
                                      blwh_mean <- rast(glue("C:/Users/kaila/Dropbox/Kaila_newMCSs/Species Projections/blwh_month-means/{model}_blwh_{decade}-{month}_mean.grd"))
                                      lbst_mean <- rast(glue("C:/Users/kaila/Dropbox/Kaila_newMCSs/Species Projections/lbst_month-means/{model}_lbst_{decade}-{month}_mean.grd"))

                                      # Set conditions to call the sanctuary habitat function
                                      sanctuaries <- c("gf", "cb", "mb", "ch", "ci")
                                      output = as.data.frame(foreach(sanctuary=sanctuaries, .combine="rbind") %do% find_mean_sanctuary_habitat(sanctuary)) %>% tibble::rownames_to_column("sanctuary")

                                      # Add conditions of the call to the output dataframe
                                      output$model = model
                                      output$decade = decade
                                      output$month = month

                                      # Return!
                                      return(output)
                                    }

# Make output legible
NMS_contemporaneous_habitat$sanctuary[NMS_contemporaneous_habitat$sanctuary=="result.1"] <- "gf"
NMS_contemporaneous_habitat$sanctuary[NMS_contemporaneous_habitat$sanctuary=="result.2"] <- "cb"
NMS_contemporaneous_habitat$sanctuary[NMS_contemporaneous_habitat$sanctuary=="result.3"] <- "mb"
NMS_contemporaneous_habitat$sanctuary[NMS_contemporaneous_habitat$sanctuary=="result.4"] <- "ch"
NMS_contemporaneous_habitat$sanctuary[NMS_contemporaneous_habitat$sanctuary=="result.5"] <- "ci"
NMS_contemporaneous_habitat <- NMS_contemporaneous_habitat %>% rename("blwh" = "V1", "lbst" = "V2")

# Export
write.csv(NMS_contemporaneous_habitat, "C:/Users/kaila/Dropbox/Kaila_newMCSs/Species Projections/monthly_mean_habitats.csv")

# End the data analysis
end=Sys.time()
dif=end-start # takes 1.5 mins for 10 combinations
print(dif)

#3. Detect detrended habitat changes ####

# Reminder: Step 1 is to clear the global environment! We only want necessary objects exported to parallel process runs

# Create a database of sanctuary sizes - will be used for relative change
{# Load NMS SpatVectors
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
nms_cells <- rbind(cb_cells, mb_cells, ch_cells, ci_cells, gf_cells)}
# Find the sizes of each NMS
sanctuary_sizes = data.frame(sanctuary=c("cb", "ci", "ch", "mb", "gf"), sizes=c(nrow(cb_cells), nrow(ci_cells), nrow(ch_cells), nrow(mb_cells), nrow(gf_cells)))
# Clean up after yourself
rm(uqid, cb, mb, ch, ci, gf, cb_cells, mb_cells, ch_cells, ci_cells, gf_cells)

# Establish base conditions for the run
model <- "gfdl"; upper <- toupper(model)
mcs <- vroom(glue("{GitHub directory}/MCSs Detected/mcs_{model}_detrended.csv")) %>% right_join(nms_cells) %>% dplyr::filter(month(date_start) >= 06, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 06, year(date_start) <= 2099)
th <- vroom("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/monthly_mean_habitats.csv") %>% filter(model==upper) # th for typical habitat
crs_ex <- rast("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/GFDL_blwh/GFDL_blwh_1980-01-01_mean.grd")
rm(nms_cells)

# Start the parallel process
start=Sys.time()
registerDoParallel(19)

# Run the parallel process - calling it dd for detrended delta :)
dd <- foreach(i=1:nrow(mcs), 
              .packages = c("terra", "glue", "lubridate", "dplyr", "foreach", "vroom"),
              .verbose = T,
              .export = ls(globalenv()),
              .combine = c("rbind")) %dopar% {
                # Establish base conditions
                inpt <- mcs[i,]; upper <- toupper(model)
                # Get dates of MCS
                dates <- seq(as.Date(inpt$date_start), as.Date(inpt$date_end), by="+1 day")
                # Clean up the dates
                dates <- dates[!(format(dates,"%m") == "02" & format(dates, "%d") == "29")]
                month <- month(dates[round(length(dates)/2)])
                if (nchar(month)==1) {month <- as.character(glue("0{month}"))}
                month_options <- c(06, 07, 08, 09, 10)
                if (as.integer(month) %in% month_options == F) {month <- month_options[(which(abs(month_options - as.numeric(month)) == min(abs(month_options - as.numeric(month)))))]}
                if (nchar(month)==1) {month <- as.character(glue("0{month}"))}
                year <- year(dates[round(length(dates)/2)])
                decade <- year - (year %% 10)
                prev_decade <- decade - 10
                next_decade <- decade + 10
                
                # List files for the MCS
                if (model == "IPSL") {
                  blwh_list = foreach(d=1:length(dates), .combine=c) %do% glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/kaila_working_feb7/IPSL_blwh/IPSL_blwh_{dates[d]}_mean.grd")}
                if (model != "IPSL") {
                  blwh_list = foreach(d=1:length(dates), .combine=c) %do% glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/{toupper(model)}_blwh/{toupper(model)}_blwh_{dates[d]}_mean.grd")}
                lbst_list = foreach(d=1:length(dates), .combine=c) %do% glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/{toupper(model)}_lbst/{toupper(model)}_lbst_{dates[d]}_mean.grd")
                
                # Stack and average the rasters
                blwh_mean <- rast(blwh_list) %>% mean()
                lbst_mean <- rast(lbst_list) %>% mean()
                
                # Prepare for the sanctuary function
                blwh_threshold <- 0.5033957; lbst_threshold <- 0.501748
                
                # Define a function to find mcs habitat by sanctuary
                find_mcs_sanctuary_habitat <- function(sanctuary, ...) { # sanctuary should be lowercase abbreviation
                  # Open and project shapefile for the sanctuary
                  sanct_shape <- vect(glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/NMS Shapefiles/{sanctuary}/{sanctuary}nms_py.shp")) %>% project(crs(crs_ex))
                  
                  # Crop rasts to sanctuary
                  blwh_sanct <- blwh_mean %>% mask(sanct_shape) %>% crop(sanct_shape)
                  lbst_sanct <- lbst_mean %>% mask(sanct_shape) %>% crop(sanct_shape)
                  
                  # Find core habitat
                  blwh_hab <- length(which(values(blwh_sanct) >= blwh_threshold))
                  lbst_hab <- length(which(values(lbst_sanct) >= lbst_threshold))
                  # Return
                  return(c(blwh_hab, lbst_hab))
                }
                
                # Call the function
                sanctuaries <- c("gf", "cb", "mb", "ch", "ci")
                outpt = as.data.frame(foreach(sanctuary=sanctuaries, .combine="rbind") %do% find_mcs_sanctuary_habitat(sanctuary)) %>% tibble::rownames_to_column("sanctuary")
                
                # Organize output
                outpt$id <- i
                outpt$sanctuary[outpt$sanctuary=="result.1"] <- "gf"
                outpt$sanctuary[outpt$sanctuary=="result.2"] <- "cb"
                outpt$sanctuary[outpt$sanctuary=="result.3"] <- "mb"
                outpt$sanctuary[outpt$sanctuary=="result.4"] <- "ch"
                outpt$sanctuary[outpt$sanctuary=="result.5"] <- "ci"
                outpt <- outpt %>% rename("blwh_mcs" = "V1", "lbst_mcs" = "V2")
                outpt$month <- month; outpt$decade <- decade
                
                # Gather typical habitat data
                {outpt$blwh_typ2 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==decade)]
                outpt$lbst_typ2 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==decade)]
                if (decade == "2090") {
                  outpt$blwh_typ1 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==prev_decade)]
                  outpt$blwh_typ3 <- (outpt$blwh_typ1 + outpt$blwh_typ2) / 2
                  outpt$lbst_typ1 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==prev_decade)]
                  outpt$lbst_typ3 <- (outpt$lbst_typ1 + outpt$lbst_typ2) / 2
                }
                if (decade == "1980") {
                  outpt$blwh_typ3 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==next_decade)]
                  outpt$blwh_typ1 <- (outpt$blwh_typ3 + outpt$blwh_typ2) / 2
                  outpt$lbst_typ3 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==next_decade)]
                  outpt$lbst_typ1 <- (outpt$lbst_typ3 + outpt$lbst_typ2) / 2
                }
                if (decade != "2090" & decade != "1980") {
                  outpt$blwh_typ1 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==prev_decade)]
                  outpt$blwh_typ3 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==next_decade)]
                  outpt$lbst_typ1 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==prev_decade)]
                  outpt$lbst_typ3 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==next_decade)]
                }
                outpt$blwh_typ <- (outpt$blwh_typ1 + outpt$blwh_typ2 + outpt$blwh_typ3) / 3
                outpt$lbst_typ <- (outpt$lbst_typ1 + outpt$lbst_typ2 + outpt$lbst_typ3) / 3}
                
                # Calculate habitat changes
                outpt$blwh_dl <- outpt$blwh_mcs - outpt$blwh_typ
                outpt$lbst_dl <- outpt$lbst_mcs - outpt$lbst_typ
                # Calculate relative change
                outpt <- left_join(outpt, sanctuary_sizes)
                outpt$blwh_reldl <- outpt$blwh_dl/outpt$sizes*100
                outpt$lbst_reldl <- outpt$lbst_dl/outpt$sizes*100
                
                return(outpt)
              }

end=Sys.time()
dif=end-start; print(dif)
write.csv(dd, glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Habitat Detected/contemp_newthresh_{model}.csv"))

#4. Detect normal habitat changes ####

# This will be very similar to #3. except 1980-2010 will serve as the use case for all MCSs.

# Reminder: Step 1 is to clear the global environment! We only want necessary objects exported to parallel process runs

# Create a database of sanctuary sizes - will be used for relative change
{# Load NMS SpatVectors
  uqid <- rast("{GitHub directory}/roms_unique_id.grd")
  # Load shapefiles of the NMSs
  cb <- vect("{GitHub directory}/NMS Shapefiles/cb/cbnms_py.shp")
  mb <- vect("{GitHub directory}/NMS Shapefiles/mb/mbnms_py.shp")
  ch <- vect("{GitHub directory}/NMS Shapefiles/ch/chnms_py.shp")
  ci <- vect("{GitHub directory}/NMS Shapefiles/ci/cinms_py.shp")
  gf <- vect("{GitHub directory}/NMS Shapefiles/gf/gfnms_py.shp")
  # Use SpatVectors of NMSs to find the cell ids within the NMSs
  cb_cells <- terra::extract(uqid, cb) %>% na.omit() %>% dplyr::select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="cb")
  mb_cells <- terra::extract(uqid, mb) %>% na.omit() %>% dplyr::select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="mb")
  ch_cells <- terra::extract(uqid, ch) %>% na.omit() %>% dplyr::select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="ch")
  ci_cells <- terra::extract(uqid, ci) %>% na.omit() %>% dplyr::select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="ci")
  gf_cells <- terra::extract(uqid, gf) %>% na.omit() %>% dplyr::select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="gf")
  nms_cells <- rbind(cb_cells, mb_cells, ch_cells, ci_cells, gf_cells)}
# Find the sizes of each NMS
sanctuary_sizes = data.frame(sanctuary=c("cb", "ci", "ch", "mb", "gf"), sizes=c(nrow(cb_cells), nrow(ci_cells), nrow(ch_cells), nrow(mb_cells), nrow(gf_cells)))
# Clean up after yourself
rm(uqid, cb, mb, ch, ci, gf, cb_cells, mb_cells, ch_cells, ci_cells, gf_cells)

# Establish base conditions for the run
model <- "ipsl"; upper <- toupper(model)
mcs_n <- normal_mcs <- vroom(glue("{GitHub directory}/MCSs Detected/mcs_{model}_normal.csv")) %>% filter(climatology=="traditional") %>% right_join(nms_cells) %>% dplyr::filter(month(date_start) >= 06, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 06, year(date_start) <= 2099)
th <- vroom("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/monthly_mean_habitats.csv") %>% filter(model==upper) # th for typical habitat - we created this file earlier in this script
crs_ex <- rast("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/GFDL_blwh/GFDL_blwh_1980-01-01_mean.grd")
rm(nms_cells)

# Start the parallel process
start=Sys.time()
registerDoParallel(19)

# Run the parallel process - calling it dd for detrended delta :)
dd <- foreach(i=1:nrow(mcs_n), #nrow(mcs_n)
              .packages = c("terra", "glue", "lubridate", "dplyr", "foreach", "vroom"),
              .verbose = T,
              .export = ls(globalenv()),
              .combine = c("rbind")) %dopar% {
                # Establish base conditions
                inpt <- mcs_n[i,]; upper <- toupper(model)
                # Get dates of MCS
                dates <- seq(as.Date(inpt$date_start), as.Date(inpt$date_end), by="+1 day")
                # Clean up the dates
                dates <- dates[!(format(dates,"%m") == "02" & format(dates, "%d") == "29")]
                month <- month(dates[round(length(dates)/2)])
                if (nchar(month)==1) {month <- as.character(glue("0{month}"))}
                month_options <- c(06, 07, 08, 09, 10)
                if (as.integer(month) %in% month_options == F) {month <- month_options[(which(abs(month_options - as.numeric(month)) == min(abs(month_options - as.numeric(month)))))]}                
                year <- year(dates[round(length(dates)/2)])
                decade <- year - (year %% 10)
                
                # List files for the MCS
                if (model == "IPSL") {
                  blwh_list = foreach(d=1:length(dates), .combine=c) %do% glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/kaila_working_feb7/IPSL_blwh/IPSL_blwh_{dates[d]}_mean.grd")}
                if (model != "IPSL") {
                  blwh_list = foreach(d=1:length(dates), .combine=c) %do% glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/{toupper(model)}_blwh/{toupper(model)}_blwh_{dates[d]}_mean.grd")}
                lbst_list = foreach(d=1:length(dates), .combine=c) %do% glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Species Projections/{toupper(model)}_lbst/{toupper(model)}_lbst_{dates[d]}_mean.grd")
                
                # Stack and average the rasters
                blwh_mean <- rast(blwh_list) %>% mean()
                lbst_mean <- rast(lbst_list) %>% mean()
                
                # Prepare for the sanctuary function
                blwh_threshold <- 0.5033957; lbst_threshold <- 0.501748
                
                # Define a function to find mcs habitat by sanctuary
                find_mcs_sanctuary_habitat <- function(sanctuary, ...) { # sanctuary should be lowercase abbreviation
                  # Open and project shapefile for the sanctuary
                  sanct_shape <- vect(glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/NMS Shapefiles/{sanctuary}/{sanctuary}nms_py.shp")) %>% project(crs(crs_ex))
                  
                  # Crop rasts to sanctuary
                  blwh_sanct <- blwh_mean %>% mask(sanct_shape) %>% crop(sanct_shape)
                  lbst_sanct <- lbst_mean %>% mask(sanct_shape) %>% crop(sanct_shape)
                  
                  # Find core habitat
                  blwh_hab <- length(which(values(blwh_sanct) >= blwh_threshold))
                  lbst_hab <- length(which(values(lbst_sanct) >= lbst_threshold))
                  # Return
                  return(c(blwh_hab, lbst_hab))
                }
                
                # Call the function
                sanctuaries <- c("gf", "cb", "mb", "ch", "ci")
                outpt = as.data.frame(foreach(sanctuary=sanctuaries, .combine="rbind") %do% find_mcs_sanctuary_habitat(sanctuary)) %>% tibble::rownames_to_column("sanctuary")
                
                # Organize output
                outpt$id <- i
                outpt$sanctuary[outpt$sanctuary=="result.1"] <- "gf"
                outpt$sanctuary[outpt$sanctuary=="result.2"] <- "cb"
                outpt$sanctuary[outpt$sanctuary=="result.3"] <- "mb"
                outpt$sanctuary[outpt$sanctuary=="result.4"] <- "ch"
                outpt$sanctuary[outpt$sanctuary=="result.5"] <- "ci"
                outpt <- outpt %>% rename("blwh_mcs" = "V1", "lbst_mcs" = "V2")
                outpt$month <- month; outpt$decade <- decade
                
                # Gather typical habitat data
                {outpt$blwh_typ2 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==1990)]
                  outpt$lbst_typ2 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==1990)]
                  outpt$blwh_typ1 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==1980)]
                  outpt$blwh_typ3 <- th$blwh[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==2000)]
                  outpt$lbst_typ1 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==1980)]
                  outpt$lbst_typ3 <- th$lbst[which(th$sanctuary==outpt$sanctuary & th$month==month & th$decade==2000)]}
                  outpt$blwh_typ <- (outpt$blwh_typ1 + outpt$blwh_typ2 + outpt$blwh_typ3) / 3
                  outpt$lbst_typ <- (outpt$lbst_typ1 + outpt$lbst_typ2 + outpt$lbst_typ3) / 3
                
                # Calculate habitat changes
                outpt$blwh_dl <- outpt$blwh_mcs - outpt$blwh_typ
                outpt$lbst_dl <- outpt$lbst_mcs - outpt$lbst_typ
                # Calculate relative change
                outpt <- left_join(outpt, sanctuary_sizes)
                outpt$blwh_reldl <- outpt$blwh_dl/outpt$sizes*100
                outpt$lbst_reldl <- outpt$lbst_dl/outpt$sizes*100
                
                return(outpt)
              }

end=Sys.time()
dif=end-start; print(dif)

write.csv(dd, glue("/Users/EcoCast/Dropbox/Kaila_newMCSs/Habitat Detected/contemp_newthresh_{model}_normal.csv"))